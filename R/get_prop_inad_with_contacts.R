get_prop_inad_with_contacts <- function(human_specimen, afp_data, end_date = Sys.Date()) {

  start_date <- lubridate::floor_date(end_date %m-% lubridate::years(1), unit = "year")

  contacts_hs_regex <- human_specimen |>
    dplyr::mutate(is_contact = dplyr::if_else(stringr::str_detect(SpecimenEPID,
                                                                  "-[Cc]+[0-9]+"),
                                              TRUE, FALSE)) |>
    dplyr::filter(is_contact)

  case_epids_w_contacts <- unique(contacts_hs_regex$EPID)

  stool_data <- generate_stool_data(afp_data, start_date, end_date)

  prop_inad_contacts <- stool_data |>
    dplyr::mutate(quarter = lubridate::quarter(date)) |>
    dplyr::select(year, quarter, ctry, epid, adequacy.final2) |>
    dplyr::filter(adequacy.final2 == "Inadequate") |>
    dplyr::mutate(has_contacts = dplyr::if_else(epid %in% case_epids_w_contacts, TRUE, FALSE)) |>
    dplyr::group_by(year, ctry, quarter) |>
    dplyr::summarize(w_contacts = sum(has_contacts, na.rm = TRUE),
                     n = dplyr::n()) |>
    dplyr::mutate(prop_inad_w_contact = round(w_contacts / n * 100))

  # Generate the full table
  full_table <- tidyr::expand_grid(
    year = c(year(end_date) - 1, year(end_date)),
    ctry = unique(afp_data$place.admin.0),
    quarter = 1:4
  )

  prop_inad_contacts <- dplyr::left_join(full_table, prop_inad_contacts) |>
    tidyr::replace_na(list(w_contacts = 0, n = 0))

  prop_inad_contacts_label <- prop_inad_contacts |>
    dplyr::mutate(prop_inad_w_contact_label = paste0(prop_inad_w_contact, " (", w_contacts, "/", n, ")")) |>
    dplyr::select(year, ctry, quarter, prop_inad_w_contact_label) |>
    tidyr::pivot_wider(names_from = year, values_from = prop_inad_w_contact_label)

  prop_inad_contacts_diff <- prop_inad_contacts |>
    dplyr::select(year, ctry, quarter, w_contacts) |>
    tidyr::pivot_wider(names_from = year, values_from = w_contacts)
  prop_inad_contacts_diff["comparison"] <- prop_inad_contacts_diff[[4]] - prop_inad_contacts_diff[[3]]
  prop_inad_contacts_diff["prop_diff"] <- round((prop_inad_contacts_diff["comparison"]) /
                                                  prop_inad_contacts_diff[, 3]* 100, 0)
  prop_inad_contacts_diff <- prop_inad_contacts_diff |>
    dplyr::mutate(prop_diff = dplyr::if_else(prop_diff == Inf,
                                             !!dplyr::sym(names(prop_inad_contacts_diff)[4]) * 100,
                                             prop_diff)) |>
    dplyr::select(ctry, quarter, comparison, prop_diff)

  final_table <- dplyr::left_join(prop_inad_contacts_label, prop_inad_contacts_diff)

  return(final_table)

}

#' Get proportion of AFP lab pendings
#'
#' @description
#' Obtains the number of lab pending in the AFP dataset from the previous 3 months
#' since the end_date specified.
#'
#' @param afp_data `tibble` AFP linelist.
#' @param end_date `str` End date of analysis. Defaults to current date.
#'
#' @returns `tibble` Summarizes the lab pending samples from the last three months.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' get_proportion_lab_pending(raw_data$afp)
#' }
get_proportion_lab_pending <- function(afp_data, end_date = Sys.Date()) {

  summary <- afp_data |>
    # only look at samples greater than 90 days
    dplyr::filter(stooltolabdate < lubridate::floor_date(end_date %m-% months(3), unit = "months")) |>
    dplyr::mutate(month = lubridate::month(stooltolabdate, label = TRUE),
                  year = lubridate::year(stooltolabdate)) |>
    dplyr::group_by(whoregion, country = place.admin.0) |>
    dplyr::summarize(pending_samples = sum(cdc.classification.all2 == "LAB PENDING", na.rm = TRUE),
                     prop_pending_samples = round(pending_samples / dplyr::n() * 100),
                     prop_label = paste0(prop_pending_samples, " (", pending_samples, "/", dplyr::n(), ")"),
                     .groups = "drop") |>
    dplyr::select(-prop_pending_samples)

  full_table <- tidyr::expand_grid(
    country = unique(afp_data$place.admin.0)
  ) |>
    dplyr::mutate(whoregion = get_region(country)) |>
    dplyr::mutate(whoregion = dplyr::case_when(
      country == "SAO TOME AND PRINCIPE" ~ "AFRO",
      country == "CYPRUS" ~ "EMRO",
      .default = whoregion
    ))

  final_summary <- full_table |>
    dplyr::left_join(summary) |>
    tidyr::replace_na(list(pending_samples = 0,
                           prop_label = "0 (0/0)"))

  return(final_summary)

}

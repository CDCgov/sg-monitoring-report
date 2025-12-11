#' Obtain the proportion of inadequate samples without classification
#'
#' @description
#' Approximates the ERCs' work and determining the number of cases classified.
#'
#'
#' @param afp_data `tibble` The AFP dataset.
#' @param end_date `str` End date of the analysis
#'
#' @returns `tibble` Summary of proportion cases classified by country and quarter.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' prop_classified <- get_prop_case_classified(raw_data$afp)
#' }
get_prop_case_classified <- function(afp_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)
  start_date <- lubridate::floor_date(end_date - lubridate::years(1), unit = "year")

  stool_data <- sirfunctions::generate_stool_data(afp_data, start_date, end_date)

  summary <- stool_data |>
    dplyr::mutate(month = lubridate::month(date, label = TRUE),
                  quarter = lubridate::quarter(date),
                  case_age = as.numeric(difftime(end_date, date, units = "days"))) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  case_age >= 90,
                  adequacy.final2 == "Inadequate") |>
    dplyr::select(dplyr::any_of(c("epid", "cdc.classification.all2",
                                  "dateonset",
                                  "classification", "classificationvdpv", # per POLIS API doc, `classification` is final classification
                                  "ctry", "whoregion",
                                  "year", "quarter")))

  pending_summary <- summary |>
    dplyr::group_by(ctry, year, quarter) |>
    dplyr::summarise(no_pending = sum(cdc.classification.all2 != "PENDING", na.rm = TRUE), # pending classification
                     cases = dplyr::n())

  full_grid <- tidyr::expand_grid(
    ctry = unique(afp_data$place.admin.0),
    year = c(lubridate::year(end_date) - 1, lubridate::year(end_date)),
    quarter = c(1,2,3,4))

  pending_summary <- dplyr::left_join(full_grid, pending_summary) |>
    tidyr::replace_na(list("no_pending" = 0, cases = 0))

  final_summary <- pending_summary |>
    dplyr::mutate(prop_pending = round(no_pending / cases * 100, 0),
                  prop_pending_label = paste0(prop_pending, " (", no_pending, "/", cases, ")")) |>
    dplyr::select(-dplyr::all_of(c("no_pending", "cases", "prop_pending"))) |>
    tidyr::pivot_wider(names_from = year, values_from = prop_pending_label) |>
    dplyr::arrange(ctry, quarter)

  return(final_summary)

}

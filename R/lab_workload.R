#' Calculate lab workload
#'
#' @description
#' Calculates the lab workload by country from the year of the end date and the
#' year before. Workload is defined as the number of samples sent in a year up to
#' the end date.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str` End date of the analysis.
#'
#' @returns `tibble` Summary of the lab workload by country.
#' @export
#'
#' @examples
lab_workload <- function(lab_data, end_date = Sys.Date()) {
  start_date <- lubridate::floor_date(end_date, unit = "years")
  prev_year_end_date <- Sys.Date() - lubridate::years(1)
  prev_year_start_date <- lubridate::floor_date(prev_year_end_date, unit = "years")

  prev_year_load <- lab_data |>
    dplyr::filter(dplyr::between(DateStoolReceivedinLab,
                                 prev_year_start_date, prev_year_end_date)) |>
    dplyr::mutate(month = lubridate::month(DateStoolReceivedinLab, label = TRUE),
                  year = lubridate::year(DateStoolReceivedinLab)) |>
    dplyr::group_by(whoregion, country, year, month) |>
    dplyr::summarise(n = dplyr::n())

  current_year_load <- lab_data |>
    dplyr::filter(dplyr::between(DateStoolReceivedinLab,
                                 start_date, end_date)) |>
    dplyr::mutate(month = lubridate::month(DateStoolReceivedinLab, label = TRUE),
                  year = lubridate::year(DateStoolReceivedinLab)) |>
    dplyr::group_by(whoregion, country, year, month) |>
    dplyr::summarise(n = dplyr::n())

  summary <- dplyr::bind_rows(prev_year_load, current_year_load) |>
    dplyr::filter(month %in% unique(current_year_load$month)) |>
    tidyr::pivot_wider(names_from = year, values_from = n)

  summary["difference"] <- summary[5] - summary[4]

  return(summary)

}

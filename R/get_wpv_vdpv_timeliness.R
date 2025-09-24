#' Obtains timeliness of results for WPV/VDPV samples
#'
#' @description
#' Calculates the median timeliness between onset date to date of final lab results for
#' the year specified in the end_date and the year before.
#'
#' @param pos `tibble` Positives dataset.
#' @param end_date `str` End date of analysis. Defaults to current date.
#' @param type `str` Either AFP or ENV.
#'
#' @returns `tibble` Summary of the timeliness interval
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data()
#' get_wpv_vdpv_timeliness(raw_data$pos)
#' }
get_wpv_vdpv_timeliness <- function(pos, end_date = Sys.Date(), type = "AFP") {

  end_date <- lubridate::as_date(end_date)
  month_end_date <- lubridate::month(end_date, TRUE)

  if (!type %in% c("AFP", "ENV")) {
    cli::cli_alert_warning("Only 'AFP' and 'ENV' are supported by the type parameter.")
  }

  summary <- pos |>
    dplyr::filter(source == type, whoregion %in% c("AFRO", "EMRO")) |>
    dplyr::mutate(month = lubridate::month(dateonset, label = TRUE),
                  year = lubridate::year(dateonset),
                  days.on.notif.hq = as.numeric(lubridate::as_date(datenotificationtohq) - dateonset)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month < month_end_date,
                  !is.na(days.on.notif.hq),
                  dplyr::between(days.on.notif.hq, 0, 365),
                  stringr::str_detect(measurement, "WILD|VDPV")) |>
    dplyr::group_by(whoregion, country = place.admin.0, year, month) |>
    dplyr::summarize(median = median(days.on.notif.hq, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year)

  # Get for other countries not in the positives file
  complete_dataset <- tidyr::expand_grid(
    country = unique(pos$place.admin.0),
    year = unique(summary$year),
    month = unique(summary$month)
  ) |>
    dplyr::left_join(pos |>
                       dplyr::select(country = place.admin.0, whoregion) |>
                       dplyr::distinct())

  summary <- dplyr::full_join(complete_dataset, summary)

  summary <- summary |>
    tidyr::pivot_wider(names_from = year, values_from = median)

  summary["comparison"] <- summary[, 5] - summary[, 4]
  summary <- summary |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "Same",
      comparison > 0 ~ "Increase",
      comparison < 0 ~ "Decrease",
      .default = "No data from both years"
    ))

  return(summary)

}

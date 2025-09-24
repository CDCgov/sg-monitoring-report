#' Obtain the timeliness interval between collection to final results of negative samples
#'
#' @description
#' Calculates the median timeliness of negative samples between collection date to final lab results.
#' The function will calculate for the year specified in the end date and the previous year.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str` End date of the analysis. Defaults to the current date.
#'
#' @returns `tibble` Summary table of timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' get_negative_lab_processing_timeliness(lab_data)
#' }
get_negative_lab_processing_timeliness <- function(lab_data, end_date = Sys.Date()) {
  end_date <- lubridate::as_date(end_date)
  month_end_date <- lubridate::month(end_date, TRUE)

  summary <- lab_data |>
    dplyr::mutate(month = lubridate::month(CaseDate, label = TRUE),
                  days.collect.notif.hq = as.numeric(difftime(DateNotificationtoHQ,DateStoolCollected, units = "days"))) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month < month_end_date,
                  !is.na(days.collect.notif.hq),
                  dplyr::between(days.collect.notif.hq, 0, 365),
                  FinalCellCultureResult %in% c("Negative", "NPEV", NA)
                  ) |>
    dplyr::group_by(whoregion, country, culture.itd.cat, year, month) |>
    dplyr::summarize(median = median(days.collect.notif.hq, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year)

  complete_dataset <- tidyr::expand_grid(
    country = unique(lab_data$country),
    year = unique(summary$year),
    month = unique(summary$month)
  ) |>
    dplyr::left_join(lab_data |>
                       dplyr::select(country, whoregion, culture.itd.cat) |>
                       dplyr::distinct())

  summary <- dplyr::full_join(complete_dataset, summary)
  summary <- summary |>
    tidyr::pivot_wider(names_from = year, values_from = median)

  summary["comparison"] <- summary[, 6] - summary[, 5]
  summary <- summary |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "Same",
      comparison > 0 ~ "Increase",
      comparison < 0 ~ "Decrease",
      .default = "No data from both years"
    ))

  return(summary)
}

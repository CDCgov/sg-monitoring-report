#' Get timeliness of stool collection to shipment
#'
#' @description
#' Calculates the median timeliness from the year specified at the end date for
#' the time interval between stool collection to arrival in lab.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str`End date of the analysis. Defaults to current date.
#' @param temporal_scale `str` Whether to group the results by month or quarter.
#'
#' @returns `tibble` Summary of stool collection to shipment timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' get_stool_shipment_timeliness(lab_data)
#' }
get_stool_shipment_timeliness <- function(lab_data, end_date = Sys.Date(), temporal_scale = "quarter") {

  temporal_scale <- stringr::str_to_lower(temporal_scale)

  if (!temporal_scale %in% c("month", "quarter")) {
    cli::cli_abort("Only 'month' and 'quarter' are valid arguments for temporal_scale.")
  }

  end_date <- lubridate::as_date(end_date)
  month_end_date <- lubridate::month(end_date, TRUE)

  if (temporal_scale == "quarter") {
    lab_data <- lab_data |>
        dplyr::mutate(quarter = lubridate::quarter(CaseDate))
  }

  summary <- lab_data |>
    dplyr::mutate(month = lubridate::month(CaseDate, label = TRUE),
                  days.collect.rec.lab = as.numeric(DateStoolReceivedinLab - DateStoolCollected)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month < month_end_date,
                  !is.na(days.collect.rec.lab),
                  dplyr::between(days.collect.rec.lab, 0, 365)) |>
    dplyr::group_by(whoregion, country, culture.itd.cat, year, !!dplyr::sym(temporal_scale)) |>
    dplyr::summarize(median = median(days.collect.rec.lab, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year)

  if (temporal_scale == "quarter") {
    complete_dataset <- tidyr::expand_grid(
      country = unique(lab_data$country),
      year = unique(summary$year),
      quarter = c(1,2,3,4)
    )
  } else {
    complete_dataset <- tidyr::expand_grid(
      country = unique(lab_data$country),
      year = unique(summary$year),
      month = unique(summary$month)
    )
  }

  complete_dataset <- complete_dataset |>
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

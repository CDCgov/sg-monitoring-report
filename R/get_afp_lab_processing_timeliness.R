#' Timeliness of lab processing
#'
#' @description
#' Calculates the median time AFP samples arrive in lab to final lab results
#' from the previous three months for the past three years.
#'
#' @param lab_data `tibble` Lab data with AFP samples.
#' @param end_date `str` End date of the analysis. Defaults to today's date.
#'
#' @returns `tibble` Summary table of median timeliness of sample received in lab
#' to final lab results.
#' @export
#'
#' @examples
#' \dontrun{
#' get_afp_lab_processing_timeliness(lab_data)
#' }
get_afp_lab_processing_timeliness <- function(lab_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)
  end_date_month <- end_date - months(1)
  start_date_month <- end_date - months(3)

  # Define the three month periods
  included_months <- dplyr::tibble(dates = seq(lubridate::floor_date(end_date - lubridate::years(3) - months(3)),
                                               lubridate::floor_date(end_date - months(1)),
                                               by = "months")) |>
    dplyr::mutate(month = months(dates, abbreviate = TRUE),
                  year = lubridate::year(dates)) |>
    dplyr::select(month, year) |>
    dplyr::filter(month %in% format(seq(lubridate::floor_date(end_date - months(3), unit = "months"),
                                                          end_date - months(1),
                                                          by = "months"), format = "%b"))

  valid_lab_data <- lab_data |>
    # Lab processing timeliness
    # From date stool received in lab to final rRTPCR results
    dplyr::mutate(days.rec.lab.final = as.numeric(difftime(DateNotificationtoHQ, DateStoolReceivedinLab, units = "days")),
                  year_month = lubridate::floor_date(CaseDate, unit = "months"),
                  month = lubridate::month(CaseDate, label = TRUE)) |>
    # Filter erroneous data
    dplyr::filter(dplyr::between(days.rec.lab.final, 0, 365))

  full_grid <- tidyr::expand_grid(
    country = unique(lab_data$country),
    year = unique(included_months$year),
    month = unique(included_months$month)) |>
    dplyr::right_join(dplyr::distinct(lab_data |> dplyr::select(whoregion, country)))

  summary <- valid_lab_data |>
    dplyr::group_by(whoregion, country, year, month) |>
    dplyr::summarize(median = as.numeric(median(days.rec.lab.final, na.rm = TRUE)), .groups = "drop")

  summary_full <- dplyr::left_join(full_grid, summary)

  current_year <- summary_full |>
    dplyr::filter(year == year(end_date)) |>
    dplyr::select(-year) |>
    dplyr::rename(!!paste0(year(end_date), " Median") := median)

  previous_years <- summary_full |>
    dplyr::filter(year != year(end_date)) |>
    dplyr::group_by(whoregion, country, month) |>
    dplyr::summarize(median = median(median, na.rm = TRUE)) |>
    dplyr::rename(!!paste0(year(end_date) - 3, "-", year(end_date) - 1, " Median") := median)

  summary <- dplyr::left_join(previous_years, current_year)

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

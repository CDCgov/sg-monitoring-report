#' Calculate Lab Timeliness Indicators
#'
#' @description
#' The function calculate the median timeliness interval for the current year up to the end date and
#' the median of the previous three years.
#'
#' @details
#' To request updated lab data, email Minh-Ly's group.
#'
#' The yearly median is only calculated up to the same month/day of the end date starting on Jan 1st.
#' That is if the end date is 2025-08-15, then the 2025 median will be from 2025-01-01 to 2025-08-15,
#' the 2024 median will be from 2024-01-01 to 2024-08-15. The three year median will be based on
#' 2022-2024 medians.
#'
#' @param lab_data `tibble` Lab data.
#' @param end_date `str` End date of the calculations. Defaults to `Sys.Date()`.
#'
#' @returns `tibble` A summary of median timeliness
#' @export
#'
#' @examples
#' \dontrun{
#' lab_data <- readr::read_csv("lab_data.csv")
#' lab_timely_indicators(lab_data)
#' }
lab_timely_indicators <- function(lab_data, end_date = Sys.Date()) {
  lab_data <- get_lab_intervals(lab_data)
  end_date <- lubridate::as_date(end_date)
  month_day_cutoff <- format(end_date, "%m-%d")
  current_year <- lubridate::year(end_date)
  previous_years <- (current_year - 3):(current_year - 1)

  timely_intervals <- c(
    "days.lab.culture",
    "days.culture.itd",
    "days.seq.ship",
    "days.seq.rec.res"
  )

  # Calculate medians for previous 3 years (same period as current year)
  medians_3_years <- NULL
  for (i in timely_intervals) {
    indicator_medians <- get_year_lab_median(lab_data, i, previous_years, month_day_cutoff)
    # Take median of the yearly medians for each region/country
    summary <- indicator_medians |>
      dplyr::group_by(whoregion, country) |>
      dplyr::summarize(
        interval = i,
        `Median of Last 3 Years` = median(median, na.rm = TRUE),
        .groups = "drop"
      )
    medians_3_years <- dplyr::bind_rows(medians_3_years, summary)
  }

  # Current year median
  median_current_year <- NULL
  for (i in timely_intervals) {
    indicator_medians <- get_year_lab_median(lab_data, i, current_year, month_day_cutoff)
    summary <- indicator_medians |>
      dplyr::mutate(interval = i) |>
      dplyr::rename(!!paste0(current_year, " Median") := median) |>
      dplyr::select(whoregion, country, interval, !!paste0(current_year, " Median"))
    median_current_year <- dplyr::bind_rows(median_current_year, summary)
  }

  lab_interval_summary <- dplyr::left_join(
    medians_3_years, median_current_year,
    by = c("whoregion", "country", "interval")
  )

  lab_interval_summary <- lab_interval_summary |>
    dplyr::mutate(
      comparison = .data[[paste0(current_year, " Median")]] - `Median of Last 3 Years`
    ) |>
    dplyr::arrange(interval, dplyr::desc(comparison))

  # Create combinations of region, country, and interval
  complete_table <- tidyr::expand_grid(
    country = unique(lab_data$country),
    interval = c(
      "days.lab.culture",
      "days.culture.itd",
      "days.seq.ship",
      "days.seq.rec.res"
    )) |>
    dplyr::left_join(
      lab_data |>
        dplyr::distinct(country, whoregion))

  # Full join
  lab_interval_summary <- dplyr::full_join(complete_table, lab_interval_summary)
  lab_interval_summary <- lab_interval_summary |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "Same",
      comparison > 0 ~ "Increase",
      comparison < 0 ~ "Decrease",
      .default = "No data available for both years"
    ))

  return(lab_interval_summary)
}

# Private function

#' Obtain the median of medians across years
#'
#' @description
#' The function calculates the median interval for the lab timeliness indicators.
#' It summarizes data at the yearly level and then takes the median/average of the years
#' between the start and end dates.
#'
#'
#' @param lab_data `tibble` Lab data obtained from WHO SLD team.
#' @param indicator `str` Lab timeliness interval. Valid values are:
#' - days.lab.culture
#' - days.culture.itd
#' - days.seq.ship
#' - days.seq.rec.res
#' @param years `int` a vector of years
#' @param month_day_cutoff `str`The month-day of the end date. Ensures years are calculated
#' from January to up to the month-day cutoff.
#'
#' @returns `tibble` Summary of the median
#' @keywords internal
#'
#' @examples
get_year_lab_median <- function(lab_data, indicator, years, month_day_cutoff) {
  valid_indicators <- c(
    "days.lab.culture",
    "days.culture.itd",
    "days.seq.ship",
    "days.seq.rec.res"
  )
  if (!indicator %in% valid_indicators) {
    cli::cli_alert_warning("Not a valid indicator. Please use: ")
    cli::cli_li(valid_indicators)
    cli::cli_abort("Pass a valid indicator and try again.")
  }
  indicator_filter <- switch(
    indicator,
    "days.lab.culture" = "t1",
    "days.culture.itd" = "t2",
    "days.seq.ship" = "t3",
    "days.seq.rec.res" = "t4"
  )

  # For each year, filter from Jan 1 to month_day_cutoff
  medians <- lapply(years, function(y) {
    start_date <- as.Date(sprintf("%d-01-01", y))
    end_date <- as.Date(sprintf("%d-%s", y, month_day_cutoff))
    lab_data |>
      dplyr::filter(
        dplyr::between(DateStoolCollected, start_date, end_date),
        !!dplyr::sym(indicator_filter)
      ) |>
      dplyr::group_by(whoregion, country) |>
      dplyr::summarize(
        median = median(.data[[indicator]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(year = y)
  })
  medians <- dplyr::bind_rows(medians)

  return(medians)
}

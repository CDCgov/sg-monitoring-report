#' Build AFP Cases Reported Summary
#'
#' Calculates AFP case counts for the current 6-month rolling window and
#' compares to the same months across the prior 3 years. Creates threshold
#' using a +/-50% around the historical median. The window is dynamic and handles
#' cross-year periods (e.g. Oct 2025 - Mar 2026).
#'
#'
#' @param afp_data A data frame containing AFP case data. Must include
#'   \code{dateonset} and \code{place.admin.0} columns.
#' @param end_date Date to end the current reporting window.
#'   Defaults to \code{Sys.Date()}. Typically passed as the last day of the previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country-month containing:
#'     \code{place.admin.0}, \code{whoregion}, \code{month_label},
#'     \code{current_period_counts}, \code{prior_3yr_median},
#'     \code{prior_yrs_w_data}, \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, period start/end
#'     dates, human-readable period labels, number of months and prior years
#'     assessed, and the threshold rule applied.}
#'     }
#'
#' @examples
#' \dontrun{
#' end_date <- ceiling_date(Sys.Date() %m-% months(1), unit = "month") %m-% days(1)
#' result <- build_afp_cases_reported(raw_data$afp, end_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_afp_cases_reported <- function(afp_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data)
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)
  window_start <- lubridate::floor_date(end_date %m-% months(5), unit = "month")

  current_months <- seq(window_start, end_date, by = "month")

  # create all possible combinations for current 6 months assessment
  current_combos <- tibble::tibble(
    year      = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month     = lubridate::month(current_months, label = TRUE, abbr = TRUE)
  )

  # create all possible combinations for prior 3 years assessment
  prior_combos <- dplyr::bind_rows(
    dplyr::mutate(current_combos, year = year - 1),
    dplyr::mutate(current_combos, year = year - 2),
    dplyr::mutate(current_combos, year = year - 3)
  )


  # Period Labels -----
  # create labels from dates to export with final data
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(end_date, "%b %Y"))
  prior_period_label <- paste0(format(window_start %m-% lubridate::years(1:3), "%b %Y" )," - ",format(end_date %m-% lubridate::years(1:3), "%b %Y"), collapse = ", ")


  # Prepare Data -----

  # Prepare month and year columns
  afp_prep <- afp_data  |>
    dplyr::mutate(
      year = lubridate::year(dateonset),
      month_num = lubridate::month(dateonset),
      month = lubridate::month(dateonset, label = TRUE, abbr = TRUE))


  # Current Period Counts -----

  # Counts of cases
  current_counts <- afp_prep |>
    inner_join(current_combos, by = c("year", "month_num", "month")) |>  #limit to only current 6 months, safer to join by month_num instead of month
    dplyr::group_by(place.admin.0, month_num, month) |>
    dplyr::summarize(current_period_counts = dplyr::n(), .groups = "drop")

  # Create a full grid of all combinations for completeness (country, year, month)
  current_full <- tidyr::expand_grid(
    place.admin.0 = unique(afp_prep$place.admin.0),
    current_combos |> dplyr::select(year, month_num, month)) |>
    # Combine
    dplyr::left_join(current_counts, by = c("place.admin.0", "month_num", "month")) |>
    dplyr::mutate(current_period_counts = tidyr::replace_na(current_period_counts, 0))


  # Prior Period Counts -----
  prior_counts <- afp_prep |>
    inner_join(prior_combos, by = c("year", "month_num")) |>  #limit to the comparison periods of prior three years
    dplyr::group_by(place.admin.0, year, month_num) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  prior_full <- tidyr::expand_grid(
    place.admin.0 = unique(afp_prep$place.admin.0),
    prior_combos |> dplyr::select(year, month_num, month)) |>
    dplyr::left_join(prior_counts, by = c("place.admin.0", "year", "month_num")) |>
    dplyr::group_by(place.admin.0, month_num, month) |>
    dplyr::summarize(prior_3yr_median = round(median(n, na.rm = TRUE)),
                     prior_yrs_w_data = sum(!is.na(n)),
                     .groups = "drop"
                     )

  # Join for full table -----
  final_summary <- current_full |>
    dplyr::left_join(prior_full, by = c("place.admin.0", "month_num", "month")) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    # add region
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(place.admin.0),
      # percent change from prior 3-year median
      perc_change = dplyr::case_when(
        prior_3yr_median == 0 & current_period_counts == 0 ~ 0,
        prior_3yr_median == 0 & current_period_counts > 0 ~ Inf,
        TRUE ~ round((current_period_counts - prior_3yr_median) / prior_3yr_median * 100)
      ),
      # flag
      flag = dplyr::case_when(
        is.na(prior_3yr_median) ~ "Incomplete Data",
        prior_3yr_median == 0 & current_period_counts == 0 ~ "Within Target",
        prior_3yr_median == 0 & current_period_counts > 0 ~ "Above Target",
        perc_change > 50 ~ "Above Target",
        perc_change < -50 ~ "Below Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review")
      ) |>
    dplyr::select(place.admin.0, whoregion, month_label, current_period_counts, prior_3yr_median, prior_yrs_w_data, perc_change, flag)

 # Return -----
  meta <- list(
    indicator_code = "AFP_cases_reported",
    indicator_label = "AFP cases reported",
    unit = "Month",
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 3,
    threshold_rule = "+/-50% of 3-year median",
    definition = "Number of AFP cases reported. Within Target if the number of AFP cases reported for the month is within +/-50% of the prior 3-year median for that month. Below Target if AFP cases reported are more than 50% lower than the prior 3-year median.",
    above_target_definition = "Above Target if AFP cases reported are more than 50% higher than the prior 3-year median.",
    incomplete_data_definition = "Incomplete Data if no prior AFP data are available for all 3 prior years.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta))

}

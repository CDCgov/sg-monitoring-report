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
#'   Defaults to `Sys.Date()`. Typically passed as the last day of the previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country-month containing:
#'     \code{place.admin.0}, \code{whoregion}, \code{month_label},
#'     \code{current_period_counts}, \code{prior_median},
#'     \code{prior_yrs_w_data}, \code{lower_50pct}, \code{upper_50pct},
#'     and \code{flag}.}
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
    "dateonset column required" = "dateonset" %in% names(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data)
  )

  # Date Windows -----
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
    left_join(current_counts, by = c("place.admin.0", "month_num", "month")) |>
    dplyr::mutate(current_period_counts = tidyr::replace_na(current_period_counts, 0))


  # Prior Period Counts -----
  prior_counts <- afp_prep |>
    inner_join(prior_combos, by = c("year", "month_num")) |>  #limit to the comparison periods of prior three years
    dplyr::group_by(place.admin.0, year, month_num) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  prior_full <- tidyr::expand_grid(
    place.admin.0 = unique(afp_prep$place.admin.0),
    prior_combos |> dplyr::select(year, month_num, month)) |>
    left_join(prior_counts, by = c("place.admin.0", "year", "month_num")) |>
    group_by(place.admin.0, month_num, month) |>
    dplyr::summarize(prior_median = round(median(n, na.rm = TRUE)),
                     prior_yrs_w_data = sum(!is.na(n)),
                     .groups = "drop"
                     )

  # Join for full table -----
  final_summary <- current_full |>
    left_join(prior_full, by = c("place.admin.0", "month_num", "month")) |>
    mutate(month_label = paste0(month, " ", year)) |>
    select(-year, -month, -month_num) |>
    # add region
    mutate(
      # add region
      whoregion = sirfunctions::get_region(place.admin.0),
      # thresholds
      upper_50pct = prior_median * 1.5,
      lower_50pct = pmax(0, prior_median *.5),  # floor at 0 for safety
      # flag
      flag = case_when(
        is.na(prior_median) ~ "Incomplete Data",
        current_period_counts <= upper_50pct & current_period_counts >= lower_50pct ~ "Within Target",
        current_period_counts > upper_50pct ~ "Above Target",
        current_period_counts < lower_50pct ~ "Below Target",
        TRUE ~ "Review")
      ) |>
    dplyr::select(place.admin.0, whoregion, month_label, current_period_counts, prior_median, prior_yrs_w_data, lower_50pct, upper_50pct, flag)

 # Return -----
  meta <- list(
    indicator_code = "AFP_cases_reported",
    indicator_label = "AFP cases reported",
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 3,
    threshold_rule = "±50% of 3-year median"
  )

  return(list(
    data = final_summary,
    metadata = meta))

}

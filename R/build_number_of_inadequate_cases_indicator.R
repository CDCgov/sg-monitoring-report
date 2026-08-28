#' Build Number of Inadequate Cases Summary
#'
#' Calculates inadequate AFP case counts for the current 6-month rolling window
#' and compares to the same months across the prior 3 years. Creates threshold
#' using a +/-50% around the historical median. The window is dynamic and handles
#' cross-year periods (e.g. Oct 2025 - Mar 2026).
#'
#' @details
#' Inadequate cases are defined using \code{sirfunctions::generate_stool_data()}
#' and filtering to \code{adequacy.final2 == "Inadequate"}. The generated
#' \code{date} field is used to assign cases to months.
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
#' result <- build_number_of_inadequate_cases(raw_data$afp, end_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_number_of_inadequate_cases <- function(afp_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "dateonset column required" = "dateonset" %in% names(afp_data),
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

  # Generate stool data for current and prior comparison windows.
  # This creates adequacy.final2, which is used to define inadequate cases.
  stool_start_date <- window_start %m-% lubridate::years(3)

  stool_data <- sirfunctions::generate_stool_data(
    afp_data,
    start_date = stool_start_date,
    end_date = end_date)

  inad_prep <- stool_data |>
    dplyr::filter(adequacy.final2 == "Inadequate") |>
    dplyr::mutate(
      place.admin.0 = ctry,
      date = lubridate::as_date(date),
      year = lubridate::year(date),
      month_num = lubridate::month(date),
      month = lubridate::month(date, label = TRUE, abbr = TRUE))


  # Current Period Counts -----

  # Counts of inadequate cases
  current_counts <- inad_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(place.admin.0, month_num, month) |>
    dplyr::summarize(current_period_counts = dplyr::n(), .groups = "drop")

  # Create a full grid of all combinations for completeness (country, year, month)
  current_full <- tidyr::expand_grid(
    place.admin.0 = unique(afp_data$place.admin.0),
    current_combos |> dplyr::select(year, month_num, month)) |>
    # Combine
    dplyr::left_join(current_counts, by = c("place.admin.0", "month_num", "month")) |>
    dplyr::mutate(current_period_counts = tidyr::replace_na(current_period_counts, 0))


  # Prior Period Counts -----
  prior_counts <- inad_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num")) |>
    dplyr::group_by(place.admin.0, year, month_num) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  prior_full <- tidyr::expand_grid(
    place.admin.0 = unique(afp_data$place.admin.0),
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
        prior_3yr_median == 0 & current_period_counts > 0 ~ "Below Target",
        perc_change > 50 ~ "Below Target",
        perc_change < -50 ~ "Above Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review")
    ) |>
    dplyr::select(place.admin.0, whoregion, month_label, current_period_counts, prior_3yr_median, prior_yrs_w_data, perc_change, flag)

  # Return -----
  meta <- list(
    indicator_code = "number_inadequate_cases",
    indicator_label = "Number of inadequate cases",
    unit = "Month",
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 3,
    threshold_rule = "+/-50% of 3-year median",
    definition = "Number of inadequate AFP cases. Within Target if the number of inadequate cases reported in the most recent completed month is within +/-50% of the prior 3-year median for that month. Below Target if inadequate cases are more than 50% higher than the prior 3-year median.",
    above_target_definition = "Above Target if inadequate cases are more than 50% lower than the prior 3-year median.",
    incomplete_data_definition = "Incomplete Data if no prior inadequate AFP case data are available for all 3 prior years.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta))

}

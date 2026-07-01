#' Build Number of Active ES Sites Indicator
#'
#' Calculates the number of active environmental surveillance (ES) sites for
#' the current 6-month rolling window. Each month is compared to the same month
#' in the prior year. Returns results in long format with one row per country
#' per month.
#'
#' @details
#' A site is counted as active for a given assessment month if it has at least
#' 5 collections in the 12-month rolling period ending on the last day of that
#' month. The active-site count is anchored to each month end.
#'
#' A country showing 0 active sites may represent a country with ES activity
#' where no sites met the >= 5 collection threshold in that 12 month rolling
#' periods, or a country with no ES data at all. These cases are not
#' distinguished in the output and are both flagged
#' as \code{"No Current Active ES"}.
#'
#' @param es_data A data frame containing ES sample data. Must include
#'   \code{ADM0_NAME}, \code{collect.date}, and \code{site.name}.
#' @param end_date Date to end the current reporting window.
#'   Defaults to \code{Sys.Date()}. Typically passed as the last day of the
#'   previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country per month containing:
#'     \code{country}, \code{whoregion}, \code{month_label},
#'     \code{current_active_sites}, \code{prior_active_sites},
#'     \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing \code{indicator_code},
#'     \code{indicator_label}, \code{current_period_start},
#'     \code{current_period_end}, \code{current_period_label},
#'     \code{prior_period_label}, \code{n_current_months},
#'     \code{n_prior_years}, \code{threshold_rule}, \code{definition},
#'     and \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' end_date <- lubridate::floor_date(Sys.Date(), unit = "month") %m-% days(1)
#' result <- build_number_of_active_ES_sites(raw_data$es, end_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_number_of_active_ES_sites <- function(es_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "es_data must be a data frame" = is.data.frame(es_data),
    "ADM0_NAME column required" = "ADM0_NAME" %in% names(es_data),
    "collect.date column required" = "collect.date" %in% names(es_data),
    "site.name column required" = "site.name" %in% names(es_data)
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  window_start <- lubridate::floor_date(end_date %m-% months(5), unit = "month")
  current_months <- seq(window_start, end_date, by = "month")

  # create all possible combinations for current 6 months assessment
  # Unlike other indicators, current_combos includes period_end
  # as real dates because the 12-month rolling window is anchored to each
  # month's actual end date, not just a year/month label
  current_combos <- tibble::tibble(
    year = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month = lubridate::month(current_months, label = TRUE, abbr = TRUE),
    period_end = lubridate::ceiling_date(current_months, unit = "month") %m-% days(1)
  )

  # create all possible combinations for prior year for comparison
  prior_combos <- current_combos |>
    dplyr::mutate(
      year = year - 1,
      period_end = period_end %m-% lubridate::years(1))


  # Period Labels -----
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(end_date, "%b %Y"))
  prior_period_label   <- paste0(format(min(prior_combos$period_end) |>
                                          lubridate::floor_date(unit = "month"), "%b %Y"), " - ",
                                 format(max(prior_combos$period_end), "%b %Y"))


  # Assessment Windows -----
  # Bind all data windows and create dates needed for the rolling 12 month windows for each period
  # Rolling_start defines the beginning of the 12-month lookback window for each period
  assessment_windows <- dplyr::bind_rows(
    dplyr::mutate(current_combos, period_type = "current"),
    dplyr::mutate(prior_combos, period_type = "prior")) |>
    dplyr::mutate(rolling_start = period_end %m-% months(12) + days(1))


  # Prepare Data -----

  # Initial ES prep - remove observations with missing values for key data variables
  es_prep <- es_data |>
    dplyr::select(ADM0_NAME, collect.date, site.name) |>
    dplyr::mutate(collect.date = lubridate::as_date(collect.date)) |>
    dplyr::filter(!is.na(ADM0_NAME),
                  !is.na(collect.date),
                  !is.na(site.name))


  # Site monthly counts & last collection date
  # Create collections per site per calendar month
  site_month_counts <- es_prep |>
    dplyr::filter(dplyr::between(collect.date,
                                 min(assessment_windows$rolling_start),
                                 max(assessment_windows$period_end))) |>
    dplyr::mutate(collect_month = lubridate::floor_date(collect.date, unit = "month")) |> #set all to first of month for ease in grouping
    dplyr::group_by(ADM0_NAME, site.name, collect_month) |>
    dplyr::summarize(n_samples = dplyr::n(), # total samples per month
                     .groups = "drop")


  # Active Site Counts -----
  # cross_join pairs every site-month record with all 12 assessment windows.
  # between() then keeps only pairings where the collection month falls within
  # that assessment window's 12-month lookback.
  # group_by is on the assessment month (not collection month) so sum(n_samples)
  # collapses all qualifying collection months into a single 12-month rolling total
  # for that particular assessment month.
  # Sites with >= 5 samples qualify as active.

  active_site_counts <- site_month_counts |>
    dplyr::cross_join(assessment_windows) |>
    dplyr::filter(dplyr::between(collect_month, rolling_start, period_end)) |>
    dplyr::group_by(period_type, year, month_num, month, ADM0_NAME, site.name) |> # groups by assessment month, which is associated with all rolling 12 months in it's window
    dplyr::summarize(n_samples_12_mo = sum(n_samples), # samples in the rolling 12 months
                     .groups = "drop") |>
    dplyr::filter(n_samples_12_mo >= 5) |> # keep only Active sites that had at least 5 samples in the rolling 12 months
    dplyr::group_by(period_type, year, month_num, month, ADM0_NAME) |>
    dplyr::summarize(active_sites = dplyr::n(), .groups = "drop")


  # Current Period Counts -----
  current_counts <- active_site_counts |>
    dplyr::filter(period_type == "current") |>
    dplyr::select(-period_type) |>
    dplyr::rename(
      country = ADM0_NAME,
      current_active_sites = active_sites)


  # Prior Period Counts -----
  prior_counts <- active_site_counts |>
    dplyr::filter(period_type == "prior") |>
    dplyr::select(-period_type) |>
    dplyr::rename(
      country = ADM0_NAME,
      prior_active_sites = active_sites)


  # Full grid and join for full table -----
  # Note: after replace_na, a 0 may represent a country where no sites met the >= 5 threshold to be Active,
  # or could also represent a country that had no ES data at all in the window
  final_summary <- tidyr::expand_grid(
    country = unique(es_data$ADM0_NAME),
    current_combos |> dplyr::select(year, month_num, month)) |>
    dplyr::left_join(current_counts, by = c("country", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_counts |> dplyr::select(country, month_num, prior_active_sites),
      by = c("country", "month_num")) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(country),
      # for counts, NA is assumed as 0 active sites (either no ES or no Active ES)
      current_active_sites = tidyr::replace_na(current_active_sites, 0),
      prior_active_sites = tidyr::replace_na(prior_active_sites, 0),
      # Create percent change of prior year count
      perc_change = round((current_active_sites - prior_active_sites) /
                                     prior_active_sites * 100),
      flag = dplyr::case_when(
        current_active_sites == 0 & prior_active_sites == 0 ~ "No Current Active ES",
        is.infinite(perc_change) ~ "Above Target",  # prior = 0, current > 0
        perc_change < -50 ~ "Below Target",
        perc_change >  50 ~ "Above Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review"))|>
    dplyr::select(country, whoregion, month_label, current_active_sites, prior_active_sites, perc_change, flag)


  # Return -----
  meta <- list(
    indicator_code = "number_active_es_sites",
    indicator_label = "Number of active ES sites",
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 1,
    threshold_rule = "+/-50% of the same-month active-site count from the prior year",
    definition = "Number of active ES sites defined as sites with at least 5 collections over the past 12-month rolling period.",
    possible_statuses = c("Within Target", "Below Target", "Above Target",
                          "No Current Active ES", "Review")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

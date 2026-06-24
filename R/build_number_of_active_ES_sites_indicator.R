#' Build Number of Active ES Sites Indicator
#'
#' Calculates the number of active environmental surveillance (ES) sites for
#' the current 6-month rolling window. Each month is compared to the same month
#' in the prior year. Returns results in long format with one row per country
#' per month.
#'
#' @details
#' Active ES sites are calculated using the same logic as
#' \code{sirfunctions::get_es_site_age()}, but with a 5-collection threshold.
#' A site is counted as active if it has at least 5 collections in the prior
#' 12-month rolling period and has a sample collection history spanning at least
#' 12 months. The active-site count is anchored to each month end.
#'
#' @param es_data A data frame containing ES sample data. Must include
#'   \code{ADM0_NAME}, \code{collect.date}, and \code{site.name}.
#' @param end_date Date to end the current reporting window.
#'   Defaults to \code{Sys.Date()}. Typically passed as the last day of the
#'   previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country-month containing:
#'     \code{country}, \code{whoregion}, \code{month_label},
#'     \code{current_active_sites}, \code{prior_active_sites},
#'     \code{perc_change}, \code{lower_50pct}, \code{upper_50pct},
#'     and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, period start/end
#'     dates, human-readable period labels, number of months and prior years
#'     assessed, and the threshold rule applied.}
#' }
#'
#' @examples
#' \dontrun{
#' end_date <- lubridate::floor_date(Sys.Date(), unit = "month") - 1
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
  end_date <- lubridate::as_date(end_date)
  current_months <- rev(seq(lubridate::floor_date(end_date, unit = "month"),
                            by = "-1 month",
                            length.out = 6))
  window_start <- min(current_months)
  current_month_ends <- lubridate::ceiling_date(current_months, unit = "month") - 1

  # create all possible combinations for current 6 months assessment
  current_combos <- tibble::tibble(
    year = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month = lubridate::month(current_months, label = TRUE, abbr = TRUE),
    period_start = current_months,
    period_end = current_month_ends
  )

  # create all possible combinations for prior year
  prior_combos <- current_combos |>
    dplyr::mutate(
      year = year - 1,
      period_start = as.Date(paste(year, month_num, "01", sep = "-")),
      period_end = lubridate::ceiling_date(period_start, unit = "month") - 1
    )


  # Period Labels -----
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(end_date, "%b %Y"))
  prior_period_label <- paste0(format(min(prior_combos$period_start), "%b %Y"), " - ",
                               format(max(prior_combos$period_end), "%b %Y"))


  # Prepare Data -----
  es_prep <- es_data |>
    dplyr::select(ADM0_NAME, collect.date, site.name) |>
    dplyr::mutate(collect.date = lubridate::as_date(collect.date)) |>
    dplyr::filter(!is.na(ADM0_NAME),
                  !is.na(collect.date),
                  !is.na(site.name))

  assessment_windows <- dplyr::bind_rows(
    current_combos |>
      dplyr::mutate(period_type = "current"),
    prior_combos |>
      dplyr::mutate(period_type = "prior")
  ) |>
    dplyr::mutate(
      rolling_start = do.call(c, lapply(period_start, function(x) {
        seq(as.Date(x, origin = "1970-01-01"), by = "-11 months", length.out = 2)[2]
      }))
    )

  site_first_collection <- es_prep |>
    dplyr::filter(collect.date <= max(assessment_windows$period_end)) |>
    dplyr::group_by(ADM0_NAME, site.name) |>
    dplyr::summarize(first_collect_date = min(collect.date), .groups = "drop")

  site_month_counts <- es_prep |>
    dplyr::filter(dplyr::between(collect.date,
                                 min(assessment_windows$rolling_start),
                                 max(assessment_windows$period_end))) |>
    dplyr::mutate(collect_month = lubridate::floor_date(collect.date, unit = "month")) |>
    dplyr::group_by(ADM0_NAME, site.name, collect_month) |>
    dplyr::summarize(
      n_samples = dplyr::n(),
      latest_collect_date = max(collect.date),
      .groups = "drop"
    )

  active_site_counts <- site_month_counts |>
    dplyr::cross_join(assessment_windows) |>
    dplyr::filter(dplyr::between(collect_month, rolling_start, period_start)) |>
    dplyr::group_by(period_type, year, month_num, month, ADM0_NAME, site.name) |>
    dplyr::summarize(
      n_samples_12_mo = sum(n_samples),
      latest_collect_date = max(latest_collect_date),
      .groups = "drop"
    ) |>
    dplyr::left_join(site_first_collection, by = c("ADM0_NAME", "site.name")) |>
    dplyr::mutate(
      sampling_interval = lubridate::interval(first_collect_date, latest_collect_date),
      site_age = lubridate::time_length(sampling_interval, unit = "month")
    ) |>
    dplyr::filter(n_samples_12_mo >= 5, site_age >= 12) |>
    dplyr::distinct(period_type, year, month_num, month, ADM0_NAME, site.name) |>
    dplyr::group_by(period_type, year, month_num, month, ADM0_NAME) |>
    dplyr::summarize(active_sites = dplyr::n(), .groups = "drop")


  # Current Period Counts -----
  current_counts <- active_site_counts |>
    dplyr::filter(period_type == "current") |>
    dplyr::select(-period_type) |>
    dplyr::rename(
      country = ADM0_NAME,
      current_active_sites = active_sites
    )


  # Prior Period Counts -----
  prior_counts <- active_site_counts |>
    dplyr::filter(period_type == "prior") |>
    dplyr::select(-period_type) |>
    dplyr::rename(
      country = ADM0_NAME,
      prior_active_sites = active_sites
    )


  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    country = unique(es_data$ADM0_NAME),
    current_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(current_counts, by = c("country", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_counts |> dplyr::select(country, month_num, prior_active_sites),
      by = c("country", "month_num")
    ) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(country),
      # for counts, NA is assumed as 0
      current_active_sites = tidyr::replace_na(current_active_sites, 0),
      prior_active_sites = tidyr::replace_na(prior_active_sites, 0),
      # thresholds
      upper_50pct = prior_active_sites * 1.5,
      lower_50pct = pmax(0, prior_active_sites * 0.5),
      # percent change is undefined when the prior-year value is 0 and current is > 0
      perc_change = dplyr::case_when(
        prior_active_sites == 0 & current_active_sites == 0 ~ 0,
        prior_active_sites == 0 ~ NA_real_,
        TRUE ~ round((current_active_sites - prior_active_sites) / prior_active_sites * 100)
      ),
      flag = dplyr::case_when(
        current_active_sites == 0 & prior_active_sites == 0 ~ "No ES",
        current_active_sites <= upper_50pct & current_active_sites >= lower_50pct ~ "Within Target",
        current_active_sites > upper_50pct ~ "Above Target",
        current_active_sites < lower_50pct ~ "Below Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(country, whoregion, month_label, current_active_sites,
                  prior_active_sites, perc_change, lower_50pct, upper_50pct, flag)


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
    definition = "Number of active ES sites defined as sites at least 12 months old with at least 5 collections over the past 12-month rolling period.",
    possible_statuses = c("Within Target", "Below Target", "Above Target",
                          "No Current Active ES Sites", "Review")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

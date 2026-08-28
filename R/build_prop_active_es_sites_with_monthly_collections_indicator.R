#' Build Proportion of Active ES Sites With Monthly Collections Indicator
#'
#' Calculates the proportion of active environmental surveillance (ES) sites
#' with at least one collection in each month of the current 6-month rolling
#' window. Returns results in long format with one row per country per month.
#'
#' @details
#' A site is counted as active for a given assessment month if it has at least
#' 5 collections in the 12-month rolling period ending on the last day of that
#' month. Among those active sites, the indicator calculates the proportion with
#' at least one collection in the assessment month.
#'
#' A country showing 0 active sites may represent a country with ES activity
#' where no sites met the >= 5 collection threshold in that 12-month rolling
#' period, or a country with no ES data at all. These cases are not
#' distinguished in the output and are flagged as
#' \code{"No Current Active ES"}.
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
#'     \code{active_sites}, \code{sites_with_1_collection},
#'     \code{prop_sites_with_1_collection}, and \code{flag}.}
#'   \item{metadata}{A named list containing \code{indicator_code},
#'     \code{indicator_label}, \code{current_period_start},
#'     \code{current_period_end}, \code{current_period_label},
#'     \code{n_current_months}, \code{threshold_rule}, \code{definition},
#'     and \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' end_date <- lubridate::floor_date(Sys.Date(), unit = "month") %m-% days(1)
#' result <- build_proportion_of_active_es_sites_with_monthly_collections(raw_data$es, end_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_prop_active_es_sites_with_monthly_collections <- function(es_data,
                                                                         end_date = Sys.Date()) {

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
  current_combos <- tibble::tibble(
    year = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month = lubridate::month(current_months, label = TRUE, abbr = TRUE),
    period_start = lubridate::floor_date(current_months, unit = "month"),
    period_end = lubridate::ceiling_date(current_months, unit = "month") %m-% days(1)
  )


  # Period Labels -----
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(end_date, "%b %Y"))


  # Assessment Windows -----
  assessment_windows <- current_combos |>
    dplyr::mutate(rolling_start = period_end %m-% months(12) + days(1))


  # Prepare Data -----

  # Initial ES prep - remove observations with missing values for key data variables
  es_prep <- es_data |>
    dplyr::select(ADM0_NAME, collect.date, site.name) |>
    dplyr::mutate(collect.date = lubridate::as_date(collect.date)) |>
    dplyr::filter(!is.na(ADM0_NAME),
                  !is.na(collect.date),
                  !is.na(site.name))


  # Site monthly counts -----
  # Create collections per site per calendar month
  site_month_counts <- es_prep |>
    dplyr::filter(dplyr::between(collect.date,
                                 min(assessment_windows$rolling_start),
                                 max(assessment_windows$period_end))) |>
    dplyr::mutate(collect_month = lubridate::floor_date(collect.date, unit = "month")) |>
    dplyr::group_by(ADM0_NAME, site.name, collect_month) |>
    dplyr::summarize(n_samples = dplyr::n(),
                     .groups = "drop")


  # Active Site-Months -----
  # cross_join pairs every site-month record with all 6 assessment windows.
  # between() then keeps only pairings where the collection month falls within
  # that assessment window's 12-month lookback.
  # Sites with >= 5 samples qualify as active for that assessment month.
  active_site_months <- site_month_counts |>
    dplyr::cross_join(assessment_windows) |>
    dplyr::filter(dplyr::between(collect_month, rolling_start, period_end)) |>
    dplyr::group_by(year, month_num, month, period_start, period_end,
                    ADM0_NAME, site.name) |>
    dplyr::summarize(n_samples_12_mo = sum(n_samples),
                     .groups = "drop") |>
    dplyr::filter(n_samples_12_mo >= 5)


  # Same-Month Collection Counts -----
  # Active sites are defined using a rolling 12-month denominator.
  # This object keeps one row per country, site, and collection month so that
  # same-month collection counts can be joined back to the active-site list for
  # each assessment month.
  monthly_collections <- site_month_counts |>
    dplyr::select(ADM0_NAME, site.name, collect_month, n_samples_current_month = n_samples)


  # Country-Month Summary -----
  # Joins the active-site denominator to the same-month collection counts above.
  # Sites with a matching collection month and n_samples_current_month >= 1
  # are counted in sites_with_1_collection. Sites without a matching same-month
  # collection do not count toward sites_with_1_collection, but remain in
  # active_sites. The final proportion is:
  # active sites with >= 1 collection in the month / all active sites that month.
  monthly_summary <- active_site_months |>
    dplyr::left_join(
      monthly_collections,
      by = dplyr::join_by(ADM0_NAME, site.name, period_start == collect_month)
    ) |>
    dplyr::mutate(
      has_monthly_collection = n_samples_current_month >= 1
    ) |>
    dplyr::group_by(ADM0_NAME, year, month_num, month) |>
    dplyr::summarize(
      active_sites = dplyr::n(),
      sites_with_1_collection = sum(has_monthly_collection, na.rm = TRUE),
      prop_sites_with_1_collection = round(sites_with_1_collection / active_sites * 100),
      .groups = "drop"
    )


  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    country = unique(es_data$ADM0_NAME),
    current_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(
      monthly_summary |> dplyr::rename(country = ADM0_NAME),
      by = c("country", "year", "month_num", "month")
    ) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(country),
      flag = dplyr::case_when(
        is.na(active_sites) ~ "No Current Active ES",
        active_sites == 0 ~ "No Current Active ES",
        prop_sites_with_1_collection >= 80 ~ "Within Target",
        prop_sites_with_1_collection < 80 ~ "Below Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(country, whoregion, month_label, active_sites,
                  sites_with_1_collection, prop_sites_with_1_collection, flag)


  # Return -----
  meta <- list(
    indicator_code = "prop_active_es_sites_monthly_collections",
    indicator_label = "Proportion of active ES sites with monthly collections",
    unit = "Month",
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    n_current_months = 6,
    threshold_rule = "Within Target if at least 80% of active ES sites have at least 1 collection in the assessment month",
    definition = "Proportion of active ES sites with at least one collection per month. Active sites are defined as sites with at least 5 collections in the rolling 12-month period. Within Target if at least 80% of active ES sites have at least 1 collection in the month. Below Target if less than 80% of active ES sites have at least 1 collection in the month.",
    other_status_definitions = "No Current Active ES if there are no active sites in the period.",
    possible_statuses = c("Within Target", "Below Target", "No Current Active ES")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

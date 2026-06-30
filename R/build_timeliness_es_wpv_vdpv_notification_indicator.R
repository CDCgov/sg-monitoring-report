#' Build Timeliness of ES WPV/VDPV Notification Indicator
#'
#' Calculates the median number of days from ES sample collection to HQ
#' notification for WPV/VDPV-positive ES samples across two rolling 3-month
#' windows: the most recent completed 3-month period and the immediately
#' preceding 3-month period. Each window is compared to the same 3-month period
#' in the prior year. Returns results in long format with one row per country
#' per window.
#'
#' @details
#' Only ES samples with \code{wpv == 1} or \code{vdpv == 1} are included.
#' Windows are defined by \code{date.notification.to.hq}, so samples are
#' included based on when they were notified to HQ, not when they were
#' collected. Timeliness is measured as
#' \code{date.notification.to.hq - collection.date}. Cases with a \code{days_to_notification}
#' outside 0-365 days are excluded as likely data quality issues.
#' Cases with a missing notification date are also excluded.
#' Lab location \code{es.lab.type} is retained in the output for context only
#' and is not used for the target flag.
#'
#' @param es_data A data frame containing ES data. Must include
#'   \code{ADM0_NAME}, \code{collection.date}, \code{date.notification.to.hq},
#'   \code{wpv}, and \code{vdpv}.
#' @param end_date Date used to determine the reporting window. Defaults to
#'   \code{Sys.Date()}.
#' @param lab_loc Laboratory type metadata. Must include \code{country} and
#'   \code{es.lab.type}. Defaults to \code{sirfunctions::get_lab_locs()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per window
#'     containing: \code{country}, \code{whoregion}, \code{window},
#'     \code{current_period}, \code{prior_period}, \code{es.lab.type},
#'     \code{current_count}, \code{current_median_days}, \code{prior_count},
#'     \code{prior_median_days}, \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, unit, all four
#'     period labels, threshold rule, definition, and possible flag values.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_timeliness_es_wpv_vdpv_notification_indicator(raw_data$es, end_date)
#' result$data
#' result$metadata$recent_period_label
#' }
#'
#' @export
build_timeliness_es_wpv_vdpv_notification_indicator <- function(es_data,
                                                             end_date = Sys.Date(),
                                                             lab_loc = sirfunctions::get_lab_locs()) {

  # Basic initial checks -----
  required_es_cols <- c("ADM0_NAME", "collection.date", "date.notification.to.hq",
                        "wpv", "vdpv")
  required_lab_cols <- c("country", "es.lab.type")

  stopifnot(
    "es_data must be a data frame" = is.data.frame(es_data),
    "required columns missing from es_data" = all(required_es_cols %in% names(es_data)),
    "lab_loc must be a data frame" = is.data.frame(lab_loc),
    "required columns missing from lab_loc" = all(required_lab_cols %in% names(lab_loc))
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  # Recent 3-month window - last complete month
  recent_end <- lubridate::floor_date(end_date, unit = "month") %m-% days(1)
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Recent 3-month window - one year prior for comparison
  recent_prior_end <- recent_end %m-% lubridate::years(1)
  recent_prior_start <- recent_start %m-% lubridate::years(1)

  # Earlier 3-month window - the 3 months preceding the recent window
  earlier_end <- lubridate::floor_date(recent_start, unit = "month") %m-% days(1)
  earlier_start <- lubridate::floor_date(earlier_end %m-% months(2), unit = "month")

  # Earlier 3-month window - one year prior for comparison
  earlier_prior_end <- earlier_end %m-% lubridate::years(1)
  earlier_prior_start <- earlier_start %m-% lubridate::years(1)

  # Period Labels -----
  recent_period_label <- paste0(format(recent_start, "%b %Y"), " - ", format(recent_end, "%b %Y"))
  recent_prior_period_label <- paste0(format(recent_prior_start, "%b %Y"), " - ", format(recent_prior_end, "%b %Y"))
  earlier_period_label <- paste0(format(earlier_start, "%b %Y"), " - ", format(earlier_end, "%b %Y"))
  earlier_prior_period_label <- paste0(format(earlier_prior_start, "%b %Y"), " - ", format(earlier_prior_end, "%b %Y"))

  eligibility_note <- paste0(
    "Windows are defined by date.notification.to.hq — samples are included based on when ",
    "results were reported to HQ, not when they were collected. ",
    "This avoids bias from samples with onset in the window whose results have not yet arrived."
  )


  # Prepare Data -----
  lab_type <- lab_loc |>
    dplyr::distinct(country, es.lab.type) |>
    dplyr::filter(!is.na(es.lab.type))

  es_prep <- es_data |>
    dplyr::rename(country = ADM0_NAME) |>
    dplyr::mutate(
      days_to_notification = as.numeric(lubridate::as_date(date.notification.to.hq) - lubridate::as_date(collection.date))) |>
    dplyr::left_join(lab_type, by = "country") |>
    dplyr::filter(wpv == 1 | vdpv == 1,
                  date.notification.to.hq <= recent_end,  # keep within our time frame
                  !is.na(days_to_notification),
                  dplyr::between(days_to_notification, 0, 365)) # data quality limitation


  # Helper to summarize counts and median for a given window -----
  summarize_window <- function(data, start, end) {
    data |>
      dplyr::filter(dplyr::between(date.notification.to.hq, start, end)) |>
      dplyr::group_by(country) |>
      dplyr::summarise(
        counts = dplyr::n(),
        median_days = median(days_to_notification, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Period Medians -----
  recent_counts <- summarize_window(es_prep, recent_start, recent_end)
  recent_prior_counts <- summarize_window(es_prep, recent_prior_start, recent_prior_end)
  earlier_counts <- summarize_window(es_prep, earlier_start, earlier_end)
  earlier_prior_counts <- summarize_window(es_prep, earlier_prior_start, earlier_prior_end)


  # Create Full Grid of all Countries + Region + Lab Type -----
  full_grid <- tibble::tibble(
    country = unique(es_data$ADM0_NAME)) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(country)) |>
    dplyr::left_join(lab_type, by = "country")


  # Build Period Summaries -----
  build_period <- function(full_grid, current, prior) {
    full_grid |>
      dplyr::left_join(current, by = "country") |>
      dplyr::rename(
        current_count = counts,
        current_median_days = median_days
      ) |>
      dplyr::left_join(prior, by = "country") |>
      dplyr::rename(
        prior_count = counts,
        prior_median_days = median_days
      ) |>
      dplyr::mutate(
        perc_change = round((current_median_days - prior_median_days) / prior_median_days * 100),
        flag = dplyr::case_when(
          # if either medians are 0, likely data quality issue as collection and notification date are all the same across all samples
          current_median_days == 0 | prior_median_days == 0 ~ "Incomplete Data",
          # missing data — cannot calculate change
          is.na(perc_change) ~ "Incomplete Data",
          # threshold
          perc_change < -50 ~ "Above Target",
          perc_change > 50 ~ "Below Target",
          dplyr::between(perc_change, -50, 50) ~ "Within Target",
          TRUE ~ "Review"
        )
      )
  }

  # Create individual period summaries -----
  recent_summary <- build_period(full_grid, recent_counts, recent_prior_counts) |>
    dplyr::mutate(
      window = "recent",
      current_period = recent_period_label,
      prior_period = recent_prior_period_label
    )

  earlier_summary <- build_period(full_grid, earlier_counts, earlier_prior_counts) |>
    dplyr::mutate(
      window = "earlier",
      current_period = earlier_period_label,
      prior_period = earlier_prior_period_label
    )

  # Join for full table -----
  final_summary <- dplyr::bind_rows(recent_summary, earlier_summary) |>
    dplyr::select(
      country, whoregion, window, current_period, prior_period, es.lab.type,
      current_count, current_median_days, prior_count, prior_median_days,
      perc_change, flag
    )

  # Return -----
  meta <- list(
    indicator_code = "timeliness_es_wpv_vdpv_notification",
    indicator_label = "Timeliness of ES WPV/VDPV Notification",
    unit = "Quarter",
    end_date = end_date,
    eligibility_note = eligibility_note,
    recent_period_label = recent_period_label,
    recent_prior_period_label = recent_prior_period_label,
    earlier_period_label = earlier_period_label,
    earlier_prior_period_label = earlier_prior_period_label,
    n_current_quarters = 2,
    n_prior_years = 1,
    threshold_rule = "+/-50% of the median from the same 3-month period one year prior",
    definition = "Median days between collection to notification to HQ for an ES WPV/VDPV sample. Within Target if the median timeliness of samples notified in the most recent completed three-month period is within +/-50% compared with the same three-month period of the previous year.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

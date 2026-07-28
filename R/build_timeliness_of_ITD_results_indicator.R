#' Build Timeliness of ITD Results Indicator
#'
#' Calculates the median number of days from virus isolation result to ITD
#' result across two rolling 3-month windows: the most recent completed
#' 3-month period and the immediately preceding 3-month period. Each window is
#' compared to the same 3-month period in the prior year. Returns results in
#' long format with one row per culture/ITD lab per window.
#'
#' @details
#' Timeliness is measured as
#' \code{DateFinalrRTPCRResults - DateFinalCellCultureResult}, matching the
#' \code{days.culture.itd} interval from \code{get_lab_intervals()}. Samples
#' are assigned to windows by \code{DateFinalrRTPCRResults}, so the reporting
#' period reflects completed ITD results. Intervals outside 0-365 days are
#' excluded as likely data quality issues. Same-day intervals are recoded from
#' 0 to 1 day before medians are calculated. Records with a missing, blank, or
#' unknown \code{culture.itd.lab} are excluded.
#'
#' @param lab_data A data frame containing lab data. Must include
#'   \code{culture.itd.lab}, \code{DateFinalCellCultureResult}, and
#'   \code{DateFinalrRTPCRResults}.
#' @param end_date The maximum date available in the lab dataset. Typically
#'   passed as \code{max(lab_data$DateFinalrRTPCRResults, na.rm = TRUE)}.
#'   Defaults to \code{Sys.Date()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per lab per window
#'     containing: \code{culture.itd.lab}, \code{window},
#'     \code{current_period}, \code{prior_period}, \code{current_n},
#'     \code{current_median_days}, \code{prior_n},
#'     \code{prior_median_days}, \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing: \code{indicator_code},
#'     \code{indicator_label}, \code{unit}, \code{lab_end_date},
#'     \code{eligibility_note}, period labels, \code{n_current_quarters},
#'     \code{n_prior_years}, \code{threshold_rule}, \code{definition}, and
#'     \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' max_lab_date <- max(lab_data$DateFinalrRTPCRResults, na.rm = TRUE)
#' result <- build_timeliness_of_ITD_results_indicator(lab_data, max_lab_date)
#' result$data
#' result$metadata$recent_period_label
#' }
#'
#' @export
build_timeliness_of_ITD_results_indicator <- function(lab_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  required_cols <- c(
    "culture.itd.lab",
    "DateFinalCellCultureResult",
    "DateFinalrRTPCRResults"
  )

  stopifnot(
    "lab_data must be a data frame" = is.data.frame(lab_data),
    "required columns missing from lab_data" = all(required_cols %in% names(lab_data))
  )

  # Date Windows -----
  end_date <- lubridate::as_date(end_date)

  # Recent 3-month window ending on the supplied end_date
  recent_end <- end_date
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Recent 3-month window - one year prior for comparison
  recent_prior_end <- recent_end %m-% lubridate::years(1)
  recent_prior_start <- recent_start %m-% lubridate::years(1)

  # Earlier 3-month window - the 3 months preceding the recent window
  earlier_end <- lubridate::floor_date(recent_start, unit = "month") %m-% lubridate::days(1)
  earlier_start <- lubridate::floor_date(earlier_end %m-% months(2), unit = "month")

  # Earlier 3-month window - one year prior for comparison
  earlier_prior_end <- earlier_end %m-% lubridate::years(1)
  earlier_prior_start <- earlier_start %m-% lubridate::years(1)

  # Period Labels -----
  recent_period_label <- paste0(
    format(recent_start, "%b %Y"),
    " - ",
    format(recent_end, "%b %Y")
  )
  recent_prior_period_label <- paste0(
    format(recent_prior_start, "%b %Y"),
    " - ",
    format(recent_prior_end, "%b %Y")
  )
  earlier_period_label <- paste0(
    format(earlier_start, "%b %Y"),
    " - ",
    format(earlier_end, "%b %Y")
  )
  earlier_prior_period_label <- paste0(
    format(earlier_prior_start, "%b %Y"),
    " - ",
    format(earlier_prior_end, "%b %Y")
  )

  eligibility_note <- paste0(
    "Lab data end date: ", format(end_date, "%b %d, %Y"), ", based on the ",
    "last day of the month of the maximum date in the lab data for Case date. ",
    "Data may be incomplete for the latest month. ",
    "Samples are assigned to windows by DateFinalrRTPCRResults."
  )

  # Prepare Data -----

  # remove missing, blank, or unknown culture/ITD labs
  lab_data_clean <- lab_data |>
    dplyr::mutate(culture.itd.lab = trimws(as.character(culture.itd.lab))) |>
    dplyr::filter(
      !is.na(culture.itd.lab),
      culture.itd.lab != "",
      tolower(culture.itd.lab) != "unknown"
    )

  # create indicator lab data
  lab_prep <- lab_data_clean |>
    dplyr::mutate(
      DateFinalCellCultureResult = lubridate::as_date(DateFinalCellCultureResult),
      DateFinalrRTPCRResults = lubridate::as_date(DateFinalrRTPCRResults),
      days.culture.itd = as.numeric(DateFinalrRTPCRResults - DateFinalCellCultureResult),
      # 0 days is treated as 1 day because same-day PCR still represents 1 day of work.
      days.culture.itd = dplyr::if_else(days.culture.itd == 0, 1, days.culture.itd),
      t2 = !is.na(days.culture.itd) & dplyr::between(days.culture.itd, 0, 365)
    ) |>
    dplyr::filter(t2)

  labs <- lab_data_clean |>
    dplyr::distinct(culture.itd.lab)

  full_grid <- labs

  # Helper to summarize counts and median for a given window -----
  summarize_window <- function(data, start, end) {
    data |>
      dplyr::filter(dplyr::between(DateFinalrRTPCRResults, start, end)) |>
      dplyr::group_by(culture.itd.lab) |>
      dplyr::summarise(
        n = dplyr::n(),
        median_days = median(days.culture.itd, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Period Medians -----
  recent_counts <- summarize_window(lab_prep, recent_start, recent_end)
  recent_prior_counts <- summarize_window(lab_prep, recent_prior_start, recent_prior_end)
  earlier_counts <- summarize_window(lab_prep, earlier_start, earlier_end)
  earlier_prior_counts <- summarize_window(lab_prep, earlier_prior_start, earlier_prior_end)

  # Build Period Summaries -----
  build_period <- function(full_grid, current, prior) {
    full_grid |>
      dplyr::left_join(current, by = "culture.itd.lab") |>
      dplyr::rename(
        current_n = n,
        current_median_days = median_days
      ) |>
      dplyr::left_join(prior, by = "culture.itd.lab") |>
      dplyr::rename(
        prior_n = n,
        prior_median_days = median_days
      ) |>
      dplyr::mutate(
        current_n = tidyr::replace_na(current_n, 0),
        prior_n = tidyr::replace_na(prior_n, 0),
        perc_change = round((current_median_days - prior_median_days) / prior_median_days * 100),
        flag = dplyr::case_when(
          # No samples in one or both windows means the comparison cannot be made.
          current_n == 0 & prior_n == 0 ~ "No ITD samples",
          current_n == 0 & prior_n != 0 ~ "No current ITD samples",
          current_n != 0 & prior_n == 0 ~ "No prior ITD samples",

          # Missing medians or other non-computable comparisons.
          is.na(perc_change) ~ "Incomplete Data",

          # Standard +/-50% target rule.
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
      culture.itd.lab, window, current_period, prior_period,
      current_n, current_median_days, prior_n, prior_median_days,
      perc_change, flag
    )

  # Return -----
  meta <- list(
    indicator_code = "timeliness_of_ITD_results",
    indicator_label = "Timeliness of ITD Results",
    unit = "Quarter",
    lab_end_date = end_date,
    eligibility_note = eligibility_note,
    recent_period_label = recent_period_label,
    recent_prior_period_label = recent_prior_period_label,
    earlier_period_label = earlier_period_label,
    earlier_prior_period_label = earlier_prior_period_label,
    n_current_quarters = 2,
    n_prior_years = 1,
    threshold_rule = "+/-50% of the same 3-month period median from the prior year",
    definition = paste0(
      "Median days between virus isolation results to ITD results. ",
      "On target if the median timeliness for the current three-month period ",
      "is +/-50% compared with the previous year median of that three-month period."
    ),
    possible_statuses = c(
      "Within Target",
      "Below Target",
      "Above Target",
      "No ITD samples",
      "No current ITD samples",
      "No prior ITD samples",
      "Incomplete Data"
    )
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

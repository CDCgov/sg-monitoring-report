#' Build WPV/VDPV Notification Timeliness Indicator
#'
#' Calculates the median number of days from onset to HQ notification for
#' WPV and VDPV positive AFP cases across two rolling 3-month windows: the
#' most recent 3-month window and the immediately preceding 3-month
#' window. Each window is compared to the same 3-month window in the prior year.
#' Returns results in long format with one row per country per window.
#'
#' @details
#' Only AFP-sourced cases with WILD or VDPV string contained in the \code{measurement}
#' field are included. Cases with a \code{days_to_notification} outside the
#' range 0–365 are excluded as likely data quality issues. Cases with a
#' missing notification date are also excluded.
#'
#' @param pos A data frame containing positive case data. Must include
#'   \code{dateonset}, \code{place.admin.0}, \code{datenotificationtohq},
#'   \code{source}, and \code{measurement}.
#' @param end_date Date to end the current reporting window.
#'   Defaults to `Sys.Date()`. Typically passed as the last day of the previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per window
#'     containing: \code{ctry}, \code{whoregion}, \code{window},
#'     \code{current_period}, \code{prior_period}, \code{current_count},
#'     \code{current_median_days}, \code{prior_count}, \code{prior_median_days},
#'     \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, all
#'     four period labels, threshold rule, and possible flag values.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_wpv_vdpv_timeliness_indicator(raw_data$pos)
#' result$data
#' result$metadata$recent_period_label
#' }
#'
#' @export
build_wpv_vdpv_timeliness_indicator <- function(pos, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "pos data must be a data frame" = is.data.frame(pos),
    "dateonset column required" = "dateonset" %in% names(pos),
    "place.admin.0 column required" = "place.admin.0" %in% names(pos),
    "datenotificationtohq column required" = "datenotificationtohq" %in% names(pos),
    "source column required" = "source" %in% names(pos),
    "measurement column required" = "measurement" %in% names(pos)
  )


  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  # Recent 3-month window — last complete month
  recent_end <- lubridate::floor_date(end_date, unit = "month") %m-% days(1)
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Recent 3-month window - one year prior for comparison
  recent_prior_end <- recent_end   %m-% lubridate::years(1)
  recent_prior_start <- recent_start %m-% lubridate::years(1)

  # Earlier 3 month window - the 3 months preceding the recent window
  earlier_end <- lubridate::floor_date(recent_start, unit = "month") %m-% days(1)
  earlier_start <- lubridate::floor_date(earlier_end %m-% months(2), unit = "month")

  # Earlier 3-month window - one year prior for comparison
  earlier_prior_end <- earlier_end   %m-% lubridate::years(1)
  earlier_prior_start <- earlier_start %m-% lubridate::years(1)

  # Period Labels -----
  recent_period_label <- paste0(format(recent_start, "%b %Y"), " - ", format(recent_end, "%b %Y"))
  recent_prior_period_label <- paste0(format(recent_prior_start, "%b %Y"), " - ", format(recent_prior_end, "%b %Y"))
  earlier_period_label <- paste0(format(earlier_start, "%b %Y"), " - ", format(earlier_end, "%b %Y"))
  earlier_prior_period_label <- paste0(format(earlier_prior_start, "%b %Y"), " - ", format(earlier_prior_end, "%b %Y"))


  # Prepare Data -----
  pos_prep <- pos |>
    dplyr::filter(source == "AFP") |>
    dplyr::mutate(ctry = place.admin.0, # rename for easy merging below
                  days_to_notification = as.numeric(lubridate::as_date(datenotificationtohq) - dateonset),
                  whoregion = sirfunctions::get_region(place.admin.0)) |> #ensures correct region assignment
    dplyr::filter(datenotificationtohq <= recent_end,  # keep within our time frame
                  !is.na(days_to_notification),
                  dplyr::between(days_to_notification, 0, 365), # data quality limitation
                  stringr::str_detect(measurement, "WILD|VDPV"))


  # Helper to summarize counts and median for a given window -----
  summarize_window <- function(data, start, end) {
    data |>
      dplyr::filter(dplyr::between(dateonset, start, end)) |>
      dplyr::group_by(ctry) |>
      dplyr::summarize(
        counts = dplyr::n(),
        median_days = median(days_to_notification, na.rm = TRUE),
        .groups = "drop")
    }


  # Period Medians -----
  # Compute medians for each of the four time periods
  recent_counts        <- summarize_window(pos_prep, recent_start, recent_end)
  recent_prior_counts  <- summarize_window(pos_prep, recent_prior_start, recent_prior_end)
  earlier_counts       <- summarize_window(pos_prep, earlier_start, earlier_end)
  earlier_prior_counts <- summarize_window(pos_prep, earlier_prior_start, earlier_prior_end)


  # Create Full Grid of all Countries + Region -----
  full_grid <- tibble::tibble(
    ctry = unique(pos$place.admin.0)) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))

  # Build Period Summaries -----
  # Helper function to join each median data to full country list, calculate percent change,
  # and apply flag for a single period.
  # Called twice below — once for the recent window and once for the earlier window.
  build_period <- function(full_grid, current, prior) {
    full_grid |>
      dplyr::left_join(current, by = "ctry") |>
      dplyr::rename(
        current_count = counts,
        current_median_days = median_days
        ) |>
      dplyr::left_join(prior, by = "ctry") |>
      dplyr::rename(
        prior_count = counts,
        prior_median_days = median_days
      ) |>
      dplyr::mutate(
        perc_change = round((current_median_days - prior_median_days) / prior_median_days * 100),
        flag = dplyr::case_when(
          # missing data — cannot calculate change
          is.na(perc_change) ~ "Incomplete Data", # handles either or both missing
          prior_median_days == 0 ~ "Incomplete Data", # safety - would be Inf
          # threshold
          perc_change >= 50 ~ "Below Target",
          perc_change < 50 ~ "On Target",
          TRUE ~ "Review"
        )
      )
  }


  # Create individual period summaries -----
  recent_summary <- build_period(full_grid, recent_counts, recent_prior_counts) |>
    dplyr::mutate(window = "recent",
                  current_period  = recent_period_label,
                  prior_period = recent_prior_period_label)

  earlier_summary <- build_period(full_grid, earlier_counts, earlier_prior_counts) |>
    dplyr::mutate(window = "earlier",
                  current_period  = earlier_period_label,
                  prior_period = earlier_prior_period_label)


  # Join for full table -----
  final_summary <- dplyr::bind_rows(recent_summary, earlier_summary) |>
    dplyr::select(ctry, whoregion, window, current_period, prior_period,
                  current_count, current_median_days, prior_count, prior_median_days,
                  perc_change, flag)


  # Return -----
  meta <- list(
    indicator_code = "wpv_vdpv_timeliness",
    indicator_label = "Timely AFP WPV/VDPV Detection",
    recent_period_label = recent_period_label,
    recent_prior_period_label = recent_prior_period_label,
    earlier_period_label = earlier_period_label,
    earlier_prior_period_label = earlier_prior_period_label,
    threshold_rule = "Below Target if median increases by more than 50 percent compared to same 3-month period in the prior year",
    definition = "",
    possible_statuses = c("On Target", "Below Target", "Incomplete Data")
  )


  return(list(
    data = final_summary,
    metadata = meta))

}



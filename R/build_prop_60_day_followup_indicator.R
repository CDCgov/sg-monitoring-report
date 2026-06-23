#' Build Proportion 60-Day Follow-Up Indicator
#'
#' Calculates the proportion of inadequate AFP cases with completed 60-day
#' follow-up for two rolling 3-month window: the most recent fully eligible window and the
#' immediately preceding 3-month window. Each window is compared to the same 3-month window
#' in the prior year. Returns results in long format with one row per country per window.
#'
#' @details
#' Unlike other build functions, this function does not take an \code{end_date}
#' parameter. The eligible window is determined by a 120-day lag from
#' \code{Sys.Date()} — cases must have onset >= 120 days ago to have a resolved
#' follow-up status. The most recent full month where all cases are eligible
#' becomes the end of the recent window. Results are therefore tied to the
#' date the function is run.
#'
#' @param afp_data A data frame containing AFP case-level data. Must include
#'   `dateonset` and `place.admin.0`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per window
#'     containing: \code{ctry}, \code{whoregion}, \code{window},
#'     \code{current_period}, \code{prior_period},
#'     \code{current_inad_cases}, \code{current_got_60day},
#'     \code{current_prop_60day}, \code{prior_inad_cases},
#'     \code{prior_got_60day}, \code{prior_prop_60day},
#'     \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, eligibility
#'     cutoff date, eligibility note, all four period labels, threshold rule,
#'     and possible flag values.}
#'
#' @examples
#' \dontrun{
#' result <- build_prop_60_day_follow_up(raw_data$afp)
#' result$data
#' result$metadata$recent_period_label
#' }
#'
#' @export
build_prop_60_day_follow_up <- function(afp_data) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "dateonset column required"     = "dateonset" %in% names(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data)
  )

  # Date Windows -----

  # Eligible window is determined by 120-day lag from today(), not end_date.
  # Cases must have onset >= 120 days ago to be eligible to assess follow-up status.
  # This requirement comes from sirfunctions::generate_60_day_table_data() which is a dependency below
  eligibility_cutoff <- Sys.Date() - days(120)

  # Recent 3-month window — last complete month where all cases are eligible
  recent_end   <- lubridate::floor_date(eligibility_cutoff, unit = "month") %m-% days(1)
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Recent 3-month window - one year prior for comparison
  recent_prior_end   <- recent_end   %m-% lubridate::years(1)
  recent_prior_start <- recent_start %m-% lubridate::years(1)

  # Earlier 3 month window - the 3 months preceding the recent window
  earlier_end <- lubridate::floor_date(recent_start, unit = "month") %m-% days(1)
  earlier_start <- lubridate::floor_date(earlier_end %m-% months(2), unit = "month")

  # Earlier 3-month window - one year prior for comparison
  earlier_prior_end   <- earlier_end   %m-% lubridate::years(1)
  earlier_prior_start <- earlier_start %m-% lubridate::years(1)

  # Period Labels -----
  recent_period_label  <- paste0(format(recent_start, "%b %Y"), " - ", format(recent_end, "%b %Y"))
  recent_prior_period_label    <- paste0(format(recent_prior_start, "%b %Y"), " - ", format(recent_prior_end, "%b %Y"))
  earlier_period_label  <- paste0(format(earlier_start, "%b %Y"), " - ", format(earlier_end, "%b %Y"))
  earlier_prior_period_label    <- paste0(format(earlier_prior_start, "%b %Y"), " - ", format(earlier_prior_end, "%b %Y"))

  eligibility_note <- paste0(
    "Eligible window is based on a 120-day follow-up lag. ",
    "Cases with onset after ", format(eligibility_cutoff, "%b %d, %Y"),
    " are excluded from analysis as follow-up may not yet be complete.",
    " Analysis dates are then set to the last full month prior to ", format(eligibility_cutoff, "%b %d, %Y"), ".")

  # Prepare data from sirfunctions -----
  # Use earlier_prior_start as start date required parameter in functions - will capture data for all four periods
  # Reminder: generate_60_day_table_data applies the 120-day eligibility cutoff internally.
  stool_data <- sirfunctions::generate_stool_data(
    afp_data,
    start_date = earlier_prior_start,
    end_date   = recent_end)

  case_60d <- sirfunctions::generate_60_day_table_data(
    stool_data,
    start_date = earlier_prior_start,
    end_date   = recent_end)

  # Filter to eligible inadequate cases only -----
  # got60day %in% c(0,1) retains only cases where follow-up was due and resolvable (onset >= 120 days ago).
  # got60day 0 represents cases that do not have follow-up date OR follow-up finding,
  # got60day 1 represents cases that have a follow-up date OR follow-up findings
  # Cases are excluded if they are not inadequate or if got60day == 99  which are those not yet due.
  eligible_cases <- case_60d |>
    dplyr::filter(got60day %in% c(0, 1),
                  adequacy.final2 == "Inadequate")


  # Helper to summarize counts for a given window -----
  summarize_window <- function(data, start, end) {
    data |>
      dplyr::filter(dplyr::between(date, start, end)) |>
      dplyr::group_by(ctry) |>
      dplyr::summarize(
        inad_cases   = dplyr::n(),
        got_60day    = sum(got60day == 1, na.rm = TRUE),
        prop_60day   = round(got_60day / inad_cases * 100),
        .groups = "drop"
      )
  }


  # Period Counts -----
  # Count follow-up completions for each of the four time periods
  recent_counts        <- summarize_window(eligible_cases, recent_start, recent_end)
  recent_prior_counts  <- summarize_window(eligible_cases, recent_prior_start, recent_prior_end)
  earlier_counts       <- summarize_window(eligible_cases, earlier_start, earlier_end)
  earlier_prior_counts <- summarize_window(eligible_cases, earlier_prior_start, earlier_prior_end)


  # Create Full Grid of all Countries + Region -----
  full_grid <- tibble::tibble(
    ctry = unique(afp_data$place.admin.0)) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))

  # Helper function to join each counts data to full country list, calculate percent change,
  # and apply flag for a single period. Called twice below — once for the
  # recent window and once for the earlier window.
  build_period <- function(full_grid, current, prior) {
    full_grid |>
      dplyr::left_join(current, by = "ctry") |>
      dplyr::rename(
        current_inad_cases = inad_cases,
        current_got_60day = got_60day,
        current_prop_60day = prop_60day
      ) |>
      dplyr::left_join(prior, by = "ctry") |>
      dplyr::rename(
        prior_inad_cases = inad_cases,
        prior_got_60day = got_60day,
        prior_prop_60day = prop_60day
      ) |>
      dplyr::mutate(
        perc_change = round((current_prop_60day - prior_prop_60day) / prior_prop_60day * 100),
        flag = dplyr::case_when(
          # missing data — cannot calculate change
          is.na(perc_change) ~ "Incomplete Data", # handles either or both missing
          # threshold,
          prior_prop_60day == 0 ~ "Incomplete Data", # handles perc_change Inf
          perc_change < -50 ~ "Below Target",
          perc_change > 50 ~ "Above Target",
          dplyr::between(perc_change, -50, 50) ~ "Within Target",
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
                  current_inad_cases, current_got_60day, current_prop_60day,
                  prior_inad_cases, prior_got_60day, prior_prop_60day,
                  perc_change, flag)


  # Return -----
  meta <- list(
    indicator_code = "prop_60day_followup",
    indicator_label = "Proportion 60-day follow-up done",
    eligibility_cutoff = eligibility_cutoff,
    eligibility_note = eligibility_note,
    recent_period_label = recent_period_label,
    recent_prior_period_label = recent_prior_period_label,
    earlier_period_label = earlier_period_label,
    earlier_prior_period_label = earlier_prior_period_label,
    n_prior_years = 1,
    threshold_rule = "Below Target if proportion declines by more than 50 percent compared to same 3-month period in the prior year",
    definition = "Proportion of 60-day follow-ups completed. On Target if the proportion of 60-day follow-ups completed in the most recent three month period is within +/- 50% of the proportion of 60-days follow-ups completed in the same three month period of the previous year. ",
    possible_statuses = c("Above Target", "Below Target", "Within Target", "Incomplete Data")
  )


  return(list(
    data = final_summary,
    metadata = meta))

}

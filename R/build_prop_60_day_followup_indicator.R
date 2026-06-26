#' Build Proportion 60-Day Follow-Up Indicator
#'
#' Calculates the proportion of inadequate AFP cases with completed 60-day
#' follow-up for two rolling 3-month windows. Returns results in long format
#' with one row per country per window.
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
#'   \code{dateonset} and \code{place.admin.0}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per window
#'     containing: \code{ctry}, \code{whoregion}, \code{period},
#'     \code{period_start}, \code{period_end}, \code{inad_cases},
#'     \code{got_60day}, \code{prop_60day}, and \code{flag}.}
#'   \item{metadata}{A named list containing \code{indicator_code},
#'     \code{indicator_label}, \code{eligibility_cutoff},
#'     \code{eligibility_note}, \code{recent_period_label},
#'     \code{earlier_period_label}, \code{threshold_rule},
#'     \code{definition}, and \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_prop_60_day_follow_up(raw_data$afp)
#' result$data
#' result$metadata$recent_period_label
#' }
#'
#' @export
build_prop_60_day_follow_up_indicator <- function(afp_data) {

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

  # Recent 3-month window — last complete 3 months where all cases are eligible
  recent_end   <- lubridate::floor_date(eligibility_cutoff, unit = "month") %m-% days(1)
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Earlier 3 month window - the 3 months preceding the recent window
  earlier_end <- lubridate::floor_date(recent_start, unit = "month") %m-% days(1)
  earlier_start <- lubridate::floor_date(earlier_end %m-% months(2), unit = "month")


  # Period Labels -----
  recent_period_label  <- paste0(format(recent_start, "%b %Y"), " - ", format(recent_end, "%b %Y"))
  earlier_period_label  <- paste0(format(earlier_start, "%b %Y"), " - ", format(earlier_end, "%b %Y"))

  eligibility_note <- paste0(
    "Eligible window is based on a 120-day follow-up lag. ",
    "Cases with onset after ", format(eligibility_cutoff, "%b %d, %Y"),
    " are excluded from analysis as follow-up may not yet be complete.",
    " Analysis dates are then set to the last full month prior to ", format(eligibility_cutoff, "%b %d, %Y"), ".")


  # Prepare data from sirfunctions -----
  # Use earlier_start as start date required parameter in functions
  # Reminder: generate_60_day_table_data applies the 120-day eligibility cutoff internally.
  stool_data <- sirfunctions::generate_stool_data(
    afp_data,
    start_date = earlier_start,
    end_date   = recent_end)

  case_60d <- sirfunctions::generate_60_day_table_data(
    stool_data,
    start_date = earlier_start,
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
  # Count follow-up completions for each of the two time periods and attache period label
  recent_counts <- summarize_window(eligible_cases, recent_start, recent_end) |>
    dplyr::mutate(period = recent_period_label)

  earlier_counts <- summarize_window(eligible_cases, earlier_start, earlier_end) |>
    dplyr::mutate(period = earlier_period_label)


  # Create Full Grid of all Countries + Region + Timeperiod -----
    full_grid <- tidyr::expand_grid(
      ctry = unique(afp_data$place.admin.0),
      period = c(recent_period_label, earlier_period_label) ) |> # force reverse chronological order
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))


  # Join all for full table -----
  final_summary <- full_grid |>
    dplyr::left_join(
      dplyr::bind_rows(earlier_counts, recent_counts),
      by = c("ctry", "period")
    )  |>
    dplyr::mutate(
      flag = dplyr::case_when(
        is.na(prop_60day)  ~ "Incomplete Data",
        prop_60day >= 50   ~ "Within Target",
        prop_60day < 50    ~ "Off Target",
        TRUE               ~ "Review"
      ) ) |>
    dplyr::select(
      ctry, whoregion, period, inad_cases, got_60day, prop_60day, flag)


  # Return -----
  meta <- list(
    indicator_code = "prop_60day_followup",
    indicator_label = "Proportion 60-day follow-up done",
    eligibility_cutoff = eligibility_cutoff,
    eligibility_note = eligibility_note,
    recent_period_label = recent_period_label,
    earlier_period_label = earlier_period_label,
    threshold_rule = "Off Target if proportion of completed 60-day follow ups is < 50% in a given time period",
    definition = paste0(
      "Proportion of 60-day follow-ups completed per period. ",
      "Within Target if >= 50% of follow-ups are completed. ",
      "Off Target if < 50%. Each country has one row per period: ",
      "recent (", recent_period_label, ") and earlier (", earlier_period_label, ")."
    ),
    possible_statuses = c("Within Target", "Off Target", "Incomplete Data")
  )


  return(list(
    data = final_summary,
    metadata = meta
    ))

}

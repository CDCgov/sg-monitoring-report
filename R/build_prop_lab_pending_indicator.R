#' Build Proportion Lab Pending Indicator
#'
#' Calculates the proportion of AFP samples pending in labs among cases with
#' onset dates 90 to 365 days before the report end date. Results are returned
#' by country with a simple threshold flag.
#'
#' @param afp_data A data frame containing AFP case-level data. Must include
#'   `place.admin.0`, `cdc.classification.all2`, and either `dateonset` or
#'   `date_onset`.
#' @param end_date Date used to determine case age. Defaults to `Sys.Date()`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country containing:
#'     \code{ctry}, \code{whoregion}, \code{period},
#'     \code{eligible_samples}, \code{pending_samples},
#'     \code{prop_lab_pending}, \code{prop_label}, and \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, age bounds,
#'     eligibility period, threshold rule, and possible flag values.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_prop_lab_pending(raw_data$afp, end_date)
#' result$data
#' result$metadata$threshold_rule
#' }
#'
#' @export
build_prop_lab_pending <- function(afp_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data),
    "cdc.classification.all2 column required" = "cdc.classification.all2" %in% names(afp_data)
  )

  onset_col <- dplyr::case_when(
    "dateonset" %in% names(afp_data) ~ "dateonset",
    "date_onset" %in% names(afp_data) ~ "date_onset",
    TRUE ~ NA_character_
  )

  if (is.na(onset_col)) {
    cli::cli_abort("Either `dateonset` or `date_onset` column is required.")
  }

  # Date eligibility -----
  end_date <- lubridate::as_date(end_date) #only allow date onset >90 days or <365 days
  min_case_age_days <- 90
  max_case_age_days <- 365

  earliest_onset <- end_date - lubridate::days(max_case_age_days)
  latest_onset <- end_date - lubridate::days(min_case_age_days)
  period_label <- paste0(
    format(earliest_onset, "%b %d, %Y"),
    " - ",
    format(latest_onset, "%b %d, %Y")
  )

  # Prepare eligible data -----
  afp_prep <- afp_data |>
    dplyr::mutate(
      date_onset = lubridate::as_date(.data[[onset_col]]),
      case_age_days = as.numeric(end_date - date_onset)
    )

  eligible_cases <- afp_prep |>
    dplyr::filter(
      dplyr::between(case_age_days, min_case_age_days, max_case_age_days)
    )

  # Country-level summary -----
  summary <- eligible_cases |>
    dplyr::group_by(ctry = place.admin.0) |>
    dplyr::summarize(
      eligible_samples = dplyr::n(),
      pending_samples = sum(cdc.classification.all2 == "LAB PENDING", na.rm = TRUE),
      prop_lab_pending = round(pending_samples / eligible_samples * 100),
      prop_label = paste0(prop_lab_pending, " (", pending_samples, "/", eligible_samples, ")"),
      .groups = "drop"
    )

  full_grid <- tibble::tibble(
    ctry = unique(afp_data$place.admin.0)
  ) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))

  final_summary <- full_grid |>
    dplyr::left_join(summary, by = "ctry") |>
    dplyr::mutate(
      eligible_samples = tidyr::replace_na(eligible_samples, 0L),
      pending_samples = tidyr::replace_na(pending_samples, 0L),
      percent_prop = dplyr::if_else(is.na(prop_label), "No eligible samples", prop_label), #return percentage and proportion pending for time period
      time_period = period_label,
      flag = dplyr::case_when(
        eligible_samples == 0 ~ "Incomplete Data",
        is.na(prop_lab_pending) ~ "Incomplete Data",
        prop_lab_pending < 10 ~ "On Target",
        prop_lab_pending >= 10 ~ "Off Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(
      ctry, whoregion, time_period, eligible_samples, pending_samples,
      prop_lab_pending, percent_prop, flag
    )

  # Return -----
  meta <- list(
    indicator_code = "prop_lab_pending",
    indicator_label = "Proportion lab pending",
    end_date = end_date,
    earliest_onset = earliest_onset,
    latest_onset = latest_onset,
    period_label = period_label,
    min_case_age_days = min_case_age_days,
    max_case_age_days = max_case_age_days,
    threshold_rule = "On Target if less than 10 percent of AFP samples with onset 90 to 365 days before the end date are lab pending",
    definition = "Proportion of AFP samples pending in labs among cases with onset dates 90 to 365 days before the report end date.",
    possible_statuses = c("On Target", "Below Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}


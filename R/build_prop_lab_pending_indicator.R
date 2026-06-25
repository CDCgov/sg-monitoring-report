#' Build Proportion Lab Pending Indicator
#'
#' Calculates the proportion of AFP samples with a lab pending classification
#' among cases with onset 90 to 365 days before the report end date. Returns
#' one row per country. There is no time comparator.
#'
#' @details
#' A 90-day lag is applied based on case age calculated from \code{end_date}.
#' Only cases where \code{case_age} falls between 90 and 365 days are
#' eligible, defined using onset date. This window ensures cases have had
#' sufficient time to receive a lab result and excludes very old cases.
#' Lab pending is defined as \code{cdc.classification.all2 == "LAB PENDING"}.
#'
#' @param afp_data A data frame containing AFP case-level data. Must include
#'   \code{place.admin.0}, \code{cdc.classification.all2}, and \code{dateonset}.
#' @param end_date Date used to calculate case age and define the eligible
#'   window. Defaults to \code{Sys.Date()}. Typically passed as the last day
#'   of the previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country containing:
#'     \code{ctry}, \code{whoregion}, \code{eligible_samples},
#'     \code{pending_samples}, \code{prop_lab_pending}, and \code{flag}.}
#'   \item{metadata}{A named list containing \code{indicator_code},
#'     \code{indicator_label}, \code{end_date}, \code{eligibility_start},
#'     \code{eligibility_end}, \code{eligibility_note}, \code{threshold_rule},
#'     \code{definition}, and \code{possible_statuses}.}
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
    "cdc.classification.all2 column required" = "cdc.classification.all2" %in% names(afp_data),
    "dateonset column required" = "dateonset" %in% names(afp_data)
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  # Only cases within last 365 days
  start_date <- end_date - days(365)
  eligibility_end <- end_date - days(90)

  eligibility_note <- paste0(
    "Eligible cases have onset between ", format(start_date, "%b %d, %Y"),
    " and ", format(eligibility_end, "%b %d, %Y"), " (90 to 365 days before ",
    format(end_date, "%b %d, %Y"), "). ",
    "The 90-day lag ensures cases have had sufficient time to receive a lab result."
  )

  # Prepare eligible data -----
  eligible_cases <- afp_data |>
    dplyr::mutate(case_age = as.numeric(end_date - lubridate::as_date(dateonset))) |>
    dplyr::filter(dplyr::between(case_age, 90, 365))


  # Country-level summary -----
  summary <- eligible_cases |>
    dplyr::group_by(ctry = place.admin.0) |>
    dplyr::summarize(
      eligible_samples = dplyr::n(),
      pending_samples = sum(cdc.classification.all2 == "LAB PENDING", na.rm = TRUE),
      prop_lab_pending = round(pending_samples / eligible_samples * 100),
      .groups = "drop"
    )

  # Create Full Grid of all Countries + Region -----
  full_grid <- tibble::tibble(
    ctry = unique(afp_data$place.admin.0)) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))


  # Join for full table -----
  final_summary <- full_grid |>
    dplyr::left_join(summary, by = "ctry") |>
    dplyr::mutate(
      flag = dplyr::case_when(
        is.na(eligible_samples) ~ "Incomplete Data",
        eligible_samples == 0 ~ "Incomplete Data", # safety
        is.na(prop_lab_pending) ~ "Incomplete Data", # safety
        prop_lab_pending < 10 ~ "Within Target",
        prop_lab_pending >= 10 ~ "Off Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(ctry, whoregion, eligible_samples, pending_samples, prop_lab_pending, flag)

  # Return -----
  meta <- list(
    indicator_code = "prop_lab_pending",
    indicator_label = "Proportion lab pending",
    end_date = end_date,
    eligibility_start = start_date,
    eligibility_end = eligibility_end,
    eligibility_note = eligibility_note,
    threshold_rule = "Off Target if 10% or more of AFP samples with onset 90 to 365 days before the end date are lab pending",
    definition = "Proportion of AFP samples 90 to 365 days old that are lab pending.",
    possible_statuses = c("Within Target", "Off Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}


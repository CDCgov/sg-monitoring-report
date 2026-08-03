#' Build Proportion of Inadequate Cases Classified Indicator
#'
#' Calculates the proportion of inadequate AFP cases with onset between 90 and
#' 365 days ago that have a classification. Returns one row per country.
#' There is no time comparator.
#'
#' @details
#' A 90-day waiting period is applied based on case age calculated from \code{end_date}.
#' Only cases where \code{case_age} falls between 90 and 365 days are
#' eligible, defined using onset date. This window ensures cases have had
#' sufficient time to receive a classification and excludes very old cases.
#'
#' @param afp_data A data frame containing AFP case-level data. Must include
#'   \code{dateonset}, \code{place.admin.0}, and \code{cdc.classification.all2}.
#' @param end_date Date used to calculate case age and define the
#'   eligible window. Defaults to \code{Sys.Date()}. Typically passed as the
#'   last day of the previous month.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A data frame with one row per country containing:
#'     \code{ctry}, \code{whoregion}, \code{inad_cases},
#'     \code{n_classified}, \code{n_unclassified},
#'     \code{prop_unclassified}, and \code{flag}.}
#'   \item{metadata}{A named list containing \code{indicator_code},
#'     \code{indicator_label}, \code{end_date}, \code{eligibility_start},
#'     \code{eligibility_end}, \code{eligibility_note}, \code{threshold_rule},
#'     \code{definition}, and \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_prop_inadequate_classified(raw_data$afp, end_date)
#' result$data
#' result$metadata$eligibility_note
#' }
#'
#' @export
build_prop_inadequate_classified <- function(afp_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "dateonset column required" = "dateonset" %in% names(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data),
    "cdc.classification.all2 column required" = "cdc.classification.all2"  %in% names(afp_data)
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)
  # Only cases within last 365 days
  start_date <- end_date - days(365)

  eligibility_note <- paste0(
    "Eligible cases have onset between ", format(start_date, "%b %d, %Y"),
    " and ", format(end_date - days(90), "%b %d, %Y"), " (90 to 365 days before ",
    format(end_date, "%b %d, %Y"), "). ",
    "The 90-day waiting period ensures cases have had sufficient time to receive a classification."
  )

  # Prepare data from sirfunctions -----
  stool_data <- sirfunctions::generate_stool_data(
    afp_data,
    start_date = start_date,
    end_date   = end_date)

  # Filter to eligible inadequate cases only -----
  eligible_cases <- stool_data |>
    dplyr::mutate(case_age = as.numeric(end_date - lubridate::as_date(date))) |>
    dplyr::filter(adequacy.final2 == "Inadequate",
                  dplyr::between(case_age, 90, 365))


  # Summarize per country -----
  summary <- eligible_cases |>
    dplyr::group_by(ctry) |>
    dplyr::summarize(
      inad_cases   = dplyr::n(),
      n_classified = sum(!cdc.classification.all2 %in% c("PENDING", "LAB PENDING"), na.rm = TRUE),
      n_unclassified = sum(cdc.classification.all2 %in% c("PENDING", "LAB PENDING"), na.rm = TRUE),
      prop_unclassified   = round(n_unclassified / inad_cases * 100),
      .groups = "drop")


  # Create Full Grid of all Countries + Region -----
  full_grid <- tibble::tibble(
    ctry = unique(afp_data$place.admin.0)
  ) |>
    dplyr::mutate(whoregion = sirfunctions::get_region(ctry))


  # Join for full table -----
  final_summary <- full_grid |>
    dplyr::left_join(summary, by = "ctry") |>
    dplyr::mutate(
      flag = case_when(
        is.na(prop_unclassified) ~ "Incomplete Data", # handles when all data is missing
        prop_unclassified < 10 ~ "Within Target",
        prop_unclassified >= 10 ~ "Off Target",
        TRUE ~ "Review")) |>
    dplyr::select(ctry, whoregion, inad_cases, n_classified, n_unclassified,
                  prop_unclassified, flag)


  # Return -----
  meta <- list(
    indicator_code     = "prop_inad_classified",
    indicator_label    = "Proportion of Inadequate Cases Classified",
    unit               = "Single value",
    end_date           = end_date,
    eligibility_start  = start_date,
    eligibility_end    = end_date - days(90),
    eligibility_note   = eligibility_note,
    threshold_rule     = "Off Target if 10% or more of eligible inadequate cases are unclassified",
    definition         = "Proportion of inadequate AFP cases with onset 90 to 365 days before the report end date that are unclassified. Within Target if less than 10% of eligible inadequate cases are unclassified. Off Target if 10% or more of eligible inadequate cases are unclassified.",
    incomplete_data_definition = "Incomplete Data if there are no eligible inadequate cases.",
    possible_statuses  = c("Within Target", "Off Target", "Incomplete Data")
  )

  return(list(
    data     = final_summary,
    metadata = meta
  ))

  }

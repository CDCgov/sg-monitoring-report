#' Title
#'
#' @param afp_data
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
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
    "The 90-day lag ensures cases have had sufficient time to receive a classification."
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
  final_summary <- eligible_cases |>
    dplyr::group_by(ctry) |>
    dplyr::summarize(
      inad_cases   = dplyr::n(),
      n_classified = sum(cdc.classification.all2 != "PENDING", na.rm = TRUE),
      n_unclassified = sum(cdc.classification.all2 == "PENDING", na.rm = TRUE),
      prop_unclassified   = round(n_unclassified / inad_cases * 100),
      .groups = "drop") |>
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(ctry),
      flag = case_when(
        prop_unclassified <= 10 ~ "Within Target",
        prop_unclassified > 10 ~ "Above Target",
        TRUE ~ "Review")) |>
    dplyr::select(ctry, whoregion, inad_cases, n_classified, n_unclassified,
                  prop_unclassified, flag)

  meta <- list(
    indicator_code     = "prop_inad_classified",
    indicator_label    = "Proportion of Inadequate Cases Classified",
    eligibility_note   = eligibility_note,
    threshold_rule     = "Above Target if more than 10% of eligible inadequate cases are unclassified",
    definition         = "",
    possible_statuses  = c("Within Target", "Above Target")
  )

  return(list(
    data     = final_summary,
    metadata = meta
  ))

  }

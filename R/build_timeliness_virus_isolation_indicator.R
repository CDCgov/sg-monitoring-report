#' Build Timeliness of Virus Isolation Indicator
#'
#' Calculates the median number of days from receipt in lab to virus isolation
#' result for the current rolling 6-month window. Each month is compared to the
#' same month pooled across the prior 3 years. Returns results in long format
#' with one row per culture/ITD lab per month.
#'
#' @details
#' Timeliness is measured as
#' \code{DateFinalCellCultureResult - DateStoolReceivedinLab}. Samples are
#' assigned to months by \code{DateFinalCellCultureResult}, so the reporting
#' month reflects completed virus isolation results. Intervals outside 0-365
#' days are excluded as likely data quality issues. Records with a missing,
#' blank, or unknown \code{culture.itd.lab} are excluded.
#'
#' @param lab_data A data frame containing lab data. Must include
#'   \code{culture.itd.lab}, \code{DateStoolReceivedinLab}, and
#'   \code{DateFinalCellCultureResult}.
#' @param end_date The maximum date available in the lab dataset for the variable representing the
#'   culture result. Typically passed as \code{max(lab_data$DateFinalCellCultureResult, na.rm = TRUE)}.
#'   Defaults to \code{Sys.Date()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per lab per month
#'     containing: \code{culture.itd.lab}, \code{month_label},
#'     \code{current_n}, \code{current_median_days}, \code{prior_3yr_n},
#'     \code{prior_3yr_median_days}, \code{prior_yrs_w_data},
#'     \code{perc_change}, and \code{flag}.}
#'   \item{metadata}{A named list containing: \code{indicator_code},
#'     \code{indicator_label}, \code{unit}, \code{lab_end_date},
#'     \code{eligibility_note}, \code{current_period_start},
#'     \code{current_period_end}, \code{current_period_label},
#'     \code{prior_period_label}, \code{n_current_months},
#'     \code{n_prior_years}, \code{threshold_rule}, \code{definition}, and
#'     \code{possible_statuses}.}
#' }
#'
#' @examples
#' \dontrun{
#' max_lab_date <- max(lab_data$DateFinalCellCultureResult, na.rm = TRUE)
#' result <- build_timeliness_virus_isolation_indicator(lab_data, max_lab_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_timeliness_virus_isolation_indicator <- function(lab_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  required_cols <- c(
    "culture.itd.lab",
    "DateStoolReceivedinLab",
    "DateFinalCellCultureResult"
  )

  stopifnot(
    "lab_data must be a data frame" = is.data.frame(lab_data),
    "required columns missing from lab_data" = all(required_cols %in% names(lab_data))
  )

  # Date Windows -----
  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  # Lab data end date may fall mid-month — step back to last fully completed month
  analysis_end <- lubridate::floor_date(end_date, unit = "month") %m-% days(1)
  window_start <- lubridate::floor_date(analysis_end %m-% months(5), unit = "month")

  current_months <- seq(window_start, analysis_end, by = "month")

  current_combos <- tibble::tibble(
    year = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month = lubridate::month(current_months, label = TRUE, abbr = TRUE)
  )

  prior_combos <- dplyr::bind_rows(
    dplyr::mutate(current_combos, year = year - 1),
    dplyr::mutate(current_combos, year = year - 2),
    dplyr::mutate(current_combos, year = year - 3)
  )

  # Period Labels -----
  current_period_label <- paste0(
    format(window_start, "%b %Y"),
    " - ",
    format(analysis_end, "%b %Y")
  )

  prior_period_label <- paste0(
    format(window_start %m-% lubridate::years(1:3), "%b %Y"),
    " - ",
    format(analysis_end %m-% lubridate::years(1:3), "%b %Y"),
    collapse = ", "
  )

  eligibility_note <- paste0(
    "Lab data end date is: ", format(end_date, "%b %d, %Y"), ", based on the ",
    "latest date in POLIS for date of final cell culture result (DateFinalCellCultureResult). ",
    "Analysis window is derived from the last complete month prior to lab data end date. ",
    "Samples are assigned to months by DateFinalCellCultureResult."
  )

  # Prepare Data -----

  lab_prep <- lab_data |>
    dplyr::mutate(
      # Prepare month and year columns
      year = lubridate::year(DateFinalCellCultureResult),
      month_num = lubridate::month(DateFinalCellCultureResult),
      month = lubridate::month(DateFinalCellCultureResult, label = TRUE, abbr = TRUE),
      # Clean lab variable for filtering below
      culture.itd.lab = trimws(as.character(culture.itd.lab)),
      # Days recieved in lab to results
      days_lab_to_culture = as.numeric(DateFinalCellCultureResult - DateStoolReceivedinLab)) |>
    dplyr::filter(
      # Remove missing, blank, or unknown culture/ITD labs
      !is.na(culture.itd.lab),
      culture.itd.lab != "", # safety guard
      tolower(culture.itd.lab) != "unknown", # safety guard
      !is.na(days_lab_to_culture),
      dplyr::between(days_lab_to_culture, 0, 365))


  # Current Period Counts -----

  current_counts <- lab_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, year, month_num, month) |>
    dplyr::summarise(
      current_n = dplyr::n(),
      current_median_days = median(days_lab_to_culture, na.rm = TRUE),
      .groups = "drop"
    )

  # Prior Period Counts -----

  # Median of valid samples pooled across the same month in the prior 3 years
  prior_counts <- lab_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, month_num, month) |>
    dplyr::summarise(
      prior_3yr_n = dplyr::n(),
      prior_3yr_median_days = median(days_lab_to_culture, na.rm = TRUE),
      prior_yrs_w_data = dplyr::n_distinct(year),
      .groups = "drop"
    )


  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    culture.itd.lab = unique(lab_prep$culture.itd.lab),
    current_combos |> dplyr::select(year, month_num, month)) |>
    dplyr::left_join(current_counts, by = c("culture.itd.lab", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_counts |>
        dplyr::select(culture.itd.lab, month_num, prior_3yr_n,
                      prior_3yr_median_days, prior_yrs_w_data),
      by = c("culture.itd.lab", "month_num")) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      current_n = tidyr::replace_na(current_n, 0),
      prior_3yr_n = tidyr::replace_na(prior_3yr_n, 0),
      prior_yrs_w_data = tidyr::replace_na(prior_yrs_w_data, 0L),
      perc_change = round(
        (current_median_days - prior_3yr_median_days) / prior_3yr_median_days * 100
      ),
      flag = dplyr::case_when(
        # missing data — cannot calculate change
        current_n == 0 & prior_3yr_n == 0 ~ "No virus isolation data",
        current_n == 0 & prior_3yr_n != 0 ~ "No current virus isolation data",
        current_n != 0 & prior_3yr_n == 0 ~ "No prior virus isolation data",
        is.na(perc_change) ~ "Incomplete Data", # safety guard if something falls through the above

        # threshold
        perc_change < -50 ~ "Above Target",
        perc_change > 50 ~ "Below Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(
      culture.itd.lab, month_label, current_n, current_median_days,
      prior_3yr_n, prior_3yr_median_days, prior_yrs_w_data,
      perc_change, flag
    )


  # Return -----
  meta <- list(
    indicator_code = "timeliness_virus_isolation",
    indicator_label = "Timeliness of Virus Isolation",
    unit = "Month",
    lab_end_date = analysis_end,
    eligibility_note = eligibility_note,
    current_period_start = window_start,
    current_period_end = analysis_end,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 3,
    threshold_rule = "+/-50% of the pooled same-month median from the prior 3 years",
    definition = paste0(
      "Median days between received in lab to virus isolation results. ",
      "Within target if the median timeliness for the current month is +/-50% ",
      "compared with the pooled previous 3-year median of that month."),
    possible_statuses = c("Within Target", "Below Target", "Above Target", "No virus isolation data",
      "No current virus isolation data","No prior virus isolation data", "Incomplete Data", "Review")
    )


  return(list(
    data = final_summary,
    metadata = meta
  ))

}

#' Build Lab Workload Indicator
#'
#' Calculates the number of samples received in each culturing lab for the
#' current rolling 6-month window. Each month is compared to the median count
#' from the same month across the prior 3 years. Returns results in long format
#' with one row per culturing lab per month.
#'
#' @details
#' Workload is counted using \code{DateStoolReceivedinLab}.
#' \code{prior_yrs_w_data} records how many of those prior years had at least
#' one sample. Missing, blank, and unknown \code{culture.itd.lab} values are
#' excluded.
#'
#' @param lab_data A data frame containing lab data. Must include
#'   \code{culture.itd.lab} and \code{DateStoolReceivedinLab}.
#' @param end_date Date used to end the current 6-month reporting window.
#'   Typically passed as \code{max(lab_data$DateStoolReceivedinLab, na.rm = TRUE)}.
#'   This date represents the date received in culturing lab.
#'   Defaults to \code{Sys.Date()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per lab per month
#'     containing: \code{culture.itd.lab}, \code{month_label},
#'     \code{current_n}, \code{prior_3yr_median}, \code{prior_yrs_w_data},
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
#' max_lab_date <- max(lab_data$DateStoolReceivedinLab, na.rm = TRUE)
#' result <- build_lab_workload_indicator(lab_data, max_lab_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_lab_workload_indicator <- function(lab_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  required_cols <- c("culture.itd.lab", "DateStoolReceivedinLab")
  stopifnot(
    "lab_data must be a data frame" = is.data.frame(lab_data),
    "required columns missing from lab_data" = all(required_cols %in% names(lab_data))
  )

  # Date Windows -----
  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)
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
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(analysis_end, "%b %Y"))
  prior_period_label <- paste0(
    format(window_start %m-% lubridate::years(1:3), "%b %Y"),
    " - ",
    format(analysis_end %m-% lubridate::years(1:3), "%b %Y"),
    collapse = ", "
  )

  eligibility_note <- paste0(
    "Lab data end date: ", format(end_date, "%b %d, %Y"), ". ",
    "Analysis window is derived from the last complete month prior to the analysis end date. ",
    "Samples are counted by DateStoolReceivedinLab."
  )

  # Prepare Data -----

  # create indicator lab data
  # filters out missing, blank, and unknown lab names
  lab_prep <- lab_data |>
    dplyr::mutate(
      culture.itd.lab = trimws(as.character(culture.itd.lab)),
      DateStoolReceivedinLab = lubridate::as_date(DateStoolReceivedinLab),
      # Prepare month and year columns
      year = lubridate::year(DateStoolReceivedinLab),
      month_num = lubridate::month(DateStoolReceivedinLab),
      month = lubridate::month(DateStoolReceivedinLab, label = TRUE, abbr = TRUE)
    ) |>
    dplyr::filter(
      !is.na(DateStoolReceivedinLab),
      !is.na(culture.itd.lab),
      culture.itd.lab != "",
      tolower(culture.itd.lab) != "unknown"
    )

  # Current Period Counts -----
  current_counts <- lab_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, year, month_num, month) |>
    dplyr::summarise(
      current_n = dplyr::n(),
      .groups = "drop"
    )

  # Prior Period Counts -----
  prior_counts <- lab_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, year, month_num, month) |>
    dplyr::summarise(
      n = dplyr::n(),
      .groups = "drop"
    )

  # Create prior same-month counts across the prior 3 years for comparison
  prior_full <- tidyr::expand_grid(
    culture.itd.lab = unique(lab_prep$culture.itd.lab),
    prior_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(prior_counts, by = c("culture.itd.lab", "year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, month_num, month) |>
    dplyr::summarise(
      prior_3yr_median = median(n, na.rm = TRUE),
      prior_yrs_w_data = sum(!is.na(n)),
      .groups = "drop"
    )

  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    culture.itd.lab = unique(lab_prep$culture.itd.lab),
    current_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(current_counts, by = c("culture.itd.lab", "year", "month_num", "month")) |>
    dplyr::left_join(prior_full, by = c("culture.itd.lab", "month_num", "month")) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      # For counts, NA is to be assumed as 0
      current_n = tidyr::replace_na(current_n, 0),
      # Create percent change of current count from prior 3-year median
      perc_change = round((current_n - prior_3yr_median) / prior_3yr_median * 100),
      # For workload, higher than expected volume is flagged as Below Target
      # because excess workload is the concern; lower than expected volume is
      # flagged as Above Target.
      flag = dplyr::case_when(
        prior_yrs_w_data == 0 ~ "Incomplete Data",
        perc_change > 50 ~ "Below Target",
        perc_change < -50 ~ "Above Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(
      culture.itd.lab, month_label, current_n, prior_3yr_median,
      prior_yrs_w_data, perc_change, flag
    )

  # Return -----
  meta <- list(
    indicator_code = "lab_workload",
    indicator_label = "Lab Workload",
    unit = "Month",
    lab_end_date = end_date,
    eligibility_note = eligibility_note,
    current_period_start = window_start,
    current_period_end = analysis_end,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 3,
    threshold_rule = "+/-50% of the same-month median from the prior 3 years",
    definition = "Number of samples received in the culturing lab. Within Target if the number of samples for the current month is within +/-50% compared with the previous 3-year median of that month.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

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
#' @param end_date The maximum date available in the lab dataset. Typically
#'   passed as \code{max(lab_data$DateFinalCellCultureResult, na.rm = TRUE)}.
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
    "Lab data end date: ", format(end_date, "%b %d, %Y"), ". ",
    "Analysis window is derived from the last complete month prior to lab data end date. ",
    "Samples are assigned to months by DateFinalCellCultureResult."
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
      DateStoolReceivedinLab = lubridate::as_date(DateStoolReceivedinLab),
      DateFinalCellCultureResult = lubridate::as_date(DateFinalCellCultureResult),
      days.lab.culture = as.numeric(DateFinalCellCultureResult - DateStoolReceivedinLab),
      t1 = !is.na(days.lab.culture) & dplyr::between(days.lab.culture, 0, 365),
      year = lubridate::year(DateFinalCellCultureResult),
      month_num = lubridate::month(DateFinalCellCultureResult),
      month = lubridate::month(DateFinalCellCultureResult, label = TRUE, abbr = TRUE)
    ) |>
    dplyr::filter(t1)

  labs <- lab_data_clean |>
    dplyr::distinct(culture.itd.lab)

  # Current Period Counts -----
  current_counts <- lab_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, year, month_num, month) |>
    dplyr::summarise(
      current_n = dplyr::n(),
      current_median_days = median(days.lab.culture, na.rm = TRUE),
      .groups = "drop"
    )

  # Prior Period Counts -----

  # Median of valid samples pooled across the same month in the prior 3 years
  prior_counts <- lab_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(culture.itd.lab, month_num, month) |>
    dplyr::summarise(
      prior_3yr_n = dplyr::n(),
      prior_3yr_median_days = median(days.lab.culture, na.rm = TRUE),
      prior_yrs_w_data = dplyr::n_distinct(year),
      .groups = "drop"
    )

  prior_summary <- tidyr::expand_grid(
    labs,
    prior_combos |> dplyr::distinct(month_num, month)
  ) |>
    dplyr::left_join(prior_counts, by = c("culture.itd.lab", "month_num", "month"))

  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    labs,
    current_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(current_counts, by = c("culture.itd.lab", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_summary |>
        dplyr::select(
          culture.itd.lab, month_num, prior_3yr_n,
          prior_3yr_median_days, prior_yrs_w_data
        ),
      by = c("culture.itd.lab", "month_num")
    ) |>
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
        current_median_days == 0 | prior_3yr_median_days == 0 ~ "No virus isolation data",
        is.na(perc_change) ~ "No virus isolation data",
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
    lab_end_date = end_date,
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
      "On target if the median timeliness for the current month is +/-50% ",
      "compared with the pooled previous 3-year median of that month."
    ),
    possible_statuses = c(
      "Within Target",
      "Below Target",
      "Above Target",
      "No virus isolation data"
    )
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

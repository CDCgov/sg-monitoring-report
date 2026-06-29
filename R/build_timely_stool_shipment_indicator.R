#' Build Timely Stool Shipment Indicator
#'
#' Calculates the median number of days from stool collection to receipt in lab
#' for the current rolling 6-month window. Each month is compared to the same
#' month in the prior year. Returns results in long format with one row per
#' country per month.
#'
#' @details
#' Timeliness is measured as \code{DateStoolReceivedinLab - DateStoolCollected}.
#' Timeliness intervals outside 0-365 days are excluded as likely data quality
#' issues. \code{CaseDate} is used to assign samples to months.
#'
#' @param lab_data A data frame containing lab data. Must include
#'   \code{CaseDate}, \code{country}, \code{culture.itd.cat},
#'   \code{DateStoolCollected}, and \code{DateStoolReceivedinLab}.
#' @param end_date The maximum date available in the lab dataset. Typically
#'   passed as \code{max(lab_data$CaseDate, na.rm = TRUE)}.
#'   Defaults to \code{Sys.Date()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per month
#'     containing: \code{country}, \code{whoregion}, \code{month_label},
#'     \code{culture.itd.cat}, \code{current_n}, \code{current_median_days},
#'     \code{prior_n}, \code{prior_median_days}, \code{perc_change}, and
#'     \code{flag}.}
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
#' max_lab_date <- max(lab_data$CaseDate, na.rm = TRUE)
#' result <- build_timely_stool_shipment_indicator(lab_data, max_lab_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_timely_stool_shipment_indicator <- function(lab_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  required_cols <- c("CaseDate", "country", "culture.itd.cat",
                     "DateStoolCollected", "DateStoolReceivedinLab")
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

  prior_combos <- dplyr::mutate(current_combos, year = year - 1)

  # Period Labels -----
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(analysis_end, "%b %Y"))
  prior_period_label <- paste0(format(window_start %m-% lubridate::years(1), "%b %Y"), " - ",
                               format(analysis_end %m-% lubridate::years(1), "%b %Y"))

  eligibility_note <- paste0(
    "Lab data end date: ", format(end_date, "%b %d, %Y"), ". ",
    "Analysis window is derived from the last complete month prior to lab data end date. ",
    "Lab data may lag behind end date used for AFP indicators."
  )

  # Prepare Data -----

  # create country+culture.itd.category lookup
  culture_cat_lookup <- lab_data |>
    dplyr::distinct(country, culture.itd.cat) |>
    dplyr::filter(!is.na(culture.itd.cat))

  # create indicator lab data
  lab_prep <- lab_data |>
    dplyr::mutate(
      year = lubridate::year(CaseDate),
      month_num = lubridate::month(CaseDate),
      month = lubridate::month(CaseDate, label = TRUE, abbr = TRUE),
      # Days collection to lab
      days_collect_to_lab = as.numeric(lubridate::as_date(DateStoolReceivedinLab) - lubridate::as_date(DateStoolCollected))) |>
    dplyr::filter(
      !is.na(days_collect_to_lab),
      dplyr::between(days_collect_to_lab, 0, 365)) # data quality limitation


  # Current Period Counts -----
  current_counts <- lab_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(country, culture.itd.cat, year, month_num, month) |>
    dplyr::summarise(
      current_n = dplyr::n(),
      current_median_days = median(days_collect_to_lab, na.rm = TRUE),
      .groups = "drop"
    )

  # Prior Period Counts -----
  prior_counts <- lab_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(country, culture.itd.cat, month_num, month) |>
    dplyr::summarise(
      prior_n = dplyr::n(),
      prior_median_days = median(days_collect_to_lab, na.rm = TRUE),
      .groups = "drop"
    )

  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    country = unique(lab_data$country),
    current_combos |> dplyr::select(year, month_num, month)) |>
    dplyr::left_join(culture_cat_lookup, by = "country") |>
    dplyr::left_join(current_counts, by = c("country", "culture.itd.cat", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_counts |> dplyr::select(country, culture.itd.cat, month_num, prior_n, prior_median_days),
      by = c("country", "culture.itd.cat", "month_num")
    ) |>
    dplyr::mutate(month_label = paste0(month, " ", year)) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      # add region
      whoregion = sirfunctions::get_region(country),
      # For counts, NA is to be assumed as 0
      current_n = tidyr::replace_na(current_n, 0),
      prior_n = tidyr::replace_na(prior_n, 0),
      # Create percent change of median
      perc_change = round((current_median_days - prior_median_days) / prior_median_days * 100),
      flag = dplyr::case_when(
        # if either medians are 0, likely data quality issue as all samples were delivered same day
        current_median_days == 0 | prior_median_days == 0 ~ "Incomplete Data",
        # missing data — cannot calculate change
        is.na(perc_change) ~ "Incomplete Data",
        # threshold
        perc_change < -50 ~ "Above Target",
        perc_change > 50 ~ "Below Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(
      country, whoregion, month_label, culture.itd.cat, current_n, current_median_days,
      prior_n, prior_median_days, perc_change, flag)

  # Return -----
  meta <- list(
    indicator_code = "timely_stool_shipment",
    indicator_label = "Timely Stool Shipment",
    unit = "Month",
    lab_end_date = end_date,
    eligibility_note = eligibility_note,
    current_period_start = window_start,
    current_period_end = analysis_end,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 1,
    threshold_rule = "+/-50% of the same-month median from the prior year",
    definition = "Median days from collection to shipment to lab. Within Target if the median timeliness of the most recent completed month is within +/-50% compared with the same month of the previous year.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

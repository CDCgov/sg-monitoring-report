#' Build Timeliness of ES Shipment Indicator
#'
#' Calculates the median number of days from ES sample collection to arrival in
#' lab for the current rolling 6-month window. Each month is compared to the
#' same month in the prior year. Returns results in long format with one row per
#' country per month.
#'
#' @details
#' Timeliness is measured as \code{date.received.in.lab - collection.date}.
#' Timeliness intervals outside 0-365 days are excluded as likely data quality
#' issues. \code{collection.date} is used to assign samples to months. Lab
#' location \code{es.lab.type} is retained in the output for context only and is
#' not used for the target flag. This builder uses the supplied analysis end date;
#' callers should pass the desired analysis \code{end_date}.
#'
#' @param es_data A data frame containing ES data. Must include
#'   \code{ADM0_NAME}, \code{collection.date}, and
#'   \code{date.received.in.lab}.
#' @param end_date Date used to end the current 6-month reporting window.
#'   Defaults to \code{Sys.Date()}.
#' @param lab_loc Laboratory type metadata. Must include \code{country} and
#'   \code{es.lab.type}. Defaults to \code{sirfunctions::get_lab_locs()}.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data}{A long-format data frame with one row per country per month
#'     containing: \code{country}, \code{whoregion}, \code{month_label},
#'     \code{es.lab.type}, \code{current_count}, \code{current_median_days},
#'     \code{prior_count}, \code{prior_median_days}, \code{perc_change}, and
#'     \code{flag}.}
#'   \item{metadata}{A named list containing indicator label, unit, period
#'     labels, threshold rule, definition, and possible flag values.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- build_timeliness_of_es_shipment_indicator(raw_data$es, end_date)
#' result$data
#' result$metadata$current_period_label
#' }
#'
#' @export
build_timeliness_of_es_shipment_indicator <- function(es_data,
                                                      end_date = Sys.Date(),
                                                      lab_loc = sirfunctions::get_lab_locs()) {

  # Basic initial checks -----
  required_es_cols <- c("ADM0_NAME", "collection.date", "date.received.in.lab")
  required_lab_cols <- c("country", "es.lab.type")

  stopifnot(
    "es_data must be a data frame" = is.data.frame(es_data),
    "required columns missing from es_data" = all(required_es_cols %in% names(es_data)),
    "lab_loc must be a data frame" = is.data.frame(lab_loc),
    "required columns missing from lab_loc" = all(required_lab_cols %in% names(lab_loc))
  )

  # Date Windows -----
  end_date <- lubridate::as_date(end_date)
  window_start <- lubridate::floor_date(end_date %m-% months(5), unit = "month")

  current_months <- seq(window_start, end_date, by = "month")

  current_combos <- tibble::tibble(
    year = lubridate::year(current_months),
    month_num = lubridate::month(current_months),
    month = lubridate::month(current_months, label = TRUE, abbr = TRUE)
  )

  prior_combos <- dplyr::mutate(current_combos, year = year - 1)

  # Period Labels -----
  current_period_label <- paste0(format(window_start, "%b %Y"), " - ", format(end_date, "%b %Y"))
  prior_period_label <- paste0(
    format(window_start %m-% lubridate::years(1), "%b %Y"),
    " - ",
    format(end_date %m-% lubridate::years(1), "%b %Y")
  )

  eligibility_note <- paste0(
    "Analysis window ends on the supplied end_date: ",
    format(end_date, "%b %d, %Y"),
    "."
  )

  # Prepare Data -----
  lab_type <- lab_loc |>
    dplyr::select(country, es.lab.type) |>
    dplyr::distinct() |>
    dplyr::group_by(country) |>
    dplyr::summarise(
      es.lab.type = paste(sort(unique(es.lab.type)), collapse = "; "),
      .groups = "drop"
    )

  es_prep <- es_data |>
    dplyr::rename(country = ADM0_NAME) |>
    dplyr::mutate(
      collection.date = lubridate::as_date(collection.date),
      date.received.in.lab = lubridate::as_date(date.received.in.lab),
      year = lubridate::year(collection.date),
      month_num = lubridate::month(collection.date),
      month = lubridate::month(collection.date, label = TRUE, abbr = TRUE),
      days_collection_to_lab = as.numeric(date.received.in.lab - collection.date)
    ) |>
    dplyr::left_join(lab_type, by = "country") |>
    dplyr::filter(
      !is.na(days_collection_to_lab),
      dplyr::between(days_collection_to_lab, 0, 365)
    )

  # Current Period Counts -----
  current_counts <- es_prep |>
    dplyr::inner_join(current_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(country, year, month_num, month) |>
    dplyr::summarise(
      current_count = dplyr::n(),
      current_median_days = median(days_collection_to_lab, na.rm = TRUE),
      .groups = "drop"
    )

  # Prior Period Counts -----
  prior_counts <- es_prep |>
    dplyr::inner_join(prior_combos, by = c("year", "month_num", "month")) |>
    dplyr::group_by(country, month_num, month) |>
    dplyr::summarise(
      prior_count = dplyr::n(),
      prior_median_days = median(days_collection_to_lab, na.rm = TRUE),
      .groups = "drop"
    )

  # Full grid and join for full table -----
  final_summary <- tidyr::expand_grid(
    country = unique(es_data$ADM0_NAME),
    current_combos |> dplyr::select(year, month_num, month)
  ) |>
    dplyr::left_join(lab_type, by = "country") |>
    dplyr::left_join(current_counts, by = c("country", "year", "month_num", "month")) |>
    dplyr::left_join(
      prior_counts |> dplyr::select(country, month_num, prior_count, prior_median_days),
      by = c("country", "month_num")
    ) |>
    dplyr::mutate(
      month_label = paste0(month, " ", year)
    ) |>
    dplyr::select(-year, -month, -month_num) |>
    dplyr::mutate(
      whoregion = sirfunctions::get_region(country),
      current_count = tidyr::replace_na(current_count, 0),
      prior_count = tidyr::replace_na(prior_count, 0),
      perc_change = round((current_median_days - prior_median_days) / prior_median_days * 100),
      flag = dplyr::case_when(
        is.na(perc_change) ~ "Incomplete Data",
        prior_median_days == 0 ~ "Incomplete Data",
        perc_change < -50 ~ "Above Target",
        perc_change > 50 ~ "Below Target",
        dplyr::between(perc_change, -50, 50) ~ "Within Target",
        TRUE ~ "Review"
      )
    ) |>
    dplyr::select(
      country, whoregion, month_label, es.lab.type,
      current_count, current_median_days, prior_count,
      prior_median_days, perc_change, flag
    )

  # Return -----
  meta <- list(
    indicator_code = "timeliness_of_es_shipment",
    indicator_label = "Timeliness of ES Shipment",
    unit = "Month",
    end_date = end_date,
    eligibility_note = eligibility_note,
    current_period_start = window_start,
    current_period_end = end_date,
    current_period_label = current_period_label,
    prior_period_label = prior_period_label,
    n_current_months = 6,
    n_prior_years = 1,
    threshold_rule = "+/-50% of the same-month median from the prior year",
    definition = "Median number of days between collection and arrival in lab for ES samples. Within Target if the median number of days for the month is within +/-50% compared with the same month of the previous year. Below Target if the median is more than 50% higher than the same month of the previous year.",
    above_target_definition = "Above Target if the median is more than 50% lower than the same month of the previous year.",
    incomplete_data_definition = "Incomplete Data if the current or prior median is missing or the prior median is 0.",
    possible_statuses = c("Within Target", "Below Target", "Above Target", "Incomplete Data")
  )

  return(list(
    data = final_summary,
    metadata = meta
  ))

}

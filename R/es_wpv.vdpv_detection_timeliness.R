#' Timeliness of WPV/VDPV Detection from ES Samples
#' @description Days between ES sample collection and final laboratory result for WPV/VDPV positive ES samples (Targets: 35 days for full capacity labs, 46 days for limited capacity labs; ≥80% target)
#' @param es_data ES data
#' @param end_date Reference date for analysis (default: current date)
#' @returns Grouped tibble with WPV/VDPV detection timeliness by region, country, and month
#' @export
#' @examples
#' \dontrun{
#' es_wpv.vdpv_detection_timeliness(es_data)
#' }
es_wpv.vdpv_detection_timeliness <- function(es_data, end_date = Sys.Date()) {

  # Filter for WPV/VDPV positive samples
  es_data <- es_data |>
    dplyr::filter(wpv == 1 | vdpv == 1)

  if(nrow(es_data) == 0) {
    cli::cli_abort("No WPV/VDPV positive ES samples found")
  }

  # Data validation and target year filtering
  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date)

  valid_data <- activity_dates_data_validation(
    data = es_data,
    date_columns = c("collection.date", "date.final.combined.result"),
    categorical_columns = c("wpv", "vdpv")
  ) |>
    dplyr::filter(lubridate::year(collection.date) %in% (current_year - 1):current_year,
    lubridate::month(collection.date) <= current_month)
  if (nrow(valid_data) == 0) cli::cli_abort("Unable to calculate detection timeliness due to missing data")

  # Calculate detection timeliness results
  result <- valid_data |>
    dplyr::mutate(
      days_to_final_result = as.numeric(as.Date(date.final.combined.result) - as.Date(collection.date)),
      year = lubridate::year(collection.date),
      month = lubridate::month(collection.date),
      month_name = month.abb[month],
      meets_full_capacity_target = days_to_final_result <= 35,
      meets_limited_capacity_target = days_to_final_result <= 46,
      wpv_detected = wpv == 1,
      vdpv_detected = vdpv == 1
    ) |>
    dplyr::group_by(who.region, ADM0_NAME, year, month, month_name) |>
    dplyr::summarise(
      total_wpv_vdpv_detected = dplyr::n(),
      wpv_detected = sum(wpv_detected, na.rm = TRUE),
      vdpv_detected = sum(vdpv_detected, na.rm = TRUE),
      detection_within_35days = sum(meets_full_capacity_target, na.rm = TRUE),
      pct_full_capacity_lab = round(detection_within_35days / dplyr::n() * 100, 1),
      detection_within_46days = sum(meets_limited_capacity_target, na.rm = TRUE),
      pct_limited_capacity_lab = round(detection_within_46days / dplyr::n() * 100, 1),
      .groups = "drop"
    )

  # Calculate monthly median comparisons
  baseline_data <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) == current_year - 1) |>
    dplyr::mutate(
      days_to_final_result = as.numeric(as.Date(date.final.combined.result) - as.Date(collection.date)),
      month = lubridate::month(collection.date)
    ) |>
    dplyr::group_by(who.region, ADM0_NAME, month) |>
    dplyr::summarise(median_baseline = ifelse(dplyr::n() > 0, round(median(days_to_final_result, na.rm = TRUE), 1), NA_real_), .groups = "drop")

  current_data <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) == current_year) |>
    dplyr::mutate(
      days_to_final_result = as.numeric(as.Date(date.final.combined.result) - as.Date(collection.date)),
      month = lubridate::month(collection.date)
    ) |>
    dplyr::group_by(who.region, ADM0_NAME, month) |>
    dplyr::summarise(median_current = ifelse(dplyr::n() > 0, round(median(days_to_final_result, na.rm = TRUE), 1), NA_real_), .groups = "drop")

  result <- result |>
    dplyr::left_join(baseline_data, by = c("who.region", "ADM0_NAME", "month")) |>
    dplyr::left_join(current_data, by = c("who.region", "ADM0_NAME", "month")) |>
    dplyr::mutate(
      !!paste0("median_", current_year-1) := median_baseline,
      !!paste0("median_", current_year) := median_current,
      median_difference = ifelse(is.na(median_baseline) | is.na(median_current), NA_real_, round(median_baseline - median_current, 1))
    ) |>
    dplyr::select(-median_baseline, -median_current, -month) |>
    dplyr::rename_with(~ dplyr::case_when(. == "who.region" ~ "region", . == "ADM0_NAME" ~ "country", TRUE ~ .))

  # Output
  message("* WPV/VDPV detection timeliness (targets: 35 days full capacity, 46 days limited capacity, ≥80% target for both)")
  result |>
    print(width = Inf)
}

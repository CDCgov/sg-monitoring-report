#' ES Timeliness Analysis
#' @description Calculate timeliness metrics for ES samples, both shipment to lab and WPV/VDPV detection
#' @param es_data ES data
#' @param lab_loc Laboratory type data with columns 'country' and 'es.lab.type'
#' @param end_date Reference date for analysis (default: current date)
#' @returns Validation metrics and Timeliness results for both indicators grouped by region, country, and month
#' @export
#' @examples
#' \dontrun{
#' # Default to analyze both shipment and detection timeliness
#' es_timeliness(es_data, lab_loc)
#'
#' # With custom end date
#' es_timeliness(es_data, lab_loc, end_date = as.Date("2022-05-25"))
#' }
es_timeliness <- function(es_data, lab_loc, end_date = Sys.Date()) {

  # Initial Setup
  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date)
  admin0_columns <- c("ADM0_NAME", "admin.0.officialname", "admin.0.vizname", "country.iso3")
  baseline_years <- (current_year - 3):(current_year - 1)

  # Add lab type once (only affects shipment)
  es_data$es.lab.type <- NA_character_
  for(i in seq_len(nrow(lab_loc))) {
    for(col in admin0_columns[admin0_columns %in% names(es_data)]) {
      matches <- which(toupper(trimws(es_data[[col]])) == toupper(trimws(lab_loc$country[i])))
      if(length(matches) > 0) {
        es_data$es.lab.type[matches] <- lab_loc$es.lab.type[i]
        break
      }
    }
  }

  # Calculate shipment timeliness
  message("ES Shipment Data Validation")
  shipment_result <- activity_dates_data_validation(es_data, c("collection.date", "date.received.in.lab"), "es.lab.type") |>
    dplyr::filter(
      lubridate::year(collection.date) %in% (current_year - 3):current_year,
      lubridate::month(collection.date) <= current_month
    ) |>
    dplyr::mutate(
      days_diff = as.numeric(as.Date(date.received.in.lab) - as.Date(collection.date)),
      meets_target = dplyr::case_when(
        tolower(trimws(es.lab.type)) == "in-country" ~ days_diff <= 3,
        tolower(trimws(es.lab.type)) == "international" ~ days_diff <= 7,
        TRUE ~ days_diff <= 7
      ),
      timeliness_type = "es_shipment"
    )

  # Calculate wpv/vdpv detection timeliness
  message("ES-WPV/VDPV Detection Timeliness Data Validation")
  detection_result <- es_data |>
    dplyr::filter(wpv == 1 | vdpv == 1) |>
    activity_dates_data_validation(c("collection.date", "date.final.combined.result"), c("wpv", "vdpv")) |>
    dplyr::filter(
      lubridate::year(collection.date) %in% (current_year - 1):current_year,
      lubridate::month(collection.date) <= current_month
    ) |>
    dplyr::mutate(
      days_diff = as.numeric(as.Date(date.final.combined.result) - as.Date(collection.date)),
      meets_target = days_diff <= 35,
      timeliness_type = "es_wpv/vdpv_detection"
    )

  # Combined data result
  all_results <- dplyr::bind_rows(
    if (nrow(shipment_result) > 0) shipment_result else NULL,
    if (nrow(detection_result) > 0) detection_result else NULL
  ) |>
    dplyr::mutate(
      year = lubridate::year(collection.date),
      month = lubridate::month(collection.date)
    )

  if (nrow(all_results) == 0) cli::cli_abort("Unable to calculate timeliness due to unavailable data")

  # Calculate baseline and current medians
  baseline_label <- paste0("median_", min(baseline_years), "-", max(baseline_years))
  current_label <- paste0("median_", current_year)

  baseline_medians <- all_results |>
    dplyr::filter((timeliness_type == "es_shipment" & year %in% baseline_years) |
                    (timeliness_type == "es_wpv/vdpv_detection" & year == (current_year - 1))) |>
    dplyr::group_by(timeliness_type, who.region, ADM0_NAME, month) |>
    dplyr::summarise(!!baseline_label := round(median(days_diff, na.rm = TRUE), 1), .groups = "drop")

  current_medians <- all_results |>
    dplyr::filter(year == current_year) |>
    dplyr::group_by(timeliness_type, who.region, ADM0_NAME, month) |>
    dplyr::summarise(!!current_label := round(median(days_diff, na.rm = TRUE), 1), .groups = "drop")

  # Process final results
  final_result <- all_results |>
    dplyr::group_by(timeliness_type, who.region, ADM0_NAME, year, month) |>
    dplyr::summarise(
      month_name = first(month.abb[month]),
      pct_met_target = round(mean(meets_target, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) |>
    dplyr::left_join(baseline_medians, by = c("timeliness_type", "who.region", "ADM0_NAME", "month")) |>
    dplyr::left_join(current_medians, by = c("timeliness_type", "who.region", "ADM0_NAME", "month")) |>
    dplyr::mutate(
      diff = round(.data[[baseline_label]] - .data[[current_label]], 1),
      `difference (days)` = dplyr::case_when(
        is.na(diff) ~ NA_character_,
        diff == 0 ~ "same",
        abs(diff) <= 1 ~ paste0(ifelse(diff > 0, "less", "more"), " by ", abs(diff), " day"),
        TRUE ~ paste0(ifelse(diff > 0, "less", "more"), " by ", abs(diff), " days")
      )
    ) |>
    dplyr::select(-diff, -month) |>
    dplyr::rename(region = who.region, country = ADM0_NAME)

  # Output
  message("* ES Timeliness Indicators:")
  message("  - Lab shipment targets: in-country ≤3 days, international ≤7 days, unknown lab type ≤7 days")
  message("  - Poliovirus detection targets: full capacity ≤35 days, limited capacity ≤46 days (using 35-day target for % met)")
  message("  - Baseline periods: Shipment ", min(baseline_years), "-", max(baseline_years), " / Detection ", current_year - 1, " vs Current year: ", current_year)

  final_result |> print(width = Inf)
}

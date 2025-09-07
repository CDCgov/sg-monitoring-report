#' Timeliness of ES Shipment
#' @description Days between ES sample collection and lab receipt of shipped samples (Target: 3 days for Domestic Labs and 7 days for International Labs)
#' @param es_data ES data
#' @param lab_loc Laboratory type data with columns 'country' and 'es.lab.type'
#' @param end_date Reference date for analysis (default: current date)
#' @returns Grouped tibble with lab receipt timeliness by region, country, and month
#' @export
#' @examples
#' \dontrun{
#' es_sample_shipment_timeliness(es_data, lab_loc)
#' }
es_sample_shipment_timeliness <- function(es_data, lab_loc, end_date = Sys.Date()) {
  # Prepare data with lab types
  admin0_columns <- c("ADM0_NAME", "admin.0.officialname", "admin.0.vizname", "country.iso3")
  es_data <- es_data |>
    dplyr::mutate(es.lab.type = NA_character_)

  for(i in seq_len(nrow(lab_loc))) {
    for(col in admin0_columns[admin0_columns %in% names(es_data)]) {
      matches <- which(toupper(trimws(es_data[[col]])) == toupper(trimws(lab_loc$country[i])))
      if(length(matches) > 0) {
        es_data$es.lab.type[matches] <- lab_loc$es.lab.type[i]
        break}
    }
  }

  # Data validation and target year filtering
  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date)

  valid_data <- activity_dates_data_validation(
    data = es_data,
    date_columns = c("collection.date", "date.received.in.lab"),
    categorical_columns = c("es.lab.type")) |>
    dplyr::filter(lubridate::year(collection.date) %in% (current_year - 3):current_year,
      lubridate::month(collection.date) <= current_month)

  if (nrow(valid_data) == 0) cli::cli_abort("Unable to calculate timeliness due to missing data")

  # Calculate timeliness results
  result <- valid_data |>
    dplyr::mutate(
      days_to_lab_receipt = as.numeric(as.Date(date.received.in.lab) - as.Date(collection.date)),
      year = lubridate::year(collection.date),
      month = lubridate::month(collection.date),
      month_name = month.abb[month],
      meets_target = dplyr::case_when(
        tolower(trimws(es.lab.type)) == "in-country" ~ days_to_lab_receipt <= 3,
        tolower(trimws(es.lab.type)) == "international" ~ days_to_lab_receipt <= 7,
        TRUE ~ days_to_lab_receipt <= 7)) |>
    dplyr::group_by(who.region, ADM0_NAME, year, month, month_name) |>
    dplyr::summarise(
      es_samples_with_valid_dates = dplyr::n(),
      lab_receipt_within_target = sum(meets_target, na.rm = TRUE),
      pct_lab_receipt_within_target = round(lab_receipt_within_target / sum(!is.na(meets_target)) * 100, 1),
      .groups = "drop")

  # Calculate and join median comparisons
  baseline_data <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) %in% (current_year-3):(current_year-1)) |>
    dplyr::mutate(days_to_lab_receipt = as.numeric(as.Date(date.received.in.lab) - as.Date(collection.date)), month = lubridate::month(collection.date)) |>
    dplyr::group_by(who.region, ADM0_NAME, month) |>
    dplyr::summarise(median_baseline = ifelse(dplyr::n() > 0, round(median(days_to_lab_receipt, na.rm = TRUE), 1), NA_real_), .groups = "drop")

  current_data <- valid_data |>
    dplyr::filter(lubridate::year(collection.date) == current_year) |>
    dplyr::mutate(days_to_lab_receipt = as.numeric(as.Date(date.received.in.lab) - as.Date(collection.date)), month = lubridate::month(collection.date)) |>
    dplyr::group_by(who.region, ADM0_NAME, month) |>
    dplyr::summarise(median_current = ifelse(dplyr::n() > 0, round(median(days_to_lab_receipt, na.rm = TRUE), 1), NA_real_), .groups = "drop")

  result <- result |>
    dplyr::left_join(baseline_data, by = c("who.region", "ADM0_NAME", "month")) |>
    dplyr::left_join(current_data, by = c("who.region", "ADM0_NAME", "month")) |>
    dplyr::mutate(
      !!paste0("median_", current_year-3, "_", current_year-1) := median_baseline,
      !!paste0("median_", current_year) := median_current,
      median_difference = ifelse(is.na(median_baseline) | is.na(median_current), NA_real_, round(median_baseline - median_current, 1)),
      `difference (days)` = dplyr::case_when(
        is.na(median_difference) ~ NA_character_,
        median_difference == 0 ~ "same",
        abs(median_difference) <= 1 ~ paste0(
          ifelse(median_difference > 0, "less by ", "more by "),
          abs(median_difference), " day"
        ),
        median_difference > 0 ~ paste0("less by ", median_difference, " days"),
        median_difference < 0 ~ paste0("more by ", abs(median_difference), " days")
      )
    ) |>
    dplyr::select(-median_baseline, -median_current, -month, -median_difference) |>
    dplyr::rename_with(~ dplyr::case_when(. == "who.region" ~ "region", . == "ADM0_NAME" ~ "country", TRUE ~ .))

  # Output
  message("* ES samples lab receipt timeliness (in-country: 3 days, international: 7 days, unknown lab type: 7 days)")
  result |>
    print(width = Inf)
}

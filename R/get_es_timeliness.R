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
#' es_timeliness(es_data, lab_loc, end_date = as.Date("2024-06-20"))
#' }
get_es_timeliness <- function(es_data, lab_loc = sirfunctions::get_lab_locs(), end_date = Sys.Date()) {
  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date, TRUE)

  valid_es_data <- es_data |>
    dplyr::rename(country = "ADM0_NAME") |>
    dplyr::mutate(days.col.rec.lab = as.numeric(difftime(date.received.in.lab, collection.date, units = "days")),
                  days.col.notif.hq = as.numeric(difftime(date.notification.to.hq, collection.date, units = "days")),
                  month = lubridate::month(collection.date, label = TRUE),
                  year = lubridate::year(collection.date)) |>
    dplyr::filter(dplyr::between(year, current_year - 1, current_year),
                  month <= current_month)

  # Join lab information
  valid_es_data <- dplyr::left_join(valid_es_data,
                                    lab_loc |> dplyr::select(country, es.lab.type))


  timeliness_summary <- valid_es_data |>
    dplyr::select(who.region, country, es.lab.type, year, month, days.col.rec.lab) |>
    dplyr::filter(dplyr::between(days.col.rec.lab, 0, 365)) |>
    dplyr::group_by(who.region, year, country, es.lab.type, month) |>
    dplyr::summarize(median_lab_shipment = median(days.col.rec.lab, na.rm = TRUE),
                     median_lab_shipment_label = paste0(median_lab_shipment, " (n=", dplyr::n(), ")"))


  timeliness_summary_vdpv_wpv <- valid_es_data |>
    dplyr::filter(wpv == 1 | vdpv == 1,
                  dplyr::between(days.col.notif.hq, 0, 365)) |>
    dplyr::select(who.region, country, es.lab.type, year, month, days.col.notif.hq) |>
    dplyr::group_by(who.region, year, country, es.lab.type, month) |>
    dplyr::summarize(median_wpv_vdpv_detection = median(days.col.notif.hq, na.rm = TRUE),
                     median_wpv_vdpv_detection_label = paste0(median_wpv_vdpv_detection, " (n=", dplyr::n(), ")"))

  timeliness_summary_all <- dplyr::left_join(timeliness_summary,
                                             timeliness_summary_vdpv_wpv)

  # Create combinations of year, month, country
  complete_table <- tidyr::expand_grid(
    year = c(current_year - 1, current_year),
    month = unique(valid_es_data$month),
    country = unique(valid_es_data$country)) |>
    dplyr::left_join(
      lab_loc |>
        dplyr::distinct(country, who.region, es.lab.type))

  # Ensure that all countries and months are accounted for
  timeliness_summary_labels <- dplyr::left_join(complete_table,
                                                timeliness_summary_all) |>
    dplyr::select(-median_lab_shipment, -median_wpv_vdpv_detection) |>
    dplyr::rename(median_lab_shipment = "median_lab_shipment_label",
                  median_wpv_vdpv_detection = "median_wpv_vdpv_detection_label") |>
    tidyr::pivot_longer(cols = dplyr::any_of(c("median_lab_shipment", "median_wpv_vdpv_detection")),
                        names_to = "category",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = "year", values_from = "value")

  timeliness_summary_full <- dplyr::left_join(complete_table,
                                              timeliness_summary_all) |>
    dplyr::select(-median_lab_shipment_label, -median_wpv_vdpv_detection_label) |>
    tidyr::pivot_longer(cols = dplyr::any_of(c("median_lab_shipment", "median_wpv_vdpv_detection")),
                        names_to = "category",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = "year", values_from = "value") |>
    dplyr::mutate(diff = round(.data[[paste0(current_year)]] - .data[[paste0(current_year - 1)]], 1)) |>
    dplyr::arrange(month, who.region, country, category) |>
    dplyr::mutate(trend = dplyr::case_when(
      diff == 0 ~ "Same",
      diff > 0 ~ "Increase",
      diff < 0 ~ "Decrease",
      .default = "No data available for both years"
    ), current_year_timeliness = dplyr::case_when(
      category == "median_lab_shipment" & .data[[paste0(current_year)]] <= 3 & es.lab.type == "In-country" ~ "Timely",
      category == "median_lab_shipment" & .data[[paste0(current_year)]] <= 7 & es.lab.type == "International" ~ "Timely",
      category == "median_lab_shipment" & .data[[paste0(current_year)]] > 3 & es.lab.type == "In-country" ~ "Not timely",
      category == "median_lab_shipment" & .data[[paste0(current_year)]] > 7 & es.lab.type == "International" ~ "Not timely",

      category == "median_wpv_vdpv_detection" & .data[[paste0(current_year)]] <= 35 & es.lab.type == "In-country" ~ "Timely",
      category == "median_wpv_vdpv_detection" & .data[[paste0(current_year)]] <= 46 & es.lab.type == "International" ~ "Timely",
      category == "median_wpv_vdpv_detection" & .data[[paste0(current_year)]] > 35 & es.lab.type == "In-country" ~ "Not timely",
      category == "median_wpv_vdpv_detection" & .data[[paste0(current_year)]] > 46 & es.lab.type == "International" ~ "Not timely"
    ),
    trend_summary = dplyr::case_when(
      trend == "Increase" & current_year_timeliness == "Timely" ~ "Worse but still timely this year",
      trend == "Increase" & current_year_timeliness == "Not timely" ~ "Worse and not timely this year",
      trend == "Decrease" & current_year_timeliness == "Timely" ~ "Improved from last year and timely this year",
      trend == "Decrease" & current_year_timeliness == "Not timely" ~ "Improved from last year but not timely this year",
      trend == "Same" & current_year_timeliness == "Not timely" ~ "Not timely like last year",
      trend == "Same" & current_year_timeliness == "Timely" ~ "Timely like last year",
      .default = "Unable to detect trend"),
    timeliness_target = dplyr::case_when(
      es.lab.type == "In-country" & category == "median_lab_shipment" ~ 3,
      es.lab.type == "International" & category == "median_lab_shipment" ~ 7,
      es.lab.type == "In-country" & category == "median_wpv_vdpv_detection" ~ 35,
      es.lab.type == "International" & category == "median_wpv_vdpv_detection" ~ 46,
    ))

  # Combine with counts
  timeliness_summary_complete <- dplyr::full_join(timeliness_summary_labels,
                                              timeliness_summary_full |>
                                                dplyr::select(-dplyr::any_of(c(6,7))),
                                              by = c("month", "country", "who.region", "es.lab.type",
                                                                              "category"))

  return(timeliness_summary_complete)

}

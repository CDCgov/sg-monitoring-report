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
calculate_es_timeliness <- function(es_data, lab_loc, end_date = Sys.Date()) {

  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date)
  admin0_columns <- c("ADM0_NAME", "admin.0.officialname", "admin.0.vizname", "country.iso3")
  baseline_years <- (current_year - 3):(current_year - 1)

  valid_es_data <- dplyr::left_join(es_data |> dplyr::rename(country = "ADM0_NAME"),
                                    lab_loc) |>
    dplyr::mutate(days.col.rec.lab = as.numeric(difftime(date.received.in.lab, collection.date, units = "days")),
                  days.col.notif.hq = as.numeric(difftime(date.notification.to.hq, collection.date, units = "days")),
                  month = lubridate::month(collection.date, label = TRUE),
                  year = lubridate::year(collection.date)) |>
    dplyr::filter(!is.na(days.col.rec.lab) | dplyr::between(days.col.rec.lab, 0, 365),
                  year >= current_year - 1)

  timeliness_summary <- valid_es_data |>
    dplyr::select(who.region, country, year, month, days.col.rec.lab) |>
    dplyr::group_by(who.region, year, country, month) |>
    dplyr::summarize(median_lab_shipment = median(days.col.rec.lab, na.rm = TRUE))
  timeliness_summary_vdpv_wpv <- valid_es_data |>
    dplyr::filter(stringr::str_detect(virus.type, "VDPV|WILD")) |>
    dplyr::select(who.region, country, year, month, days.col.notif.hq) |>
    dplyr::group_by(who.region, year, country, month) |>
    dplyr::summarize(median_wpv_vdpv_detection = median(days.col.notif.hq, na.rm = TRUE))

  timeliness_summary_all <- dplyr::full_join(timeliness_summary,
                                             timeliness_summary_vdpv_wpv)

  # Create combinations of year, month, country
  complete_table <- tidyr::expand_grid(
    year = c(current_year - 1, current_year),
    month = unique(valid_es_data$month),
    country = unique(es_data$country)
  )

  # Ensure that all countries and months are accounted for
  timeliness_summary_full <- dplyr::full_join(complete_table,
                                              timeliness_summary_all) |>
    tidyr::pivot_longer(cols = dplyr::any_of(c("median_lab_shipment", "median_wpv_vdpv_detection")),
                        names_to = "category",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = "year", values_from = "value")

  return(timeliness_summary_full)

}

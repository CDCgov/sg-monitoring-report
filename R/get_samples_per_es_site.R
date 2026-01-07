#' Get number of samples per site
#'
#' @description
#' Calculates the number of samples per site.
#'
#'
#' @param es_data `tibble` Global ES data.
#' @param end_date `str` End date of the analysis.
#'
#' @returns `tibble` Number of samples per site
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data()
#' es_summary <- get_samples_per_es_site(raw_data$es)
#' }
get_samples_per_es_site <- function(es_data, end_date = Sys.Date()) {
  end_date <- lubridate::as_date(end_date)


  # Get age of sites
  site_ages <- sirfunctions:::get_es_site_age(es_data, end_date)

  # Get latest collection date and see if there's a "missed" collection
  latest_collection <- es_data |>
    dplyr::filter(who.region %in% c("AFRO", "EMRO")) |>
    dplyr::group_by(ADM0_NAME, site.name, site.status) |>
    dplyr::summarize(earliest_collection = min(collect.date, na.rm = TRUE),
                     last_collection = max(collect.date, na.rm = TRUE)) |>
    dplyr::mutate(days_since_last_collection = difftime(end_date, last_collection, units = "days"),
                  collection_two_mo = dplyr::if_else(days_since_last_collection <= 60, "Yes", "No"))

  # Latest EV detection
  latest_ev_det <- es_data |>
    dplyr::filter(who.region %in% c("AFRO", "EMRO"),
                  ev.detect == 1) |>
    dplyr::group_by(ADM0_NAME, site.name, site.status) |>
    dplyr::summarize(last_detection = max(collect.date, na.rm = TRUE)) |>
    dplyr::mutate(days_since_last_det = difftime(end_date, last_detection),
                  detection_two_mo = dplyr::if_else(days_since_last_det <= 60, "Yes", "No"))

  # Combine
  summary <- dplyr::left_join(latest_collection, latest_ev_det) |>
    dplyr::left_join(site_ages)

  # Determine if considered an "active site"
  summary <- summary |>
    dplyr::mutate(active_site = dplyr::if_else(n_samples_12_mo >= 10 &
                                          site_age >= 12 &
                                          site.status != "CLOSED", "Yes", "No"),
                  site_age = round(site_age)) |>
    dplyr::select(-sampling_interval) |>
    dplyr::mutate(failing_performance = dplyr::case_when(
      n_samples_12_mo < 10 & collection_two_mo == "No" & site_age >= 12 ~ "Yes (No collections two months, <10 samples in rolling 12-month period)",
      collection_two_mo == "No" & site_age >= 12 ~ "Yes (No collections from last two months)",
      n_samples_12_mo < 10 & site_age >= 12 ~ "Yes (<10 samples collected in 12-month rolling period)",
      collection_two_mo == "Yes" & n_samples_12_mo >= 10 & site_age >= 12 ~ "No",
      site_age < 12 ~ "Unable to assess, (Site < 12 months)",
      .default = "Unable to assess"
      )
    )

  return(summary)

  }

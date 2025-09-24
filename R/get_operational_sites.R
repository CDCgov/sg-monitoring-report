#' Number of operational ES sites per country
#'
#' @description
#' Calculate the number of operational sites per country.
#'
#' @param es_data `tibble` ES surveillance data
#' @param end_date `str` Reference end_date for analysis. Defaults to the current date.
#'
#' @returns `tibble` Number of operational sites per country.
#' @export
#' @examples
#' \dontrun{
#' es_samples_operational_sites(es_data)
#' es_samples_operational_sites(es_data, end_date = as.Date("2024-06-30"))
#' }
get_operational_sites <- function(es_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)
  current_month <- lubridate::month(end_date, TRUE)
  current_year <- lubridate::year(end_date)

  active_sites <- sirfunctions:::get_es_site_age(es_data, end_date)
  prev_year_active_sites <- sirfunctions:::get_es_site_age(es_data, end_date - lubridate::years(1))

  # Filter for active sites only
  active_sites <- active_sites |>
    dplyr::filter(n_samples_12_mo >= 10, site_age >= 12)
  prev_year_active_sites <- prev_year_active_sites |>
    dplyr::filter(n_samples_12_mo >= 10, site_age >= 12)

  current_year_active_site_summary <- active_sites |>
    dplyr::distinct() |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::summarize(operational_sites = dplyr::n()) |>
    dplyr::mutate(year = current_year)
  prev_year_active_site_summary <- prev_year_active_sites |>
    dplyr::distinct() |>
    dplyr::group_by(ADM0_NAME) |>
    dplyr::summarize(operational_sites = dplyr::n()) |>
    dplyr::mutate(year = current_year - 1)

  active_site_summary <- dplyr::full_join(current_year_active_site_summary,
                                          prev_year_active_site_summary)

  # Get combinations of all countries and year
  all_country_year <- tidyr::expand_grid(
    year = c(current_year - 1, current_year),
    ADM0_NAME = unique(es_data$ADM0_NAME)
  )

  # Combine to get complete picture
  active_site_summary <- dplyr::full_join(all_country_year, active_site_summary)

  # Replace NAs with 0 and pivot
  active_site_summary <- active_site_summary |>
    dplyr::mutate(operational_sites = ifelse(is.na(operational_sites), 0, operational_sites)) |>
    dplyr::rename(ctry = ADM0_NAME)

  # Yearly summary
  active_site_summary_wide <- active_site_summary |>
    dplyr::mutate(year = paste0(current_month," ", year)) |>
    tidyr::pivot_wider(names_from = year, values_from = operational_sites)
  active_site_summary_wide["comparison"] <- active_site_summary_wide[, 3] - active_site_summary_wide[, 2]
  active_site_summary_wide <- active_site_summary_wide |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "same",
      comparison > 0 ~ "increase",
      comparison < 0 ~ "decrease"
    ))

  cli::cli_alert_info(paste0("Note: active sites are anchored based on the end date specified.",
                             " An active site is a site open for at least 12 months",
                             " with 10 samples in the past 12 months."))

  return(active_site_summary_wide |> dplyr::arrange(comparison))

}

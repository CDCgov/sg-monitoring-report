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

  # Obtain operational sites
  operational_sites <- site_ages |>
    dplyr::filter(n_samples_12_mo >= 3, site_age >= 12) |>
    dplyr::mutate(operational_site = TRUE)

  # Get latest collection date and see if there's a "missed" collection
  latest_collection <- es_data |>
    dplyr::select(env.sample.id, site.name, country = ADM0_NAME, collect.date) |>
    dplyr::filter(collect.date >= (end_date %m-% months(12))) |> # limit to last 12 months
    dplyr::mutate(month = month(collect.date, label = TRUE),
                  year = year(collect.date)) |>
    dplyr::left_join(operational_sites) |>
    dplyr::filter(operational_site) |>
    dplyr::group_by(year, month, country, site.name) |>
    dplyr::summarize(n_collections = dplyr::n()) |>
    dplyr::ungroup()

  latest_collection_complete <- latest_collection |>
    tidyr::complete(
      month = month.abb,
      year = unique(latest_collection$year),
      nesting(country, site.name),
      fill = list(n_collections = 0)
    ) |>
    dplyr::mutate(month = factor(month, ordered = TRUE, levels = month.abb))

  monthly_summary <- latest_collection_complete |>
    dplyr::group_by(year, country, month) |>
    dplyr::summarize(median_collections = median(n_collections, na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(ym = paste0(year, "-", month, "-1")) |>
    dplyr::mutate(ym = readr::parse_date(ym, format = "%Y-%b-%d")) |>
    dplyr::filter(ym >= (end_date %m-% months(12)))

  return(monthly_summary)

  }

#' ES Samples Per Site Analysis
#' @description Analyze ES performance metrics at country level on operational sites, sample collections, and median trends
#' @param es_data ES surveillance data
#' @param end_date Reference end_date for analysis (default end_date: current date)
#' @returns number of operational sites per country and total sample colleciton from operational sites each month
#' @export
#' @examples
#' \dontrun{
#' es_samples_operational_sites(es_data)
#' es_samples_operational_sites(es_data, end_date = as.Date("2024-06-30"))
#' }
es_samples_operational_sites <- function(es_data, end_date = Sys.Date()) {

  current_year <- lubridate::year(end_date)
  thirteen_months_ago <- end_date - lubridate::years(1) - lubridate::days(30)

  message("ES Samples Per Site Analysis")

  # Operational sites and monthly data
  operational_sites <- es_data |>
    dplyr::filter(!is.na(site.id), !is.na(collection.date)) |>
    dplyr::distinct(who.region, ADM0_NAME, site.id) |>
    dplyr::left_join(
      es_data |> dplyr::filter(collection.date >= thirteen_months_ago, collection.date <= end_date) |>
        dplyr::count(who.region, ADM0_NAME, site.id),
      by = c("who.region", "ADM0_NAME", "site.id")
    ) |>
    dplyr::filter(n >= 10) |>
    dplyr::select(-n)

  data <- es_data |>
    dplyr::inner_join(operational_sites, by = c("who.region", "ADM0_NAME", "site.id")) |>
    dplyr::filter(lubridate::year(collection.date) %in% c(current_year - 1, current_year)) |>
    dplyr::count(who.region, ADM0_NAME, site.id,
                 year = lubridate::year(collection.date),
                 month = lubridate::month(collection.date))

  # Aggregate and combine
  final_result <- data |>
    dplyr::filter(year == current_year) |>
    dplyr::group_by(who.region, ADM0_NAME, year, month) |>
    dplyr::summarise(operational_sites = n_distinct(site.id), total_sample_collections = sum(n),
                     median_samples_per_month = median(n), .groups = "drop") |>
    dplyr::left_join(
      data |> dplyr::filter(year == current_year - 1) |>
        dplyr::group_by(who.region, ADM0_NAME, month) |>
        dplyr::summarise(operational_sites_previous_year = n_distinct(site.id),
                         previous_year_median = median(n), .groups = "drop"),
      by = c("who.region", "ADM0_NAME", "month")
    ) |>
    dplyr::mutate(
      month_name = month.abb[month],
      operational_sites_previous_year = ifelse(is.na(operational_sites_previous_year), 0, operational_sites_previous_year),
      operational_sites_change = paste0(ifelse(operational_sites_previous_year == 0, "",
                                               ifelse((operational_sites - operational_sites_previous_year) > 0, "+", "")),
                                        round(((operational_sites - operational_sites_previous_year) / pmax(operational_sites_previous_year, 1)) * 100, 1), "%"),
      median_difference = dplyr::case_when(
        is.na(previous_year_median) ~ NA_character_,
        median_samples_per_month == previous_year_median ~ "same",
        abs(median_samples_per_month - previous_year_median) == 1 ~
          paste0(ifelse(median_samples_per_month > previous_year_median, "more", "less"), " by 1 sample"),
        TRUE ~ paste0(ifelse(median_samples_per_month > previous_year_median, "more by ", "less by "),
                      abs(median_samples_per_month - previous_year_median), " samples")
      )
    ) |>
    dplyr::select(region = who.region, country = ADM0_NAME, year, month_name,
                  operational_sites, operational_sites_previous_year, operational_sites_change,
                  total_sample_collections, median_samples_per_month, previous_year_median,
                  median_difference) |>
    dplyr::arrange(region, country, year, match(month_name, month.abb))

  message("* Operational sites: ≥10 collections in 13-month period")
  final_result |> print(width = Inf)
}

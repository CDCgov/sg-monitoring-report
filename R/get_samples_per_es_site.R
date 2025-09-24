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

  summary <- es_data |>
    dplyr::filter(collect.yr >= lubridate::year(end_date) - 1,
                  who.region %in% c("AFRO", "EMRO")) |>
    dplyr::mutate(
      month = lubridate::month(collect.date, label = TRUE)
    ) |>
    dplyr::select(ctry = ADM0_NAME, site.name, month, collect.yr, site.status) |>
    dplyr::group_by(ctry, site.name, month, collect.yr, site.status) |>
    dplyr::summarize(n_samples = n()) |>
    tidyr::pivot_wider(names_from = collect.yr, values_from = n_samples) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(5, 6)), \(x) ifelse(is.na(x), 0, x)))

  summary["comparison"] <- summary[, 6] - summary[, 5]
  summary <- summary |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "same",
      comparison > 0 ~ "increase",
      comparison < 0 ~ "decrease"
    ))

  return(summary)

  }

#' Get median AFP cases reported
#'
#'  @description
#'  Calculates the median monthly AFP cases per country in the current year and
#'  the median of the previous three years.
#'
#' @param afp_data `tibble` AFP data.
#' @param end_date `str` End date of analysis. Defaults to the current date.
#'
#' @returns `tibble` Summary of monthly cases reported
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data()
#' summary <- get_afp_cases_reported(raw_data$afp)
#' }
get_afp_cases_reported <- function(afp_data, end_date = Sys.Date()) {

  # Prepare month and year columns
  afp_only <- afp_data |>
    dplyr::mutate(
      month = lubridate::month(dateonset, label = TRUE, abbr = TRUE),
      year = lubridate::year(dateonset)
    )

  # Define years and months to include
  years_to_include <- (lubridate::year(end_date) - 3):lubridate::year(end_date)
  months_to_include <- format(
    seq(lubridate::floor_date(end_date, unit = "years"), end_date, by = "months"),
    format = "%b"
  )

  # Get unique countries and their region mapping
  country_region <- afp_only |>
    dplyr::select(place.admin.0, whoregion) |>
    dplyr::mutate(whoregion = case_when(
      place.admin.0 == "CHINA" ~ "WPRO",
      place.admin.0 == "ETHIOPIA" ~ "AFRO",
      place.admin.0 == "INDIA" ~ "SEARO",
      place.admin.0 == "PAKISTAN" ~ "EMRO",
      .default = whoregion
    )) |>
    dplyr::distinct()


  # Create a full grid of all combinations for completeness (country, year, month)
  full_grid <- tidyr::expand_grid(
    place.admin.0 = unique(afp_only$place.admin.0),
    year = years_to_include,
    month = months_to_include
  ) |>
    dplyr::left_join(country_region, by = "place.admin.0")

  # Summarize actual counts
  summary <- afp_only |>
    dplyr::group_by(place.admin.0, year, month) |>
    dplyr::summarize(n = dplyr::n(), .groups = "drop")

  # Left join to fill in missing months/years with zero
  summary_full <- full_grid |>
    dplyr::left_join(summary, by = c("place.admin.0", "year", "month")) |>
    dplyr::mutate(n = tidyr::replace_na(n, 0))

  # Split into current year and previous years
  current_year <- summary_full |>
    dplyr::filter(year == lubridate::year(end_date)) |>
    tidyr::pivot_wider(names_from = year, values_from = n)

  previous_year_min <- lubridate::year(end_date) - 3
  previous_year_max <- lubridate::year(end_date) - 1
  previous_years <- summary_full |>
    dplyr::filter(year != lubridate::year(end_date)) |>
    dplyr::group_by(whoregion, place.admin.0, month) |>
    dplyr::summarize(median = median(n, na.rm = TRUE),
                     avg = mean(n, na.rm = TRUE),
                     sd = round(sd(n, na.rm = TRUE)),
                     mad = round(mad(n, na.rm = TRUE)), .groups = "drop") |>
    dplyr::rename(!!paste0(previous_year_min, "-", previous_year_max, " Median") := median,
                  !!paste0(previous_year_min, "-", previous_year_max, " Average") := avg,
                  !!paste0(previous_year_min, "-", previous_year_max, " SD") := sd,
                  !!paste0(previous_year_min, "-", previous_year_max, " MAD") := mad)

  final_summary <- dplyr::left_join(previous_years,
                                    current_year,
                                    by = c("whoregion", "place.admin.0", "month")) |>
    dplyr::arrange(place.admin.0)

  return(final_summary)
}

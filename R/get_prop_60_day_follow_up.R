#' Obtain proportion of follow-up completed
#'
#' @description
#' Calculate the proportion of follow-ups completed from the current year compared
#' to the previous year.
#'
#'
#' @param afp_data `tibble` AFP linelist.
#' @param end_date `str` End date of analysis. Defaults to current date.
#'
#' @returns `tibble` Summary table of follow-up from current and previous year by country
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data()
#' prop_60 <- get_prop_60_day_follow_up(raw_data$afp_data)
#' }
get_prop_60_day_follow_up <- function(afp_data, end_date = Sys.Date()) {

  end_date <- lubridate::as_date(end_date)
  current_year <- lubridate::year(end_date)
  current_month <- lubridate::month(end_date, TRUE)

  afp_start_date <- min(afp_data$dateonset, na.rm = TRUE)
  stool_data <- sirfunctions::generate_stool_data(afp_data,
                                                  afp_start_date,
                                                  end_date)
  case_60d <- sirfunctions::generate_60_day_table_data(stool_data,
                                                       afp_start_date,
                                                       end_date)
  summary <- case_60d |>
    dplyr::mutate(month = lubridate::month(date, TRUE)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month < current_month,
                  got60day %in% c(0, 1),
                  adequacy.final2 == "Inadequate") |>
    dplyr::group_by(year, ctry, month) |>
    dplyr::summarize(
      prop_w_follow_up = round(sum(got60day == 1, na.rm = TRUE) / sum(!is.na(got60day)) * 100))


  full_grid <- tidyr::expand_grid(
    ctry = unique(afp_data$place.admin.0),
    year = c(current_year - 1, current_year),
    month = lubridate::month(seq(1, 12), T))

  summary <- dplyr::left_join(full_grid, summary)
  summary <- summary |>
    tidyr::pivot_wider(names_from = year, values_from = prop_w_follow_up)

  summary["comparison"] <- summary[, 4] - summary[, 3]
  summary <- summary |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "Same",
      comparison > 0 ~ "Increase",
      comparison < 0 ~ "Decrease",
      .default = "No data from both years"
    ))

  return(summary)

}

#' Obtain proportion of follow-up completed
#'
#' @description
#' Calculate the proportion of follow-ups completed from the current year compared
#' to the previous year.
#'
#'
#' @param afp_data `tibble` AFP linelist.
#' @param end_date `str` End date of analysis. Defaults to current date.
#' @param temporal_scale `str` Whether to group analysis by year or quarter.
#'
#' @returns `tibble` Summary table of follow-up from current and previous year by country
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data()
#' prop_60 <- get_prop_60_day_follow_up(raw_data$afp_data)
#' }
get_prop_60_day_follow_up <- function(afp_data, end_date = Sys.Date(), temporal_scale = "quarter") {

  temporal_scale <- stringr::str_to_lower(temporal_scale)

  if (!temporal_scale %in% c("month", "quarter")) {
    cli::cli_abort("Only 'month' and 'quarter' are valid arguments for temporal_scale.")
  }

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

  if (temporal_scale == "quarter") {
    case_60d <- case_60d |>
      dplyr::mutate(quarter = lubridate::quarter(date))
  }

  summary <- case_60d |>
    dplyr::mutate(month = lubridate::month(date, TRUE)) |>
    dplyr::filter(year >= lubridate::year(end_date) - 1,
                  month < current_month,
                  got60day %in% c(0, 1),
                  adequacy.final2 == "Inadequate") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("year", "ctry", temporal_scale)))) |>
    dplyr::summarize(
      inad_cases = sum(!is.na(got60day)),
      got_60_day = sum(got60day == 1, na.rm = TRUE),
      prop_w_follow_up = round(got_60_day / inad_cases * 100)) |>
    dplyr::mutate(label = paste0(got_60_day, "/", inad_cases, " inadequates (", prop_w_follow_up,"%)"))


  if (temporal_scale == "month") {
    full_grid <- tidyr::expand_grid(
      ctry = unique(afp_data$place.admin.0),
      year = c(current_year - 1, current_year),
      month = lubridate::month(seq(1, 12), T))
  } else {
    full_grid <- tidyr::expand_grid(
      ctry = unique(afp_data$place.admin.0),
      year = c(current_year - 1, current_year),
      quarter = c(1, 2, 3, 4))
  }

  summary <- dplyr::left_join(full_grid, summary)

  summary_with_label <- summary |>
    dplyr::select(-inad_cases, -got_60_day, -prop_w_follow_up) |>
    tidyr::pivot_wider(names_from = year, values_from = label)

  summary_wo_label <- summary |>
    dplyr::select(-inad_cases, -got_60_day, -label) |>
    tidyr::pivot_wider(names_from = year, values_from = prop_w_follow_up)

  summary_wo_label["comparison"] <- summary_wo_label[, 4] - summary_wo_label[, 3]
  summary_wo_label <- summary_wo_label |>
    dplyr::mutate(trend = dplyr::case_when(
      comparison == 0 ~ "Same",
      comparison > 0 ~ "Increase",
      comparison < 0 ~ "Decrease",
      .default = "No data from both years"
    ))

  summary <- dplyr::left_join(summary_wo_label |>
                                dplyr::select(-any_of(c(3,4))), summary_with_label)

  return(summary)

}

#' Get proportion of AFP lab pendings
#'
#' @description
#' Obtains the number of lab pending in the AFP dataset from the previous 3 months
#' since the end_date specified.
#'
#' @param afp_data `tibble` AFP linelist.
#' @param end_date `str` End date of analysis. Defaults to current date.
#'
#' @returns `tibble` Summarizes the lab pending samples from the last three months.
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data <- get_all_polio_data()
#' get_proportion_lab_pending(raw_data$afp)
#' }
get_proportion_lab_pending <- function(afp_data, end_date = Sys.Date()) {

  summary <- afp_data |>
    dplyr::filter(stooltolabdate >= lubridate::floor_date(end_date - months(3), unit = "months"),
                  cdc.classification.all2 == "LAB PENDING") |>
    dplyr::mutate(month = lubridate::month(stooltolabdate, label = TRUE),
                  year = lubridate::year(stooltolabdate)) |>
    dplyr::group_by(whoregion, country = place.admin.0, year, month) |>
    dplyr::summarize(pending_samples = dplyr::n(), .groups = "drop")

  return(summary)

}

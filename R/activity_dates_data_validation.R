#' Data validation of activity dates and chronological sequences
#'
#' @description Checks completeness of required dates and validates logical date sequences to identify valid records.
#'
#' @param data Data containing records to validate.
#' @param date_columns Character vector of date column names to validate.
#' @param .group_by `str` Column name or a vector of column names.
#' @returns `tibble` Summary with validation metrics and results.
#' @export
#' @examples
#' \dontrun{
#' raw_data <- sirfunctions::get_all_polio_data(attach.spatial.data = FALSE)
#' # ES surveillance: collection before lab receipt
#' activity_dates_data_validation(
#'   raw_data$es,
#'   date_columns = c("collection.date", "date.received.in.lab"),
#'   categorical_columns = c("es.lab.type"))
#'
#' # AFP surveillance: onset, notification, then investigation
#' activity_dates_data_validation(
#'   raw_data$afp,
#'   date_columns = c("date.onset", "notification.date", "investigation.date"))
#' }
activity_dates_data_validation <- function(data, date_columns = NULL, .group_by = NULL) {
  total_records <- nrow(data)
  valid_data <- data
  results <- list(total_records = total_records)
  default_date_cols <- data |>
    dplyr::select(dplyr::contains("date")) |>
    names()

  # Date data availability
  if (!is.null(date_columns)) {
    date_cols <- date_columns[date_columns %in% default_date_cols]
    missingness_summary <- get_column_missingness(data, date_cols, .group_by)
  } else {
    missingness_summary <- get_column_missingness(data, default_date_cols, .group_by)
  }

  return (missingness_summary)

}

# private functions

#' Get proportion of column missingness
#'
#' @param data `tibble` Dataset
#' @param cols `str` A column name or a vector of names to determine proportion
#' of missingness
#'
#' @returns `tibble` Summary of missingness for each column specified in `cols`.
#' @keywords internal
#'
get_column_missingness <- function(data, cols, .group_by = NULL) {
  total_records <- data %>%
    {
      if (!is.null(.group_by)) {
        dplyr::group_by(., dplyr::across(dplyr::any_of(.group_by)))
      } else {
        .
      }
    } |>
    dplyr::summarize(across(dplyr::any_of(cols), \(x) dplyr::n())) |>
    tidyr::pivot_longer(cols = dplyr::any_of(cols), names_to = "column", values_to = "total")

  missing_count <- data %>%
    {
      if (!is.null(.group_by)) {
        dplyr::group_by(., dplyr::across(dplyr::any_of(.group_by)))
      } else {
        .
      }
    } |>
    dplyr::summarize(across(dplyr::any_of(cols), \(x) sum(is.na(x)))) |>
    tidyr::pivot_longer(cols = dplyr::any_of(cols), names_to = "column", values_to = "missing")

  valid_data <- data %>%
    {
      if (!is.null(.group_by)) {
        dplyr::group_by(., dplyr::across(dplyr::any_of(.group_by)))
      } else {
        .
      }
    } |>
    dplyr::summarize(across(dplyr::any_of(cols), \(x) sum(!is.na(x)))) |>
    tidyr::pivot_longer(cols = dplyr::any_of(cols), names_to = "column", values_to = "present")

  results <- dplyr::full_join(missing_count, valid_data) |>
    dplyr::full_join(total_records) |>
    dplyr::mutate(prop_complete = round(present / total * 100, 2))

  return(results)
}

#' Data validation of activity dates and chronological sequences
#'
#' @description Checks completeness of required dates and validates logical date sequences to identify valid records.
#'
#' @param data Data containing records to validate.
#' @param date_columns Character vector of date column names to validate.
#'   Column order matters: provide dates in expected chronological sequence (earliest activity first, latest activity last).
#'   Function validates that each subsequent date occurs on or after the previous date.
#' @param categorical_columns `str` Optional character vector of categorical column names for analyzing validation results by subgroups.
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
activity_dates_data_validation <- function(data, date_columns = NULL, categorical_columns = NULL) {
  total_records <- nrow(data)
  valid_data <- data
  results <- list(total_records = total_records)

  # Date data availability
  if (!is.null(date_columns)) {
    date_cols <- date_columns[date_columns %in% names(data)]

    # Filter valid_data and collect missing counts per column
    for (col in date_cols) {
      missing_count <- sum(is.na(data[[col]]))
      if (missing_count > 0) results[[paste0(col, "_missing_count")]] <- missing_count
      valid_data <- valid_data |>
        dplyr::filter(!is.na(.data[[col]]))
    }

    results$missing_date_records <- total_records - nrow(valid_data)
    results$complete_date_records <- nrow(valid_data)

    # Automatic date logic check when 2+ date columns
    if (length(date_cols) >= 2) {
      invalid_count <- 0
      for (i in 1:(length(date_cols) - 1)) {
        if (all(date_cols[i:(i+1)] %in% names(data))) {
          invalid_count <- invalid_count + sum(as.Date(data[[date_cols[i+1]]]) < as.Date(data[[date_cols[i]]]), na.rm = TRUE)
          valid_data <- valid_data |>
            dplyr::filter(as.Date(.data[[date_cols[i+1]]]) >= as.Date(.data[[date_cols[i]]]))
        }
      }
      results$invalid_date_logic_records <- invalid_count
      results$valid_records_for_analysis <- nrow(valid_data)
      results$valid_records_percentage <- round((nrow(valid_data) / total_records) * 100, 1)
    }
  }

  # Categorical analysis and output
  if (!is.null(categorical_columns)) {
    # Helper function for empty values
    is_empty <- function(x) is.null(x) | is.na(x) | nchar(gsub("[^A-Za-z0-9]", "", trimws(x))) == 0

    for (col in categorical_columns[categorical_columns %in% names(valid_data)]) {
      # Prepare column data
      col_data <- tolower(trimws(valid_data[[col]]))
      valid_vals <- unique(col_data)[!is_empty(unique(col_data))]

      # Skip column if entirely empty
      if (length(valid_vals) == 0) next

      # Count valid categories
      for (val in valid_vals) {
        results[[paste0(col, "_", val, "_count")]] <- sum(col_data == val, na.rm = TRUE)
      }

      # Count empty values
      empty_count <- sum(is_empty(valid_data[[col]]))
      if (empty_count > 0) results[[paste0(col, "_no_data_count")]] <- empty_count
    }
  }

  # Output
  result_tibble <- dplyr::tibble(metric = names(results), value = unlist(results))
  print(result_tibble, n = Inf)
  invisible(valid_data)
}

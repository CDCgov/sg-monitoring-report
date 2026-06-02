#' Title
#'
#' @param afp_data
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
build_prop_inadequate_classified <- function(afp_data, end_date = Sys.Date()) {

  # Basic initial checks -----
  stopifnot(
    "afp_data must be a data frame" = is.data.frame(afp_data),
    "dateonset column required" = "dateonset" %in% names(afp_data),
    "place.admin.0 column required" = "place.admin.0" %in% names(afp_data)
  )

  # Date Windows -----

  # Ensure end_date is a date type
  end_date <- lubridate::as_date(end_date)

  # Recent 3-month window — last complete month
  recent_end   <- lubridate::floor_date(end_date, unit = "month") %m-% days(1)
  recent_start <- lubridate::floor_date(recent_end %m-% months(2), unit = "month")

  # Recent 3-month window - one year prior for comparison
  recent_prior_end   <- recent_end   %m-% lubridate::years(1)
  recent_prior_start <- recent_start %m-% lubridate::years(1)

  # Period Labels -----
  recent_period_label  <- paste0(format(recent_start, "%b %Y"), " - ", format(recent_end, "%b %Y"))
  recent_prior_period_label    <- paste0(format(recent_prior_start, "%b %Y"), " - ", format(recent_prior_end, "%b %Y"))

  # Prepare data from sirfunctions -----
  # Use recent_prior_start as start date required parameter in functions - will capture data for both periods
  stool_data <- sirfunctions::generate_stool_data(
    afp_data,
    start_date = recent_prior_start,
    end_date   = recent_end)

  # Filter to eligible inadequate cases only -----
  eligible_cases <- stool_data |>
    dplyr::mutate(case_age = as.numeric(difftime(recent_end, date, units = "days"))) |>
    dplyr::filter(adequacy.final2 == "Inadequate",
                  case_age > 90)

  # Helper to summarize counts for a given window -----
  summarize_window <- function(data, start, end) {
    data |>
      dplyr::filter(dplyr::between(date, start, end)) |>
      dplyr::group_by(ctry) |>
      dplyr::summarize(
        inad_cases   = dplyr::n(),
        no_classified = sum(cdc.classification.all2 != "PENDING", na.rm = TRUE),
        prop_classified   = round(no_classified / inad_cases * 100),
        .groups = "drop"
      )
  }

  # Period Counts -----
  # Count number pending for each of the four time periods
  recent_counts        <- summarize_window(eligible_cases, recent_start, recent_end)
  recent_prior_counts  <- summarize_window(eligible_cases, recent_prior_start, recent_prior_end)



}

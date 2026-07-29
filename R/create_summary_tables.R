# these are helpful functions for summarizing the output from the build functions of the indicators
# they do not need to be public functions or referenced outside of the generate_data_for_report.R code

create_summary_tables_monthly <- function(data, label, country_var_name, flag_values, incomplete_values, latest_month){

  #divide into two quarters for grouping
  latest_month <- max(lubridate::my(data$month_label))
  #re-extract the max dates from the text columns of the tables
  latest_q_end <- latest_month
  latest_q_start <- lubridate::floor_date(latest_month %m-% months(2), unit = "month")
  earlier_q_end <- lubridate::floor_date(latest_month %m-% months(3), unit = "month")
  earlier_q_start <- lubridate::floor_date(latest_month %m-% months(6), unit = "month")

  #get the number of months a country has been flagged for merging into the country-level table
  num_months_below <- data |>
    #create a column for quarter
    dplyr::mutate(period = dplyr::case_when(dplyr::between(lubridate::my(month_label), latest_q_start, latest_q_end) ~ "Current",
                                            dplyr::between(lubridate::my(month_label), earlier_q_start, earlier_q_end) ~ "Earlier")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(country_var_name, "period")))) |>
    #count number of countries that had at least 2 months below target (or whatever designation is used)
    dplyr::summarise(months_below = sum(flag %in% flag_values), .groups = "drop") |>
    dplyr::filter(months_below >= 2) |>
    dplyr::filter(period == "Current")

  #build id_cols dynamically depending on whether sg_priority_level exists in the data
  #this allows us to use the same function for the lab indicators, which do not contain sg_priority_level
  if ("sg_priority_level" %in% names(data)) {
    id_cols_vec <- c("whoregion", "sg_priority_level", country_var_name, "period")
  } else {
    id_cols_vec <- c("whoregion", country_var_name, "period")
  }

  #pivot wider so the columns are the months and merge the number of months below the target
  summary_table_country <- data |>
    dplyr::mutate(period = dplyr::case_when(dplyr::between(lubridate::my(month_label), latest_q_start, latest_q_end) ~ "Current",
                                            dplyr::between(lubridate::my(month_label), earlier_q_start, earlier_q_end) ~ "Earlier"))  |>
    #filter for now to simplify the table
    dplyr::filter(period == "Current")|>
    tidyr::pivot_wider(id_cols = dplyr::all_of(id_cols_vec), values_from = flag, names_from = month_label) |>
    dplyr::right_join(num_months_below) |>
    dplyr::rename(Country = dplyr::all_of(country_var_name)) |>
    dplyr::mutate(Flag = label)

  summary_table_region <- data |>
    #create a column for quarter
    dplyr::mutate(period = dplyr::case_when(dplyr::between(lubridate::my(month_label), latest_q_start, latest_q_end) ~ "Current",
                                            dplyr::between(lubridate::my(month_label), earlier_q_start, earlier_q_end) ~ "Earlier")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("whoregion", "period", country_var_name)))) |>
    #count number of countries that had at least 2 months below target (or whatever designation is used)
    dplyr::summarise(months_below = sum(flag %in% flag_values) >= 2,
                     #countries that had all 3 months incomplete
                     num_months_incomplete = sum(flag %in% incomplete_values) == 3,
                     .groups = "drop_last") |>
    #count number of countries
    dplyr::summarise(num_below = sum(months_below),
                     num_incomplete = sum(num_months_incomplete),
                     total_n = dplyr::n(),
                     num_complete = sum(!num_months_incomplete),
                     #get the list of countries that meet the criteria for below or incomplete
                     below_countries = list(.data[[country_var_name]][months_below]),
                     incomplete_countries = list(.data[[country_var_name]][num_months_incomplete]),
                     .groups = "drop") |>
    dplyr::mutate(flagname = label,
                  #remove countries with incomplete data from the denominator
                  string = paste0(num_below, "/", num_complete, " (", round(num_below/num_complete,2)*100, "%)"),
                  #make the lists of countries readable
                  countries_below = purrr::map_chr(below_countries, ~paste(.x, collapse = ", ")),
                  countries_incomplete = purrr::map_chr(incomplete_countries, ~paste(.x, collapse = ", "))
    ) |>
    dplyr::select(whoregion, flagname, string, period, countries_below, countries_incomplete)

  return(list(
    country_table = summary_table_country,
    region_table = summary_table_region
  ))
}

#separate logic/functions for quarterly indicators
create_summary_table_quarterly <- function(data, label, country_var_name, period_var, flag_values, incomplete_values){
  #get the latest month for identifying the latest period
  latest_month <- max(lubridate::my(substr(dplyr::pull(data, dplyr::all_of(period_var)), 0, 8)))

  #build select_cols dynamically depending on whether sg_priority_level exists in the data
  #this allows us to use the same function for the lab indicators, which do not contain sg_priority_level
  if ("sg_priority_level" %in% names(data)) {
    select_cols_vec <- c("whoregion", "sg_priority_level", country_var_name, "gperiod", period_var)
  } else {
    select_cols_vec <- c("whoregion", country_var_name, "gperiod", period_var)
  }

  #pull out the countries below target for the summary table
  summary_table_country <- data |>
    dplyr::mutate(gperiod = dplyr::case_when(lubridate::my(substr(.data[[period_var]], 0, 8)) == latest_month ~ "Current",
                                             TRUE ~ "Earlier")) |>
    #filter to countries with values that are flagged
    dplyr::filter(flag %in% flag_values) |>
    dplyr::select(dplyr::all_of(select_cols_vec)) |>
    dplyr::rename(Country = dplyr::all_of(country_var_name),
                  Months = dplyr::all_of(period_var)) |>
    dplyr::mutate(Flag = label)

  summary_table_region <- data |>
    dplyr::mutate(period = dplyr::case_when(lubridate::my(substr(.data[[period_var]], 0, 8)) == latest_month ~ "Current",
                                            TRUE ~ "Earlier")) |>
    dplyr::group_by(whoregion, period) |>
    #count number of countries with flags by region and period
    dplyr::summarise(num_below = sum(flag %in% flag_values),
                     num_incomplete = sum(flag %in% incomplete_values),
                     total_n = dplyr::n(),
                     #need to get the list of countries that are not meeting the flag
                     below_countries = list(.data[[country_var_name]][flag %in% flag_values]),
                     incomplete_countries = list(.data[[country_var_name]][flag %in% incomplete_values]),
                     .groups = "drop") |>
    dplyr::mutate(flagname = label,
                  #remove countries with incomplete data from the denominator
                  string = paste0(num_below, "/", (total_n-num_incomplete), " (", round(num_below/(total_n-num_incomplete),2)*100, "%)"),
                  countries_below = purrr::map_chr(below_countries, ~paste(.x, collapse = ", ")),
                  countries_incomplete = purrr::map_chr(incomplete_countries, ~paste(.x, collapse = ", "))
    ) |>
    dplyr::select(whoregion, flagname, string, period, countries_below, countries_incomplete)

  return(list(
    country_table = summary_table_country,
    region_table = summary_table_region
  ))
}

#there are also 2 indicators that don't have any time periods
create_summary_table <- function(data, label, country_var_name, flag_values, incomplete_values, raw_value_col){

  #build select_cols dynamically depending on whether sg_priority_level exists in the data
  #this allows us to use the same function for the lab indicators, which do not contain sg_priority_level
  if ("sg_priority_level" %in% names(data)) {
    select_cols_vec <- c("whoregion", "sg_priority_level", country_var_name, raw_value_col)
  } else {
    select_cols_vec <- c("whoregion", country_var_name, raw_value_col)
  }

  summary_table_country <- data |>
    #filter to countries with values that are flagged
    dplyr::filter(flag %in% flag_values) |>
    dplyr::select(dplyr::all_of(select_cols_vec)) |>
    dplyr::rename(Country = dplyr::all_of(country_var_name),
                  Pct = dplyr::all_of(raw_value_col)) |>
    dplyr::mutate(Flag = label)

  summary_table_region <- data |>
    dplyr::group_by(whoregion) |>
    dplyr::summarise(num_below = sum(flag %in% flag_values),
                     num_incomplete = sum(flag %in% incomplete_values),
                     total_n = dplyr::n(),
                     #need to get the list of countries that are not meeting the flag
                     below_countries_list = list(.data[[country_var_name]][flag %in% flag_values]),
                     incomplete_countries_list = list(.data[[country_var_name]][flag %in% incomplete_values]),
                     .groups="drop") |>
    dplyr::mutate(flagname = label,
                  #remove countries with incomplete data from the denominator
                  string = paste0(num_below, "/", (total_n-num_incomplete), " (", round(num_below/(total_n-num_incomplete),2)*100, "%)"),
                  countries_below = purrr::map_chr(below_countries_list, ~paste(.x, collapse = ", ")),
                  countries_incomplete = purrr::map_chr(incomplete_countries_list, ~paste(.x, collapse = ", ")),
                  period = "Current"
    ) |>
    dplyr::select(whoregion, flagname, string, period, countries_below, countries_incomplete)

  return(list(
    country_table = summary_table_country,
    region_table = summary_table_region
  ))
}


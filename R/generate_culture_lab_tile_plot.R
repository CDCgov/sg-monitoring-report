generate_culture_lab_tile_plot <- function(culture_lab_intervals = NULL,
                                           lab_workload = NULL,
                                           lab_end_date = Sys.Date()) {
  culture_indicators <- c("Timeliness of\nvirus isolation",
                          "Timeliness of\nITD results",
                          "Timeliness of\nshipment for sequencing",
                          "Lab workload")

  final_table <- process_culture_lab_performance(culture_lab_intervals, lab_workload, lab_end_date)
  final_table <- final_table |>
    dplyr::filter(!is.na(culture.itd.lab)) |>
    dplyr::mutate(indicator = factor(indicator, levels = culture_indicators, ordered = TRUE))

  plot <- generate_performance_tile_plot(final_table, "culture.itd.lab", "Culture/ITD Lab")

  return(plot)

}

process_culture_lab_performance <- function(culture_lab_intervals = NULL,
                                            lab_workload = NULL,
                                            lab_end_date = NULL) {

  ## Timeliness of virus isolation results ----
  lab_isolation_filtered <- culture_lab_intervals |>
    dplyr::filter(interval == "days.lab.culture") |>
    dplyr::mutate(I13  = dplyr::case_when(
      prop_diff > 30 ~ "Below target",
      prop_diff <=30 ~ "On target",
      is.na(prop_diff) ~ "To Be Determined"
    )) |>
    dplyr::select(culture.itd.lab, I13)

  ## Timeliness of ITD results ----
  lab_itd_filtered <- culture_lab_intervals |>
    dplyr::filter(interval == "days.culture.itd") |>
    dplyr::mutate(I14  = dplyr::case_when(
      prop_diff > 30 ~ "Below target",
      prop_diff <=30 ~ "On target",
      is.na(prop_diff) ~ "To Be Determined"
    )) |>
    dplyr::select(culture.itd.lab, I14)

  ## Timeliness of shipment for sequencing ----
  lab_ship_filtered <- culture_lab_intervals |>
    dplyr::filter(interval == "days.seq.ship") |>
    dplyr::mutate(I15  = dplyr::case_when(
      prop_diff > 30 ~ "Below target",
      prop_diff <=30 ~ "On target",
      is.na(prop_diff) ~ "To Be Determined"
    )) |>
    dplyr::select(culture.itd.lab, I15)

  ## Lab workload ----
  lab_workload_filtered <- lab_workload |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(lab_end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::mutate(I16  = dplyr::case_when(
      comparison_pct > 30 ~ "Below target",
      comparison_pct <= 30 ~ "On target",
      is.na(comparison_pct) ~ "To Be Determined"
    )) |>
    dplyr::select(culture.itd.lab, I16)

  final_table <- dplyr::left_join(lab_isolation_filtered, lab_itd_filtered) |>
    dplyr::left_join(lab_ship_filtered) |>
    dplyr::left_join(lab_workload_filtered) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "To Be Determined", value),
                  indicator = dplyr::case_when(
                    indicator == "I13" ~ "Timeliness of\nvirus isolation",
                    indicator == "I14" ~ "Timeliness of\nITD results",
                    indicator == "I15" ~ "Timeliness of\nshipment for sequencing",
                    indicator == "I16" ~ "Lab workload",
                    .default = indicator
                  ))

  return(final_table)

}

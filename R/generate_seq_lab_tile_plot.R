generate_seq_lab_tile_plot <- function(seq_lab_interval = NULL) {
  final_table <- process_seq_lab_performance(seq_lab_interval)
  plot <- generate_performance_tile_plot(final_table, "seq.lab", "Sequencing Lab")

  return(plot)

}

process_seq_lab_performance <- function(seq_lab_interval) {
  ## Timeliness of sequencing results ----
  lab_seq_filtered <- seq_lab_interval |>
    dplyr::mutate(I17  = dplyr::case_when(
      get(names(seq_lab_interval)[4]) > 7 ~ "Below target",
      get(names(seq_lab_interval)[4]) <= 7 ~ "On target",
      is.na(get(names(seq_lab_interval)[4])) ~ "To Be Determined"
    )) |>
    dplyr::select(seq.lab, I17) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "To Be Determined", value),
                  indicator = dplyr::case_when(
                    indicator == "I17" ~ "Timeliness of\nsequencing results",
                    .default = indicator
                  ))

  return(lab_seq_filtered)
}

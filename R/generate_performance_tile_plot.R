generate_perfomance_tile_plot <- function(afp_cases_reported,
                                          prop_60,
                                          lab_pending,
                                          afp_wpv_vdpv,
                                          negative_lab_processing,
                                          afp_shipment_timeliness,
                                          afp_lab_processing,
                                          es_shipment,
                                          es_wpv_vdpv,
                                          es_sites,
                                          es_site_samples,
                                          lab_intervals,
                                          end_date = Sys.Date(),
                                          lab_end_date = Sys.Date(),
                                          who_region = "AFRO",
                                          lab_locs = NULL
                                          ) {

  # AFP ----
  ## Number of AFP cases reported ----
  afp_cases_reported <- afp_cases_reported |>
    dplyr::mutate(month = factor(month, levels = c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ), ordered = TRUE)) |>
    dplyr::arrange(place.admin.0, month)

  afp_cases_reported_filtered <- afp_cases_reported |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE))
  afp_cases_reported_filtered["mad_2"] <- afp_cases_reported_filtered[[5]] - afp_cases_reported_filtered[[8]] * 2
  afp_cases_reported_filtered <- afp_cases_reported_filtered |>
    dplyr::mutate(mad_2 = if_else(mad_2 < 0 | is.na(mad_2), 0, mad_2))
  afp_cases_reported_filtered["I1"] <- afp_cases_reported_filtered[[9]] < afp_cases_reported_filtered[[10]]
  afp_cases_reported_filtered <- afp_cases_reported_filtered |>
    dplyr::mutate(I1 = dplyr::case_when(
      I1 == TRUE ~ "Failing",
      I1 == FALSE ~ "Good",
      is.na(I1) ~ "Unable to determine"
      ))

  # AFP reported final
  afp_cases_reported_filtered <- afp_cases_reported_filtered |>
    dplyr::select(whoregion, place.admin.0, I1)

  ## Proportion of 60-days follow-up done ----
  prop_60_filtered <- prop_60 |>
    dplyr::filter(quarter == (lubridate::quarter(end_date) - 1)) |>
    dplyr::select(place.admin.0 = ctry, comparison) |>
    dplyr::mutate(I2 = dplyr::case_when(
      is.na(comparison) ~ "Unable to determine",
      comparison <= -50 ~ "Failing",
      comparison > -50 ~ "Good"
    )) |>
    dplyr::select(-comparison)

  ## Proportion of lab pending ----
  lab_pending_filtered <- lab_pending |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::mutate(prop = stringr::str_extract(prop_label, "[0-9]+"),
                  n_sample = str_extract(prop_label, "(?<=/)\\d+")) |>
    dplyr::mutate(prop = as.numeric(prop),
                  n_sample = as.numeric(n_sample)) |>
    dplyr::group_by(country) |>
    dplyr::summarize(pending = sum(pending_samples, na.rm = TRUE),
                     total = sum(n_sample, na.rm = TRUE)) |>
    dplyr::mutate(prop = pending/total * 100) |>
    dplyr::mutate(I3 = dplyr::case_when(
      prop >= 75 ~ "Failing",
      prop < 75 ~ "Good"
    )) |>
    dplyr::select(place.admin.0 = country, I3)

  ## Timeliness of AFP VDPV/WILD detection ----
  afp_wpv_vdpv_filtered <- afp_wpv_vdpv |>
    dplyr::select(place.admin.0 = country, month, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("2"),
                        names_to = "year",
                        values_to = "median_timely") |>
    dplyr::arrange(place.admin.0, year, month) |>
    dplyr::filter(year == lubridate::year(end_date)) |>
    dplyr::group_by(place.admin.0) |>
    dplyr:: summarize(median = median(median_timely, na.rm = TRUE))

  if (is.null(lab_locs)) {
    lab_loc_info <- sirfunctions::get_lab_locs(lab_locs)
  }

  afp_wpv_vdpv_filtered <- afp_wpv_vdpv_filtered |>
    dplyr::left_join(lab_loc_info |>
                       dplyr::select(place.admin.0 = country, seq.capacity)) |>
    dplyr::mutate(
      I4 =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & median <= 35 ~ "Good",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & median > 35 ~ "Failing",
                  seq.capacity == "no" & median <= 46 ~ "Good",
                  seq.capacity == "no" & median > 46 ~ "Failing",
                  .default = "Unable to determine"
        )
    )

  ## Timeliness of negative samples ----
  negative_lab_processing_filtered <- negative_lab_processing |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(lab_end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::select(country, culture.itd.cat, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(dplyr::starts_with("2"), names_to = "year", values_to = "median") |>
    dplyr::group_by(country, culture.itd.cat) |>
    dplyr::summarize(monthly_median = median(median, na.rm = TRUE)) |>
    dplyr::mutate(I5 = dplyr::case_when(
      monthly_median <= 35 & culture.itd.cat == "In-country culture/ITD" ~ "Good",
      monthly_median > 35 & culture.itd.cat == "In-country culture/ITD" ~ "Failing",
      monthly_median <= 46 & culture.itd.cat == "International culture/ITD" ~ "Good",
      monthly_median > 46 & culture.itd.cat == "International culture/ITD" ~ "Failing",
      .default = "Unable to determine"
    )) |>
    dplyr::select(place.admin.0 = country, I5)

  ## Timeliness of stool specimen shipment ----
  afp_shipment_timeliness_filtered <- afp_shipment_timeliness |>
    dplyr::filter(quarter == lubridate::quarter(end_date) - 1) |>
    dplyr::select(country, culture.itd.cat, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(dplyr::starts_with("2"), names_to = "year", values_to = "median") |>
    dplyr::group_by(country, culture.itd.cat) |>
    dplyr::summarize(monthly_median = median(median, na.rm = TRUE)) |>
    dplyr::mutate(I6 = dplyr::case_when(
      monthly_median <= 3 & culture.itd.cat == "In-country culture/ITD" ~ "Good",
      monthly_median > 3 & culture.itd.cat == "In-country culture/ITD" ~ "Failing",
      monthly_median <= 7 & culture.itd.cat == "International culture/ITD" ~ "Good",
      monthly_median > 7 & culture.itd.cat == "International culture/ITD" ~ "Failing",
      .default = "Unable to determine"
    )) |>
    dplyr::select(place.admin.0 = country, I6)

  ## Timeliness of lab processing - AFP ----
  afp_lab_processing_filtered <- afp_lab_processing |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(lab_end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::select(country, month, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(dplyr::starts_with("2"), names_to = "year", values_to = "median") |>
    dplyr::ungroup() |>
    dplyr::group_by(country) |>
    dplyr::summarize(monthly_median = median(median, na.rm = TRUE)) |>
    dplyr::mutate(I7 = dplyr::case_when(
      monthly_median <= 14 ~ "Good",
      monthly_median > 14 ~ "Failing",
      .default = "Unable to determine"
    )) |>
    dplyr::select(place.admin.0 = country, I7)


  # ES ----
  ## Timeliness of ES shipment ----

  ## Timeliness for ES VDPV/WPV detection ----

  ## Number of operational ES sites ----

  ## Number of samples per site ----

  # Lab ----
  ## Timeliness of virus isolation results ----

  ## Timeliness of ITD results ----

  ## Timeliness of shipment for sequencing ----

  ## Timeliness of sequencing results ----

  ## Lab workload ----

  # Synthesize
  final_table <- afp_cases_reported_filtered |>
    dplyr::left_join(prop_60_filtered) |>
    dplyr::left_join(lab_pending_filtered) |>
    dplyr::left_join(afp_wpv_vdpv_filtered) |>
    dplyr::left_join(negative_lab_processing_filtered) |>
    dplyr::left_join(afp_shipment_timeliness_filtered) |>
    dplyr::left_join(afp_lab_processing_filtered) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::filter(whoregion == who_region) |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "Unable to determine", value),
                  indicator = dplyr::case_when(
                    indicator == "I1" ~ "I1. Number of AFP cases reported",
                    indicator == "I2" ~ "I2. Prop 60-day follow-up done",
                    indicator == "I3" ~ "I3. Prop lab pending",
                    indicator == "I4" ~ "I4. Timely AFP WPV/VDPV detection",
                    indicator == "I5" ~ "I5. Timely detection of negative AFP samples",
                    indicator == "I6" ~ "I6. Timely stool shipment",
                    indicator == "I7" ~ "I7. Timely lab processing",
                    .default = indicator
                  ))

  plot <- ggplot2::ggplot(data = final_table,
                          ggplot2::aes(fill = value, x = indicator, y = stringr::str_to_title(place.admin.0))) +
    geom_tile(color = "white",
              lwd = 0.8,
              linetype = 1) +
    ggplot2::scale_fill_manual(
      values = c(
        "Good" = "#0070c0",
        "Failing" = "#FF4021",
        "Unable to determine" = "lightgrey"),
      name = "Indicator Performance",
      na.value = "lightgrey"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::ylab("Country") +
    ggplot2::xlab("") +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 6, color = "black"),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.text.y.left = element_text(angle = 0, hjust = 1)
    )

  return(plot)
}

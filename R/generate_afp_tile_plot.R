generate_afp_tile_plot <- function(afp_cases_reported = NULL,
                                   prop_60 = NULL,
                                   lab_pending = NULL,
                                   prop_classified = NULL,
                                   afp_wpv_vdpv = NULL,
                                   negative_lab_processing = NULL,
                                   afp_shipment_timeliness = NULL,
                                   afp_lab_processing = NULL,
                                   end_date = Sys.Date(),
                                   lab_end_date = Sys.Date(),
                                   lab_locs = get_lab_locs(),
                                   who_region = NULL) {

  final_table <- process_afp_performance(afp_cases_reported,
                                         prop_60,
                                         lab_pending,
                                         prop_classified,
                                         afp_wpv_vdpv,
                                         negative_lab_processing,
                                         afp_shipment_timeliness,
                                         afp_lab_processing,
                                         end_date,
                                         lab_end_date,
                                         lab_locs)
  if (!is.null(who_region)) {
    final_table <- final_table |>
      dplyr::filter(whoregion == who_region)
  }
  plot <- generate_performance_tile_plot(final_table, "place.admin.0", "Country")

  return(plot)

}

process_afp_performance <- function(afp_cases_reported = NULL,
                                    prop_60 = NULL,
                                    lab_pending = NULL,
                                    prop_classified = NULL,
                                    afp_wpv_vdpv = NULL,
                                    negative_lab_processing = NULL,
                                    afp_shipment_timeliness = NULL,
                                    afp_lab_processing = NULL,
                                    end_date = Sys.Date(),
                                    lab_end_date = Sys.Date(),
                                    lab_locs = NULL) {
  # Calculate previous quarter/semesters
  prev_quarter_to_report <- (lubridate::quarter(end_date) - 1)
  if (prev_quarter_to_report == 0) {
    prev_quarter_to_report <- 4
  }
  prev_sem_to_report <- (lubridate::semester(end_date) - 1)
  if (prev_sem_to_report == 0) {
    prev_sem_to_report <- 2
  }

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
      I1 == TRUE ~ "Below target",
      I1 == FALSE ~ "On target",
      is.na(I1) ~ "To Be Determined"
    ))

  # AFP reported final
  afp_cases_reported_filtered <- afp_cases_reported_filtered |>
    dplyr::select(whoregion, place.admin.0, I1)

  ## Proportion of 60-days follow-up done ----
  prop_60_filtered <- prop_60 |>
    dplyr::filter(semester == prev_sem_to_report) |>
    dplyr::select(place.admin.0 = ctry, comparison) |>
    dplyr::mutate(I2 = dplyr::case_when(
      is.na(comparison) ~ "To Be Determined",
      comparison <= -80 ~ "Below target",
      comparison > -80 ~ "On target"
    )) |>
    dplyr::select(-comparison)

  ## Proportion of lab pending ----
  lab_pending_filtered <- lab_pending |>
    dplyr::mutate(prop = stringr::str_extract(prop_label, "[0-9]+"),
                  n_sample = str_extract(prop_label, "(?<=/)\\d+")) |>
    dplyr::mutate(prop = as.numeric(prop),
                  n_sample = as.numeric(n_sample)) |>
    dplyr::group_by(country) |>
    dplyr::summarize(pending = sum(pending_samples, na.rm = TRUE),
                     total = sum(n_sample, na.rm = TRUE)) |>
    dplyr::mutate(prop = pending/total * 100) |>
    dplyr::mutate(I3 = dplyr::case_when(
      prop >= 80 ~ "Below target",
      prop < 80 ~ "On target"
    )) |>
    dplyr::select(place.admin.0 = country, I3)

  ## Timeliness of AFP VDPV/WILD detection ----
  afp_wpv_vdpv_filtered <- afp_wpv_vdpv |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(end_date,unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::select(place.admin.0 = country,
                  dplyr::starts_with("2")) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("2"),
                        names_to = "year",
                        values_to = "median_timely") |>
    dplyr::arrange(place.admin.0, year) |>
    dplyr::filter(year == lubridate::year(end_date)) |>
    dplyr::group_by(place.admin.0) |>
    dplyr:: summarize(median = median(median_timely, na.rm = TRUE))

  if (is.null(lab_locs)) {
    lab_locs <- sirfunctions::get_lab_locs(lab_locs)
  }

  afp_wpv_vdpv_filtered <- afp_wpv_vdpv_filtered |>
    dplyr::left_join(lab_locs |>
                       dplyr::select(place.admin.0 = country, seq.capacity)) |>
    dplyr::mutate(
      I4 =
        case_when(stringr::str_detect(.data$seq.capacity, "[Yy]es") & median <= 35 ~ "On target",
                  stringr::str_detect(.data$seq.capacity, "[Yy]es") & median > 35 ~ "Below target",
                  seq.capacity == "no" & median <= 46 ~ "On target",
                  seq.capacity == "no" & median > 46 ~ "Below target",
                  .default = "To Be Determined"
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
      monthly_median <= 35 & culture.itd.cat == "In-country culture/ITD" ~ "On target",
      monthly_median > 35 & culture.itd.cat == "In-country culture/ITD" ~ "Below target",
      monthly_median <= 46 & culture.itd.cat == "International culture/ITD" ~ "On target",
      monthly_median > 46 & culture.itd.cat == "International culture/ITD" ~ "Below target",
      .default = "To Be Determined"
    )) |>
    dplyr::select(place.admin.0 = country, I5)

  ## Timeliness of stool specimen shipment ----
  afp_shipment_timeliness_filtered <- afp_shipment_timeliness |>
    dplyr::filter(quarter == lubridate::quarter(end_date) - 1) |>
    dplyr::select(country, culture.itd.cat, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(dplyr::starts_with("2"), names_to = "year", values_to = "median") |>
    dplyr::filter(year == as.character(lubridate::year(lab_end_date))) |>
    dplyr::mutate(I6 = dplyr::case_when(
      median <= 3 & culture.itd.cat == "In-country culture/ITD" ~ "On target",
      median > 3 & culture.itd.cat == "In-country culture/ITD" ~ "Below target",
      median <= 7 & culture.itd.cat == "International culture/ITD" ~ "On target",
      median > 7 & culture.itd.cat == "International culture/ITD" ~ "Below target",
      .default = "To Be Determined"
    )) |>
    dplyr::select(place.admin.0 = country, I6)

  ## Timeliness of lab processing - AFP ----
  afp_lab_processing_filtered <- afp_lab_processing |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(lab_end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::select(country, month, dplyr::starts_with("2")) |>
    tidyr::pivot_longer(dplyr::starts_with("2"), names_to = "year", values_to = "median") |>
    dplyr::filter(stringr::str_detect(year, as.character(lubridate::year(lab_end_date)))) |>
    dplyr::mutate(I7 = dplyr::case_when(
      median <= 14 ~ "On target",
      median > 14 ~ "Below target",
      .default = "To Be Determined"
    )) |>
    dplyr::select(place.admin.0 = country, I7)


  ## Proportion cases classified ----
  prop_classified_filtered <- prop_classified |>
    dplyr::filter(quarter == (lubridate::quarter(end_date) - 1)) |>
    dplyr::select(place.admin.0 = ctry, comparison = diff) |>
    dplyr::mutate(I8 = dplyr::case_when(
      is.na(comparison) ~ "To Be Determined",
      comparison <= -80 ~ "Below target",
      comparison > -80 ~ "On target"
    )) |>
    dplyr::select(-comparison)

  # Synthesize
  final_table <- afp_cases_reported_filtered |>
    dplyr::left_join(prop_60_filtered) |>
    dplyr::left_join(lab_pending_filtered) |>
    dplyr::left_join(afp_wpv_vdpv_filtered) |>
    dplyr::left_join(negative_lab_processing_filtered) |>
    dplyr::left_join(afp_shipment_timeliness_filtered) |>
    dplyr::left_join(afp_lab_processing_filtered) |>
    dplyr::left_join(prop_classified_filtered) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "To Be Determined", value),
                  indicator = dplyr::case_when(
                    indicator == "I1" ~ "Number of AFP \ncases reported",
                    indicator == "I2" ~ "Proportion 60-day\nfollow-up done",
                    indicator == "I3" ~ "Proportion lab\npending",
                    indicator == "I4" ~ "Timely AFP WPV/VDPV \ndetection",
                    indicator == "I5" ~ "Timely detection of \nnegative AFP samples",
                    indicator == "I6" ~ "Timely stool\nshipment",
                    indicator == "I7" ~ "Timely lab\nprocessing",
                    indicator == "I8" ~ "Proportion inadequate\ncases classified",
                    .default = indicator
                  ))

  return(final_table)

}

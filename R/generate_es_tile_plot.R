generate_es_tile_plot <- function(es_shipment = NULL,
                                  es_wpv_vdpv = NULL,
                                  es_sites = NULL,
                                  es_site_samples = NULL,
                                  who_region = NULL,
                                  end_date = Sys.Date()
                                  ) {
  final_table <- process_es_performance(es_shipment,
                                        es_wpv_vdpv,
                                        es_sites,
                                        es_site_samples,
                                        end_date)

  if (!is.null(who_region)) {
    final_table <- final_table |>
      dplyr::filter(whoregion == who_region)
  }

  plot <- generate_performance_tile_plot(final_table, "country", "Country")

  return(plot)
}

process_es_performance <- function(es_shipment = NULL,
                                   es_wpv_vdpv = NULL,
                                   es_sites = NULL,
                                   es_site_samples = NULL,
                                   end_date = Sys.Date()) {

  ## Timeliness of ES shipment ----
  es_shipment_filtered <- es_shipment |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::mutate(I9 = dplyr::case_when(
      is.na(current_year_timeliness) ~ "To Be Determined",
      current_year_timeliness == "Not timely" ~ "Below target",
      current_year_timeliness == "Timely" ~ "On target"
    )) |>
    dplyr::select(country, whoregion = who.region, I9)

  ## Timeliness for ES VDPV/WPV detection ----
  es_wpv_vdpv_filtered <- es_wpv_vdpv |>
    dplyr::filter(month == lubridate::month(lubridate::floor_date(end_date,
                                                                  unit = "month") %m-% months(1),
                                            label = TRUE)) |>
    dplyr::mutate(I10 = dplyr::case_when(
      is.na(current_year_timeliness) ~ "To Be Determined",
      current_year_timeliness == "Not timely" ~ "Below target",
      current_year_timeliness == "Timely" ~ "On target"
    )) |>
    dplyr::select(country, whoregion = who.region, I10)

  ## Number of operational ES sites ----
  es_sites_filtered <- es_sites |>
    dplyr::mutate(I11 = dplyr::case_when(
      is.na(prop_diff) ~ "To Be Determined",
      prop_diff <= -20 ~ "Below target",
      prop_diff > -20 ~ "On target"
    )) |>
    dplyr::select(country = ctry, I11)

  ## Number of samples per site ----
  # Site-level indicator, rather than a country level...but here's an attempt
  es_site_samples_filtered <- es_site_samples |>
    dplyr::group_by(country = ADM0_NAME) |>
    dplyr::summarize(good_active_sites = sum(active_site == "Yes" & collection_two_mo == "Yes", na.rm = TRUE),
                     active_sites = sum(active_site == "Yes")) |>
    dplyr::mutate(active_sites_performance = round(good_active_sites / active_sites * 100)) |>
    dplyr::mutate(I12 = dplyr::case_when(
      is.na(active_sites_performance) ~ "To Be Determined",
      active_sites_performance < 80 ~ "Below target",
      active_sites_performance >= 80 ~ "On target"
    )) |>
    dplyr::select(country, I12)

  final_table <- dplyr::left_join(es_shipment_filtered, es_wpv_vdpv_filtered) |>
    dplyr::left_join(es_sites_filtered) |>
    dplyr::left_join(es_site_samples_filtered) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "To Be Determined", value),
                  indicator = dplyr::case_when(
                    indicator == "I9" ~ "Timeliness of\nES shipment",
                    indicator == "I10" ~ "Timeliness of\nES WPV/VDPV detection",
                    indicator == "I11" ~ "Proportion operational\nES sites",
                    indicator == "I12" ~ "Proportion of performant\nES active sites",
                    .default = indicator
                  ))

  return(final_table)

}

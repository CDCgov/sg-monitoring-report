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
  # Compare from previous month...however idk how well I can capture it.
  es_site_samples_filtered <- es_site_samples |>
    dplyr::filter(ym >= lubridate::floor_date(end_date %m-% months(1), unit = "months")) |>
    dplyr::select(country, ym, median_collections) |>
    tidyr::pivot_wider(names_from = ym, values_from = median_collections)
  es_site_samples_filtered["diff"] <- (es_site_samples_filtered[, 3] - es_site_samples_filtered[, 2]) * 100
  es_site_samples_filtered <- es_site_samples_filtered |>
    dplyr::mutate(I12 = dplyr::case_when(
      is.na(diff) ~ "To Be Determined",
      diff <= -50 ~ "Below target",
      diff >= 0 ~ "On target"
    )) |>
    dplyr::select(country, I12)

  final_table <- dplyr::left_join(es_shipment_filtered, es_wpv_vdpv_filtered) |>
    dplyr::left_join(es_sites_filtered) |>
    dplyr::left_join(es_site_samples_filtered) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("I", ignore.case = FALSE), values_to = "value", names_to = "indicator") |>
    dplyr::mutate(value = dplyr::if_else(is.na(value), "To Be Determined", value),
                  indicator_description = case_when(
                    indicator == "I9" ~ paste0("<br>Description:</br> median days between collection to received in lab.",
                                               " Timely if received within 3 days for in-country lab and 7 days for international lab.",
                                               "On target if median days less than or equal to targets.\n",
                                               "<br>Timeframe:</br> month before the specified end date. (",
                                               lubridate::floor_date(end_date, unit = "month") %m-% months(1), ")"),
                    indicator == "I10" ~ paste0("<br>Description:</br> median days between collection to notification to HQ for WPV/VDPV samples.",
                                                " On target if median is 35 days if in-country lab and 46 days if shipped to international lab.\n",
                                                "<br>Timeframe:</br> month before the specified end date. (",
                                                lubridate::floor_date(end_date, unit = "month") %m-% months(1), ")"),
                    indicator == "I11" ~ paste0("<br>Description:</br> Proportion of operational sites for the current month compared to the same month last year.",
                                                " Operational sites are sites with at least 3 collections in the last 12 months. ",
                                                " On target if there is less than a 20% decrease compared to the same period last year.\n",
                                                "<br>Timeframe:</br> current month of the end date versus the previous month (",
                                                lubridate::month(end_date, label = TRUE), lubridate::year(end_date), " to ",
                                                lubridate::month(end_date, label = TRUE), lubridate::year(end_date - 1), ")"),
                    indicator == "I12" ~ paste0("<br>Description:</br> Percent change of median collections for operational sites from current month compared to the previous month.",
                                                " On target if median collections from current month stays the same or increase compared to previous month.\n",
                                                "<br>Timeframe</br> current month to the previous month (",
                                                lubridate::month(end_date, label = TRUE), " to ",
                                                lubridate::month(end_date %m-% months(1), label = TRUE), ")")
                    ),

                  indicator = dplyr::case_when(
                    indicator == "I9" ~ "Timeliness of\nES shipment",
                    indicator == "I10" ~ "Timeliness of\nES WPV/VDPV detection",
                    indicator == "I11" ~ "Proportion operational\nES sites",
                    indicator == "I12" ~ "Median number of collections among\nES active sites",
                    .default = indicator
                  ))

  return(final_table)

}

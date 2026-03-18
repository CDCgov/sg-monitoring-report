
# Source functions ----
lapply(list.files("R"), \(fx) source(file.path("R", fx)))

# Load data ----
end_date <- lubridate::as_date(end_date)
raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
sirfunctions_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
lab_data <- clean_lab_data(lab_data, "2019-01-01", end_date, afp_data = raw_data$afp)

# Manual clean-up for raw_data
raw_data_df_names <- c("afp", "afp.epi", "para.case", "es", "pos", "other", "sia")

for (i in raw_data_df_names) {
  if (i == "es") {
    raw_data[[i]] <- raw_data[[i]] |>
      dplyr::mutate(ADM0_NAME = if_else(ADM0_NAME == "TURKEY",
                                            "TÜRKIYE", ADM0_NAME))
  } else {
    raw_data[[i]] <- raw_data[[i]] |>
      dplyr::mutate(place.admin.0 = if_else(place.admin.0 == "TURKEY",
                                            "TÜRKIYE", place.admin.0))
  }
}

# Manual clean-up of lab data ----
lab_data <- lab_data |>
  mutate(seq.lab = case_when(
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab == "Cameroon" ~ "NICD-South Africa",
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab == "ETHIOPIA/ KEMRI-Kenya" ~ "UVRI-Uganda",
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab %in% c("Ibadan-Nigeria, Maiduguri-Nigeria", "Nigeria") ~ "Ibadan-Nigeria",
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab == "KEMRI-Kenya" ~ "UVRI-Uganda",
    country == "UGANDA" & DateStoolCollected >= as_date("2025-02-01") ~ "UVRI-Uganda",
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab == "Senegal" ~ "NICD-South Africa",
    seq.lab == "CDC-Atlanta" & DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab == "Varied (KEMRI-Kenya/ Oman/ Jordan)" ~ "Varied (UVRI/ Oman/ Jordan)",
    .default = seq.lab
  )) |>
  mutate(seq.cat = case_when(
    DateStoolCollected >= as_date("2025-02-01") & culture.itd.lab %in% c("Ibadan-Nigeria, Maiduguri-Nigeria", "Nigeria") & seq.lab == "Ibadan-Nigeria" ~ "Not shipped for sequencing",
    country == "UGANDA" & DateStoolCollected >= as_date("2025-02-01") ~ "Not shipped for sequencing",
    .default = seq.cat
  )) |>
  mutate(seq.capacity = if_else(country %in% c("NIGERIA", "UGANDA") & DateStoolCollected >= as_date("2025-02-01"), "yes", seq.capacity))

# Load human specimen table ----
human_specimen <- sirfunctions_io("read", file_loc = "POLIS/data/human_specimen.parquet")

# Calculate max lab dates ----
max_lab_date <- max(lab_data$CaseDate, na.rm = TRUE)
max_date_notif <- paste0("Lab data recent as of ", max_lab_date, ".")

# AFP indicators ----
afp_cases_reported <- get_afp_cases_reported(raw_data$afp, end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "place.admin.0") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "place.admin.0")

lab_pending <- get_proportion_lab_pending(raw_data$afp, end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

afp_wpv_vdpv <- get_wpv_vdpv_timeliness(raw_data$pos, end_date, temporal_scale = "quarter") |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

negative_lab_processing <- get_negative_lab_processing_timeliness(lab_data, max_lab_date) |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

afp_shipment_timeliness <- get_stool_shipment_timeliness(lab_data, max_lab_date) |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

prop_60 <- get_prop_60_day_follow_up(raw_data$afp, end_date, temporal_scale = "quarter") |>
  sirfunctions:::add_risk_category(ctry_col = "ctry") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ctry")
prop_classified <- get_prop_case_classified(raw_data$afp, end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "ctry") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ctry")
prop_inad <- get_prop_inad_with_contacts(human_specimen, raw_data$afp, end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "ctry") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ctry")

# ES indicators ----
es_shipment_timeliness <- get_es_timeliness(raw_data$es, end_date = end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

es_shipment <- es_shipment_timeliness |>
  dplyr::filter(category == "median_lab_shipment") |>
  dplyr::select(-category)

es_wpv_vdpv <- es_shipment_timeliness |>
  dplyr::filter(category == "median_wpv_vdpv_detection") |>
  dplyr::select(-category)

es_sites <- suppressMessages(get_operational_sites(raw_data$es, end_date)) |>
  sirfunctions:::add_risk_category(ctry_col = "ctry") |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ctry")

es_site_samples <- suppressMessages(get_samples_per_es_site(raw_data$es, end_date)) |>
  sirfunctions:::add_risk_category(ctry_col = "country") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "country")

# Lab indicators ----
afp_lab_processing <- get_afp_lab_processing_timeliness(lab_data, max_lab_date) |>
  dplyr::mutate(`Target Days` = 14)
culture_lab_intervals <- lab_timely_indicators(lab_data,"culture", end_date = max_lab_date)
culture_lab_intervals <- culture_lab_intervals |>
  dplyr::mutate(`Target Days` = dplyr::case_when(
    interval == "days.lab.culture" ~ 14,
    interval == "days.culture.itd" ~ 7,
    interval == "days.seq.ship" ~ 7)
  )
seq_lab_interval <- lab_timely_indicators(lab_data,"seq", end_date = max_lab_date)
seq_lab_interval <- seq_lab_interval |>
  dplyr::mutate(`Target Days` = 7)
lab_workload <- suppressMessages(generate_lab_workload(lab_data, end_date = max_lab_date))

# Data for tile plots ----
afp_tile_plot_data <- process_afp_performance(afp_cases_reported, prop_60, lab_pending, prop_classified,
                                              afp_wpv_vdpv, negative_lab_processing, afp_shipment_timeliness,
                                              end_date = end_date, lab_end_date = max_lab_date)

es_tile_plot_data <- process_es_performance(es_shipment, es_wpv_vdpv, es_sites, es_site_samples, end_date = end_date)

culture_indicators <- c("Timeliness of\nvirus isolation",
                        "Timeliness of\nITD results",
                        "Timeliness of\nshipment for sequencing",
                        "Lab workload",
                        "Timeliness of\nlab processing")

culture_lab_plot_data <- process_culture_lab_performance(culture_lab_intervals, lab_workload, afp_lab_processing, max_lab_date)
culture_lab_plot_data <- culture_lab_plot_data |>
  dplyr::filter(!is.na(culture.itd.lab)) |>
  dplyr::mutate(indicator = factor(indicator, levels = culture_indicators, ordered = TRUE))

seq_lab_plot_data <- process_seq_lab_performance(seq_lab_interval)

# Save tables ----
save(end_date, max_lab_date, max_date_notif, afp_cases_reported, lab_pending,
     afp_wpv_vdpv, negative_lab_processing, afp_shipment_timeliness,
     afp_lab_processing, prop_60, prop_classified, prop_inad,
     es_shipment_timeliness, es_shipment, es_wpv_vdpv, es_sites, es_site_samples,
     culture_lab_intervals, seq_lab_interval, lab_workload, afp_tile_plot_data,
     es_tile_plot_data, culture_lab_plot_data, seq_lab_plot_data,
     file = "data_cache/cache.rda")

# Save tables in an Excel File ----
excel_output <- list()

## AFP ----
excel_output$`AFP Cases Reported` <- afp_cases_reported |>
  dplyr::rename(Region = whoregion, Country = place.admin.0)

excel_output$`Prop 60 Days Follow Up` <- prop_60 |>
  dplyr::rename(Country = ctry, Quarter = quarter, Comparison = comparison,
                Trend = trend)

excel_output$`Prop Lab Pending` <- lab_pending |>
  dplyr::rename(Country = country, Region = whoregion,
                `Pending Samples` = pending_samples,
                `Label` = prop_label)

excel_output$`Prop Case Classified` <- prop_classified |>
  dplyr::rename(Country = ctry, Quarter = quarter,
                Difference = diff, Trend = trend, Performance = performance)

excel_output$`Timely AFP WPV VDPV Det` <- afp_wpv_vdpv |>
  dplyr::rename(Country = country, Quarter = quarter, Comparison = comparison,
                Trend = trend)

## ES ----
excel_output$`Timeliness of ES Shipment` <- es_shipment |>
  dplyr::rename(
    Month = month, Country = country, Region = who.region, `Lab Type` = es.lab.type,
    Difference = diff, Trend = trend,
    `Current Year Timeliness` = current_year_timeliness,
    `Trend Summary` = trend_summary,
    `Timeliness Target` = timeliness_target
  )

excel_output$`Timely ES WPV VDPV Det` <- es_wpv_vdpv |>
  dplyr::rename(
    Month = month, Country = country, Region = who.region, `Lab Type` = es.lab.type,
    Difference = diff, Trend = trend, `Current Year Timeliness` = current_year_timeliness,
    `Trend Summary` = trend_summary,
    `Timeliness Target` = timeliness_target
  )

excel_output$`Operational Sites per Country` <- es_sites |>
  dplyr::rename(
    Country = ctry, Comparison = comparison, `% Diff` = prop_diff,
    Trend = trend
  ) |>
  dplyr::relocate(Region, .after = Country)

excel_output$`Samples by Operational Sites` <- es_site_samples |>
  dplyr::rename(
    Year = year, Country = country, Month = month, Median = median_collections,
    `Year Month` = ym
  ) |>
  dplyr::relocate(Month, `Year Month`, .after = Year)

## Lab ----
excel_output$`Culture Lab Timeliness` <- culture_lab_intervals |>
  dplyr::mutate(interval_name = dplyr::case_when(
    interval == "days.lab.culture" ~ "Virus isolation results",
    interval == "days.culture.itd" ~ "ITD results",
    interval == "days.seq.ship" ~ "Shipment for Sequencing"
  )) |>
  dplyr::rename(Interval = interval_name, `Culture Lab` = culture.itd.lab,
                Comparison = comparison, `% Diff` = prop_diff, Trend = trend) |>
  dplyr::select(-interval) |>
  dplyr::relocate(Interval, .before = `Culture Lab`)

excel_output$`Sequencing Lab Timeliness` <- seq_lab_interval |>
  dplyr::select(-interval) |>
  dplyr::rename(`Sequencing Lab` = seq.lab, Comparison = comparison, `% Diff` = prop_diff,
                Trend = trend)

## Export Excel sheet ----
if (!dir.exists("Excel_Output")) {
  dir.create("Excel_Output")
}

openxlsx::write.xlsx(excel_output,
                     file = file.path("Excel_Output", "monitoring_report_tables.xlsx"),
                     asTable = TRUE,
                     colWidths = "auto")

rm(list = ls())
gc()
load("data_cache/cache.rda")

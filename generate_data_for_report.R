

lapply(list.files("R"), \(fx) source(file.path("R", fx)))

end_date <- lubridate::as_date(end_date)

raw_data <- get_all_polio_data(attach.spatial.data = FALSE)
sirfunctions_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
lab_data <- clean_lab_data(lab_data, "2019-01-01", end_date, afp_data = raw_data$afp)
# Manual clean-up
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

human_specimen <- sirfunctions_io("read", file_loc = "POLIS/data/human_specimen.rds")
max_lab_date <- max(lab_data$CaseDate, na.rm = TRUE)
max_date_notif <- paste0("Lab data recent as of ", max_lab_date, ".")

# AFP indicators
afp_cases_reported <- get_afp_cases_reported(raw_data$afp, end_date) |>
  sirfunctions:::add_risk_category(ctry_col = "place.admin.0") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "place.admin.0")

lab_pending <- get_proportion_lab_pending(raw_data$afp, end_date) |>
  dplyr::filter(year <= lubridate::year(end_date)) |>
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

afp_lab_processing <- get_afp_lab_processing_timeliness(lab_data, max_lab_date) |>
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

# ES indicators
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
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ctry")

es_site_samples <- suppressMessages(get_samples_per_es_site(raw_data$es, end_date)) |>
  sirfunctions:::add_risk_category(ctry_col = "ADM0_NAME") |>
  dplyr::select(-Region) |>
  dplyr::relocate(dplyr::any_of("SG Priority Level"), .after = "ADM0_NAME")

# Lab indicators
culture_lab_intervals <- lab_timely_indicators(lab_data,"culture", end_date = max_lab_date)
seq_lab_interval <- lab_timely_indicators(lab_data,"seq", end_date = max_lab_date)
lab_workload <- suppressMessages(lab_workload(lab_data, end_date = max_lab_date))

save(max_lab_date, max_date_notif, afp_cases_reported, lab_pending,
     afp_wpv_vdpv, negative_lab_processing, afp_shipment_timeliness,
     afp_lab_processing, prop_60, prop_classified, prop_inad,
     es_shipment_timeliness, es_shipment, es_wpv_vdpv, es_sites, es_site_samples,
     culture_lab_intervals, seq_lab_interval, lab_workload,
     file = "data_cache/cache.rda")

if (!dir.exists("images")) {
  dir.create("images")
}

# Generate Individual Tile Plots
who_regions <- c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")

# AFP
lapply(who_regions, \(x) {generate_afp_tile_plot(afp_cases_reported, prop_60, lab_pending, prop_classified,
                                                 afp_wpv_vdpv, negative_lab_processing, afp_shipment_timeliness,
                                                 afp_lab_processing, end_date = end_date, lab_end_date = max_lab_date, who_region = x)
  ggsave(paste0("images/", x, "_afp_plot.jpg"), width = 14, height = 8)
  })

# ES
lapply(who_regions, \(x) {
  generate_es_tile_plot(es_shipment, es_wpv_vdpv, es_sites, es_site_samples, end_date = end_date, who_region = x)
  ggsave(paste0("images/", x, "_es_plot.jpg"), width = 14, height = 8)
  })

# Lab
generate_culture_lab_tile_plot(culture_lab_intervals, lab_workload, lab_end_date = max_lab_date)
ggsave(paste0("images/", "culture_lab_plot.jpg"), width = 14, height = 8)

generate_seq_lab_tile_plot(seq_lab_interval)
ggsave(paste0("images/", "seq_lab_plot.jpg"), width = 14, height = 8)

rm(list = ls())
gc()
load("data_cache/cache.rda")

# generate_data_for_report.R
# Sourced by the QMD. Loads raw data, runs all build_ indicators, and saves
# to a folder within the directory. All analytic work happens here — QMD is presentation only.


# Set Up ---------------------------------------------------------------------------------------------------------------

# Source repo functions ----
source_pattern <- function(folder, pattern) {
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  purrr::walk(files, source)
}

source_pattern("R", "^build_")
#source_pattern("R", "^create_")


# Data Prep ------------------------------------------------------------------------------------------------------------

#  Load AFP and ES data ----
end_date   <- lubridate::as_date(end_date)  # passed in from QMD before source()
raw_data   <- get_all_polio_data(attach.spatial.data = FALSE)

# Load and clean Lab data ----
sirfunctions_io("read", file_loc = get_constant("CLEANED_LAB_DATA"))
max_lab_date <- max(lab_data$CaseDate, na.rm = TRUE)
lab_data <- clean_lab_data(lab_data, "2019-01-01", max_lab_date, afp_data = raw_data$afp)

lab_data <- lab_data |>
  #need to standardize how South Africa is referred to in the culture lab column. this will eventually need to be fixed in the lab locs file...
  dplyr::mutate(culture.itd.lab = dplyr::case_when(culture.itd.lab == "South Africa" ~ "NICD-South Africa",
                                   TRUE ~ culture.itd.lab))

# Manual country name fix ----
raw_data_df_names <- c("afp", "afp.epi", "para.case", "es", "pos", "other", "sia")
for (i in raw_data_df_names) {
  col <- if (i == "es") "ADM0_NAME" else "place.admin.0"
  raw_data[[i]] <- raw_data[[i]] |>
    dplyr::mutate(!!col := dplyr::if_else(.data[[col]] == "TURKEY", "TÜRKIYE", .data[[col]]))
}

# Helper function: add risk category and priority level to any indicator data frame ----
# Kept here rather than in build_ functions since risk category is a reporting
# framework (using external file read) not an analytical one
add_risk <- function(data, ctry_col) {
  data |>
    sirfunctions:::add_risk_category(ctry_col = ctry_col) |>
    dplyr::select(-Region) |>
    dplyr::rename(sg_priority_level = `SG Priority Level`) |> # rename to make machine readable for data manips
    dplyr::relocate(sg_priority_level, .after = dplyr::all_of(ctry_col))
}

# Helper function: get month end from the max date in a date variable ----
get_month_end_from_max <- function(date_var) {
  date_var <- lubridate::as_date(date_var)

  if (all(is.na(date_var))) {
    return(as.Date(NA))
  }

  max_date <- max(date_var, na.rm = TRUE)
  next_month_start <- seq(lubridate::floor_date(max_date, unit = "month"),
                          by = "1 month",
                          length.out = 2)[2]

  next_month_start - 1
}

# Helper function: add WHO region based on physical culture/ITD lab location ----
add_culture_itd_lab_who_region <- function(data) {
  stopifnot(
    "data must be a data frame" = is.data.frame(data),
    "culture.itd.lab column required" = "culture.itd.lab" %in% names(data)
  )

  # Update this lookup if culture.itd.lab names change in lab_locs.
  culture_itd_lab_regions <- tibble::tribble(
    ~culture.itd.lab,             ~whoregion,
    "Pakistan",                   "EMRO",
    "Algeria",                    "AFRO",
    "NICD-South Africa",          "AFRO",
    "Unknown",                    NA_character_,
    "Oman",                       "EMRO",
    "Noguchi-Ghana",              "AFRO",
    "Cote d'Ivoire",              "AFRO",
    "UVRI-Uganda",                "AFRO",
    "Cameroon",                   "AFRO",
    "Central African Republic",   "AFRO",
    "DRC",                        "AFRO",
    "KEMRI-Kenya",                "AFRO",
    "Egypt",                      "EMRO",
    "Ethiopia",                   "AFRO",
    "Senegal",                    "AFRO",
    "Iran",                       "EMRO",
    "Iraq",                       "EMRO",
    "Israel",                     "EURO",
    "Jordan",                     "EMRO",
    "Kuwait",                     "EMRO",
    "Jordan/Syria",               "EMRO",
    "Tunisia",                    "EMRO",
    "Madagascar",                 "AFRO",
    "Morocco",                    "EMRO",
    "Nigeria",                    "AFRO",
    "Saudi Arabia",               "EMRO",
    "South Africa",               "AFRO",
    "Egypt/Sudan",                "EMRO",
    "Syria",                      "EMRO",
    "Oman/Jordan",                "EMRO",
    "Zambia",                     "AFRO",
    "Zimbabwe",                   "AFRO"
  )

  data |>
    dplyr::select(-dplyr::any_of("whoregion")) |>
    dplyr::mutate(culture.itd.lab = trimws(as.character(culture.itd.lab))) |>
    dplyr::left_join(culture_itd_lab_regions, by = "culture.itd.lab") |>
    dplyr::relocate(whoregion, .after = culture.itd.lab)
}

# Helper function: add WHO region based on physical sequencing lab location ----
add_seq_lab_who_region <- function(data) {
  stopifnot(
    "data must be a data frame" = is.data.frame(data),
    "seq.lab column required" = "seq.lab" %in% names(data)
  )

  # Update this lookup if seq.lab names change in lab_locs.
  seq_lab_regions <- tibble::tribble(
    ~seq.lab,                  ~whoregion,
    "Pakistan",                "EMRO",
    "Pasteur Institute-Paris", "EURO",
    "NICD-South Africa",       "AFRO",
    "Unknown",                 NA_character_,
    "CDC-Atlanta",             "PAHO",
    "Noguchi-Ghana",           "AFRO",
    "UVRI-Uganda",             "AFRO",
    "Egypt",                   "EMRO",
    "Iran",                    "EMRO",
    "Israel",                  "EURO",
    "Egypt/CDC-Atlanta",       "EMRO",
    "Tunisia",                 "EMRO",
    "Ibadan-Nigeria",          "AFRO",
    "Jordan",                  "EMRO",
    "Oman",                    "EMRO",
    "Oman/Egypt",              "EMRO"
  )

  data |>
    dplyr::select(-dplyr::any_of("whoregion")) |>
    dplyr::mutate(seq.lab = trimws(as.character(seq.lab))) |>
    dplyr::left_join(seq_lab_regions, by = "seq.lab") |>
    dplyr::relocate(whoregion, .after = seq.lab)
}

# Helper functions: create summary tables
source_pattern("R", "create_summary_tables")


# Create Indicator Results ---------------------------------------------------------------------------------------------
# Saving the full object list so QMD can access $data and $metadata

# AFP Indicators ----
# Generate indicator results
afp_cases_reported <- build_afp_cases_reported(raw_data$afp, end_date)
afp_prop_60 <- build_prop_60_day_follow_up_indicator(raw_data$afp)
afp_prop_inad_classified <- build_prop_inadequate_classified(raw_data$afp, end_date)
afp_prop_lab_pending <- build_prop_lab_pending(raw_data$afp, end_date)
afp_wpv_vdpv_timeliness <- build_wpv_vdpv_timeliness_indicator(raw_data$pos, end_date)
afp_neg_samples <- build_negative_samples_timeliness_indicator(lab_data, get_month_end_from_max(max_lab_date))
afp_timely_stool <- build_timely_stool_shipment_indicator(lab_data, get_month_end_from_max(max_lab_date))
afp_inadequate_cases <-build_number_of_inadequate_cases(raw_data$afp, end_date)

# Add risk category
afp_cases_reported$data <- add_risk(afp_cases_reported$data, "place.admin.0")
afp_prop_60$data <- add_risk(afp_prop_60$data, "ctry")
afp_prop_inad_classified$data <- add_risk(afp_prop_inad_classified$data, "ctry")
afp_prop_lab_pending$data <- add_risk(afp_prop_lab_pending$data, "ctry")
afp_wpv_vdpv_timeliness$data <- add_risk(afp_wpv_vdpv_timeliness$data, "ctry")
afp_neg_samples$data <- add_risk(afp_neg_samples$data, "country")
afp_timely_stool$data <- add_risk(afp_timely_stool$data, "country")
afp_inadequate_cases$data <-add_risk(afp_inadequate_cases$data, "place.admin.0")

# ES Indicators ----
# Generate indicator results
es_active_sites <- build_number_of_active_ES_sites(raw_data$es, end_date)
es_timely_shipment <- build_timeliness_of_es_shipment_indicator(raw_data$es, end_date)
es_wpv_vdpv_timeliness <- build_timeliness_es_wpv_vdpv_notification_indicator(raw_data$es, end_date)
es_prop_active_sites_collections <- build_prop_active_es_sites_with_monthly_collections(raw_data$es, end_date)

# Add risk category
es_active_sites$data <- add_risk(es_active_sites$data, "country")
es_timely_shipment$data <- add_risk(es_timely_shipment$data, "country")
es_wpv_vdpv_timeliness$data <- add_risk(es_wpv_vdpv_timeliness$data, "country")
es_prop_active_sites_collections$data <- add_risk(es_prop_active_sites_collections$data, "country")

# Lab Indicators ----
lab_virus_isolation_timeliness <- build_timeliness_virus_isolation_indicator(lab_data, get_month_end_from_max(max_lab_date))
lab_virus_ITD_results_timeliness <- build_timeliness_of_ITD_results_indicator(lab_data, get_month_end_from_max(max_lab_date))
lab_sequencing_shipment_timeliness <- build_timeliness_of_shipment_for_sequencing_indicator(lab_data, get_month_end_from_max(max_lab_date))
lab_workload <- build_lab_workload_indicator(lab_data, get_month_end_from_max(max_lab_date))
lab_sequencing_timeliness <- build_timeliness_of_sequencing_results_indicator(lab_data, get_month_end_from_max(max_lab_date))

# Add lab WHO region based on physical lab location
lab_virus_isolation_timeliness$data <- add_culture_itd_lab_who_region(lab_virus_isolation_timeliness$data)
lab_virus_ITD_results_timeliness$data <- add_culture_itd_lab_who_region(lab_virus_ITD_results_timeliness$data)
lab_sequencing_shipment_timeliness$data <- add_culture_itd_lab_who_region(lab_sequencing_shipment_timeliness$data)
lab_workload$data <- add_culture_itd_lab_who_region(lab_workload$data)
lab_sequencing_timeliness$data <- add_seq_lab_who_region(lab_sequencing_timeliness$data)

# Summary Tables ----

#uses the functions from create_summary_tables.R
afp_cases_summary <- create_summary_tables_monthly(afp_cases_reported$data,"01. AFP Cases Reported","place.admin.0","Below Target","Incomplete Data")
afp_neg_samples_summary <- create_summary_tables_monthly(afp_neg_samples$data,"06. Negative Sample Timeliness","country","Below Target","Incomplete Data")
afp_stool_timeliness_summary <- create_summary_tables_monthly(afp_timely_stool$data,"07. Stool Shipment Timeliness","country","Below Target","Incomplete Data")
afp_inad_cases_summary <- create_summary_tables_monthly(afp_inadequate_cases$data,"08. Inadequate Cases","place.admin.0","Below Target","Incomplete Data")

es_prop_active_sites_summary <- create_summary_tables_monthly(es_prop_active_sites_collections$data,"09. Proportion of Active ES Sites with Monthly Collections","country","Below Target","No Current Active ES")
es_num_active_sites_summary <- create_summary_tables_monthly(es_active_sites$data,"10. Number of Active ES Sites","country","Below Target","No Current Active ES")
es_timely_shipment_summary <- create_summary_tables_monthly(es_timely_shipment$data,"11. Timeliness of ES Shipment", "country", "Below Target","Incomplete Data")

afp_prop_60_summary <- create_summary_table_quarterly(afp_prop_60$data, "02. Proportion 60-Day Follow-Up Completed", "ctry", "period", "Off Target", "Incomplete Data")
afp_timely_wpvvdpv_summary <- create_summary_table_quarterly(afp_wpv_vdpv_timeliness$data, "05. Timeliness of AFP WPV/VDPV Detection", "ctry", "current_period", "Below Target", "Incomplete Data")
es_wpvvdpv_timeliness_summary <- create_summary_table_quarterly(es_wpv_vdpv_timeliness$data, "12. Timeliness of ES WPV/VDPV Notification", "country", "current_period", "Below Target","Incomplete Data")

afp_prop_inad_unclassified_summary<-create_summary_table(afp_prop_inad_classified$data, "03. Proportion Inadequate Cases Unclassified", "ctry","Off Target","Incomplete Data", "prop_unclassified")
afp_prop_lab_pending_summary<-create_summary_table(afp_prop_lab_pending$data, "04. Proportion Lab Pending", "ctry", "Off Target", "Incomplete Data", "prop_lab_pending")

#smush them together
region_table <- dplyr::bind_rows(afp_cases_summary$region_table,
                               afp_prop_60_summary$region_table,
                               afp_prop_inad_unclassified_summary$region_table,
                               afp_prop_lab_pending_summary$region_table,
                               afp_timely_wpvvdpv_summary$region_table,
                               afp_neg_samples_summary$region_table,
                               afp_stool_timeliness_summary$region_table,
                               afp_inad_cases_summary$region_table,
                               es_prop_active_sites_summary$region_table,
                               es_num_active_sites_summary$region_table,
                               es_timely_shipment_summary$region_table,
                               es_wpvvdpv_timeliness_summary$region_table) |>
            dplyr::select(whoregion, flagname, string, period, countries_below, countries_incomplete) |>
            tidyr::pivot_wider(id_cols=c(whoregion, flagname), values_from=c(string, countries_below, countries_incomplete), names_from=period)

country_table_monthly <- dplyr::bind_rows(afp_cases_summary$country_table,
                                        afp_neg_samples_summary$country_table,
                                        afp_stool_timeliness_summary$country_table,
                                        afp_inad_cases_summary$country_table,
                                        es_prop_active_sites_summary$country_table,
                                        es_num_active_sites_summary$country_table,
                                        es_timely_shipment_summary$country_table)

country_table_quarterly <- dplyr::bind_rows(afp_prop_60_summary$country_table,
                                          es_wpvvdpv_timeliness_summary$country_table,
                                          afp_timely_wpvvdpv_summary$country_table)

country_table_noperiod <- dplyr::bind_rows(afp_prop_inad_unclassified_summary$country_table,
                                         afp_prop_lab_pending_summary$country_table)

#lab tables:
lab_virus_isolation_timeliness_summary <- create_summary_tables_monthly(lab_virus_isolation_timeliness$data, "13. Timeliness of Virus Isolation", "culture.itd.lab", "Below Target","No current virus isolation data")
lab_workload_summary <- create_summary_tables_monthly(lab_workload$data, "16. Lab Workload", "culture.itd.lab", "Below Target","Incomplete Data")

lab_itd_timeliness_summary <- create_summary_table_quarterly(lab_virus_ITD_results_timeliness$data, "14. Timeliness of ITD results", "culture.itd.lab", "current_period", "Below Target", c("No prior ITD samples", "No current ITD samples"))
lab_ship_timeliness_summary <- create_summary_table_quarterly(lab_sequencing_shipment_timeliness$data, "15. Timeliness of Shipment for Sequencing", "culture.itd.lab", "current_period", "Below Target", c("No current shipment for sequencing samples", "No shipment for sequencing samples", "No prior shipment for sequencing samples"))
lab_seq_timeliness_summary <- create_summary_table_quarterly(lab_sequencing_timeliness$data, "17. Timeliness of Sequencing Results", "seq.lab", "current_period", "Below Target", "No current sequenced samples")

lab_region_table <- dplyr::bind_rows(lab_virus_isolation_timeliness_summary$region_table,
                                   lab_workload_summary$region_table,
                                   lab_itd_timeliness_summary$region_table,
                                   lab_ship_timeliness_summary$region_table,
                                   lab_seq_timeliness_summary$region_table) |>
                  dplyr::select(whoregion, flagname, string, period, countries_below, countries_incomplete) |>
                  tidyr::pivot_wider(id_cols=c(whoregion, flagname), values_from=c(string, countries_below, countries_incomplete),names_from=period)

lab_country_table_monthly <- dplyr::bind_rows(lab_virus_isolation_timeliness_summary$country_table,
                                            lab_workload_summary$country_table)

lab_country_table_quarterly <- dplyr::bind_rows(lab_itd_timeliness_summary$country_table,
                                              lab_ship_timeliness_summary$country_table,
                                              lab_seq_timeliness_summary$country_table)


# Visuals ---

# build each indicator's plot data
afp_cases_plot <- make_monthly_plot_data(afp_cases_reported$data, "place.admin.0", "current_period_counts", "prior_3yr_median", "01. Number of AFP Cases")
afp_negdet_plot <- make_monthly_plot_data(afp_neg_samples$data, "country", "current_median_days", "prior_median_days", "06. Negative Sample Timeliness - Median days")
afp_stoolship_plot <- make_monthly_plot_data(afp_timely_stool$data, "country", "current_median_days", "prior_median_days", "07. Stool Shipment Timeliness - Median days")
afp_inad_plot <- make_monthly_plot_data(afp_inadequate_cases$data, "place.admin.0", "current_period_counts", "prior_3yr_median", "08. Number of Inadequate Cases")
es_prop_active_plot <- make_monthly_plot_data(es_prop_active_sites_collections$data, "country", "prop_sites_with_1_collection", flag_label = "09. Proportion of ES sites with active collection", threshold = 80)
num_es_plot <- make_monthly_plot_data(es_active_sites$data, "country", "current_active_sites", "prior_active_sites", "10. Number of ES sites with active collection")
es_ship_plot <- make_monthly_plot_data(es_timely_shipment$data, "country", "current_median_days", "prior_median_days", "11. Timeliness of ES Shipment - Median days")

#stack all monthly indicator data into a long data so they can be visualized on the same plot
plotdata_m <- dplyr::bind_rows(afp_cases_plot, afp_negdet_plot, afp_stoolship_plot, afp_inad_plot, es_prop_active_plot, num_es_plot, es_ship_plot)

# lab monthly data
lab_isolat_plot<-make_monthly_plot_data(lab_virus_isolation_timeliness$data, "culture.itd.lab", "current_median_days", "prior_3yr_median_days", "13. Timeliness of Virus Isolation - Median days")
lab_workload_plot<-make_monthly_plot_data(lab_workload$data, "culture.itd.lab", "current_n", "prior_3yr_median", "16. Lab Workload - number of samples")

plotdata_m_lab <- dplyr::bind_rows(lab_isolat_plot, lab_workload_plot) |>
  dplyr::rename(Lab = Country)

# afp/es indicators that have a "window" comparison (e.g., current quarter v previous quarter)
afp_wpv_vdpv_timeliness_plot <- make_window_plot_data(afp_wpv_vdpv_timeliness$data, "ctry", "Country", "05. Timeliness of AFP WPV/VDPV Detection")
es_wpv_vdpv_timeliness_plot <- make_window_plot_data(es_wpv_vdpv_timeliness$data, "country", "Country", "12. Timeliness of ES WPV/VDPV Detection")
plotdata_q <- dplyr::bind_rows(afp_wpv_vdpv_timeliness_plot,es_wpv_vdpv_timeliness_plot)


# lab indicators that have a "window" comparison (e.g., current quarter v previous quarter)
lab_ITD_timeliness_plot <- make_window_plot_data(lab_virus_ITD_results_timeliness$data, "culture.itd.lab", "Lab", "14. Timeliness of ITD results")
lab_seq_ship_plot <- make_window_plot_data(lab_sequencing_shipment_timeliness$data, "culture.itd.lab", "Lab", "15. Timeliness of Shipment for Sequencing")
lab_seq_plot <- make_window_plot_data(lab_sequencing_timeliness$data, "seq.lab", "Lab", "17. Timeliness of Sequencing Results")

plotdata_lab_q <- bind_rows(lab_ITD_timeliness_plot, lab_seq_ship_plot, lab_seq_plot)

# these last 3 indicators use a "threshold" for comparison and can be plotted on the same chart with a little manipulation - not worth it to functionalize

afp_prop_60_plot <- afp_prop_60$data |>
  dplyr::select(period, ctry, prop_60day) |>
  dplyr::rename(Country = ctry) |>
  dplyr::mutate(Flag = "02. Proportion 60-Day Follow Up Completed",
                Threshold = 50,
                is_missing = is.na(prop_60day),
                value = dplyr::if_else(is_missing, 0, prop_60day)) |>
  dplyr::select(Country, period, value, is_missing, Flag, Threshold) |>
  dplyr::mutate(row_key = paste0(Country, "_", Flag, "_", period))

afp_prop_inad_plot <- afp_prop_inad_classified$data |>
  dplyr::select(ctry, prop_unclassified) |>
  dplyr::rename(Country = ctry) |>
  dplyr::mutate(Flag = "03. Proportion Inadequate Cases Unclassified",
                Threshold = 10,
                is_missing = is.na(prop_unclassified),
                value = dplyr::if_else(is_missing, 0, prop_unclassified),
                period = paste0(
                  format(lubridate::ymd(afp_prop_inad_classified$metadata$eligibility_start), "%b %Y"),
                  " - ",
                  format(lubridate::ymd(afp_prop_inad_classified$metadata$eligibility_end), "%b %Y"))
  ) |>
  dplyr::select(Country, period, value, is_missing, Flag, Threshold)

afp_prop_lab_plot <- afp_prop_lab_pending$data |>
  dplyr::select(ctry, prop_lab_pending) |>
  dplyr::rename(Country = ctry) |>
  dplyr::mutate(Flag = "04. Proportion Lab Pending",
                Threshold = 10,
                is_missing = is.na(prop_lab_pending),
                value = dplyr::if_else(is_missing, 0, prop_lab_pending),
                period = paste0(
                  format(lubridate::ymd(afp_prop_lab_pending$metadata$eligibility_start), "%b %Y"),
                  " - ",
                  format(lubridate::ymd(afp_prop_lab_pending$metadata$eligibility_end), "%b %Y"))
  ) |>
  dplyr::select(Country, period, value, is_missing, Flag, Threshold)

plotdata_q1 <- dplyr::bind_rows(afp_prop_60_plot, afp_prop_inad_plot, afp_prop_lab_plot)

# Save -------------------------------------------------------------------------------------------------------

# Save tables ----
if(!dir.exists("datatables")){
  dir.create("datatables")
}
save(end_date, max_lab_date, afp_cases_reported, afp_prop_60, afp_prop_inad_classified,
     afp_prop_lab_pending, afp_wpv_vdpv_timeliness, afp_neg_samples, afp_timely_stool,
     afp_inadequate_cases, es_active_sites, es_timely_shipment, es_wpv_vdpv_timeliness,
     es_prop_active_sites_collections, lab_virus_isolation_timeliness,
     lab_virus_ITD_results_timeliness, lab_sequencing_shipment_timeliness,
     lab_workload, lab_sequencing_timeliness,
     region_table, country_table_monthly, country_table_quarterly, country_table_noperiod,
     lab_region_table, lab_country_table_monthly, lab_country_table_quarterly,
     plotdata_m, plotdata_m_lab,
     plotdata_q, plotdata_lab_q, plotdata_q1,
     file = "datatables/datatables.rda")

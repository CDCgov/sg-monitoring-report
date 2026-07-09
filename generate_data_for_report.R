# generate_data_for_report.R
# Sourced by the QMD. Loads raw data, runs all build_ indicators, and saves
# to cache. All analytical work happens here — QMD is presentation only.


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
lab_data <- clean_lab_data(lab_data, "2019-01-01", end_date, afp_data = raw_data$afp)
max_lab_date <- max(lab_data$CaseDate, na.rm = TRUE)

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


# Create Indicator Results ---------------------------------------------------------------------------------------------
# Saving the full object list so QMD can access $data and $metadata

# AFP Indicators ----
# Generate indicator results
afp_cases_reported <- build_afp_cases_reported(raw_data$afp, end_date)
afp_prop_60 <- build_prop_60_day_follow_up_indicator(raw_data$afp)
afp_prop_inad_classified <- build_prop_inadequate_classified(raw_data$afp, end_date)
afp_prop_lab_pending <- build_prop_lab_pending(raw_data$afp, end_date)
afp_wpv_vdpv_timeliness <- build_wpv_vdpv_timeliness_indicator(raw_data$pos, end_date)
afp_neg_samples <- build_negative_samples_timeliness_indicator(lab_data, max_lab_date)
afp_timely_stool <- build_timely_stool_shipment_indicator(lab_data, max_lab_date)

# Add risk category
afp_cases_reported$data <- add_risk(afp_cases_reported$data, "place.admin.0")
afp_prop_60$data <- add_risk(afp_prop_60$data, "ctry")
afp_prop_inad_classified$data <- add_risk(afp_prop_inad_classified$data, "ctry")
afp_prop_lab_pending$data <- add_risk(afp_prop_lab_pending$data, "ctry")
afp_wpv_vdpv_timeliness$data <- add_risk(afp_wpv_vdpv_timeliness$data, "ctry")
afp_neg_samples$data <- add_risk(afp_neg_samples$data, "country")
afp_timely_stool$data <- add_risk(afp_timely_stool$data, "country")


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
lab_virus_isolation_timeliness <- build_timeliness_virus_isolation_indicator(lab_data, max(lab_data$DateFinalCellCultureResult, na.rm = TRUE))




# Save and Cache -------------------------------------------------------------------------------------------------------

# NEED TO ADD


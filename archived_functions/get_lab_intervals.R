#' Calculate lab intervals
#'
#' @description
#' Calculate the lab intervals of each sample in the lab dataset. The intervals calculated are
#' timeliness of virus isolation results (days.lab.culture),
#' ITD results (days.culture.itd), shipment for sequencing (days.seq.ship), and
#' sequencing results (days.seq.rec.res). An alternative timeliness indicator for sequencing results
#' is measured from the date of ITD results to sequencing results (days.itd.res.seq.res).
#' The `t1-t5` filters for valid intervals only. That is, intervals that are not N/A, negative, nor
#' more than 365 days.
#'
#'
#' @param lab_data `tibble` Lab data, namely the regional lab data obtained from WHO.
#'
#' @details
#' To request updated lab data, email Minh-Ly's group.
#'
#' @returns `tibble` The lab dataset with additional timeliness columns added.
#' @internal
#'
#' @examples
#' \dontrun{
#' lab_data <- readr::read_csv("lab_data.csv")
#' lab_data <- get_lab_intervals(lab_data)
#' }
get_lab_intervals <- function(lab_data) {

  lab_data |>
    dplyr::mutate(
      # Timeliness of virus isolation results
      # Start date: receipt at WHO-accredited lab, end date: culture results
      # Target: \u226414 days
      days.lab.culture = .data$DateFinalCellCultureResult - .data$DateStoolReceivedinLab,
      t1 = dplyr::if_else(!is.na(.data$days.lab.culture) &
                            (.data$days.lab.culture >= 0 & .data$days.lab.culture <= 365),
                          TRUE, FALSE),
      # Timeliness of ITD results (Amanda added this)
      # Start date: culture results, end date: ITD results
      # Target: \u22647 days
      days.culture.itd = .data$DateFinalrRTPCRResults - .data$DateFinalCellCultureResult,
      t2 = dplyr::if_else(!is.na(.data$days.culture.itd) &
                            (.data$days.culture.itd >= 0 & .data$days.culture.itd <= 365),
                          TRUE, FALSE),
      # Timeliness of shipment for sequencing
      # Start date: ITD result, end date: arrival at sequencing lab (
      # (Amanda updated start date here to be consistent with GPSAP 2025-26 indicator)
      # Target: \u22647 days
      days.seq.ship = .data$DateIsolateRcvdForSeq - .data$DateFinalrRTPCRResults,
      t3 = dplyr::if_else(!is.na(.data$days.seq.ship) &
                            (.data$days.seq.ship >= 0 & .data$days.seq.ship <= 365),
                          TRUE, FALSE),
      # Timeliness of sequencing results
      # Start date: arrival at sequencing lab, end.date: sequencing results
      # Target: \u22647 days
      days.seq.rec.res = .data$DateofSequencing - .data$DateIsolateRcvdForSeq,
      t4 = dplyr::if_else(!is.na(.data$days.seq.rec.res) &
                            (.data$days.seq.rec.res >= 0 & .data$days.seq.rec.res <= 365),
                          TRUE, FALSE),

      # ALTERNATIVE for Timeliness of sequencing results
      # Start date: ITD results, end date: sequencing results
      days.itd.res.seq.res = .data$DateofSequencing - .data$DateFinalrRTPCRResults,
      t5 =  dplyr::if_else(!is.na(.data$days.itd.res.seq.res) &
                             (.data$days.itd.res.seq.res >= 0 & .data$days.itd.res.seq.res <= 365),
                           TRUE, FALSE)
    )

}

#' test data and test taker selection
#'
#' @param longitudinal logical; whether longitudinal PVs are estimated
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. for competence domain (e.g. "MA", "RE")
#' @param data data.frame; xTargetCompetencies
#' @param wave character; wave of test ("wx")
#' @param min_valid numeric; minimum number of valid test responses per person
#'
#' @return list of response data.frame (a list with one data.frame for each
#' assessment in the longitudinal case) and xTargetCompetencies; only participants
#' with min_valid valid responses are returned
#' @noRd

select_test_responses_and_test_takers <- function(longitudinal, SC, domain,
                                                  data, wave, min_valid) {
  if (longitudinal) {
    if (SC == "SC6" && domain == "RE") {
      resp <- select_longitudinal_sc6_reading(data, SC, domain, min_valid)
    } else {
      resp <- list()
      for (i in seq(length(item_labels[[domain]][[SC]]))) {
        resp[[i]] <- select_data(data, SC, domain, wave = i, min_valid)
        if (SC %in% c("SC3", "SC4") & domain == "EF") {
          resp[[i]] <- impute_english_competence_data(resp[[i]], SC, wave = i)
        }
      }
    }
    data <-
      data[
        data[["ID_t"]] %in% unique(unlist(lapply(resp,
                                                 function(x) {x[["ID_t"]]}))), ]
    data <- data[order(data[["ID_t"]]), ]
  } else {
    # reading has been tested twice for different samples in SC6:
    # estimating simultaneously might cause problems because of different
    # information available for background model (large quantities of missing data)
    if (SC == "SC6" & domain == "RE") {
      data <- select_correct_cross_sc6_reading_subsample(data, wave)
    }
    resp <- select_data(data, SC, domain, wave, min_valid)
    data <- data[data[["ID_t"]] %in% resp[["ID_t"]], ]
    data <- data[order(data[["ID_t"]]), ]
    if (SC == "SC4" & domain == "ST") {
      resp <- aggregate_scientific_thinking_items(resp)
    }
    if (SC %in% c("SC3", "SC4") & domain == "EF") {
      resp <- impute_english_competence_data(resp, SC, wave)
    }
  }
  list(resp = resp, data = data)
}


#' actual selection of items and test takers
#'
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. for competence domain (e.g. "MA", "RE")
#' @param data data.frame; xTargetCompetencies
#' @param wave character; wave of test ("wx")
#' @param min_valid numeric; minimum number of valid test responses per person
#'
#' @return response data.frame; only participants with min_valid valid responses
#' are returned
#' @noRd
select_data <- function(data, SC, domain, wave, min_valid) {
  resp <- data[, c("ID_t", item_labels[[domain]][[SC]][[wave]])]
  resp <- resp[rowSums(!is.na(resp[, -1])) >= min_valid, ]
  resp <- resp[order(resp[["ID_t"]]), ]
  resp
}


#' selection of subsample in SC6 reading (cross)
#'
#' before items and test takers are selected, the respective subsample is
#' extracted
#'
#' @param data data.frame; xTargetCompetencies
#' @param wave character; wave of test ("wx")
#'
#' @return response data.frame; only participants with min_valid valid responses
#' are returned
#' @noRd
select_correct_cross_sc6_reading_subsample <- function(data, wave) {
  if (wave == "w3") {
    data <- data[data[["wave_w3"]] == 1, ]
  } else if (wave == "w5") {
    data <- data[data[["wave_w3"]] == 0 & data[["wave_w5"]] == 1, ]
  }
  data
}


#' aggregation of individual ST items to PC items
#'
#' they were published in their sub-item form in the SUF instead of as PC items
#'
#' @param resp data.frame; xTargetCompetencies
#'
#' @return response data.frame with aggregated items; individual items are
#' dropped
#' @noRd
aggregate_scientific_thinking_items <- function(resp) {
  names(resp)[which(names(resp) == "stg12cmt06_c")] <- "stg12mt06_c"
  names(resp)[which(names(resp) == "stg12cpd03_c")] <- "stg12pd03_c"
  items <-
    c("stg12nhs_c", "stg12egs_c", "stg12mts_c", "stg12cws_c", "stg12pds_c")
  for (i in items) {
    resp[[i]] <- rowSums(resp[, grep(substr(i, 1, 7), names(resp))],
                         na.rm = TRUE)
  }
  resp <- resp[, c("ID_t", items)]
  resp
}

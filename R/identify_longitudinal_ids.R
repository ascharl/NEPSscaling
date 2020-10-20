#' identify longitudinal ids for linking
#'
#' @param SC ...
#' @param domain ...
#' @param data ...
#' @param waves ...
#' @param eap ...
#'
#' @noRd

identify_longitudinal_ids <- function (SC, domain, data, waves, eap) {
  longitudinal_IDs <- list()
  if (SC == "SC6" && domain == "RE") {
    longitudinal_IDs[["w3"]] <- data[["ID_t"]][
      data[["wave_w3"]] == 1 & !is.na(data[["rea9_sc1u"]])
    ]
    longitudinal_IDs[["w5"]] <- data[["ID_t"]][
      data[["wave_w5"]] == 1 & data[["wave_w3"]] == 0 &
        !is.na(data[["rea9_sc1u"]])
    ]
  } else {
    for (i in seq(2, length(waves))) {
      longitudinal_IDs[[i - 1]] <- data[["ID_t"]][
        !is.na(eap[, paste0("eap_", gsub("_", "", waves[i-1]))]) &
          !is.na(eap[, paste0("eap_", gsub("_", "", waves[i]))])
      ]
    }
  }
  longitudinal_IDs
}
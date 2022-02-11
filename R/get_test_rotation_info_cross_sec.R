#' test rotation / testlet position for cross-sectional estimation
#'
#' @param rotation logical; whether test rotation is to be considered
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param wave character; wave of assessment ("wx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#' @param resp data.frame; response data + ID_t
#' @param bgdata data.frame or NULL; background data
#' @param ID_t data.frame; ID_t of test takers (only column)
#'
#' @return list of position data.frame, rotation indicator, original data.frame,
#' resp data.frame, ID_t data.frame and bgdata data.frame
#' @noRd
get_test_rotation_info_cross_sec <- function(rotation, data, SC, wave,
                                             domain, resp, bgdata, ID_t) {
  position <- create_facet(data, SC, wave, domain)
  rotation <- adjust_rotation_indicator(rotation, position)
  if (rotation) {
    res <- adjust_data_with_rotation_info(data, resp, ID_t, bgdata, position)
    data <- res$data
    resp <- res$resp
    ID_t <- res$ID_t
    bgdata <- res$bgdata
    position <- res$position
  }
  list(position = position, rotation = rotation, data = data,
       resp = resp, ID_t = ID_t, bgdata = bgdata)
}


#' test rotation / testlet position for cross-sectional estimation
#'
#' @param data data.frame; xTargetCompetencies etc.
#' @param SC character; starting cohort ("SCx")
#' @param wave character; wave of assessment ("wx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#'
#' @return position data.frame also containing ID_t
#' @noRd
create_facet <- function(data, SC, wave, domain) {
  position <- data[, "ID_t", drop = FALSE]
  # construct facet to correct for rotation design
  if ((SC == "SC1" & wave %in% c("w1", "w7", "w8")) |
      (SC == "SC4" & wave %in% c("w3")) |
      (SC == "SC5" & wave %in% c("w7")) |
      (SC == "SC2" & wave %in% c("w6"))) {
    # no test rotation or position variable available
    position[["position"]] <- 1
  } else {
    position[["position"]] <- data[[paste0("tx80211_", wave)]]
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 1])] <- 1
    position[["position"]][
      !is.na(position[["position"]]) &
        (position[["position"]] %in%
           testlet_position[[SC]][[domain]][[wave]][, 2])] <- 2
    # a mysterious no reading administered person appeared in SC2 / RE / w9
    # set all variables that are not 1 or 2 to NA
    position[["position"]][
      !is.na(position[["position"]]) &
        !(position[["position"]] %in% c(1, 2))] <- NA
  }
  # possible NAs lead to further data selection
  position <- position[!is.na(position[["position"]]), ]
  position
}


#' test rotation / testlet position for cross-sectional estimation
#'
#' @param rotation logical; whether test rotation is to be considered
#' @param position data.frame; position indicator + ID_t
#'
#' @return boolean whether rotation is still to be considered
#' @noRd
adjust_rotation_indicator <- function(rotation, position) {
  rotation & length(unique(position[["position"]])) > 1
}


#' test rotation / testlet position for cross-sectional estimation
#'
#' @param rotation logical; whether test rotation is to be considered
#' @param data data.frame; xTargetCompetencies etc.
#' @param resp data.frame; response data + ID_t
#' @param bgdata data.frame or NULL; background data
#' @param ID_t data.frame; ID_t of test takers (only column)
#' @param position data.frame; position indicator + ID_t
#'
#' @return list of position data.frame, original data.frame,
#' resp data.frame, ID_t data.frame and bgdata data.frame; the latters' rows
#' are filtered to conform to the position data.frame
#' @noRd
adjust_data_with_rotation_info <- function(data, resp, ID_t, bgdata, position) {
  data <- data[data[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
  resp <- resp[resp[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
  ID_t <- ID_t[ID_t[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
  if (!is.null(bgdata)) {
    if (inherits(bgdata, "list") ) {
      for (i in 1:length(bgdata)) {
        bgdata[[i]] <-
          bgdata[[i]][bgdata[[i]][["ID_t"]] %in% position[["ID_t"]], ,
                      drop = FALSE]
      }
    } else {
      bgdata <- bgdata[bgdata[["ID_t"]] %in% position[["ID_t"]], , drop = FALSE]
    }
  }
  # # possible NAs in position variable treated as third group
  # position[is.na(position[["position"]]), "position"] <- 3
  # format position effect information
  position <- position[, "position", drop = FALSE]
  list(data = data, resp = resp, ID_t = ID_t, bgdata = bgdata,
       position = position)
}

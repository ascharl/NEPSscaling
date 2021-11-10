#' not reached missing values as proxy for processing time and sets user defined
#' missing values as NA
#' @param include_nr logical; whether not-reached missings should be included
#' as processing time proxies
#' @param longitudinal logical; whether longitudinal pvs are to be estimated
#' @param data data.frame; xTargetCompetencies
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "RE", "SC")
#' @param wave character; wave of test assessment ("wx")
#' @param waves character; waves of test assessment in the longitudinal case
#' ("_wx", "_wy")
#'
#' @return list of not-reached proxies, original data with missing values set
#' to NA, and updated include_nr indicator
#' @noRd
not_reached_as_proxy <- function(include_nr, longitudinal, data, SC, domain,
                                 wave, waves) {
  nr <- NULL
  if (include_nr) {
    sel <- determine_response_items(SC, domain, data, wave, longitudinal)
    nr <- calculate_not_reached_per_person(data, sel, waves, SC, domain,
                                           longitudinal)
    nr <- remove_constant_not_reached(nr, sel)
  }
  # set user-defined missings to NA
  data[data < -15] <- NA # assumption: WLEs this low are not to be expected
  list(nr = nr, data = data, include_nr = ifelse(is.null(nr), FALSE, TRUE))
}

#' remove proxies with constant value for all test takers
#'
#' @param nr data.frame containing not reached proxies
#' @param sel list of character vector(s) of response items for the current wave
#'
#' @return updated not-reached proxies
#' @noRd
remove_constant_not_reached <- function(nr, sel) {
  # ID_t always > 1 value -> ignore in check for constant nr values
  if (any(lapply(lapply(nr, unique), length) == 1)) {
    ind <- which(lapply(lapply(nr, unique), length) == 1)
    if (length(ind) == length(sel)) {
      nr <- NULL
      message(
        "The number of not-reached missing values is constant. ",
        "Thus, it is not considered in the background model."
      )
    } else {
      nr <- nr[, -ind]
      message(names(ind), " is constant. It is excluded from the ",
              "background model.")
    }
  }
  nr
}

#' determine the response items to calculate the proxy over
#'
#' @param longitudinal logical; whether longitudinal pvs are to be estimated
#' @param data data.frame; xTargetCompetencies
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "RE", "SC")
#' @param wave character; wave of test assessment ("wx")
#'
#' @return list of vector(s) of response item names
#' @noRd
determine_response_items <- function(SC, domain, data, wave, longitudinal) {
  sel <- lapply(item_labels[[domain]][[SC]],
                function(it) {names(data) %in% it})
  if (longitudinal) {
    if (SC == "SC2" && domain == "SC") {
      sel <- sel[-length(sel)]
    }
  } else {
    sel <- sel[which(names(sel) == wave)] # needs to stay a list
  }
  sel
}

#' not reached missing values as proxy for processing time and sets user defined
#' missing values as NA
#'
#' @param longitudinal logical; whether longitudinal pvs are to be estimated
#' @param data data.frame; xTargetCompetencies
#' @param SC character; starting cohort ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "RE", "SC")
#' @param sel list of character vector(s); response variable names per wave
#' @param waves character; waves of test assessment in the longitudinal case
#' ("_wx", "_wy")
#'
#' @return data.frame of not-reached proxies (ID_t + one column per wave)
#' @noRd
calculate_not_reached_per_person <- function(data, sel, waves, SC, domain,
                                             longitudinal) {
  # in the longitudinal case, missing test taking for later time points
  # causes problems in imputation, if include_nr = TRUE, bgdata = NULL,
  # thus, remove NAs from data
  nr <- data.frame(ID_t = data[["ID_t"]])
  for (s in seq(length(sel))) {
    nr[[paste0("items_not_reached", waves[s])]] <-
      rowSums(data[, sel[[s]], drop = FALSE] == -94, na.rm = TRUE)
  }
  if (longitudinal & SC == "SC6" & domain == "RE") {
    nr[["items_not_reached_w3"]][is.na(data[["rea3_sc1u"]])] <- NA
    nr[["items_not_reached_w5"]][is.na(data[["rea5_sc1u"]])] <- NA
  }
  nr
}

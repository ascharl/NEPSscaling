#' disable print/cat side effects: https://stackoverflow.com/a/54136863
#' @param x an R statement
#' @noRd
quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
}

#' determine wether there are pc items in the data set
#' @param longitudinal a boolean
#' @param resp a data.frame of the participants' responses to competence items
#' in the cross-sectional case; a list containing as many data.frames as there
#' are assessment waves in the longitudinal case
#'
#' @return A vectors of booleans signifying whether the item is categorical in
#' the cross-sectional case; a list of as many boolean vectors as there are
#' assessment waves in the longitudinal case
#' @noRd
is_PCM <- function(longitudinal, resp) {
  if (longitudinal) {
    return(lapply(resp,
                  function(x) {max(apply(x[, -1], 2, max, na.rm = TRUE)) > 1}))
  }
  max(apply(resp[, -1], 2, max, na.rm = TRUE)) > 1
}

#' Determine whether the given competence test(s) was assessed in a group context
#' @param longitudinal a boolean
#' @param SC a string value of form "SCx" where x stands for the starting cohorts
#' 1 to 6
#' @param wave a string value of form "wx" where x stands for the assessment
#' wave
#'
#' @return a boolean signifying whether (in the longitudinal case, one of) the
#' assessments took place in a group setting
#' @noRd
was_assessed_in_school <- function(longitudinal, SC, wave) {
  if (longitudinal) {
    # some starting cohorts are never assessed in school context, thus, they
    # can be excluded as a whole in the longitudinal case
    if (SC %in% c("SC1", "SC5", "SC6")) {
      return(FALSE)
    }
  } else {
    # some starting cohorts transition into or out of school, thus, specific
    # waves have to be checked in the cross-sectional case
    if (!(SC == "SC2" & wave %in% c("w3", "w4", "w5", "w6", "w9")) &&
        !(SC == "SC3" & wave %in% c("w1", "w2", "w3", "w5", "w6", "w7",
                                    "w8", "w9")) &&
        !(SC == "SC4" & wave %in% c("w1", "w2", "w3", "w5", "w7"))) {
      return(FALSE)
    }
  }
  TRUE
}

#' Remove specified variables from the background data AFTER imputation, but
#' BEFORE PV estimation
#'
#' @param bgdatacom a data.frame containing one imputated data set
#' @param exclude a vector of variables to EXclude in the cross-sectional case,
#' a list of vectors of variables to EXclude in the longitudinal case. Here, the
#' vectors have to be named according to the waves to be manipulated (e.g. "w3"
#' for wave 3). It is possible to specify "exclude" only for a subset of
#' assessment waves.
#' @param j an integer value denoting the index of the assessment wave in the
#' longitudinal case; NULL in the cross-sectional case
#'
#' @return a data.frame for the IRT model WITHOUT the specified variables
#' @noRd
extract_bgdata_variables <- function(bgdatacom, exclude, waves, j) {
  if (is.null(j)) {
    # cross-sectional
    excl <- names(bgdatacom) %in% exclude
    if (any(excl)) {
      return(bgdatacom[, !excl, drop = FALSE])
    }
  } else {
    if (gsub("_", "", waves[j]) %in% names(exclude)) {
      excl <- names(bgdatacom) %in% exclude[[gsub("_", "", waves[j])]]
      if (any(excl)) {
        return(bgdatacom[, !excl, drop = FALSE])
      }
    }
  }
  bgdatacom
}

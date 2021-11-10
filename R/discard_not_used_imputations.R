#' extract correct number of further info about PV model
#'
#' @param datalist list of data.frames; contains npv PVs and completed bgdata
#' @param regr.coeff list of matrices or matrix; contains latent regression
#' coefficients
#' @param EAP.rel list of vectors or vector; contains EAP reliabilities
#' @param longitudinal logical; whether estimation is longitudinal in design
#' @param info_crit list of matrices or matrix; AIC, BIC
#' @param treeplot list of tree structure plots per imputation
#' @param variable_importance list of matrices with importance statistics for
#' predictor variables per imputation
#' @param variance list (long.) / vector (cross.) of latent variances
#' @param eap list of data.frames containing ID_t, eap(s) and se(s)
#'
#' @return list of regression coefficients, eap reliabilities, info criteria,
#' tree representations, variable importances, variances and eaps -- each with
#' imputations not selected with PV data.frame sampling removed
#' @noRd
discard_not_used_imputations <- function(datalist, regr.coeff, EAP.rel,
                                         longitudinal, info_crit, treeplot,
                                         variable_importance, variance, eap) {
  # names of pvs:
  keep <- determine_used_imputations(datalist)
  # if only one imputation was sampled or no bgdata supplied, exit
  if (length(keep[[1]]) == 1) {
    if (longitudinal) {
      names(EAP.rel) <- names(regr.coeff) <- names(info_crit) <- names(variance) <-
        names(eap) <- keep[[2]]
    }
    return(list(regr.coeff = regr.coeff, EAP.rel = EAP.rel,
                info_crit = info_crit, treeplot = treeplot,
                variable_importance = variable_importance, variance = variance,
                eap = eap))
  }
  # keep only those EAP reliability / regression coefficients with imputations
  if (longitudinal) {
    res <- select_used_imputations_long(EAP.rel, regr.coeff, info_crit,
                                        treeplot, variable_importance,
                                        variance, eap, keep[[1]])
  } else {
    res <- select_used_imputations_cross(EAP.rel, regr.coeff, info_crit,
                                         treeplot, variable_importance,
                                         variance, eap, keep)
  }
  res
}

#' determine imputations still left after PV sampling
#'
#' @param datalist list of data.frames; contains npv PVs and completed bgdata
#'
#' @return list of string and integer representation of the kept imputations
#' @noRd
determine_used_imputations <- function(datalist) {
  # for list data structures
  keep <- names(datalist)
  keep <- as.numeric(stringr::str_match(keep, "imp\\s*(.*?)\\s*pv")[, 2])
  keep <- unique(sort(keep))

  # for matrix data structures (= regr.coeff)
  keep2 <- names(datalist)
  keep2 <- stringr::str_match(keep2, pattern = "imp[0-9]+")
  keep2 <- unique(sort(keep2))

  list(keep, keep2)
}

#' extract correct number of further info about PV model
#'
#' @param regr.coeff list of matrices or matrix; contains latent regression
#' coefficients
#' @param EAP.rel list of vectors or vector; contains EAP reliabilities
#' @param info_crit list of matrices or matrix; AIC, BIC
#' @param treeplot list of tree structure plots per imputation
#' @param variable_importance list of matrices with importance statistics for
#' predictor variables per imputation
#' @param variance list (long.) / vector (cross.) of latent variances
#' @param eap list of data.frames containing ID_t, eap(s) and se(s)
#' @param keep vector of integers; represents imputations to keep
#'
#' @return list of regression coefficients, eap reliabilities, info criteria,
#' tree representations, variable importances, variances and eaps -- each with
#' imputations not selected with PV data.frame sampling removed
#' @noRd
select_used_imputations_long <- function(EAP.rel, regr.coeff, info_crit,
                                         treeplot, variable_importance,
                                         variance, eap, keep) {
  # list of length nmi of EAP rels for each wave
  EAP.rel <- EAP.rel[keep]
  eap <- eap[keep]
  variance <- variance[keep]
  regr.coeff <- regr.coeff[keep]
  info_crit <- info_crit[keep]
  treeplot <- treeplot[keep]
  variable_importance <- variable_importance[keep]
  names(EAP.rel) <- names(regr.coeff) <- names(info_crit) <- names(variance) <-
    names(eap) <- paste0("imp", keep)

  if (!is.null(treeplot)) {
    names(treeplot) <- names(variable_importance) <- paste0("imp", keep)
  }

  list(regr.coeff = regr.coeff, EAP.rel = EAP.rel, info_crit = info_crit,
       treeplot = treeplot, variable_importance = variable_importance,
       variance = variance, eap = eap)
}

#' extract correct number of further info about PV model
#'
#' @param regr.coeff list of matrices or matrix; contains latent regression
#' coefficients
#' @param EAP.rel list of vectors or vector; contains EAP reliabilities
#' @param info_crit list of matrices or matrix; AIC, BIC
#' @param treeplot list of tree structure plots per imputation
#' @param variable_importance list of matrices with importance statistics for
#' predictor variables per imputation
#' @param variance list (long.) / vector (cross.) of latent variances
#' @param eap list of data.frames containing ID_t, eap(s) and se(s)
#' @param keep list of string and integer representation of the kept imputations
#'
#' @return list of regression coefficients, eap reliabilities, info criteria,
#' tree representations, variable importances, variances and eaps -- each with
#' imputations not selected with PV data.frame sampling removed
#' @noRd
select_used_imputations_cross <- function(EAP.rel, regr.coeff, info_crit,
                                          treeplot, variable_importance,
                                          variance, eap, keep) {
  # vector of length nmi
  EAP.rel <- EAP.rel[keep[[1]]]
  eap <- eap[keep[[1]]]
  variance <- variance[keep[[1]]]
  treeplot <- treeplot[keep[[1]]]
  variable_importance <- variable_importance[keep[[1]]]
  info_crit <- info_crit[, keep[[1]]]
  names(EAP.rel) <- names(info_crit) <- names(variance) <- names(eap) <-
    paste0("imp", keep[[1]])
  regr.coeff <- regr.coeff[, grepl(paste0("Variable|",
                                          paste(keep[[2]], collapse = "|")),
                                   names(regr.coeff))]

  if (!is.null(treeplot)) {
    names(treeplot) <- names(variable_importance) <- paste0("imp", keep[[1]])
  }

  list(regr.coeff = regr.coeff, EAP.rel = EAP.rel, info_crit = info_crit,
       treeplot = treeplot, variable_importance = variable_importance,
       variance = variance, eap = eap)
}

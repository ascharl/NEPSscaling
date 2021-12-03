#' estimation of cross-sectional plausible values (dichotomous data) with
#'   correction for testlet position (multi-facet model)
#'
#' @param bgdata Background data given by user (either NULL or a data.frame)
#' @param imp Imputed background data (either NULL if is.null(bgdata) or no
#'   missing data is in background data)
#' @param resp Matrix of item responses given by test takers
#' @param waves Wave in which the competencies have been assessed (String in
#'   the form "_wX")
#' @param frmY Formula detailing how the background data is to be used for
#'   plausible values estimation -- defaults to NULL if is.null(bgdata) or to
#'   linear combination of all bgdata variables
#' @param ID_t Data.frame containing all IDs for the current test takers
#' @param type String ("cross" for cross-sectional or "long" for longitudinal)
#' @param domain String detailing the competence domain
#' @param SC String detailing which starting cohort is used
#' @param control List of control variables for plausible values estimation
#'   algorithm
#' @param npv Integer value fo number of plausible values to be returned by
#'   `NEPScaling::plausible_values()`
#' @param nmi numeric; denotes the number of multiple imputations for missing
#' covariate data (defaults to 10).
#' @param exclude list of vectors (named after the waves); contains
#' variables that are NOT to be used in the background model of the specified
#' wave
#'
#' @return list of list of eap data.frames, list of pvs (one list per imputation
#' with one data.frame per PV), list of TAM model, vector of EAP.rel, matrix
#' of regression coefficients, vector of variances, matrix of info criteria
#' @noRd
estimate_cross_rasch_corrected_for_rotation <- function(bgdata, imp,
                                                        frmY = NULL, resp,
                                                        position, waves,
                                                        ID_t, type, domain,
                                                        SC, control, npv, nmi,
                                                        exclude) {
  times <- ifelse(is.null(bgdata) || !any(is.na(bgdata)), 1, nmi)
  items <- rownames(xsi.fixed$cross[[domain]][[SC]][[gsub("_", "", waves)]])

  pvs <- list(NULL)
  EAP.rel <- info_crit <- regr.coeff <- variance <- NULL
  eap <- replicate(times, data.frame(ID_t = ID_t$ID_t), simplify = FALSE)
  for (i in 1:times) {
    res <- prepare_bgdata_frmY(imp, i, frmY)
    if (is.null(res[["bgdatacom"]])) {
      bgdatacom <- bgdata
    } else {
      bgdatacom <- res[["bgdatacom"]]
    }
    frmY <- res[["frmY"]]

    # extract bgdata specific for wave
    if (!is.null(exclude)) {
      bgdatacom <- extract_bgdata_variables(bgdatacom, exclude, waves, NULL)
      frmY <- create_formula(bgdatacom)
    }

    # estimate IRT model
    mod <- list()

    mod[[1]] <- TAM::tam.mml.mfr(
      resp = resp[, items],
      facets = position,
      formulaA = ~ 0 + item + position,
      dataY = bgdatacom[, -which(names(bgdatacom) == "ID_t"), drop = FALSE],
      formulaY = frmY,
      pid = resp$ID_t,
      irtmodel = "1PL",
      xsi.fixed = xsi.fixed[[type]][[domain]][[SC]][[gsub(
        "_", "",
        waves
      ) ]],
      verbose = FALSE
    )
    # post-processing of model
    res <- post_process_cross_tam_results(mod[[1]], npv, control,
      imp, bgdatacom, eap, i, EAP.rel, regr.coeff, pvs, info_crit, frmY,
      variance
    )
    eap <- res$eap
    regr.coeff <- res$regr.coeff
    pvs <- res$pvs
    EAP.rel <- res$EAP.rel
    info_crit <- res$info_crit
    variance <- res$variance
  }
  res <- list(
    eap = eap, pvs = pvs, mod = mod, EAP.rel = EAP.rel,
    regr.coeff = regr.coeff, info_crit = info_crit, variance = variance
  )
  res
}

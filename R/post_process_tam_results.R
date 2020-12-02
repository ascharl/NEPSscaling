#' model post-processing in cross-sectional estimation:
#' impute pvs, add eaps, eap reliability, regression coefficients to
#' previous estimation results, discard unnecessary data provided by TAM
#' and rename
#'
#' @param mod estimated TAM model
#' @param npv number of plausible values to return
#' @param control control list for TAM functions
#' @param imp imputed background data
#' @param bgdatacom completed background data set
#' @param eap list of eap values
#' @param i current iteration over background data imputations
#' @param EAP.rel list of eap reliability values
#' @param regr.coeff list of regression coefficients of latent regression
#' @param pvs list of estimated plausible values
#' @param bgdata background data (can be NULL)
#'
#' @noRd

post_process_cross_tam_results <- function(mod, npv, control, imp,
                                           bgdatacom = NULL, eap, i,
                                           EAP.rel, regr.coeff, pvs, bgdata) {
  # impute plausible values
  tmp_pvs <- impute_pvs(mod, npv, control, bgdata, imp, bgdatacom, "", 1)
  eap[[i]] <- suppressWarnings(
    dplyr::left_join(eap[[i]], mod$person[, grep("pid|EAP", names(mod$person))],
                     by = c("ID_t" = "pid"))) %>%
    dplyr::arrange(.data$ID_t)
  EAP.rel <- c(EAP.rel, mod$EAP.rel)
  # se estimation gives warning "In sqrt(-1/info_pp) : NaNs produced" because
  # item difficulty parameters are fixed --> suppress warnings!
  if (i == 1) {
    regr.coeff <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(regr.coeff) <- paste0("imp", i, "_", c("coeff", "se"))
    rownames(regr.coeff) <-
      c("Intercept",
        names(bgdata[, -which(names(bgdata) == "ID_t"), drop = FALSE]))
  } else if (i > 1) {
    tmp <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(tmp) <- paste0("imp", i, "_", c("coeff", "se"))
    regr.coeff <- cbind(regr.coeff, tmp)
  }
  pvs[[i]] <- lapply(tmp_pvs, function(x) {
    x[, -which(colnames(x) == "pweights")]
  })
  if (is.null(bgdata)) {
    for (n in 1:npv) {
      names(pvs[[i]][[n]])[which(names(pvs[[i]][[n]]) == "pid")] <- "ID_t"
    }
  }
  colnames(eap[[i]]) <- c("ID_t", "eap", "se")

  list(eap = eap, regr.coeff = regr.coeff, pvs = pvs, EAP.rel = EAP.rel)
}



#' model post-processing in longitudinal estimation:
#' impute pvs, add eaps, eap reliability, regression coefficients to
#' previous estimation results, discard unnecessary data provided by TAM
#' and rename
#'
#' @param mod estimated TAM model
#' @param npv number of plausible values to return
#' @param control control list for TAM functions
#' @param imp imputed background data
#' @param bgdatacom completed background data set
#' @param eap list of eap values
#' @param i current iteration over background data imputations
#' @param j current iteration over assessment waves
#' @param EAP.rel list of eap reliability values
#' @param regr.coeff list of regression coefficients of latent regression
#' @param bgdata background data (can be NULL)
#' @param waves character vector; assessment waves ("_wx", "_wy")
#'
#' @noRd

post_process_long_tam_results <- function(mod, npv, control, imp,
                                          bgdatacom = NULL, eap, i, j,
                                          EAP.rel, regr.coeff, bgdata,
                                          waves) {
  # impute plausible values
  tmp_pvs <- impute_pvs(mod, npv, control, bgdata, imp, bgdatacom, waves, j)
  eap[[i]] <- suppressWarnings(
    dplyr::left_join(
      eap[[i]], mod$person[, grep("pid|EAP", names(mod$person))],
      by = c("ID_t" = "pid")
    )
  ) %>% dplyr::arrange(.data$ID_t)
  if (j == 1) {
    EAP.rel[[i]] <- mod$EAP.rel
    regr.coeff[[i]] <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    rownames(regr.coeff[[i]]) <-
      c("Intercept",
        names(bgdata[, -which(names(bgdata) == "ID_t"), drop = FALSE]))
    colnames(regr.coeff[[i]]) <- paste0(c("coeff", "se"), waves[j])
  } else {
    EAP.rel[[i]] <- c(EAP.rel[[i]], mod$EAP.rel)
    tmp <- suppressWarnings(quiet(TAM::tam.se(mod)$beta))
    colnames(tmp) <- paste0(c("coeff", "se"), waves[j])
    regr.coeff[[i]] <- cbind(regr.coeff[[i]], tmp)
  }

  list(eap = eap, regr.coeff = regr.coeff, tmp_pvs = tmp_pvs,
       EAP.rel = EAP.rel)
}
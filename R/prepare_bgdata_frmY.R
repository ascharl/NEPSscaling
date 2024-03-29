#' Prepare bgdata and frmY
#'
#' @param imp list of imputed data sets
#' @param i index for imputations
#' @param frmY formula for latent regression
#'
#' @return list of completed background data (may be NULL) and regression
#' formula for latent regression
#' @noRd
prepare_bgdata_frmY <- function(imp, i, frmY) {
  if (!is.null(imp)) {
    bgdatacom <- imp[[i]]
    # for (f in seq(ncol(bgdatacom))) {
    #   if (is.factor(bgdatacom[, f])) {
    #     bgdatacom[, f] <- as.numeric(levels(bgdatacom[, f]))[bgdatacom[, f]]
    #   } else if (is.character(bgdatacom[, f])) {
    #     bgdatacom[, f] <- as.numeric(bgdatacom[, f])
    #   }
    # }
    frmY <- create_formula(bgdatacom)
    return(list(bgdatacom = bgdatacom, frmY = frmY))
  }
  list(bgdatacom = NULL, frmY = frmY)
}

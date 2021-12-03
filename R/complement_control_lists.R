#' complement control lists for missing data handling
#'   and plausible values estimation
#'
#' @param c_E control list for EAP return
#' @param c_W control list for WLE estimation and return
#' @param c_T control list for TAM and mice estimation
#'
#' @return control list complemented for missing entries
#' @noRd
complement_control_lists <- function(c_E, c_W, c_T) {
  res <- list()

  # default values
  control_ML <- list(
    ntheta = 2000,
    normal.approx = FALSE,
    samp.regr = FALSE,
    theta.model = FALSE,
    np.adj = 8, na.grid = 5,
    minbucket = 5,
    cp = 0.0001
  )
  control_ML[names(c_T)] <- c_T

  if (is.null(c_E)) {
    res[["EAP"]] <- FALSE
  } else {
    res[["EAP"]] <- c_E
  }
  if (is.null(c_W)) {
    res[["WLE"]] <- FALSE
  } else {
    res[["WLE"]] <- c_W
  }
  res[["ML"]] <- control_ML

  res
}

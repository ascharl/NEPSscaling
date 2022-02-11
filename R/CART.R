#' Imputation with CART decision trees
#'
#' @param X data.frame to be imputed
#' @param minbucket minimum number of observations per split
#' @param cp minimum complexity parameter per split
#' @param nmi number of imputations to be returned
#' @param verbose logical whether progress is to be printed
#'
#' @return list of list of imputations and related variables (in nmi data.frames),
#' list of nmi lists of string representations of imputation trees per variable,
#' and list of nmi lists of vectors of variable importance per variable
#' @noRd
CART <- function(
  X,
  minbucket = 5,
  cp = 1e-04,
  nmi,
  verbose = TRUE
){
  ANYXMIS <- any(is.na(X))
  if(ANYXMIS){
    X <- sjlabelled::remove_all_labels(X)
    X <- as.data.frame(X)
    XOBS <- !is.na(X)
    XMIS <- is.na(X)
    xmisord <- names(sort(colSums(XMIS)))[sort(colSums(XMIS)) > 0]
    for(k in xmisord){
      X[XMIS[, k], k] <- sample(X[XOBS[, k], k], sum(XMIS[, k]), replace = TRUE)
    }
  } else {
    return(X)
  }

  itermcmc <- max(c(100, nmi * 4))
  burnin <- floor(itermcmc / 2)

  keep <- sample(x = burnin:itermcmc, size = nmi, replace = FALSE)

  out <- list(imp = list(), treeplot = list(), variable_importance = list(),
              indmis = as.data.frame(XMIS))
  i <- 1
  for (ii in 1:itermcmc) {
    res <- seqcart(X, xmisord, XOBS, XMIS, minbucket, cp)
    X <- res$dataimp
    treeplot <- res$treeplot
    variable_importance <- res$variable_importance
    if (ii %in% keep) {
      out$imp[[i]] <- X
      out$treeplot[[i]] <- treeplot
      out$variable_importance[[i]] <- variable_importance
      i <- i + 1
    }
    if (verbose) {
      message('\r', "Finished:", round(ii/itermcmc * 100), "%", appendLF = FALSE)
      flush.console()
    }
  }
  out

}

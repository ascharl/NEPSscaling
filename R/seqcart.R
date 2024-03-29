#' CART imputation algorithm
#'
#' @param dataimp data.frame; to be imputed
#' @param misord vector of variable names of variables to be imputed
#' @param INDOBS indicator matrix of observed data
#' @param INDMIS indicator matrix of missing data
#' @param control1 minimum number of observations per split
#' @param control2 minimum complexity parameter per split
#' @return list of imputed data set, plots of imputation tree structures,
#' variable importance for splits
#' @noRd

seqcart <- function(
  dataimp,
  misord,
  INDOBS,
  INDMIS,
  control1,
  control2
){

  treeplot <- list()
  variable_importance <- list()
  for (k in misord) {
    yobs <- dataimp[INDOBS[, k], k]
    Xobs <- dataimp[INDOBS[, k], !(names(dataimp) %in% c("ID_t", k)), drop = FALSE]
    Xmis <- dataimp[INDMIS[, k], !(names(dataimp) %in% c("ID_t", k)), drop = FALSE]
    rpartmethod <- ifelse(is.factor(yobs), "class", "anova")
    tree <- rpart::rpart(yobs ~., data = cbind(yobs, Xobs), method = rpartmethod,
      control = rpart::rpart.control(minbucket = control1, cp = control2))
    treeplot[[k]] <- create_tree_plot(tree, k)#, sapply(Xobs, is.factor))
    variable_importance[[k]] <- tree$variable.importance
    leafdonor <- floor(as.numeric(row.names(tree$frame[tree$where, ])))
    tree$frame$yval <- as.numeric(row.names(tree$frame))
    leafmis <- predict(tree, Xmis, "vector")
    dataimp[INDMIS[, k], k] <- sapply(leafmis, function(x){
      donorpool <- yobs[leafdonor == x]
      if (length(donorpool) == 1) {
        obs <- donorpool
      } else {
        di <- sort(runif(length(donorpool) - 1))
        obs <- sample(x = donorpool, size = 1, prob = (c(di, 1) - c(0, di)))
      }
      return(obs)
    })
  }
  list(dataimp = dataimp,
       treeplot = treeplot,
       variable_importance = variable_importance)

}

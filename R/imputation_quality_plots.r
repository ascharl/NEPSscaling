#' Plots for multiple imputation
#'
#' The plots show the distribution of the original and imputed data sets per
#' variable. The plots are analogous to the xyplot, densityplot, and stripplot
#' of the mice package.
#'
#' @param pv_obj pv_obj by plausible_values() containing at least the sublists
#'   "pv" with the imputed data sets and "indmis", the indicator matrix for
#'   missing variables
#' @param var character value denoting the variable against which to plot the
#'   distribution of the imputed variables
#' @param imputation indicates which imputation number to plot; defaults to
#'   "all"; otherwise, the imputations to plot are indicated by numeric vectors
#'   (e.g., 1, 2:4, c(1,5))
#'
#' @return ggplot2 objects
#' @export
xyplot <- function(pv_obj, var, imputation = "all") {
  dat <- reshape_data(pv_obj, imputation)
  dat <- remove_fully_observed(dat, var)
  y <- rep(dat[dat$variable == var, "value"], length(unique(dat$variable)) - 1)
  ggplot2::ggplot(
    data = dat %>% dplyr::filter(dat$variable != var),
    mapping = ggplot2::aes(x = .data$value,
                           y = y,
                           color = .data$missing)
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(.~.data$variable + .data$imputation,
                      scales = "free", ncol = 5,
                      labeller = ggplot2::label_both) +
  ggplot2::ggtitle(var)
}

#' @export
densityplot <- function(pv_obj, imputation = "all") {
  dat <- reshape_data(pv_obj, imputation)
  dat <- remove_fully_observed(dat)
  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(x = .data$value,
                           color = .data$missing
                           )
  ) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(.~.data$variable + .data$imputation,
                      scales = "free", ncol = 5,
                      labeller = ggplot2::label_both)
}

#' @export
stripplot <- function(pv_obj) {
  dat <- reshape_data(pv_obj)
  dat <- remove_fully_observed(dat)
  ggplot2::ggplot(
    data = dat,
    mapping = ggplot2::aes(x = .data$imputation,
                           y = .data$value,
                           color = .data$missing)
  ) +
  ggplot2::geom_jitter() +
  ggplot2::facet_wrap(.~.data$variable, scales = "free")
}

#' @noRd
reshape_data <- function(pv_obj, imputation = "all") {
  if (is.character(imputation) && imputation == "all") {
    imputation <- 1:length(pv_obj[["pv"]])
  }
  dat1 <- lapply(imputation, function(x) {
    pv_obj[["pv"]][[x]] %>%
      dplyr::select(!dplyr::contains("PV")) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(
        cols = !dplyr::contains("ID_t"),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(imputation = as.factor(x))
  })
  dat1 <- dplyr::bind_rows(dat1)
  dat2 <- pv_obj[["indmis"]] %>%
    dplyr::select(!dplyr::contains("PV")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric),
                  ID_t = pv_obj[["pv"]][[1]][["ID_t"]]) %>%
    tidyr::pivot_longer(
      cols = !dplyr::contains("ID_t"),
      names_to = "variable",
      values_to = "missing"
    )
  dat2$missing <- as.factor(ifelse(dat2$missing == 1, "imputed", "observed"))
  dat <- dplyr::left_join(dat2, dat1, by = c("ID_t", "variable"))
  as.data.frame(dat)
}


#' @noRd
remove_fully_observed <- function(dat, var = NULL) {
  if (is.null(var)) {
    variables <- unique(dat$variable)
  } else {
    variables <- unique(dat$variable)[which(unique(dat$variable) != var)]
  }
  for (i in variables) {
    sel <- which(dat$variable == i)
    if (all(dat$missing[sel] == "observed")) {
      dat <- dat[-sel, ]
    }
  }
  dat
}

#' Plots for estimated PVs
#'
#' The plots show the distribution of the estimated plausible values.
#'
#' @param pv_obj pv_obj by plausible_values() containing at least the sublist
#'   "pv" with the imputed data sets
#' @param var character value denoting the variable against which to plot the
#'   distribution of the estimated plausible values
#' @param imputation indicates which imputation number to plot; defaults to
#'   "all"; otherwise, the imputations to plot are indicated by numeric vectors
#'   (e.g., 1, 2:4, c(1,5))
#'
#' @return ggplot2 objects
#' @export
xyplot_pvs <- function(pv_obj, var, imputation = "all") {
  dat <- reshape_data_pvs(pv_obj, imputation)
  y <- rep(dat[dat$variable == var, "value"], length(pv_obj$wave))
  ggplot2::ggplot(
    data = dat %>% dplyr::filter(grepl("PV", dat$variable)),
    mapping = ggplot2::aes(x = .data$value,
                           y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(.~.data$variable + .data$imputation,
                        scales = "free", ncol = 5,
                        labeller = ggplot2::label_both) +
    ggplot2::ggtitle(var)
}

#' @export
densityplot_pvs <- function(pv_obj, imputation = "all") {
  dat <- reshape_data_pvs(pv_obj, imputation)
  ggplot2::ggplot(
    data = dat %>% dplyr::filter(grepl("PV", dat$variable)),
    mapping = ggplot2::aes(x = .data$value)
  ) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(.~.data$variable + .data$imputation,
                        scales = "free", ncol = 5,
                        labeller = ggplot2::label_both)
}

#' @noRd
reshape_data_pvs <- function(pv_obj, imputation = "all") {
  if (is.character(imputation) && imputation == "all") {
    imputation <- 1:length(pv_obj[["pv"]])
  }
  dat <- lapply(imputation, function(x) {
    pv_obj[["pv"]][[x]] %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      tidyr::pivot_longer(
        cols = !dplyr::contains("ID_t"),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(imputation = as.factor(x))
  })
  dat <- dplyr::bind_rows(dat)
  as.data.frame(dat)
}

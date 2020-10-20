#' Print information about NEPSscaling plausible values estimation
#'
#' @param object return object of function \code{NEPScaling::plausible_values()}
#' @param ... unused
#'
#' @importFrom vctrs vec_as_names
#' @export


summary.pv_obj <- function(object, ...) {
    pv_obj <- object
    print.pv_obj(pv_obj)

    cat("\nMean of Plausible Values: \n")
    print(round(get_posterior_means(pv_obj)$pv$total, 3))

    if (get_type(pv_obj = pv_obj) == "longitudinal") {
        cat("\nItem parameters: \n")
        new_items <- paste0("items_w", get_wave(pv_obj))
        new_xsi <- paste0("xsi_w", get_wave(pv_obj))
        item_pars <- get_item_difficulties(pv_obj) %>%
            purrr::map(.f = function(mat) {
                colnames(mat) <-
                    vctrs::vec_as_names(colnames(mat), repair = "unique",
                                        quiet = TRUE)
                mat}) %>%
            purrr::map(tibble::as_tibble, rownames = "items") %>%
            purrr::map(dplyr::rename, "pos" = "...1") %>%
            purrr::map2(.y = new_items, ~dplyr::rename(.x, !!.y := "items")) %>%
            purrr::map2(.y = new_xsi, ~dplyr::rename(.x, !!.y := "xsi")) %>%
            purrr::reduce(dplyr::full_join, by = "pos") %>%
            dplyr::select(-.data$pos) %>%
            dplyr::mutate_if(.predicate = is.numeric,
                             .funs = round, digits = 3) %>%
            as.data.frame()
        print(item_pars)

        cat("\nRegression Coefficients (per imputed data set): \n")
        for (i in get_regression_coefficients(pv_obj)) {
            print(round(i, 3))
        }
    } else {
        cat("\nItem parameters: \n")
        print(round(get_item_difficulties(pv_obj), 3))

        cat("\nRegression Coefficients: \n")
        print(round(get_regression_coefficients(pv_obj), 3))
    }


}
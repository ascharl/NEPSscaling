#' not reached missing values as proxy for processing time
#' @noRd

not_reached_as_proxy <- function(include_nr, longitudinal, data, SC, domain,
                                 wave, waves) {
  nr <- NULL
  if (include_nr) {
    if (longitudinal) {
      sel <- lapply(item_labels[[SC]][[domain]],
              function(it) {names(data) %in% it})
      # in the longitudinal case, missing test taking for later time points
      # causes problems in imputation, if include_nr = TRUE, bgdata = NULL,
      # thus, remove NAs from data
      nr <- data.frame(ID_t = data[["ID_t"]])
      for (s in seq(length(sel))) {
        nr[[paste0("items_not_reached", waves[s])]] <-
          rowSums(data[, sel[[s]]] == -94, na.rm = TRUE)
      }
      if (SC == "SC6" & domain == "RE") {
        nr[["items_not_reached_w3"]][is.na(data[["rea3_sc1u"]])] <- NA
        nr[["items_not_reached_w5"]][is.na(data[["rea5_sc1u"]])] <- NA
      }
      # ID_t always > 1 value -> ignore in check for constant nr values
      if (any(lapply(lapply(nr, unique), length) == 1)) {
        ind <- which(lapply(lapply(nr, unique), length) == 1)
        if (length(ind) == length(sel)) {
          include_nr <- FALSE
          nr <- NULL
          message(
            "The number of not-reached missing values is constant. ",
            "Thus, it is not considered in the background model."
          )
        } else {
          nr <- nr[, -ind]
          message(names(ind), " is constant. It is excluded from the ",
                  "background model.")
        }
      }
    } else {
      sel <- names(data) %in% item_labels[[SC]][[domain]][[wave]]
      nr <- data.frame(ID_t = data[["ID_t"]],
               items_not_reached = rowSums(data[, sel] == -94, na.rm = TRUE))
      if (length(unique(nr[["items_not_reached"]])) == 1) {
        include_nr <- FALSE
        nr <- NULL
        message(
          "The number of not-reached missing values is constant. ",
          "Thus, it is not considered in the background model."
        )
      }
    }
  }
  # set user-defined missings to NA
  data[data < -15] <- NA # assumption: WLEs this low are not to be expected
  list(nr = nr, data = data, include_nr = include_nr)
}
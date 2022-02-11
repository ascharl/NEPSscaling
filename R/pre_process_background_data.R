#' process background data: combine processing time proxy, add ID_ts if
#' existing in data (-> value of min_valid!), but not in bgdata
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#' @param data data.frame; xTargetCompetencies
#' @param include_nr logical; whether not-reached missings are to be used
#' as processing time proxies
#' @param items_not_reached data.frame; number of items not reached per person
#' @param min_valid numeric; minimum number of required responses per person
#'
#' @return list of original data, bgdata and ID_t (all data.frames with same
#' number of rows)
#' @noRd
pre_process_background_data <- function(bgdata, data, include_nr,
                                        items_not_reached, min_valid) {
  if (!is.null(bgdata)) {
    if (inherits(bgdata, "list") ) {
      for (i in 1:length(bgdata)) {
        res <- adjust_for_min_value(min_valid, bgdata[[i]], data)
        bgdata[[i]] <- res$bgdata
        data <- res$data
        bgdata[[i]] <- substitute_missing_persons_in_bgdata(bgdata[[i]], data)
        bgdata[[i]] <- remove_columns_without_data_from_bgdata(bgdata[[i]])
      }
    } else {
      res <- adjust_for_min_value(min_valid, bgdata, data)
      bgdata <- res$bgdata
      data <- res$data
      bgdata <- substitute_missing_persons_in_bgdata(bgdata, data)
      bgdata <- remove_columns_without_data_from_bgdata(bgdata)
    }
  }
  ID_t <- data[, "ID_t", drop = FALSE]
  if (inherits(bgdata, "list") ) {
    for (i in 1:length(bgdata)) {
      bgdata[[i]] <- add_not_reached_to_bgdata(bgdata[[i]], ID_t,
                                               items_not_reached, include_nr)
    }
  } else {
    bgdata <- add_not_reached_to_bgdata(bgdata, ID_t, items_not_reached,
                                        include_nr)
  }
  list(data = data, bgdata = bgdata, ID_t = ID_t)
}


#' remove test takers with less than min_valid valid responses OR add missing
#' test takers to response data if min_valid == 0
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#' @param data data.frame; xTargetCompetencies
#' @param min_valid numeric; minimum number of required responses per person
#'
#' @return list of original data, bgdata (all data.frames with same number of
#' rows -- either more if min_valid == 0 or less otherwise)
#' @noRd
adjust_for_min_value <- function(min_valid, bgdata, data) {
  if (min_valid > 0) {
    # keep only those with competence data in background data
    bgdata <- bgdata[bgdata[["ID_t"]] %in% data[["ID_t"]], ]
  } else {
    # append subjects in background data that did not take the competence tests
    data <- suppressWarnings(
      dplyr::bind_rows(data, bgdata[!(bgdata[["ID_t"]] %in% data[["ID_t"]]),
                                    "ID_t",
                                    drop = FALSE
      ])
    )
    data <- data[order(data[["ID_t"]]), ]
  }
  list(bgdata = bgdata, data = data)
}

#' add missing test takers to bgdata
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#' @param data data.frame; xTargetCompetencies
#'
#' @return updated bgdata data.frame
#' @noRd
substitute_missing_persons_in_bgdata <- function(bgdata, data) {
  # append subjects in response data who are not present in background data
  if (nrow(bgdata) < nrow(data)) {
    bgdata <- suppressWarnings(
      dplyr::bind_rows(
        bgdata,
        data[!(data[["ID_t"]] %in% bgdata[["ID_t"]]), "ID_t", drop = FALSE]
      )
    )
    bgdata <- bgdata[order(bgdata[["ID_t"]]), ]
  }
  bgdata
}

#' remove columns from bgdata if they are completely NA
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#'
#' @return updated bgdata data.frame
#' @noRd
remove_columns_without_data_from_bgdata <- function(bgdata) {
  if (any(colSums(!is.na(bgdata)) == 0)) {
    vars <- paste(names(bgdata)[which(colSums(!is.na(bgdata)) == 0)],
                  collapse = ", ")
    message(paste0("The variables ", vars, " in the background data did not",
                   " contain values for the sample. They were dropped."))
    bgdata <- bgdata[, colSums(!is.na(bgdata)) != 0, drop = FALSE]
  }
  bgdata
}

#' add not-reached proxy to bgdata
#'
#' @param bgdata data.frame or NULL; contains background data and ID_t
#' @param ID_t data.frame; contains only ID_t
#' @param include_nr logical; whether not-reached missings are to be used
#' as processing time proxies
#' @param items_not_reached data.frame; number of items not reached per person
#'
#' @return updated bgdata data.frame
#' @noRd
add_not_reached_to_bgdata <- function(bgdata, ID_t, items_not_reached,
                                      include_nr) {
  if (include_nr) {
    bgdata <- suppressWarnings(
      dplyr::left_join(if(is.null(bgdata)) {ID_t} else {bgdata},
                       items_not_reached, by = "ID_t"))
  }
  bgdata
}

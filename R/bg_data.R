#' Background variables for plausible values estimation (adult cohort example)
#'
#' The data set contains simulated variables age, gender, number of books at
#' home and migration (loosely based on NEPS SC6)
#'
#' @format A data frame with 3000 rows and 5 variables:
#' \describe{
#'   \item{ID_t}{target IDs}
#'   \item{age}{age of the subjects; split in younger (2) and older (1)}
#'   \item{gender}{gender of the subjects, 1 male, 2 female}
#'   \item{nbooks}{number of books at home; 1 at most, 2 more than 100}
#'   \item{migration}{migration background; 1 no, 2 yes}
#' }
"bgdata_sc6"

#' Background variables for plausible values estimation (school cohort example)
#'
#' The data set contains simulated variables German grade, gender, number of
#' books at home and migration (loosely based on NEPS SC3)
#'
#' @format A data frame with 3000 rows and 5 variables:
#' \describe{
#'   \item{ID_t}{target IDs}
#'   \item{Ger_grade}{grade in German class; from 1 (very good) to 6 (very poor/fail)}
#'   \item{gender}{gender of the subjects, 1 male, 2 female}
#'   \item{nbooks}{number of books at home; 1 at most, 2 more than 100}
#'   \item{migration}{migration background; 1 no, 2 yes}
#' }
"bgdata_sc3"


#' School IDs for plausible values estimation (school cohort example)
#'
#' The data set contains simulated variables for school IDs and student - school
#' mapping
#'
#' @format A data frame with 12000 rows and 3 variables:
#' \describe{
#'   \item{ID_t}{target IDs}
#'   \item{ID_i}{school IDs}
#'   \item{wave}{assessment wave}
#' }
"CohortProfile_sc3"

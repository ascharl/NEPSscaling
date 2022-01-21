#' Create waves variables
#'
#' @param longitudinal logical indicating type of estimation
#' @param SC starting cohort; string
#' @param domain competence domain; string
#' @param wave character string of wave ("wx"); important for cross-sectional
#' case
#'
#' @return character vector of assessment waves per SC and domain of form "_wx"
#' @noRd
create_waves_vars <- function(longitudinal, SC, domain, wave) {
  # create auxiliary waves variable for longitudinal estimation
  if (longitudinal) {
    if (SC == "SC6") {
      if (domain == "RE") {
        waves <- c("_w3", "_w5", "_w9")
      }
      if (domain == "MA") {
        waves <- c("_w3", "_w9")
      }
    }
    if (SC == "SC5" && domain %in% c("RE", "MA")) {
      waves <- c("_w1", "_w12")
    }
    if (SC == "SC4") {
      if (domain == "RE") {
        waves <- c("_w2", "_w7", "_w10")
      }
      if (domain == "MA") {
         waves <- c("_w1", "_w7", "_w10")
      }
      if (domain == "IC") {
        waves <- c("_w1", "_w7")
      }
      if (domain == "SC") {
        waves <- c("_w1", "_w5")
      }
      if (domain == "EF") {
        waves <- c("_w3", "_w7")
      }
    }
    if (SC == "SC3") {
      if (domain == "RE") {
        waves <- c("_w1", "_w3", "_w6", "_w9")
      }
      if (domain == "MA") {
          waves <- c("_w1", "_w3", "_w5", "_w9")
      }
      if (domain == "SC") {
          waves <- c("_w2", "_w5", "_w8")
      }
      if (domain == "IC") {
          waves <- c("_w2", "_w5", "_w9")
      }
      if (domain %in% c("NR", "NT")) {
          waves <- c("_w3", "_w6")
      }
      if (domain %in% c("ORA", "ORB")) {
          waves <- c("_w1", "_w3", "_w5")
      }
      if (domain == "EF") {
          waves <- c("_w7", "_w9")
      }
    }
    if (SC == "SC2") {
      if (domain == "VO") {
        waves <- c("_w1", "_w3", "_w5")
      } else if (domain == "GR") {
        waves <- c("_w1", "_w3")
      } else if (domain == "MA") {
        waves <- c("_w2", "_w3", "_w4", "_w6", "_w9")
      } else if (domain == "SC") {
        waves <- c("_w1", "_w3", "_w5")
      } else if (domain == "RE") {
        waves <- c("_w6", "_w9")
      } else if (domain == "GR") {
        waves <- c("_w1", "_w3")
      }
    }
    if (SC == "SC1") {
      if (domain == "MA") {
        waves <- c("_w5", "_w7")#, "_w9")
      } else if (domain == "SC") {
        waves <- c("_w6", "_w8")
      } else if (domain == "CD") {
        waves <- c("_w1")
      }
    }
  } else {
    waves <- paste0("_", wave)
  }
  waves
}

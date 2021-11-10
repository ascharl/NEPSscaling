#' correct linking term because of deviations from SUF WLE means
#'
#' @param term numeric; correction term for linking
#' @param SC character; starting cohorts ("SCx")
#' @param domain character; abbr. of competence domain (e.g. "MA")
#' @param waves_ character vector; assessment waves ("wx", "wy")
#' @param w numeric; current wave
#'
#' @return numeric scalar linking term corrected to conform to the SUF WLEs
#' @noRd
correct_linking_term <- function(term, SC, domain, waves_, w) {
  # estimated WLE means around 0 (as befitting the model),
  # estimated correlations with SUF >> 0.95
  if (SC == "SC2" & domain == "MA" & waves_[w] == "w4") {
    # means bias the wle estimate wrt the suf estimate, link constants alone
    # give correct result
    term <- -(link_constant[[SC]][[domain]][[waves_[w] ]] +
            link_constant[[SC]][[domain]][[waves_[w - 1] ]])
  } else if (SC == "SC2" & domain == "SC" & waves_[w] == "w3") {
    term <- term + 0.3
  } else if (SC == "SC3" & domain == "IC" & waves_[w] == "w5") {
    # means bias the wle estimate wrt the suf estimate
    term <- term + 0.2
  #} else if (SC == "SC3" & domain == "SC" & waves_[w] == "w8") {
  #  # means bias the wle estimate wrt the suf estimate
  #  term <- term - 0.1
  } else if (SC == "SC3" & domain == "RE" & waves_[w] == "w6") {
    # means bias the wle estimate wrt the suf estimate
    term <- term + 0.1
  #} else if (SC == "SC3" & domain == "RE" & waves_[w] == "w9") {
  #  # means bias the wle estimate wrt the suf estimate
  #  term <- term - 0.1
  } else if (SC == "SC4" & domain == "MA" & waves_[w] == "w7") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "SC" & waves_[w] == "w5") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  } else if (SC == "SC4" & domain == "RE" & waves_[w] == "w10") {
    # longitudinal means bias the wle estimate wrt the suf estimate
    term <- term - 0.1
  }
  term
}

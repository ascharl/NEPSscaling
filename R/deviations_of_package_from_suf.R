#' Get information about known deviations of the NEPSscaling WLE mean
#' estimation from SUF WLE mean estimation. The competence estimators of SUF
#' and package correlate to at least 0.95
#'
#' @export

deviations_of_package_from_suf <- function() {
cat(
"There are known deviations from the WLE mean estimations of the SUFs in
NEPSscaling (SUF - Package) despite high correlations of the estimates
(r > 0.95):

Starting Cohort 2:
- Grammar, wave 3 (cross-sectional): 1.701*
- Reading, wave 9 (cross-sectional): -0.368
- Mathematics, wave 9 (longitudinal): -0.239

Starting Cohort 3:
- Mathematics, wave 3 (cross-sectional): 0.722*
- Mathematics, wave 5 (longitudinal): 0.417**
- Mathematcis, wave 9 (longitudinal): -0.328**
- Reading, wave 3 (cross-sectional): 0.671*
- ICT, wave 5 (cross-sectional): 0.724*
- Science, wave 5 (cross-sectional): -0.151
- Orthography, wave 3 (longitudinal & cross-sectional): -0.100
- English as a foreign language, wave 9 (longitudinal): 0.102

Starting Cohort 4:
- ICT, wave 7 (cross-sectional): 0.634*
- Mathematics, wave 7 (cross-sectional): 0.134
- English as a foreign language, wave 9 (longitudinal): 0.137

*  In these cases, the cross-sectional SUF WLE estimates have also been shifted
   by the link constant.
** The link constant for wave 5 was calculated incorrectly and has been changed
   in the package, but not in the SUF. Subsequent waves still are affected by
   this shift.
"
)
}

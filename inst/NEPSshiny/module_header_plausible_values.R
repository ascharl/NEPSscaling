# Module: Plausible Values Header
# Information about plausible values
##########################################################################################################################################
## UI
##########################################################################################################################################
header_plausible_valuesUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
    column(12,
           tags$dl(
             tags$dt("Plausible Values"),
             tags$dd(
               tags$ul(
                 tags$li("Estimators for latent constructs such as competencies"),
                 tags$li("Set of random draws out of individual respondent's latent competence distribution"),
                 tags$li("Derived from competence test and respondent characteristics (e.g., gender, socio-economic status)"),
                 tags$li("Uncertainty in random draws reflects uncertainty in competence estimation"),
                 tags$li("Background variables should at least contain all variables used for later analysis"),
                 tags$li("Unbiased on a population level, but biased on the individual level because of respondent information (i.e., group-level information)"),
                 tags$li("Special case of multiple imputation: statistical analyses with plausible values have to be performed accordingly")
               )
             )

           ),
           tags$img(style="max-width: 500px; width: 40%; height: auto;",
                    src = "structural_model_pvs.png", alt = "Structural model"),
           tags$dl(
             tags$dt("Recommended Reading"),
             tags$dd(
               tags$ol(
                 tags$li("Scharl, A., Carstensen, C.H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6 (NEPS Survey Paper No. 71). Bamberg: Leibniz Institute for Educational Trajectories, National Educational Panel Study. doi:10.5157/NEPS:SP71:1.0"),
                 tags$li("von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible values and why are they useful. IERI Monograph Series, 2, 9–36."),
                 tags$li("Lüdtke, O., & Robitzsch, A. (2017). Eine Einführung in die Plausible-Values-Technik für die psychologische Forschung. Diagnostica, 63(3), 193–205. doi:10.1026/0012-1924/a000175"),
                 tags$li("Rubin, D. B. (1987). Multiple imputation for nonresponse in surveys. doi:10.1002/9780470316696"),
                 tags$li("Mislevy, R. J. (1991). Randomization-based inference about latent variables from complex samples. Psychometrika, 56(2), 177–196. doi:10.1007/BF02294457"),
                 tags$li("Meng, X.-L. (1994). Multiple-imputation inferences with uncongenial sources of input. Statistical Science, 538–558. doi:10.1214/ss/1177010269")
               )
             )
           )
    )
  ))
}

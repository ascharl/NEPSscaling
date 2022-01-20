# Module: Citation Header
# Information about citing the NEPSscaling package
##########################################################################################################################################
## UI
##########################################################################################################################################

header_citationUI <- function(id){
  ns <- NS(id)
  tagList(
fluidRow(
  column(8,
         tags$dl(
           tags$dt("Citing the NEPSscaling package"),
           tags$dd("Scharl, A., Carstensen, C. H., & Gnambs, T. (2020). Estimating Plausible Values with NEPS Data: An Example Using Reading Competence in Starting Cohort 6. NEPS Survey Papers. https://doi.org/10.5157/NEPS:SP71:1.0"
           )
         )
  )))
}

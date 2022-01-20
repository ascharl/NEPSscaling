# Module: NEPSscaling Header
# Information about the NEPSscaling package
##########################################################################################################################################
## UI
##########################################################################################################################################
header_NEPSscalingUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
    column(12,
         tags$dl(
           tags$dt("The NEPSscaling package"),
           tags$dd(
             tags$ul(
               tags$li("Helps NEPS data users to estimate plausible values for the major competence domains"),
               tags$li("The estimation by plausible_values() is based on the psychometric results described in the respective technical reports of the substudies."),
               tags$li("To further ensure comparability between the plausible values and the WLEs, any corrections of the WLEs (e.g., for sample dropout, changes in the booklet rotation design, or linking) are acknowledged by the function (see the respective technical reports for potential corrections applied)")
             )
           ))
  ))
)
}

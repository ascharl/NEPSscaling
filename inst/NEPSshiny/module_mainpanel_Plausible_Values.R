# Module mainpanel Plausible_Values
# Estimation of plausible values

#' @param specifying arguments for plausible values estimation
#' @param customizing output parameters
#' @param customizing model parameters
#' @return plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################
Plausible_Values_mainpanelUI <- function(id, values){
  ns <- NS(id)
  tagList(
  tabsetPanel(
    tabPanel("Estimate Plausible Values", value = 2,
             h3(textOutput("plausible_values_progress"))))
  )
}

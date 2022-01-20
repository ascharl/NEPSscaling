# Module mainpanel PV_obj
# displays already existing PV_obj or downloads the created PV_obj used to estimate plausible values

#' @param upload, download and remove background data
#' @return plausible values object to use for the estimation of plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################
PV_obj_mainpanelUI <- function(id){
  ns <- NS(id)
  tagList(
  tabPanel("Estimate Plausible Values", value = 2,
           h3(textOutput("plausible_values_progress")))
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
PV_obj_mainpanelServer <- function(id){
  moduleServer(id,
               function(input, output, session){})
}

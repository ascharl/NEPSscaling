# Module: Mainpanel of background data
# displays the background data used to estimate plausible values

#' @return background data ready to use for the estimation of plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################
background_data_mainpanelUI <- function(id){
  ns <- NS(id)
  tabPanel("Manage", value = 1,
           verbatimTextOutput(ns("summary")),
           dataTableOutput(ns("bgdata_display")))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
background_data_mainpanelServer <- function(id){
  moduleServer(id, function(input, output, session){
    output$bgdata_display <- renderDataTable(
      bgdata_display(),
      options = list(pageLength = 25)
    )
  })
}

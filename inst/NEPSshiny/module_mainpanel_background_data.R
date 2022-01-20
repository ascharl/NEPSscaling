# Module: Mainpanel of background data
# displays the background data used to estimate plausible values

#' @param values: consists of the reactive values pv_obj, bgdata_raw, bgdata, bgdata_display

#' @return background data ready to use for the estimation of plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################
background_data_mainpanelUI <- function(id){
  ns <- NS(id)
  tagList(
  tabPanel("Manage", value = 1,
           verbatimTextOutput(ns("summary")),
           dataTableOutput(ns("bgdata_display")),
           id = "Panel1")
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
# ben?tigt bgdata_display
background_data_mainpanelServer <- function(id, values){
  moduleServer(id, function(input, output, session){
    output$bgdata_display <- renderDataTable(
      # bgdata_display(),
      values$bgdata_display(),
      options = list(pageLength = 25)
    )
  })
}

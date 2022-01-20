# Module Variable Importance Plot
# Visualizing the variable importance plots for the imputations

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
variable_importance_plot_mainpanelUI <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel("Plots", value = 3,
             conditionalPanel(
               condition = "output.plots_conditional_visible==3",ns=ns,
               tags$h3("Variable importance plots"),
               plotOutput(ns("variable_importance_plot")),
               textInput(ns("variable_importance_name"), label = "Plot name",
                         value = paste0("variable_importance_",
                                        gsub(":", "-", gsub(" ", "_", Sys.time())))),
               selectInput(ns("variable_importance_format"),
                           label = "Select export format",
                           choices = c("png", "RData")),
               downloadButton(outputId = ns("download_variable_importance"),
                              label = "Download plot")
             ))
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
variable_importance_plot_mainpanelServer <- function(id, values){
  moduleServer(id,function(input, output, session){

output$download_variable_importance <- downloadHandler(
  filename = function() {
    req(input$variable_importance_name, input$variable_importance_format)
    ext <- switch(input$variable_importance_format,
                  "png" = ".png",
                  "RData" = ".RData")
    paste0(input$variable_importance_name, ext)
  },
  content = function(file) {
    if (input$variable_importance_format == "RData") {
      gplot <- variable_importance_plot()
      save(gplot, file = file)
    } else {
      ggplot2::ggsave(filename = file, plot = variable_importance_plot())
    }
  }
)

observeEvent(input$plots_variable_importance_state, {
  values$plots_conditional_visible <- 3
})

})
}

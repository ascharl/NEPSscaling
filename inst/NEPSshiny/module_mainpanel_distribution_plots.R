# Module Distributions Plot
# Visualizing plausible values and imputations

#' @param specifying plot type, variables to plot, title and axis lables, and plot themes
#' @return Distribution plot of plausible values and imputations

##########################################################################################################################################
## UI
##########################################################################################################################################
distribution_plots_mainpanelUI <- function(id){
  ns <- NS(id)
  tagList(
  tabsetPanel(
    tabPanel("Plots", value = 3,
             conditionalPanel(
               condition = "output.plots_conditional_visible==1",
               tags$h3("Distribution plots"),
               plotOutput(ns("plot")),
               textInput(ns("plot_name"), label = "Plot name",
                         value = paste0("plot_",
                                        gsub(":", "-", gsub(" ", "_", Sys.time())))),
               selectInput(ns("plot_format"),
                           label = "Select export format",
                           choices = c("png", "RData")),
               downloadButton(outputId = ns("download_plot"),
                              label = "Download plot")
             )))
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
# braucht imputations_display() aus der sidebar

distribution_plots_mainpanelServer <- function(id,values){
  moduleServer(function(input, output, session){

output$imputations_display <- renderDataTable(
  imputations_display(),
  options = list(pageLength = 50)
)

output$download_plot <- downloadHandler(
  filename = function() {
    req(input$plot_name, input$plot_format)
    ext <- switch(input$plot_format,
                  "png" = ".png",
                  "RData" = ".RData")
    paste0(input$plot_name, ext)
  },
  content = function(file) {
    if (input$plot_format == "RData") {
      gplot <- imputation_plot()
      save(gplot, file = file)
    } else {
      ggplot2::ggsave(filename = file, plot = imputation_plot())
    }
  }
)

observeEvent(input$plots_distribution_plot_state, {
  values$plots_conditional_visible <- 1
})

output$plots_conditional_visible <- renderText({
  values$plots_conditional_visible
})

outputOptions(output, "plots_conditional_visible", suspendWhenHidden = FALSE)
})
}

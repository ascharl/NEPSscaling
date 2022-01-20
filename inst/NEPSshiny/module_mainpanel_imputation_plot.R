# Module Imputation Plot
# Visualizing imputation tree structures

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
imputation_plot_mainpanelUI <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel("Plots", value = 3,
             conditionalPanel(
               conditionalPanel(
                 condition = 'output.plots_conditional_visible==2',
                 tags$h3("Imputation tree plots"),
                 plotOutput(ns("cart_plot")),
                 textInput(ns("cart_name"), label = "Plot name",
                           value = paste0("cart_",
                                          gsub(":", "-", gsub(" ", "_", Sys.time())))),
                 selectInput(ns("cart_format"),
                             label = "Select export format",
                             choices = c("png", "RData")),
                 downloadButton(outputId = ns("download_cart"),
                                label = "Download plot")
               )))
  )
}


##########################################################################################################################################
## Server
##########################################################################################################################################
imputation_plot_mainpanelServer <- function(id, values){
  moduleServer(id,function(input, output, session){

    output$download_cart <- downloadHandler(
      filename = function() {
        req(input$cart_name, input$cart_format)
        ext <- switch(input$cart_format,
                      "png" = ".png",
                      "RData" = ".RData")
        paste0(input$cart_name, ext)
      },
      content = function(file) {
        if (input$cart_format == "RData") {
          gplot <- cart_plot()
          save(gplot, file = file)
        } else {
          ggplot2::ggsave(filename = file, plot = cart_plot())
        }
      }
    )

    observeEvent(input$plots_tree_structure_state, {
      values$plots_conditional_visible <- 2
    })

  })
}


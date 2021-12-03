# Module Imputation Plot
# Visualizing imputation tree structures

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
imputation_plotUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.conditionedPanels==3", ns=ns,
          hr(),
          shinyWidgets::dropdownButton(
            inputId = ns("plots_tree_structure"),
            selectInput(inputId = ns("imputation"), label = "Select imputation",
                        choices = ""),
            selectInput(inputId = ns("variable"), label = "Select variable",
                        choices = ""),
            actionButton(inputId = ns("cart_plot"), label = "Display tree plot"),

            circle = FALSE, status = "block",
            width = "100%",
            label = "Imputation tree structures"
          ))),

    mainPanel(
      tabsetPanel(
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
    ))
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
imputation_plotServer <- function(id){
  moduleServer(id,function(input, output, session){

    cart_plot <- eventReactive(input$cart_plot, {
      req(values$pv_obj, input$imputation, input$variable)
      tryCatch(
        NEPSscaling::display_tree(values$pv_obj, input$imputation, input$variable),
        error = function(e) {
          showNotification(e$message, type = "error")
        }
      )
    })
    output$cart_plot <- renderPlot(cart_plot())

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


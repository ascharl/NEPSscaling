# Module Variable Importance Plot
# Visualizing the variable importance plots for the imputations

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
variable_importance_plotUI <- function(six){
  ns <- NS(six)
  tagList(
    sidebarLayout(
      sidebarPanel(
        hr(),
        shinyWidgets::dropdownButton(
          inputId = "plots_variable_importance",
          selectInput(inputId = "imputation_var_imp",
                      label = "Select imputation", choices = ""),
          selectInput(inputId = "variable_var_imp",
                      label = "Select variable", choices = ""),
          actionButton(inputId = "variable_importance_plot",
                       label = "Display variable importance plot"),

          circle = FALSE, status = "block",
          width = "100%",
          label = "Variable importance plots for imputations"
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Plots", value = 3,
                   conditionalPanel(
                     condition = "output.plots_conditional_visible==3",
                     tags$h3("Variable importance plots"),
                     plotOutput("variable_importance_plot"),
                     textInput("variable_importance_name", label = "Plot name",
                               value = paste0("variable_importance_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     selectInput("variable_importance_format",
                                 label = "Select export format",
                                 choices = c("png", "RData")),
                     downloadButton(outputId = "download_variable_importance",
                                    label = "Download plot")
                   ))
        )
      )
    ))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
variable_importance_plotServer <- function(six){
  moduleServer(six,function(input, output, session){

    variable_importance_plot <- eventReactive(input$variable_importance_plot, {
      req(values$pv_obj, input$imputation_var_imp, input$variable_var_imp)
      NEPSscaling::display_variable_importance(values$pv_obj,
                                               input$imputation_var_imp,
                                               input$variable_var_imp)
    })
    output$variable_importance_plot <- renderPlot(variable_importance_plot())

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

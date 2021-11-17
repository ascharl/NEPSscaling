# Module Descriptive tables for plausible values and imputations
# Gives out descriptive table for PV and imputations

#' @return descriptive table for PV and imputations


##########################################################################################################################################
## UI
##########################################################################################################################################

tablesUI <- function(seven){
  ns <- NS(seven)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.conditionedPanels==5",
          shinyWidgets::radioGroupButtons("checkGroup3",
                                          choices = list(
                                            "Descriptive tables for plausible values and imputations" = 1,
                                            "Descriptive tables for item parameters" = 2,
                                            "Regression weights" = 3
                                          ),
                                          direction = "vertical"#,
                                          # individual = TRUE
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tables", value = 5,
                   conditionalPanel(
                     condition = "input.checkGroup3==1",
                     tags$h3("Descriptive table"),
                     tableOutput("imputation_table"),
                     textInput("descriptive_name", label = "Table name",
                               value = paste0("descriptives_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = "download_descriptive",
                                    label = "Download Descriptives")
                   ),
                   conditionalPanel(
                     condition = "input.checkGroup3==2",
                     tags$h3("Item parameters"),
                     tableOutput("item_difficulties"),
                     textInput("difficulties_name", label = "Table name",
                               value = paste0("difficulties_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = "download_difficulties",
                                    label = "Download table")
                   ),
                   conditionalPanel(
                     condition = "input.checkGroup3==3",
                     tags$h3("Regression weights"),
                     tableOutput("regression_table"),
                     textInput("regression_name", label = "Table name",
                               value = paste0("regression_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = "download_regression",
                                    label = "Download table")                     ))

        )
      )
    ))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
tablesServer <- function(seven){
  moduleServer(seven,function(input, output, session){

    output$download_descriptive <- downloadHandler(
      filename = function() {
        req(input$descriptive_name)
        paste0(input$descriptive_name, ".tsv")
      },
      content = function(file) {
        write.table(x = imputation_table(), file = file, sep = "\t",
                    quote = FALSE, row.names = FALSE)
      }
    )

    output$download_difficulties <- downloadHandler(
      filename = function() {
        req(input$difficulties_name)
        paste0(input$difficulties_name, ".tsv")
      },
      content = function(file) {
        write.table(x = difficulties_table(), file = file, sep = "\t",
                    quote = FALSE, row.names = FALSE)
      }
    )

    output$download_regression <- downloadHandler(
      filename = function() {
        req(input$regression_name)
        paste0(input$regression_name, ".tsv")
      },
      content = function(file) {
        write.table(x = regression_table(), file = file, sep = "\t",
                    quote = FALSE, row.names = FALSE)
      }
    )

    observeEvent(input$plots_distribution_plot_state, {
      values$tables_conditional_visible <- 1
    })
    observeEvent(input$plots_tree_structure_state, {
      values$tables_conditional_visible <- 2
    })
    observeEvent(input$plots_variable_importance_state, {
      values$tables_conditional_visible <- 3
    })
    output$tables_conditional_visible <- renderText({
      values$tables_conditional_visible
    })
    outputOptions(output, "tables_conditional_visible", suspendWhenHidden = FALSE)
  })
}

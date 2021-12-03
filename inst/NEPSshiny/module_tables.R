# Module Descriptive tables for plausible values and imputations
# Gives out descriptive table for PV and imputations

#' @return descriptive table for PV and imputations


##########################################################################################################################################
## UI
##########################################################################################################################################

tablesUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.conditionedPanels==5",ns=ns,
          shinyWidgets::radioGroupButtons(ns("checkGroup3"),
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
                     condition = "input.checkGroup3==1",ns=ns,
                     tags$h3("Descriptive table"),
                     tableOutput(ns("imputation_table")),
                     textInput(ns("descriptive_name"), label = "Table name",
                               value = paste0("descriptives_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = ns("download_descriptive"),
                                    label = "Download Descriptives")
                   ),
                   conditionalPanel(
                     condition = "input.checkGroup3==2", ns=ns,
                     tags$h3("Item parameters"),
                     tableOutput(ns("item_difficulties")),
                     textInput(ns("difficulties_name"), label = "Table name",
                               value = paste0("difficulties_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = ns("download_difficulties"),
                                    label = "Download table")
                   ),
                   conditionalPanel(
                     condition = "input.checkGroup3==3", ns=ns,
                     tags$h3("Regression weights"),
                     tableOutput(ns("regression_table")),
                     textInput(ns("regression_name"), label = "Table name",
                               value = paste0("regression_",
                                              gsub(":", "-", gsub(" ", "_", Sys.time())))),
                     downloadButton(outputId = ns("download_regression"),
                                    label = "Download table")                     ))

        )
      )
    ))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
tablesServer <- function(id){
  moduleServer(id,function(input, output, session){

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

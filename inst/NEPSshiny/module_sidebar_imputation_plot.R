# Module Imputation Plot
# Visualizing imputation tree structures

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
imputation_plot_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
          conditionalPanel(
          condition = "input.conditionedPanels==3",
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
          ))
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
imputation_plot_sidebarServer <- function(id, values){
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
  })
}

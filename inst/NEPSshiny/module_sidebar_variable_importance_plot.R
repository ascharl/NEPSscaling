# Module Variable Importance Plot
# Visualizing the variable importance plots for the imputations

#' @param select imputation and variable
#' @return Tree plot of selected imputation/variable


##########################################################################################################################################
## UI
##########################################################################################################################################
variable_importance_plot_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
        hr(),
        shinyWidgets::dropdownButton(
          inputId = ns("plots_variable_importance"),
          selectInput(inputId = ns("imputation_var_imp"),
                      label = "Select imputation", choices = ""),
          selectInput(inputId = ns("variable_var_imp"),
                      label = "Select variable", choices = ""),
          actionButton(inputId = ns("variable_importance_plot"),
                       label = "Display variable importance plot"),

          circle = FALSE, status = "block",
          width = "100%",
          label = "Variable importance plots for imputations"
        )
  )
}



##########################################################################################################################################
## Server
##########################################################################################################################################
# benötigt values$pv_obj, input$imputation_var_imp, input$variable_var_imp

variable_importance_plot_sidebarServer <- function(id, values){
  moduleServer(id,function(input, output, session){

    variable_importance_plot <- eventReactive(input$variable_importance_plot, {
      req(values$pv_obj, input$imputation_var_imp, input$variable_var_imp)
      NEPSscaling::display_variable_importance(values$pv_obj,
                                               input$imputation_var_imp,
                                               input$variable_var_imp)
    })
    output$variable_importance_plot <- renderPlot(variable_importance_plot())



  })
}

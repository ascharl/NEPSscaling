# Module Distributions Plot
# Visualizing plausible values and imputations

#' @param specifying plot type, variables to plot, title and axis lables, and plot themes
#' @return Distribution plot of plausible values and imputations

##########################################################################################################################################
## UI
##########################################################################################################################################
distribution_plots_sidebarUI <- function(id){
  ns <- NS(id)
          conditionalPanel(
          condition = "input.conditionedPanels==3", ns=ns,
          shinyWidgets::dropdownButton(
            inputId = ns("plots_distribution_plot"),
            selectInput(inputId = ns("geom"), label = "Select plot type",
                        choices = c("Histogram", "Density plot", "Scatter plot")),
            selectInput(ns("x"), label = "Select variable on x-axis",
                        choices = ""),
            selectInput(ns("y"), label = "Select variable on y-axis",
                        choices = ""),
            selectInput(ns("fill"), label = "Select variable for color coding",
                        choices = ""),
            textInput(inputId = ns("title"), label = "Plot title"),
            textInput(inputId = ns("xlab"), label = "Label of x-axis"),
            textInput(inputId = ns("ylab"), label = "Label of y-axis"),
            selectInput(ns("theme"), label = "Select plot theme",
                        choices = c("Gray", "Black and white", "Linedraw", "Light",
                                    "Dark", "Minimal", "Classic", "Void")),
            actionButton(ns("plot"), label = "Display plot"),

            circle = FALSE, status = "block",
            width = "100%",
            label = "Plots for plausible values and imputations"
          ))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
distribution_plots_sidebarServer <- function(id){
  moduleServer(id,function(input, output, session){

# select columns, filter by values, select rows
# paged table
imputations_display <- reactive({
  req(average_pvs())
  out <- average_pvs()

  if (isTruthy(input$imputations_select_cols)) { # variables for selection have been chosen
    sel <- names(out)[names(out) %in% c("ID_t", input$imputations_select_cols)]
    out <- out[, sel, drop = FALSE]
  }

  # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
  if (isTruthy(input$imputations_filter_rows)) {
    for (f in input$imputations_filter_rows) { # swap f for input$imputations_filter_rows if no loop is required!
      filter_op <- stringr::str_extract(f, "[<>=!]+")
      filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
      filter_val <- sub(".*[ <>!=]", "", f)
      out <- filter_data(filter_op, filter_var, filter_val, out)
    }
  }

  if (isTruthy(input$imputations_sort_cases)) {
    if (input$imputations_ascending) {
      out <- dplyr::arrange(out, .data[[input$imputations_sort_cases]])
    } else {
      out <- dplyr::arrange(out, dplyr::desc(.data[[input$imputations_sort_cases]]))
    }
  }

  updateSelectInput(session = session, inputId = "imputations_sort_cases",
                    label = "Sort by", choices = names(out),
                    selected = "")

  out
})


})
}

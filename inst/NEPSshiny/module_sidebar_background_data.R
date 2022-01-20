#' Module: Sidebar of background data
#' manages the background data used to estimate plausible values
#' Consists of three subsections (dropdownButtons):Manage background data, Manage PV_obj, set scale levels of background data

#' @param values: consists of the reactive values pv_obj, bgdata_raw, bgdata, bgdata_display

#' @return bgdata ready to use for the estimation of plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################

background_data_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
  conditionalPanel(
          condition = "input.Panel1== 1",
          ns=ns,
          shinyWidgets::dropdownButton(
            inputId = ns("input_bgdata"),
            fileInput(inputId = ns("import_bgdata"),
                      label = tags$strong("Import background data"),
                      multiple = FALSE, accept = c(".rds", ".sav", ".dta")),
            tags$span(style = "font-size: 0.75em;",
                      "Upload size up to 30MB. Accepts '.rds', '.sav', and '.dta' formats."),

            hr(),

            actionButton(inputId = ns("remove_bgdata"),
                         label = "Remove background data"),

            hr(),

            actionButton(inputId = ns("Display_Bgdata"),
                         label = "Inspect background data"),
            tags$span(style = "font-size: 0.75em;",
                      "The changes affect the display of the background data only."),
            shinyjs::hidden(
              selectInput(ns("bgdata_select_cols"), "Select columns", choices = "",
                          multiple = TRUE),
              textInput(ns("bgdata_filter_rows"), "Filter", placeholder = "e.g., var1 == 1"),
              selectInput(ns("bgdata_sort_cases"), "Sort by", choices = ""),
              shinyWidgets::prettyCheckbox(
                inputId = ns("bgdata_ascending"), label = "Ascending",
                status = "primary", value = TRUE, shape = "curve", outline = TRUE)
            ),
            circle = FALSE, status = "block",
            width = "100%",
            label = "Manage background data"),
          hr(),
          shinyWidgets::dropdownButton(
            inputId = ns("scale_level")),
            shinyWidgets::prettyCheckbox(
              inputId = "metric", label = "All variables are metric.",
              status = "primary", value = FALSE, shape = "curve", outline = TRUE
            ),
            selectInput(inputId = ns("ordinal"), label = "Select ordinal variables",
                        choices = "No data uploaded yet", multiple = TRUE),
            selectInput(inputId = ns("nominal"), label = "Select nominal variables",
                        choices = "No data uploaded yet", multiple = TRUE),

            circle = FALSE, status = "block",
            width = "100%",
            label = "Set scale levels of background data")
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################

background_data_sidebarServer <- function(id, values){
  moduleServer(id, function(input, output, session){

    observeEvent(input$import_bgdata, {
      req(input$import_bgdata)
      filetype <- tools::file_ext(input$import_bgdata$datapath)

      out <- switch(
        filetype,
        rds = readRDS(file = input$import_bgdata$datapath),
        sav = haven::read_spss(file = input$import_bgdata$datapath),
        dta = haven::read_dta(file = input$import_bgdata$datapath),
        validate(paste0(
          "Format of bgdata (", filetype, ") not recognized.\n",
          "Needs: R object (.rds), SPSS (.sav) or Stata (.dta) format."
        ))
      )

      if (!is.data.frame(out)) {
        showNotification("bgdata must be a data.frame.", type = "error")
        return(NULL)
      }

      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables", choices = names(out))
      updateSelectInput(session = parent, inputId = "nominal",
                        label = "Select nominal variables", choices = names(out))

      updateSelectInput(session = session, inputId = "bgdata_select_cols",
                        label = "Select columns", choices = names(out),
                        selected = "")
      updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                        label = "Sort by", choices = names(out),
                        selected = "")

      values$bgdata_raw <- haven::zap_labels(out)
    })

    observe({
      req(values$bgdata_raw)
      nominal <- input$nominal
      ordinal <- input$ordinal
      out <- values$bgdata_raw

      if (!input$metric & is.null(nominal) & is.null(ordinal)) {
        showNotification("Please specify the scale levels of the data.",
                         type = "message")
        return(NULL)
      }

      if (!is.null(ordinal) | !is.null(nominal)) {
        sel <- unique(c(ordinal, nominal))
        if (length(sel) == 1) {
          out[[sel]] <- as.factor(out[[sel]])
        } else {
          out[, sel] <- lapply(out[, sel], as.factor)
        }
      }

      choices <- colnames(out[, -which(names(out) == "ID_t")])
      updateSelectInput(session = parent, inputId = "exclude1",
                        label = "Variables to exclude from bg data",
                        choices = choices, selected = "")
      updateSelectInput(session = parent, inputId = "exclude2",
                        label = "Variables to exclude (2nd wave)",
                        choices = choices, selected = "")
      updateSelectInput(session = parent, inputId = "exclude3",
                        label = "Variables to exclude (3rd wave)",
                        choices = choices, selected = "")
      updateSelectInput(session = parent, inputId = "exclude4",
                        label = "Variables to exclude (4th wave)",
                        choices = choices, selected = "")
      updateSelectInput(session = parent, inputId = "exclude5",
                        label = "Variables to exclude (5th wave)",
                        choices = choices, selected = "")

      values$bgdata <- out
    })
    observeEvent(input$Display_Bgdata, {
      shinyjs::toggle('bgdata_select_cols')
      shinyjs::toggle('bgdata_filter_rows')
      shinyjs::toggle('bgdata_sort_cases')
      shinyjs::toggle('bgdata_ascending')
      output$text <- renderText({"ahh you pressed it"})
    })


    values$bgdata_display <- reactive({
      req(values$bgdata)
      out <- values$bgdata

      if (isTruthy(input$bgdata_select_cols)) { # variables for selection have been chosen
        sel <- names(out)[names(out) %in% c("ID_t", input$bgdata_select_cols)]
        out <- out[, sel, drop = FALSE]
      }

      # bildet nur &, nicht | ab, wobei man | auch als & umformulieren kann
      if (isTruthy(input$bgdata_filter_rows)) {
        for (f in input$bgdata_filter_rows) { # swap f for input$bgdata_filter_rows if no loop is required!
          filter_op <- stringr::str_extract(f, "[<>=!]+")
          filter_var <- stringr::word(f, 1, sep = "[ <>!=]")
          filter_val <- sub(".*[ <>!=]", "", f)
          out <- filter_data(filter_op, filter_var, filter_val, out)
        }
      }

      if (isTruthy(input$bgdata_sort_cases)) {
        if (input$bgdata_ascending) {
          out <- dplyr::arrange(out, .data[[input$bgdata_sort_cases]])
        } else {
          out <- dplyr::arrange(out, dplyr::desc(.data[[input$bgdata_sort_cases]]))
        }
      }

      updateSelectInput(session = parent, inputId = "bgdata_sort_cases",
                        label = "Sort by", choices = names(out),
                        selected = "")

      out
    })

    observeEvent(input$remove_bgdata, {
      values$bgdata_raw <- values$bgdata <- NULL

      if (is.null(values$pv_obj)) {
        updateSelectInput(session = session, inputId = "ordinal",
                          label = "Select ordinal variables", choices = "")
        updateSelectInput(session = session, inputId = "nominal",
                          label = "Select nominal variables", choices = "")

        updateSelectInput(session = session, inputId = "bgdata_select_cols",
                          label = "Select columns", choices = "",
                          selected = "")
        updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                          label = "Sort by", choices = "",
                          selected = "")
        updateSelectInput(session = session, inputId = "exclude1",
                          label = "Variables to exclude from bg data",
                          choices = "", selected = "")
        updateSelectInput(session = session, inputId = "exclude2",
                          label = "Variables to exclude (2nd wave)",
                          choices = "", selected = "")
        updateSelectInput(session = session, inputId = "exclude3",
                          label = "Variables to exclude (3rd wave)",
                          choices = "", selected = "")
        updateSelectInput(session = session, inputId = "exclude4",
                          label = "Variables to exclude (4th wave)",
                          choices = "", selected = "")
        updateSelectInput(session = session, inputId = "exclude5",
                          label = "Variables to exclude (5th wave)",
                          choices = "", selected = "")
      }
    })



})
}

# Module PV_obj
# uploades already existing PV_obj or downloades the created PV_obj used to estimate plausible values

#' @param upload, downloade and remove background data
#' @return plausible values object to use for the estimation of plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################

PV_objUI <- function(one){
  ns <- NS(one)
  tagList(
  sidebarLayout(
    sidebarPanel(
      hr(),
      shinyWidgets::dropdownButton(

        tags$strong("Download pv_obj"),
        textInput(
          "pv_obj_name", label = "Choose file name",
          value = paste0("pv_obj_", gsub(":", "-", gsub(" ", "_", Sys.time())))
        ),
        downloadButton("download_pv_obj", label = "Download pv_obj (.rds)"),
        selectInput("export_format", label = "Select export format",
                    choices = c("SPSS", "Stata", "Mplus")),
        downloadButton("export_pv_obj", label = "Export pv_obj"),

        circle = FALSE, status = "block",
        width = "100%",
        label = "Manage pv_obj"
        ,
        hr(),
        inputId = "input_pv_obj",
        fileInput(inputId = "import_state",
                  label = tags$strong("Import pv_obj"),
                  multiple = FALSE, accept = ".rds"),
        tags$span(style = "font-size: 0.75em;",
                  "Upload size up to 30MB. Accepts '.rds' format."),

        hr(),

        actionButton(inputId = "remove_pv_obj", label =  "Remove pv_obj"))),
  mainPanel(tabsetPanel(
    tabPanel("Estimate Plausible Values", value = 2,
           h3(textOutput("plausible_values_progress")))))))
}

##########################################################################################################################################
## Server
##########################################################################################################################################
PV_objServer <- function(one){
moduleServer(one,function(input, output, session){
  observe({
    req(input$import_state)
    validate(need(tools::file_ext(input$import_state$datapath) == "rds",
                  "pv_obj must be stored as '.rds' file."))

    out <- readRDS(file = input$import_state$datapath)

    if (class(out) != "pv_obj") {
      showNotification("pv_obj must be of class 'pv_obj'.", type = "error")
    } else {
      values$pv_obj <- out
    }

    if (!input$metric & !isTruthy(input$nominal) & !isTruthy(input$ordinal)) {
      updateSelectInput(session = session, inputId = "ordinal",
                        label = "Select ordinal variables",
                        choices = names(out$pv[[1]]))
      updateSelectInput(session = session, inputId = "nominal",
                        label = "Select nominal variables",
                        choices = names(out$pv[[1]]))
      showNotification("Please specify the scale levels of the data under 'Manage'.",
                       type = "message")
    }

    updateSelectInput(session, inputId = "imputation",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "imputation_var_imp",
                      choices = names(out$treeplot),
                      selected = "")
    updateSelectInput(session, inputId = "variable_var_imp",
                      choices = names(out$treeplot[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "fill",
                      choices = unique(c(input$nominal, input$ordinal)),
                      selected = "")
    updateSelectInput(session, inputId = "x",
                      choices = names(out$pv[[1]]),
                      selected = "")
    updateSelectInput(session, inputId = "y",
                      choices = names(out$pv[[1]]),
                      selected = "")
  })
  output$download_pv_obj <- downloadHandler(
    filename = function() {
      req(input$pv_obj_name)
      paste0(input$pv_obj_name, ".rds")
    },
    content = function(file) {
      req(values$pv_obj)
      saveRDS(values$pv_obj, file = file)
    }
  )

  # --------------------------- EXPORT PV_OBJ --------------------------------
  # formats: spss, stata, mplus
  # https://stackoverflow.com/a/43939912
  output$export_pv_obj <- downloadHandler(
    filename = function() {
      rep(input$pv_obj_name)
      paste0(input$pv_obj_name, ".zip")
    },
    content = function(zipfile) {
      req(values$pv_obj, input$export_format)
      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      # vector of file names for pv_obj$pv data.frames
      files <- export_files(input$export_format, input$pv_obj_name)
      if (input$export_format == "SPSS") {
        for (i in seq(length(values$pv_obj$pv))) {
          haven::write_sav(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Stata") {
        for (i in seq(length(values$pv_obj$pv))) {
          colnames(values$pv_obj[["pv"]][[i]]) <-
            gsub("[[:punct:]]", "_", colnames(values$pv_obj[["pv"]][[i]]))
          haven::write_dta(values$pv_obj$pv[[i]], path = files[i])
        }
      } else if (input$export_format == "Mplus") {
        for (i in 1:length(values$pv_obj[["pv"]])) {
          write.table(values$pv_obj[["pv"]][[i]], file = files[i],
                      dec = ".", sep = ",", row.names = FALSE)
        }
        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files) - 1])
        write(names(values$pv_obj[["pv"]][[1]]), file = "variable_names.txt")

        write(x = paste0(files[-length(files)], collapse = "\n"),
              file = files[length(files)])
      }

      zip(zipfile = zipfile, files = files)
    },
    contentType = "application/zip"
  )
})
}

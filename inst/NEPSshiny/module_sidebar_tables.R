# Module Descriptive tables for plausible values and imputations
# Gives out descriptive table for PV and imputations

#' @return descriptive table for PV and imputations


##########################################################################################################################################
## UI
##########################################################################################################################################

tables_sidebarUI <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################


tables_sidebarServer <- function(id, values){
  moduleServer(id,function(input, output, session){

    # --------------------------- CREATE REGRESSION TABLES ---------------------
     regression_table <- reactive({
      req(values$pv_obj)
      tmp <- NEPSscaling::get_regression_coefficients(values$pv_obj)
      if (NEPSscaling::get_type(values$pv_obj) == "longitudinal") {
        Variable <- tmp[[1]]$Variable
        tmp <- lapply(tmp, function(x) x[,-1]) %>%
          purrr::reduce(`+`) / length(tmp)
        tab <- data.frame(
          Variable = paste(Variable, "Wave",
                           rep(NEPSscaling::get_wave(values$pv_obj), each = nrow(tmp))),
          N = as.character(rep(NEPSscaling::get_n_testtakers(values$pv_obj), each = nrow(tmp))),
          b = unname(unlist(tmp[, seq(1, ncol(tmp), 3)])),
          beta = unname(unlist(tmp[, seq(2, ncol(tmp), 3)])),
          se = unname(unlist(tmp[, seq(3, ncol(tmp), 3)]))
        )
      } else {
        tab <- data.frame(
          Variable = tmp$Variable,
          N = as.character(NEPSscaling::get_n_testtakers(values$pv_obj)),
          b = rowMeans(as.matrix(tmp[, grepl("_coeff$", names(tmp))])),
          beta = rowMeans(as.matrix(tmp[, grepl("_std$", names(tmp))])),
          se = rowMeans(as.matrix(tmp[, grepl("_se$", names(tmp))]))
        )
      }
      tab[["95% CI of b"]] <- paste0("[", round(tab$b - 1.96 * tab$se, 3),"; ",
                                     round(tab$b + 1.96 * tab$se, 3), "]")
      tab$b <- as.character(round(tab$b, 3))
      tab$beta <- as.character(round(tab$beta, 3))
      tab$se <- as.character(round(tab$se, 3))
      tab
    })
    output$regression_table <- renderTable({
      regression_table()
    },
    caption = "Latent Regression Weights with 95% CI based on normal distribution",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
    )

    # --------------------------- CREATE SUMMARY STATISTICS --------------------
    imputation_table <- reactive({
      req(average_pvs())
      out <- psych::describe(average_pvs())
      out$vars <- rownames(out)
      out
    })
    output$imputation_table <- renderTable({
      imputation_table()
    },
    caption = "Descriptive Statistics of Average Imputed Data Sets. * Factor variables.",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
    )

    observeEvent(input$remove_pv_obj, {
      values$pv_obj <- NULL

      updateSelectInput(session, inputId = "imputation",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "variable",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "imputation_var_imp",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "variable_var_imp",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "fill",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "x",
                        choices = "",
                        selected = "")
      updateSelectInput(session, inputId = "y",
                        choices = "",
                        selected = "")

      if (is.null(values$bgdata_raw)) {
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


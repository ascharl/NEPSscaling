# Module Plausible_Values
# Estimation of plausible values

#' @param specifying arguments for plausible values estimation
#' @param customizing output parameters
#' @param customizing model parameters
#' @return plausible values

##########################################################################################################################################
## UI
##########################################################################################################################################
Plausible_ValuesUI <- function(three){
  ns <- NS(three)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.conditionedPanels==2",
          h4("Arguments for Plausible Values Estimation"),
          selectInput("select_starting_cohort",
                      label = "Starting cohort",
                      choices = 1:6,
                      selected = ""
          ),
          selectInput("select_domain",
                      label = "Competence domain",
                      choices = c(
                        "Mathematics" = "MA", "Reading" = "RE", "Science" = "SC",
                        "Information and Communication Technology" = "IC",
                        "Listening Comprehension" = "LI",
                        "English as a Foreign Language" = "EF",
                        "Native Russian" = "NR", "Native Turkish" = "NT",
                        "Orthography A" = "ORA","Orthography B" = "ORB",
                        "Scientific Thinking" = "ST", "Business Administration" = "BA",
                        "Cognitive Development" = "CD", "Grammar" = "GR",
                        "Vocabulary" = "VO"
                      ),
                      selected = ""
          ),
          selectInput("select_wave",
                      label = "Assessment wave",
                      choices = 1:12,
                      selected = ""
          ),
          textInput(inputId = "path_to_data", label = "Directory with competence data (SUFs)",
                    value = getwd()),

          shinyWidgets::dropdownButton(
            inputId = "output_parameters",
            numericInput("npv", label = "Number of plausible values",
                         value = 10, min = 1),
            numericInput("nmi", label = "Number of imputations",
                         value = 10, min = 1),
            shinyWidgets::prettyCheckbox(
              inputId = "WLE", label = "Return WLEs?",
              status = "primary", value = FALSE, shape = "curve", outline = TRUE
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "EAP", label = "Return EAPs?",
              status = "primary", value = FALSE, shape = "curve", outline = TRUE
            ),

            circle = FALSE, status = "block",
            width = "100%",
            label = "Customize output parameters"
          ),
          tags$hr(),
          shinyWidgets::dropdownButton(
            inputId = "model_parameters",
            shinyWidgets::prettyCheckbox(
              inputId = "longitudinal", label = "Use of longitudinal competence tests?",
              status = "primary", value = FALSE, shape = "curve", outline = TRUE
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "rotation",
              label = tags$text("Include position of", #br(),
                                "competence test?"),
              status = "primary", value = TRUE, shape = "curve", outline = TRUE
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "adjust_school_context",
              label = tags$text("Include proxy for school", #br(),
                                "context?"),
              status = "primary", value = TRUE, shape = "curve", outline = TRUE
            ),
            shinyWidgets::prettyCheckbox(
              inputId = "include_nr",
              label = tags$text("Include proxy for processing", #br(),
                                "speed?"),
              status = "primary", value = TRUE, shape = "curve", outline = TRUE
            ),
            numericInput("min_valid",
                         label = "Minimum number of valid answers to competence test(s)",
                         value = 3, min = 0),
            numericInput("seed",
                         label = "Seed for random number generator",
                         value = sample(0:100000, 1),
                         min = 0),
            selectInput(inputId = "exclude1", label = "Variables to exclude from bg data",
                        choices = "", multiple = TRUE),
            shinyjs::hidden(
              selectInput(inputId = "exclude2",
                          label = "Variables to exclude (2nd wave)",
                          choices = "", multiple = TRUE),
              selectInput(inputId = "exclude3",
                          label = "Variables to exclude (3rd wave)",
                          choices = "", multiple = TRUE),
              selectInput(inputId = "exclude4",
                          label = "Variables to exclude (4th wave)",
                          choices = "", multiple = TRUE),
              selectInput(inputId = "exclude5",
                          label = "Variables to exclude (5th wave)",
                          choices = "", multiple = TRUE)
            ),

            circle = FALSE, status = "block",
            width = "100%",
            label = "Customize model parameters"
          ),
          tags$hr(),
          shinyWidgets::prettyCheckbox(
            inputId = "verbose", label = "Progress reports?",
            status = "primary", value = TRUE, shape = "curve", outline = TRUE
          ),
          # other controls: not changeable!,
          hr(),
          actionButton("estimate_pv_obj", label = "Start estimation")
        )),
      mainPanel(
        tabsetPanel(
          tabPanel("Estimate Plausible Values", value = 2,
                   h3(textOutput("plausible_values_progress")))
        ))
      )
  )
}

##########################################################################################################################################
## Server
##########################################################################################################################################
Plausible_ValuesServer <- function(three){
  moduleServer(three,function(input, output, session){
observeEvent(input$estimate_pv_obj, {

  req(
    values$bgdata, input$select_starting_cohort, input$select_domain,
    input$select_wave, input$path_to_data
  )

  exclude <- NULL
  if (isTruthy(input$longitudinal) & input$longitudinal) {
    exclude <- list(
      input$exclude1, input$exclude2, input$exclude3, input$exclude4,
      input$exclude5
    )
    names(exclude) <- gsub("_", "",
                           NEPSscaling:::create_waves_vars(
                             longitudinal = input$longitudinal,
                             SC = paste0("SC", input$select_starting_cohort),
                             domain = input$select_domain, wave = NULL
                           ))
  } else if (isTruthy(input$longitudinal) & !input$longitudinal) {
    exclude <- input$exclude1
  }

  # print output to shiny to monitor progress:
  # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#30490698
  withCallingHandlers(
    {
      shinyjs::html("plausible_values_progress", "")
      out <- NEPSscaling::plausible_values(
        SC = as.numeric(input$select_starting_cohort),
        domain = input$select_domain,
        wave = as.numeric(input$select_wave),
        path = gsub("\\\\", "/", input$path_to_data),
        bgdata = values$bgdata,
        npv = as.numeric(input$npv),
        longitudinal = input$longitudinal,
        rotation = input$rotation,
        min_valid = as.numeric(input$min_valid),
        include_nr = input$include_nr,
        verbose = input$verbose,
        adjust_school_context = input$adjust_school_context,
        exclude = exclude,
        seed = input$seed,
        control = list(WLE = input$WLE, EAP = input$EAP,
                       ML = list(nmi = input$nmi))
      )
    },
    message = function(m) {
      shinyjs::html(id = "plausible_values_progress", html = m$message)
    }#,
    # error = function(e) print(sys.calls())
  )

  values$pv_obj <- out

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
  })
}

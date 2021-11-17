library(shiny)
library(xtable)


source("module_background_data.R")
source("module_PV_obj.R")
source("module_Plausible_Values.R")
source("module_distribution_plots.R")
source("module_imputation_plot.R")
source("module_variable_importance_plot.R")
source("module_tables.R")

ui <- shinyUI(
  navbarPage(id = "navbar",
        theme = bslib::bs_theme(
        bg = "#E8E6F4", fg = "black", primary = "#24195D", secondary = "#1D0E46",
        "font-size-base" = "1.5rem",
        base_font = bslib::font_google("Open Sans"),
        code_font = bslib::font_google("Open Sans")
        ),
        tabPanel(fluidRow(
          column(2, offset = 1, img(height = 50, width = 100, src = "NEPSscaling_Logo_3.png")))
          ,
          tags$head(
            tags$style(
              HTML(
                ".shiny-notification {
             position: fixed;
             top: calc(50%);
             left: calc(50%);
          }
          .btn-block {
             display: block;
             width: 100%;
             color: #E8E6F4;
             background-color: #24195D;
             word-break: break-all;
             word-wrap: break-word;
             white-space: normal;
          }
          .btn-block:hover {
             color: #E8E6F4;
             background-color: #24195D;
          }
          .btn-group-container-sw {
             display: flex;
          }
          .radiobtn {
             margin-top: 30px;
             margin-bottom: 30px;
             flex: 1;
             color: #E8E6F4;
             background-color: #24195D;
          }
          .radiobtn:hover {
             color: #E8E6F4;
             background-color: #24195D;
          }"
              )
            )
          )),
          # print output to shiny:
          # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
          shinyjs::useShinyjs(),
  tabPanel(background_dataUI("one")),
  tabPanel(PV_objUI("two")),
  tabPanel(Plausible_ValuesUI("three")),
  tabPanel(distribution_plotsUI("four")),
  tabPanel(imputation_plotUI("five")),
  tabPanel(variable_importance_plotUI("six")),
  tabPanel(tablesUI("seven"))))

server <- function(input, output){
  callModule(background_data, "one")
  callModule(PV_obj, "two")
  callModule(Plausible_Values, "three")
  callModule(distribution_plots, "four")
  callModule(imputation_plot, "five")
  callModule(variable_importance_plot, "six")
  callModule(tables, "seven")
}

shinyApp(ui, server)


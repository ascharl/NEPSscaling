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
             tabsetPanel(
             shinyjs::useShinyjs(),
             tabPanel(background_dataUI("tab1"))),
             tabPanel(PV_objUI("tab2")),
             tabPanel(Plausible_ValuesUI("tab3")),
             tabPanel(distribution_plotsUI("tab4")),
             tabPanel(imputation_plotUI("tab5")),
             tabPanel(variable_importance_plotUI("tab6")),
             tabPanel(tablesUI("tab7"))),

             header= fluidRow(
               column(2, offset = 1, img(height = 50, width = 100, src = "NEPSscaling_Logo_3.png"))
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
                   )))),
             theme = bslib::bs_theme(
               bg = "#E8E6F4", fg = "black", primary = "#24195D", secondary = "#1D0E46",
               "font-size-base" = "1.5rem",
               base_font = bslib::font_google("Open Sans"),
               code_font = bslib::font_google("Open Sans")
             )
  )
)


server <- function(input, output, session){
  moduleServer(Server_background_data, "tab1")
  moduleServer(PV_objServer,"tab2")
  moduleServer(Plausible_ValuesServer, "tab3")
  moduleServer(distribution_plotsServer, "tab4")
  moduleServer(imputation_plotServer, "tab5")
  moduleServer(variable_importance_plotServer, "tab6")
  moduleServer(tablesServer,"tab7")
}

shinyApp(ui = ui, server = server)

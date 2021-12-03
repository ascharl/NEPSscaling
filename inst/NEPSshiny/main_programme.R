library(shiny)
library(xtable)


source("module_sidebar_background_data.R")
source("module_mainpanel_background_data.R")
source("module_sidebar_PV_obj.R")
source("module_mainpanel_PV_obj.R")
source("module_sidebar_Plausible_Values.R")
source("module_mainpanel_Plausible_Values.R")
source("module_sidebar_distribution_plots.R")
source("module_mainpanel_distribution_plots.R")
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
             tabPanel(
               fluidRow(
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
               ),
               # print output to shiny:
               # https://stackoverflow.com/questions/30474538/possible-to-show-console-messages-written-with-message-in-a-shiny-ui/30490698#3049069844446666666
               shinyjs::useShinyjs(),

               ## ---------------------------------sidebar----------------------------------
               sidebarLayout(
                 sidebarPanel(
             tabsetPanel(
             shinyjs::useShinyjs(),
             tabPanel(background_data_sidebarUI("tab1")),
             tabPanel(PV_obj_sidebarUI("tab2")),
             tabPanel(Plausible_Values_sidebarUI("tab3"))),
             ),

             ## --------------------------------Main Panel-----------------------------------------------------------
             mainPanel(
               tabsetPanel(
                   tabPanel(background_data_mainpanelUI("main1")),
                   tabPanel(PV_obj_mainpanelUI("main2")),
                   tabPanel(Plausible_Values_mainpanelUI("main3")))
             )),


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
))


server <- function(input, output, session){
  moduleServer(background_data_sidebarServer, "tab1")
  moduleServer(background_data_mainpanelServer,"main1")
  moduleServer(PV_obj_sidebarServer,"tab2")
  moduleServer(PV_obj_mainpanelServer,"main2")
  moduleServer(Plausible_Values_sidebarServer, "tab3")
  #moduleServer(Plausible_Values_mainpanelServer, "main3")
  #moduleServer(distribution_plotsServer, "tab4")
  #moduleServer(imputation_plotServer, "tab5")
  #moduleServer(variable_importance_plotServer, "tab6")
  #moduleServer(tablesServer,"tab7")
}

shinyApp(ui = ui, server = server)

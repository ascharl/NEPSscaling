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

source("module_sidebar_imputation_plot.R")
source("module_mainpanel_imputation_plot.R")

source("module_sidebar_variable_importance_plot.R")
source("module_mainpanel_variable_importance_plot.R")

source("module_sidebar_tables.R")
source("module_mainpanel_tables.R")

source("module_header_NEPSscaling.R")
source("module_header_citation.R")
source("module_header_contact.R")
source("module_header_plausible_values.R")
source("module_header_CART.R")
source("module_header_NEPS.R")
source("module_header_LIfBi.R")

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
             tabPanel(background_data_sidebarUI("tab1")))
             #tabPanel(PV_obj_sidebarUI("tab2")),
             #tabPanel(Plausible_Values_sidebarUI("tab3")),
             #tabPanel(distribution_plots_sidebarUI("tab4")),
             #tabPanel(imputation_plot_sidebarUI("tab5")),
             #tabPanel(variable_importance_plot_sidebarUI("tab6")),
             #tabPanel(tables_sidebarUI("tab7")))
             ),

             ## --------------------------------Main Panel-----------------------------------------------------------
          mainPanel(
            tabsetPanel(
              tabPanel(background_data_mainpanelUI("main1")))
              #tabPanel(PV_obj_mainpanelUI("main2")),
              #tabPanel(Plausible_Values_mainpanelUI("main3")),
              #tabPanel(distribution_plots_mainpanelUI("main4")),
              #tabPanel(imputation_plot_mainpanelUI("main5")),
              #tabPanel(variable_importance_plot_sidebarUI("main6")),
              #tabPanel(tables_mainpanelUI("main7")))
             )
          )
            ),#))

             ## ------------------------------Header-----------------------------------------------------------------
               tags$i(class = "far fa-question-circle", style="font-size: 36px"),
               tabPanel(header_NEPSscalingUI("head1")),
               tabPanel(header_citationUI("head2")),
               tabPanel(header_contactUI("head3")),
               tabPanel(header_plausible_valuesUI("head4")),
               tabPanel(header_CARTUI("head5")),
               tabPanel(header_NEPSUI("head6")),
               tabPanel(header_LIfBiUI("head7"))
))





filter_data <- function(filter_op, filter_var, filter_val, out) {
  switch(filter_op,
         "<" = dplyr::filter(out, .data[[filter_var]] < filter_val),
         ">" = dplyr::filter(out, .data[[filter_var]] > filter_val),
         "<=" = dplyr::filter(out, .data[[filter_var]] <= filter_val),
         ">=" = dplyr::filter(out, .data[[filter_var]] >= filter_val),
         "==" = dplyr::filter(out, .data[[filter_var]] == filter_val),
         "!=" = dplyr::filter(out, .data[[filter_var]] != filter_val),
         showNotification(paste("Filter operator", filter_op, "not valid."),
                          type = "error")
  )
}


server <- function(input, output, session){
  session$onSessionEnded(function() {
    stopApp()
  })

  values <- reactiveValues(
    pv_obj = NULL,
    bgdata_raw = NULL,
    bgdata = NULL,
    bgdata_display = NULL
  )

  background_data_sidebarServer("tab1", values = values )
  background_data_mainpanelServer("main1", values = values)
  #PV_obj_sidebarServer("tab2")
  #PV_obj_mainpanelServer("main2")
  #Plausible_Values_sidebarServer("tab3")
  #Plausible_Values_mainpanelServer("main3")
  #distribution_plots_sidebarServer("tab4")
  #distribution_plots_mainpanelServer("main4")
  #imputation_plot_sidebarServer("tab5")
  #imputation_plot_mainpanelServer("main5")
  #variable_importance_plot_sidebarServer("tab6")
  #variable_importance_plot_mainpanelServer("main6")
  #tables_sidebarServer("tab7")
  #tables_mainpanelServer("main7")
}

shinyApp(ui = ui, server = server)

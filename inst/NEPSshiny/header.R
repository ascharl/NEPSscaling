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
           shinyjs::useShinyjs())

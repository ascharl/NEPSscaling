# Module: NEPS Header
# Information about the NEPS
##########################################################################################################################################
## UI
##########################################################################################################################################
header_NEPSUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(column(1, offset = 1, img(height = 40, width = 90, src = "NEPS_reduziert_RGB_v01.png"))),
  tabPanel("National Educational Panel Study",
         fluidRow(
           column(12,
                  tags$dl(
                    tags$dt("For information about the NEPS, visit the website"),
                    tags$dd(
                      tags$ul(
                        tags$a(href="https://www.neps-data.de/",
                               "German"),
                        tags$a(href="https://www.neps-data.de/Mainpage",
                               "English")
                      ))))))
  )
}

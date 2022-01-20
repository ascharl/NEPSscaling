# Module: LIfBi Header
# Information about the LIfBi
##########################################################################################################################################
## UI
##########################################################################################################################################
header_LIfBiUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(column(1, offset = 5, img(height = 40, width = 45, src = "LIfBi_Logo_solo_RZ.png"))),
  tabPanel("Leibnitz Institute for Educational Trajectories",
         fluidRow(
           column(12,
                  tags$dl(
                    tags$dt("For information about the LIfBi, visit the website"),
                    tags$dd(
                      tags$ul(
                        tags$a(href="https://www.lifbi.de/",
                               "German"),
                        tags$a(href="https://www.lifbi.de/LIfBi-Home",
                               "English")
                        )
                    )))))
  )
}

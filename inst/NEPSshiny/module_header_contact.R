# Module: Contact Header
# Contact information
##########################################################################################################################################
## UI
##########################################################################################################################################
header_contactUI <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
  column(8,
         HTML("If you have any questions or comments regarding NEPS<em>scaling</em>, please contact one of the following:"),
         tags$ul(
           tags$li("The project", tags$a(href="mailto:skalierung(at)lifbi.de", tags$strong("Scaling and Test Design")), " of the LIfBi, responsible for developing ", HTML("NEPS<em>scaling</em>")),
           tags$li("The", tags$a(href="https://forum.lifbi.de/", tags$strong("NEPSforum")), " for general questions regarding the edition, dissemination and use of NEPS data"),
           tags$li("The", tags$a(href="https://www.neps-data.de/Data-Center/Contact-Data-Center", tags$strong("Research Data Center")), " for further information on the NEPS data")
         )
  )))
}

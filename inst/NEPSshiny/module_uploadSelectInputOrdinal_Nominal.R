#' Module To update input depending on the selected scale level 

UI

UpdateSelectInputOrdinal_NominalServer <- function(id, values){
  moduleServer(id, function(input, output, session){

updateSelectInput(session = session, inputId = "ordinal",
                  label = "Select ordinal variables", choices = names(out))
updateSelectInput(session = parent, inputId = "nominal",
                  label = "Select nominal variables", choices = names(out))
})
}
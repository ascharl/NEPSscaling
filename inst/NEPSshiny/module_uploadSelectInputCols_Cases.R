#' Module to update input selecting coloumns or sorting cases


UI

UpdateSelectInputCols_CasesServer <- function(id, values){
  moduleServer(id, function(input, output, session){
updateSelectInput(session = session, inputId = "bgdata_select_cols",
                  label = "Select columns", choices = names(out),
                  selected = "")
updateSelectInput(session = session, inputId = "bgdata_sort_cases",
                  label = "Sort by", choices = names(out),
                  selected = "")
})
}
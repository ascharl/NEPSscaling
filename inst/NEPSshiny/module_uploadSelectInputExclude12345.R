# Exclude variables from bgdata 

UpdateSelectInputExclude12345Server <- function(id, values){
  moduleServer(id, function(input, output, session){
updateSelectInput(session = parent, inputId = "exclude1",
                  label = "Variables to exclude from bg data",
                  choices = choices, selected = "")
updateSelectInput(session = parent, inputId = "exclude2",
                  label = "Variables to exclude (2nd wave)",
                  choices = choices, selected = "")
updateSelectInput(session = parent, inputId = "exclude3",
                  label = "Variables to exclude (3rd wave)",
                  choices = choices, selected = "")
updateSelectInput(session = parent, inputId = "exclude4",
                  label = "Variables to exclude (4th wave)",
                  choices = choices, selected = "")
updateSelectInput(session = parent, inputId = "exclude5",
                  label = "Variables to exclude (5th wave)",
                  choices = choices, selected = "")
  })
  }
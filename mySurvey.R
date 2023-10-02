surveyUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::includeHTML("survey.html")
  )
}

surveyServer <- function(input, output, session) {
  # You can handle server logic here. For now, it's left empty.
}

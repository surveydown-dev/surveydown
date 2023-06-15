library(shinysurveys)

source("functions.R")

ui <- shiny::fluidPage(
  shinysurveys::surveyOutput(
    df = parse_survey_questions(file.path('survey', '_questions.yml')),
    survey_title = "A minimal title",
    survey_description = "A minimal description")
)

server <- function(input, output, session) {
  shinysurveys::renderSurvey()
}

shiny::shinyApp(ui = ui, server = server)



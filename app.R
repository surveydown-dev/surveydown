library(shiny)
library(knitr)
library(shinyjs)

# Include the modules and custom functions
source("mySurvey.R")
source("functions.R")

# Render the survey.qmd file

quarto::quarto_render("survey.qmd")

# Add www resource
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# Define UI
ui <- fluidPage(
  surveyPageUI("survey1"),
  tags$script(
    HTML(
      "$(document).on('shiny:custommessage', function(event) {
      if (event.name === 'iframeMessage') {
        var iframe = document.getElementById('survey_frame');
        iframe.contentWindow.postMessage(event.value.message, '*');
      }
    });"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  callModule(surveyPageServer, "survey1")
}

# Run the application
shinyApp(ui = ui, server = server)

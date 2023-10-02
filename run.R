library(shiny)
library(knitr)
library(shinyjs)

# Include the modules and custom functions
source("mySurvey.R")
source("functions.R")

# Render the survey.qmd file

quarto::quarto_render("survey.qmd")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$script(src = "nav.js")
  ),
  surveyUI("survey1")
)

# Define server logic
server <- function(input, output) {
  callModule(surveyServer, "survey1")
}

# Run the application
shinyApp(ui = ui, server = server)

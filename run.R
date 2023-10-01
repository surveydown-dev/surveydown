library(shiny)
library(knitr)
library(shinyjs)
library(stringr)
library(fs)
library(xml2)
library(rvest)

source('mySurvey.R')  # Source the module

quarto::quarto_render(
  "survey.qmd",
  output_format = "revealjs"
)

ui <- fluidPage(
  surveyUI("survey"),
  actionButton("prev", "Previous"),
  actionButton("next", "Next")
)

server <- function(input, output, session) {
  # A placeholder for how you might store the split sections.
  # Ideally, the Lua filter's processing result should be available here.
  pages <- list("<h1>Page 1 content</h1>", "<h1>Page 2 content</h1>")  # This is a mock example

  current_page <- reactiveVal(1)

  observeEvent(input$next, {
    current_page(min(length(pages), current_page() + 1))
  })

  observeEvent(input$prev, {
    current_page(max(1, current_page() - 1))
  })

  # Call the survey module
  surveyServer("survey", page_content = reactive(pages[[current_page()]]))
}

shinyApp(ui, server)

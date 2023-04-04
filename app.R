library(shiny)
library(shinydashboard)
library(markdown)
library(readr)
library(dplyr)
library(jsonlite)
library(shinyjs)
library(shinythemes)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Survey Platform"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Survey 1", tabName = "survey1"),
      menuItem("Survey 2", tabName = "survey2")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "survey1",
        uiOutput("survey-1-ui")
      ),
      tabItem(
        tabName = "survey2",
        uiOutput("survey-2-ui")
      )
    )
  )
)

server <- function(input, output) {

  # Load survey data
  survey_1_data <- read_file("survey-questions/survey-1.md")
  survey_2_data <- read_file("survey-questions/survey-2.md")

  # Render survey UI
  output$survey1 <- renderUI({
    markdownToHTML(survey_1_data)
  })

  output$survey2 <- renderUI({
    markdownToHTML(survey_2_data)
  })

}

shinyApp(ui = ui, server = server)

library(surveydown)

ui <- sd_ui()

server <- function(input, output, session) {
  sd_server()
}

shiny::shinyApp(ui = ui, server = server)

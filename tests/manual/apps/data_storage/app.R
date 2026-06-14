library(surveydown)

db <- sd_db_connect()

ui <- sd_ui()

server <- function(input, output, session) {
  # Verify sd_store_value() lands in the data
  sd_store_value("abc123", "stored_test")

  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)

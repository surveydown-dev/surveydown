library(surveydown)

# A fake database object mimicking a successful sd_db_connect() result
# (non-NULL db and table fields). With mode: preview, sd_server() must
# ignore it entirely -- any code path that tried to actually use this
# object as a connection pool would error and crash the app.
db <- list(db = TRUE, table = "responses")

ui <- sd_ui()

server <- function(input, output, session) {
  sd_server(db = db)
}

shiny::shinyApp(ui = ui, server = server)

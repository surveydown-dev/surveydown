#' Render an Rmd file
#'
#' @param file The file to be rendered. This should be an Rmd file.
#'
#' @return
#' Renders the specified Rmd file using `knitr::knit()`.
#'
#' @examples
#' \dontrun{
#' render("my_file.Rmd")
#' }
#' @export
render <- function(file) {
  rmarkdown::render(input = file)
}


#' Run a shiny app
#'
#' @param ui Path to the html file defining the ui.
#'
#' @return
#' Runs a local instance of a Shiny app
#'
#' @examples
#' \dontrun{
#' preview()
#' }
#' @export
preview <- function(ui) {

  # Define UI for application
  ui <- shinyUI(fluidPage(
    includeHTML(ui)
  ))

  # Define server logic (empty for this example)
  server <- function(input, output) {}

  # Run the application
  shinyApp(ui = ui, server = server)

}



#' Host a shiny app
#'
#' @param folder The folder in which the Shiny app to be hosted is located
#' @param data_url The URL of the Google Sheets document to be used as the data source for the Shiny app
#' @param api_key The API key for shinyapps.io
#'
#' @return
#' Deploys a Shiny app to shinyapps.io and connects it to a Google Sheets data source.
#'
#' @examples
#' \dontrun{
#' host("my_shiny_app_folder", "https://docs.google.com/spreadsheets/d/1A1a1A1a1A1a1A1a1A1A1a1a1A1/edit", "your_api_key")
#' }
#' @export
host <- function(folder, data_url, api_key) {

  # # Authenticate with shinyapps.io
  # rsconnect::setAccountInfo(
  #   name = "your_name",
  #   token = "your_token",
  #   secret = api_key
  # )
  #
  # # Load data from Google Sheets
  # data <- googlesheets4::read_sheet(data_url)
  #
  # # Deploy to shinyapps.io
  # rsconnect::deployApp(folder)
}

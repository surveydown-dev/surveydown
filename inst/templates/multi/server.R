server <- function(input, output, session) {

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_if()

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if()

  # Database designation and other settings
  sd_server(
    db = db
  )

}

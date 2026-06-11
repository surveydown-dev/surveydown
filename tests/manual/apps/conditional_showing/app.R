# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak("surveydown-dev/surveydown") # Development version from GitHub

# Load packages
library(surveydown)

# Database setup --------------------------------------------------------------
#
# Details at: https://surveydown.org/docs/storing-data
#
# surveydown stores data on any PostgreSQL database. We recommend
# https://supabase.com/ for a free and easy to use service.
#
# Once you have your database ready, run the following function to store your
# database configuration parameters in a local .env file:
#
# sd_db_config()
#
# Once your parameters are stored, you are ready to connect to your database.
# This template runs in preview mode (set via `mode: preview` in survey.qmd),
# which saves responses locally instead of to a database. To collect real
# responses, run sd_db_config() to store your database credentials, then
# change `mode` to `database` in the survey.qmd YAML header.

db <- sd_db_connect()

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  # Used for case 5
  # Custom function to check if pet number input is > 1
  more_than_one_pet <- function(input) {
    if (is.null(sd_value("pet_number"))) {
      return(FALSE)
    }
    num_pets <- as.numeric(sd_value("pet_number"))
    return(num_pets > 1)
  }

  # Define any conditional showing logic here (show a question if a condition is true)
  sd_show_if(
    # 1. Simple conditional showing
    sd_value("penguins_simple") == "other" ~ "penguins_simple_other",

    # 2. Complex conditional showing
    sd_value("penguins_complex") == "other" &
      sd_value("show_other") == "show" ~
      "penguins_complex_other",

    # 3. Conditional showing based on a numeric value
    sd_value("car_number") > 1 ~ "ev_ownership",

    # 4. Conditional showing based on multiple inputs
    sd_value("fav_fruits") %in% c("apple", "banana") ~ "apple_or_banana",
    length(sd_value("fav_fruits")) > 3 ~ "fruit_number",

    # 5. Conditional showing based on a custom function
    more_than_one_pet(input) ~ "pet_type",

    # 6. Conditional page showing
    sd_value("pet_preference") == 'cat' ~ 'cat_page',
    sd_value("pet_preference") == 'dog' ~ 'dog_page',

    # 7. Cross-page conditional showing: the controlling question
    # (snow_preference) is on a different page than the targets.
    # One condition uses input$ syntax, the other uses all_data$ syntax.
    input$snow_preference == "yes" ~ "snow_activity",
    all_data$snow_preference == "yes" ~ "snow_memory"
  )

  # Run surveydown server and define database
  sd_server(db = db)
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)
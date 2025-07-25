# Package setup ---------------------------------------------------------------

# Install required packages:
# install.packages("pak")
# pak::pak(c(
#   'surveydown-dev/surveydown', # Development version from GitHub
#   'here',
#   'glue',
#   'readr',
#   'dplyr'
# ))

# Load packages
library(surveydown)
library(here)
library(dplyr)
library(glue)
library(readr)

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
# For this demo, we set ignore = TRUE in the following code, which will ignore
# the connection settings and won't attempt to connect to the database. This is
# helpful if you don't want to record testing data in the database table while
# doing local testing. Once you're ready to collect survey responses, set
# ignore = FALSE or just delete this argument.

db <- sd_db_connect()

# UI setup --------------------------------------------------------------------

ui <- sd_ui()

# Server setup ----------------------------------------------------------------

server <- function(input, output, session) {
  # Store database connection for sd_sample() to access
  session$userData$db <- db
  
  # Make a 10-digit random number completion code
  completion_code <- sd_completion_code(10)

  # Store the completion code in the survey data
  sd_store_value(completion_code)

  # Read in the full survey design file
  design <- read_csv(here("data", "choice_questions.csv"))

  # Sample a random respondentID and store it in your data
  respondentID <- sd_sample(design$respID)
  sd_store_value(respondentID, "respID")

  # Filter for the rows for the chosen respondentID
  df <- design |>
    filter(respID == respondentID) |>
    # Paste on the "images/" path (images are stored in the "images" folder)
    mutate(image = paste0("images/", image))

  # Function to create the question labels based on design values
  make_cbc_options <- function(df) {
    alt1 <- df |> filter(altID == 1)
    alt2 <- df |> filter(altID == 2)
    alt3 <- df |> filter(altID == 3)

    options <- c("option_1", "option_2", "option_3")

    names(options) <- c(
      glue(
        "
      **Option 1**<br>
      <img src='{alt1$image}' width=100><br>
      **Type**: {alt1$type}<br>
      **Price**: $ {alt1$price} / lb<br>
      **Freshness**: {alt1$freshness}
    "
      ),
      glue(
        "
      **Option 2**<br>
      <img src='{alt2$image}' width=100><br>
      **Type**: {alt2$type}<br>
      **Price**: $ {alt2$price} / lb<br>
      **Freshness**: {alt2$freshness}
    "
      ),
      glue(
        "
      **Option 3**<br>
      <img src='{alt3$image}' width=100><br>
      **Type**: {alt3$type}<br>
      **Price**: $ {alt3$price} / lb<br>
      **Freshness**: {alt3$freshness}
    "
      )
    )
    return(options)
  }

  # Create the options for each choice question
  cbc1_options <- make_cbc_options(df |> filter(qID == 1))
  cbc2_options <- make_cbc_options(df |> filter(qID == 2))
  cbc3_options <- make_cbc_options(df |> filter(qID == 3))
  cbc4_options <- make_cbc_options(df |> filter(qID == 4))
  cbc5_options <- make_cbc_options(df |> filter(qID == 5))
  cbc6_options <- make_cbc_options(df |> filter(qID == 6))

  # Create each choice question - display these in your survey using sd_output()
  # Example: sd_output('cbc_q1', type = 'question')

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q1',
    label = "(1 of 6) If these were your only options, which would you choose?",
    option = cbc1_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q2',
    label = "(2 of 6) If these were your only options, which would you choose?",
    option = cbc2_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q3',
    label = "(3 of 6) If these were your only options, which would you choose?",
    option = cbc3_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q4',
    label = "(4 of 6) If these were your only options, which would you choose?",
    option = cbc4_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q5',
    label = "(5 of 6) If these were your only options, which would you choose?",
    option = cbc5_options
  )

  sd_question(
    type = 'mc_buttons',
    id = 'cbc_q6',
    label = "(6 of 6) If these were your only options, which would you choose?",
    option = cbc6_options
  )

  # Define any conditional skip logic here (skip to page if a condition is true)
  sd_skip_forward(
    input$screenout == "blue" ~ "end_screenout",
    input$consent_age == "no" ~ "end_consent",
    input$consent_understand == "no" ~ "end_consent"
  )

  # Define any conditional display logic here (show a question if a condition is true)
  sd_show_if(
    input$like_fruit %in% c("yes", "kind_of") ~ "fav_fruit"
  )

  # Database designation and other settings
  sd_server(
    db = db,
    auto_scroll = TRUE,
    rate_survey = TRUE,
    all_questions_required = TRUE
  )
}

# Launch the app
shiny::shinyApp(ui = ui, server = server)

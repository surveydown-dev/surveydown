# Server logic for a surveydown survey

This function defines the server-side logic for a 'shiny' application
used in surveydown. It handles various operations such as conditional
display, progress tracking, page navigation, database updates for survey
responses, and exit survey functionality.

All survey settings are configured in the `survey.qmd` YAML header under
`survey-settings`. See the surveydown documentation for available
options.

## Usage

``` r
sd_server(db = NULL)
```

## Arguments

- db:

  A list containing database connection information created using
  [`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md)
  function. Defaults to `NULL`. If `NULL`, will be auto-detected from
  the calling environment or remain `NULL` (ignore mode).

## Value

This function does not return a value; it sets up the server-side logic
for the 'shiny' application and exposes the `all_data` reactive list in
the parent environment.

## Details

The function performs the following tasks:

- Initializes variables and reactive values, including the `all_data`
  reactive list for accessing question responses.

- Implements conditional display logic for questions.

- Tracks answered questions and updates the progress bar.

- Handles page navigation and skip logic.

- Manages required questions.

- Performs database operation.

- Controls auto-scrolling behavior based on survey settings.

- Uses sweetalert for warning messages when required questions are not
  answered.

- Handles the exit survey process based on survey settings.

All survey behavior is configured through the `survey-settings` section
in the `survey.qmd` YAML header. Available settings include:

- `show-previous`: Show Previous button (default: FALSE)

- `use-cookies`: Enable cookie-based session management (default: TRUE)

- `auto-scroll`: Auto-scroll to next question (default: FALSE)

- `rate-survey`: Show rating question on exit (default: FALSE)

- `all-required`: Make all questions required (default: FALSE)

- `start-page`: ID of the page to start on (default: first page)

- `system-language`: Language for system messages (default: "en")

- `highlight-unanswered`: Highlight unanswered questions (default: TRUE)

- `highlight-color`: Color for highlighting (default: "gray")

- `capture-metadata`: Capture browser/IP info (default: TRUE)

- `required`: Vector of required question IDs

- `shuffled`: Vector of question IDs to shuffle (MC options or matrix
  rows)

- `all-shuffled`: Shuffle all MC options and matrix rows (default:
  FALSE)

## Progress Bar

The progress bar is updated based on the last answered question. It will
jump to the percentage corresponding to the last answered question and
will never decrease, even if earlier questions are answered later. The
progress is calculated as the ratio of the last answered question's
index to the total number of questions.

## Database Operations

If `db` is provided, the function will update the database with survey
responses. If `db` is `NULL` (ignore mode), responses will be saved to a
local CSV file.

## Accessing Question Values

The `sd_server()` function exposes the `all_data` reactive values list
that provides a reliable way to access question responses in your server
logic. Unlike directly accessing `input$question_id`, which only works
for questions on the current page, `all_data$question_id` works for all
questions by automatically restoring values from the database when
needed.

Use `all_data` in conditional logic:

      sd_skip_if(
        all_data$age < 18 ~ "parental_consent",
        all_data$employed == "yes" ~ "employment_questions"
      )

Or in custom reactive expressions:

      output$custom_text <- renderText({
        paste("You selected:", all_data$favorite_color)
      })

The `all_data` list automatically stays synchronized with question
responses and includes restored values from previous sessions (via
cookies or page refreshes), making it safe to use even when navigating
backward or refreshing the page.

## See also

[`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md),
[`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Basic sd_server() usage in app.R:
  # library(surveydown)
  #
  # db <- sd_database(
  #   host   = "db.xxxx.supabase.co",
  #   dbname = "postgres",
  #   port   = "5432",
  #   user   = "postgres",
  #   table  = "my_survey"
  # )
  #
  # server <- function(input, output, session) {
  #   sd_server(db = db)
  # }
  #
  # shiny::shinyApp(ui = sd_ui(), server = server)

  # All settings are configured in survey.qmd YAML header:
  # ---
  # survey-settings:
  #   show-previous: true
  #   auto-scroll: true
  #   required:
  #     - age
  #     - name
  # ---

  # Using all_data for conditional logic:
  # server <- function(input, output, session) {
  #   sd_skip_if(
  #     all_data$age < 18 ~ "parental_consent",
  #     all_data$country == "USA" ~ "usa_specific"
  #   )
  #
  #   sd_server(db = db)
  # }

  # Find a working directory and start from a template:
  sd_create_survey(template = "default")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

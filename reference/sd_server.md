# Server logic for a surveydown survey

This function defines the server-side logic for a 'shiny' application
used in surveydown. It handles various operations such as conditional
display, progress tracking, page navigation, database updates for survey
responses, and exit survey functionality.

## Usage

``` r
sd_server(
  db = NULL,
  required_questions = NULL,
  all_questions_required = FALSE,
  start_page = NULL,
  show_previous = FALSE,
  auto_scroll = FALSE,
  rate_survey = FALSE,
  system_language = "en",
  use_cookies = TRUE,
  highlight_unanswered = TRUE,
  highlight_color = "gray",
  capture_metadata = TRUE,
  language = NULL
)
```

## Arguments

- db:

  A list containing database connection information created using
  [`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md)
  function. Defaults to `NULL`. If `NULL`, will be auto-detected from
  the calling environment or remain `NULL` (ignore mode).

- required_questions:

  Vector of character strings. The IDs of questions that must be
  answered. Defaults to `NULL` (no required questions).

- all_questions_required:

  Logical. If `TRUE`, all questions in the survey will be required.
  Defaults to `FALSE`.

- start_page:

  Character string. The ID of the page to start on. Defaults to `NULL`
  (first page).

- show_previous:

  Logical. If `TRUE`, shows the Previous button on survey pages.
  Defaults to `FALSE`.

- auto_scroll:

  Logical. Whether to enable auto-scrolling to the next question after
  answering. Defaults to `FALSE`.

- rate_survey:

  Logical. If `TRUE`, shows a rating question when exiting the survey.
  If `FALSE`, shows a simple confirmation dialog. Defaults to `FALSE`.

- system_language:

  Set the language for the survey system messages. Include your own in a
  `messages.yml` file, or choose a built in one from the following list:
  English (`"en"`), German (`"de"`), Spanish (`"es"`), French (`"fr"`),
  Italian (`"it"`), Simplified Chinese (`"zh-CN"`). Defaults to `"en"`.
  Note: The deprecated `language` parameter is still supported for
  backward compatibility.

- use_cookies:

  Logical. If `TRUE`, enables cookie-based session management for
  storing and restoring survey progress. Defaults to `TRUE`. Can be
  overridden by `use_cookies` setting in the survey.qmd YAML header.

- highlight_unanswered:

  Logical. If `TRUE`, enables highlighting of all unanswered questions
  on page display. Defaults to `TRUE`.

- highlight_color:

  Character string. Color for highlighting unanswered questions. Options
  are "blue", "orange", "green", "purple", "gray", or "grey". Defaults
  to "gray".

- capture_metadata:

  Logical. If `TRUE`, automatically captures and stores browser
  information (browser name, version, and OS), IP address, and screen
  resolution. Defaults to `TRUE`.

- language:

  Deprecated as of v0.13.0. Use `system_language` instead. This
  parameter. is maintained for backward compatibility only.

## Value

This function does not return a value; it sets up the server-side logic
for the 'shiny' application.

## Details

The function performs the following tasks:

- Initializes variables and reactive values.

- Implements conditional display logic for questions.

- Tracks answered questions and updates the progress bar.

- Handles page navigation and skip logic.

- Manages required questions.

- Performs database operation.

- Controls auto-scrolling behavior based on the `auto_scroll` argument.

- Uses sweetalert for warning messages when required questions are not
  answered.

- Handles the exit survey process based on the `rate_survey` argument.

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

## Auto-Scrolling

When `auto_scroll` is `TRUE`, the survey will automatically scroll to
the next question after the current question is answered. This behavior
can be disabled by setting `auto_scroll = FALSE`.

## Exit Survey

When `rate_survey = TRUE`, the function will show a rating question when
the user attempts to exit the survey. When `FALSE`, it will show a
simple confirmation dialog. The rating, if provided, is saved with the
survey data.

## See also

[`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md),
[`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example files
  survey_path <- system.file("examples", "basic_survey.qmd",
                             package = "surveydown")
  app_path <- system.file("examples", "app.R",
                          package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  file.copy(app_path, file.path(temp_dir, "app.R"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Run the app
  shiny::runApp()

  # Clean up
  setwd(orig_dir)
}
```

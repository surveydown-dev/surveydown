# Store a value in the survey data

This function allows storing additional values to be included in the
survey data, such as respondent IDs or other metadata. When a database
connection is provided, it implements session persistence - if a value
already exists for the current session, storage is skipped to maintain
consistency across page refreshes.

## Usage

``` r
sd_store_value(value, id = NULL, db = NULL, auto_assign = TRUE)
```

## Arguments

- value:

  The value to be stored. This can be any R object that can be coerced
  to a character string.

- id:

  (Optional) Character string. The id (name) of the value in the data.
  If not provided, the name of the `value` variable will be used.

- db:

  (Optional) Database connection object created with sd_db_connect(). If
  provided, enables session persistence. If not provided, will
  automatically look for a variable named 'db' in the calling
  environment, or fall back to the database connection from the session.

- auto_assign:

  Logical. If `TRUE` (default), automatically assigns the stored value
  back to the original variable in the calling environment. This
  eliminates the need for explicit assignment when session persistence
  is desired. If `FALSE`, the function only returns the value without
  modifying the original variable.

## Value

The value that was stored (either the new value or existing value from
database if session persistence applies). This allows the function to be
used in variable assignments.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example files
  survey_path <- system.file("examples", "basic_survey.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Create app.R with sd_store_value() logic
  writeLines(c(
    "library(surveydown)",
    "",
    "ui <- sd_ui()",
    "",
    "server <- function(input, output, session) {",
    "  ",
    "  # Generate and store values with automatic assignment (default behavior)",
    "  respondentID <- sample(1:1000, 1)",
    "  sd_store_value(respondentID, \"respID\", db)  # respondentID automatically updated",
    "  ",
    "  completion_code <- sample(0:9, 6, replace = TRUE)",
    "  sd_store_value(completion_code)  # completion_code automatically updated",
    "  ",
    "  # Traditional assignment approach (auto_assign = FALSE)",
    "  some_value <- sd_store_value(42, \"some_value\", auto_assign = FALSE)",
    "  ",
    "  sd_server()",
    "}",
    "",
    "shiny::shinyApp(ui = ui, server = server)"
  ), file.path(temp_dir, "app.R"))

  # Run the app
  shiny::runApp()

  # Clean up
  setwd(orig_dir)
}
```

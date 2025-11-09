# Check if a question is answered

This function checks if a given question has been answered by the user.
For matrix questions, it checks if all sub-questions (rows) are
answered.

## Usage

``` r
sd_is_answered(question_id)
```

## Arguments

- question_id:

  The ID of the question to check.

## Value

A logical value: `TRUE` if the question is answered, `FALSE` otherwise.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example survey file
  survey_path <- system.file("examples", "sd_is_answered.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {

    sd_show_if(
      # If "apple_text" is answered, show the conditional question
      sd_is_answered("apple_text") ~ "other_fruit"
    )

    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```

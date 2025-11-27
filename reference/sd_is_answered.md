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

  # Use sd_is_answered() to conditionally execute code:
  # server <- function(input, output, session) {
  #   observe({
  #     if (sd_is_answered(input$age)) {
  #       message("Age question answered!")
  #     }
  #   })
  #   sd_server()
  # }

  # Find a working directory and start from a template:
  sd_create_survey(template = "default")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

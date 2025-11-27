# Define show conditions for survey questions and pages

This function is used to define conditions under which certain questions
or pages in the survey should be shown. It takes one or more formulas
where the left-hand side is the condition and the right-hand side is the
target question ID or page ID. If called with no arguments, it will
return `NULL` and set no conditions.

## Usage

``` r
sd_show_if(...)
```

## Arguments

- ...:

  One or more formulas defining show conditions. The left-hand side of
  each formula should be a condition based on input values, and the
  right-hand side should be the ID of the question or page to show if
  the condition is met.

## Value

A list of parsed conditions, where each element contains the condition
and the target question or page ID. Returns `NULL` if no conditions are
provided.

## See also

[`sd_skip_if()`](https://pkg.surveydown.org/reference/sd_skip_if.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Use sd_show_if() to conditionally show/hide questions:
  # server <- function(input, output, session) {
  #   sd_show_if(
  #     input$has_car == "yes" ~ "car_make",
  #     input$employed == "yes" ~ "job_title",
  #     input$age >= 18 ~ "adult_questions_page"
  #   )
  #   sd_server()
  # }

  # Find a working directory and start from a template:
  sd_create_survey(template = "conditional_showing")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

# Define skip conditions for survey pages

This function is used to define conditions under which certain pages in
the survey should be skipped. It takes one or more formulas where the
left-hand side is the condition and the right-hand side is the target
page ID. Only forward skipping is allowed to prevent navigation loops.

## Usage

``` r
sd_skip_if(...)
```

## Arguments

- ...:

  One or more formulas defining skip conditions. The left-hand side of
  each formula should be a condition based on input values, and the
  right-hand side should be the ID of the page to skip to if the
  condition is met. Only forward skipping (to pages later in the
  sequence) is allowed.

## Value

A list of parsed conditions, where each element contains the condition
and the target page ID.

## See also

[`sd_show_if()`](https://pkg.surveydown.org/reference/sd_show_if.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Use sd_skip_if() to skip pages based on answers:
  # server <- function(input, output, session) {
  #   sd_skip_if(
  #     input$age < 18 ~ "underage_page",
  #     input$country == "USA" ~ "usa_page",
  #     input$consent == "no" ~ "exit_page"
  #   )
  #   sd_server()
  # }

  # Find a working directory and start from a template:
  sd_create_survey(template = "conditional_skipping")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

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

  # Get path to example survey file
  survey_path <- system.file("examples", "sd_show_if.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {

    sd_show_if(
      # If "Other" is chosen, show the conditional question
      input$fav_fruit == "other" ~ "fav_fruit_other",
      # If condition is met, show specific page
      input$category == "advanced" ~ "advanced_page"
    )

    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```

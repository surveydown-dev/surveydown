# Create the UI for a surveydown survey

This function creates the user interface for a surveydown survey,
including necessary CSS and JavaScript files, and applies custom
styling. It retrieves theme and progress bar settings from the
survey.qmd file.

## Usage

``` r
sd_ui()
```

## Value

A 'shiny' UI object

## Details

The function reads the following settings from the survey.qmd YAML
header:

- `theme`: The theme to be applied to the survey.

- `barcolor`: The color of the progress bar (accepts hex colors like
  \#FF0000 or \#F00, or CSS color names like red, blue, green).

- `barposition`: The position of the progress bar (`'top'`, `'bottom'`,
  or `'none'`).

If `barcolor` is not specified or is `NULL`, the default theme color
will be used. If `barposition` is not specified, it defaults to 'top'.

## See also

[`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md) for
creating the server-side logic of the survey

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Basic sd_ui() usage in app.R:
  # library(surveydown)
  #
  # ui <- sd_ui()
  #
  # server <- function(input, output, session) {
  #   sd_server()
  # }
  #
  # shiny::shinyApp(ui = ui, server = server)

  # Find a working directory and start from a template:
  sd_create_survey(template = "default")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

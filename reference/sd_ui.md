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

- `barcolor`: The color of the progress bar (should be a valid hex
  color).

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

  # Get path to example files
  survey_path <- system.file("examples", "sd_ui.qmd",
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

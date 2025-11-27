# Create a 'Close' Button to Exit the Survey

This function creates a 'Close' button that, when clicked, will trigger
the exit process for the survey. Depending on the server-side
configuration, this may show a rating question or a simple confirmation
dialog before attempting to close the current browser tab or window.

## Usage

``` r
sd_close(label_close = NULL, label_previous = NULL, show_previous = NULL)
```

## Arguments

- label_close:

  Character string. The label of the 'Close' button. Defaults to `NULL`,
  in which case the word `"Exit Survey"` will be used.

- label_previous:

  Character string. The label for the 'Previous' button. Defaults to
  `NULL`, which uses "← Previous" (or the translated equivalent).

- show_previous:

  Logical. Whether to show the Previous button alongside the Close
  button. Set to `TRUE` to allow users to go back before closing.
  Defaults to `FALSE`. Note: Unlike
  [`sd_nav()`](https://pkg.surveydown.org/reference/sd_nav.md), this
  parameter does NOT read from the `show-previous` YAML setting.

## Value

A 'shiny' tagList containing the 'Close' button UI element and
associated JavaScript for the exit process.

## Details

The function generates a 'shiny' action button that, when clicked,
triggers the 'show_exit_modal' event. The server-side logic (controlled
by the `rate_survey` parameter in
[`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md))
determines whether to show a rating question or a simple confirmation
dialog.

The function also includes a custom message handler for closing the
window. This is necessary because some browsers may not allow JavaScript
to close windows that were not opened by JavaScript. In such cases, the
user will be prompted to close the tab manually.

## Note

The actual behavior of the exit process (whether to show a rating
question or not) is controlled by the `rate_survey` parameter in the
[`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
function, not in this UI function.

## See also

[`sd_server`](https://pkg.surveydown.org/reference/sd_server.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example files
  survey_path <- system.file("examples", "sd_close.qmd",
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

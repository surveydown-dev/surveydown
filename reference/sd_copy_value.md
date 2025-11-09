# Create a copy of a value

This function creates a copy of an input value and makes it available as
a new output. The new output can then be displayed using
[`sd_output()`](https://pkg.surveydown.org/reference/sd_output.md).

## Usage

``` r
sd_copy_value(id, id_copy)
```

## Arguments

- id:

  Character string. The ID of the input value to copy.

- id_copy:

  Character string. The ID for the new copy (must be different from
  `id`).

## Value

`NULL` invisibly. This function is called for its side effects.

## See also

[`sd_output()`](https://pkg.surveydown.org/reference/sd_output.md) for
displaying the copied value

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example survey file
  survey_path <- system.file("examples", "sd_ui.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "sd_copy_value.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {

    # Make a copy of the "name" variable to call its value a second time
    sd_copy_value(id = "name", id_copy = "name_copy")

    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```

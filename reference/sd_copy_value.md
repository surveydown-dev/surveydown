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

  # Use sd_copy_value() to copy an input value to display elsewhere:
  # server <- function(input, output, session) {
  #   sd_copy_value(id = "name", id_copy = "name_copy")
  #   ...
  # }
  # Then in survey.qmd, display it in an R chunk:
  # sd_output("name", type = "value")

  # Find a working directory and start from a template:
  sd_create_survey(template = "default")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

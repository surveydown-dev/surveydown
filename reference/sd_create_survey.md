# Create a new survey template

This function creates a new survey template by copying template files to
the specified directory. You can choose from various predefined
templates, including the default built-in template and specialized
templates from the surveydown-dev/templates repository.

## Usage

``` r
sd_create_survey(template = "default", path = getwd(), ask = TRUE)
```

## Arguments

- template:

  A character string specifying the template to use. Default is
  "default" which uses the built-in package template. Other options
  include:

  default

  :   The default built-in template

  conditional_showing

  :   Conditionally showing questions and pages

  conditional_skipping

  :   Conditionally skipping to forward pages

  conditional_stopping

  :   Conditionally stopping the navigation

  conjoint_buttons

  :   Conjoint analysis with button interface

  conjoint_tables

  :   Conjoint analysis with table interface

  custom_leaflet_map

  :   Survey with interactive Leaflet maps

  custom_plotly_chart

  :   Survey with Plotly visualizations

  external_redirect

  :   Template with external site redirects

  live_polling

  :   Live polling template for real-time surveys

  question_types

  :   Showcases all available question types

  questions_yml

  :   Survey with questions defined in a YAML file

  random_options

  :   Survey with randomized question options

  random_options_predefined

  :   Randomized options from predefined sets

  reactive_drilldown

  :   Dynamic questions with drill-down capability

  reactive_questions

  :   Survey with reactive questions

- path:

  A character string specifying the directory where the survey template
  should be created. Defaults to the current working directory.

- ask:

  Logical. If `TRUE` (default), prompts for user confirmation when
  creating the survey in the current directory. If `FALSE`, bypasses the
  confirmation prompt and proceeds without asking.

## Value

Invisible `NULL`. The function is called for its side effects.

## Details

When creating a new survey template, this function will:

1.  Check if the specified template is valid

2.  Confirm the destination path with the user (if it's the current
    directory)

3.  Download template files from GitHub if a non-default template is
    specified

4.  Copy template files to the destination directory

5.  Skip .Rproj files if one already exists in the destination

6.  Prompt for confirmation before overwriting existing files

External templates are downloaded from the surveydown-dev/templates
GitHub repository.

## Examples

``` r
if (interactive()) {
  # Create a survey with the "question_types" template in the "my_survey" directory
  sd_create_survey(template = "question_types", path = "my_survey")

  # Create a survey using the default template in the "my_survey" directory
  sd_create_survey(path = "my_survey")

  # Create a survey with default template in current directory
  sd_create_survey("default")

  # Create a survey without asking for confirmation
  sd_create_survey(template = "default", path = "my_survey", ask = FALSE)
}
```

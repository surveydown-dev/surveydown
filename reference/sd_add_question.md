# Add a Question Template to the Current Document

This function inserts a template for a surveydown question at the
current cursor position in the active RStudio document. It supports
various question types and automatically removes the function call
before inserting the template if it exists in the document.

## Usage

``` r
sd_add_question(type = "mc", id = NULL, label = NULL, chunk = FALSE)
```

## Arguments

- type:

  A character string specifying the type of question template to insert.
  Default is `"mc"` (multiple choice). Available options are:

  - `"mc"`: Multiple choice (single selection)

  - `"select"`: Dropdown selection

  - `"mc_multiple"`: Multiple choice (multiple selections)

  - `"mc_buttons"`: Multiple choice with button layout (single
    selection)

  - `"mc_multiple_buttons"`: Multiple choice with button layout
    (multiple selections)

  - `"text"`: Short text input

  - `"textarea"`: Long text input

  - `"numeric"`: Numeric input

  - `"slider"`: Slider input

  - `"date"`: Date input

  - `"daterange"`: Date range input

- id:

  A character string specifying the ID for the question. If not
  provided, a default ID based on the question type will be used. This
  ID should be unique within your survey.

- label:

  A character string specifying the label (question text) to display to
  respondents. If not provided, a default label placeholder will be
  used.

- chunk:

  Logical. If `TRUE`, the code will be generated with the R code chunk
  wrapper. Defaults to `FALSE`.

## Value

This function does not return a value. It modifies the active document
as a side effect by inserting text and potentially removing a function
call.

## Details

The function performs the following steps:

1.  Checks for and removes any existing `sd_add_question()` function
    call in the document.

2.  Inserts the appropriate question template at the current cursor
    position.

3.  If an ID is provided, replaces the default ID in the template with
    the provided ID.

4.  If a label is provided, replaces the default label in the template
    with the provided label.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Insert a default multiple choice question template
  sd_add_question()

  # Insert a text input question with custom ID and label
  sd_add_question("text", id = "user_email", label = "What is your email address?")

  # Insert a slider question template
  sd_add_question("slider", id = "satisfaction", label = "How satisfied were you with our service?")
}
```

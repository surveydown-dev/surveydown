# Output Function for Displaying reactive objects and values

Output Function for Displaying reactive objects and values

## Usage

``` r
sd_output(
  id,
  type = NULL,
  width = "100%",
  display = "text",
  inline = TRUE,
  wrapper = NULL,
  ...
)
```

## Arguments

- id:

  Character string. A unique identifier for the output element.

- type:

  Character string. Specifies the type of output corresponding with the
  question `id`. Can be `"question"`, `"value"`, `"label_option"`,
  `"label_question"`, or `NULL.` If `"question"`, it will display a
  question defined in the server. If `"value"`, it will display the
  value of question `id` selected by the respondent. If
  `"label_option"`, it will display the label of the option for question
  `id` selected by the respondent. If `"label_question"`, it will
  display the `label` argument value for question `id`. Finally, if
  `NULL`, the function behaves like
  [`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html).

- width:

  Character string. The width of the UI element. Defaults to `"100%"`.

- display:

  Character string. Specifies the display type for `"value"` outputs.
  Can be `"text"`, `"verbatim"`, or `"ui"`. Only used when
  `type = "value"`.

- inline:

  Logical. Whether to render the output inline. Defaults to `TRUE`.

- wrapper:

  Function. A function to wrap the output. Only used when
  `type = "value"`.

- ...:

  Additional arguments passed to the underlying 'shiny' functions or the
  wrapper function.

## Value

A 'shiny' UI element, the type of which depends on the input parameters.

## Details

The function behaves differently based on the `type` parameter:

- If `type` is `NULL`, it acts like
  [`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html).

- If `type` is `"question"`, it creates a placeholder for a reactive
  survey question.

- If `type` is `"value"`, it creates an output to display the value of a
  survey question, with the display style determined by the `display`
  parameter.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Use sd_output() to display reactive questions or values:
  # First, define something in server of app.R:
  # server <- function(input, output, session) {
  #   completion_code <- sd_completion_code(10)
  #   sd_store_value(completion_code)
  #   sd_server()
  # }

  # Then, display in R chunks of survey.qmd:
  # Your code is: `r sd_output("completion_code", type = 'value')`

  # Find a working directory and start from a template:
  sd_create_survey(template = "reactive_questions")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

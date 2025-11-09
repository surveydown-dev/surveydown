# Create a reactive value that is also stored in survey data

This function creates a reactive value similar to Shiny's reactive()
function, but also automatically stores the calculated value in the
survey data.

## Usage

``` r
sd_reactive(id, expr, blank_na = TRUE)
```

## Arguments

- id:

  Character string. The id (name) of the value to be stored in the data.

- expr:

  An expression that calculates a value based on inputs

- blank_na:

  Logical. If TRUE, NA values are converted to empty strings. Default is
  TRUE.

## Value

A reactive expression that can be called like a function

## Examples

``` r
# This example shows how sd_reactive would be used in the app.R file
if (interactive()) {
  library(surveydown)
  library(shiny)

  # Demo app setup
  server <- function(input, output, session) {
    # Create a reactive value that is stored in survey data
    product <- sd_reactive("product", {
      as.numeric(input$first_number) * as.numeric(input$second_number)
    })

    # Display the result
    output$result <- renderText({
      paste("The product is:", product())
    })

    # The rest of your survey setup...
    sd_server()
  }

  # In your survey.qmd file, you would use:
  # The product is: `r sd_output("product", type = "value")`
}
```

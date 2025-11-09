# Define stop conditions for survey questions

This function is used to define custom stop conditions for survey
questions. When a user attempts to navigate to the next page, these
conditions are checked first (before required question checks). If any
condition is TRUE (i.e., the stop condition is met), a modal dialog
displays the corresponding error messages and highlights the invalid
questions in red.

## Usage

``` r
sd_stop_if(...)
```

## Arguments

- ...:

  One or more formulas defining stop conditions. The left-hand side of
  each formula should be a condition that, when TRUE, will stop
  navigation and show an error (e.g., `nchar(input$zip) != 5`). The
  right-hand side should be the error message to display when the
  condition is TRUE.

## Value

This function does not return a value; it sets up stop logic for the
survey.

## Details

The function performs the following:

- Automatically determines which page each stop condition belongs to
  based on the input variables referenced in the conditions

- Groups stop conditions by page and shows them together when multiple
  conditions are triggered on the same page

- Displays error messages as a numbered list when multiple stop
  conditions are triggered on the same page, or as a single message for
  individual triggers

- Highlights invalid question containers in red

- Takes higher priority than required question checks - stop conditions
  are checked first before required question checks

## See also

[`sd_show_if`](https://pkg.surveydown.org/reference/sd_show_if.md),
[`sd_skip_if`](https://pkg.surveydown.org/reference/sd_skip_if.md)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Define a server with custom stop conditions
  server <- function(input, output, session) {

    sd_stop_if(
      # Stop if zip code is not exactly 5 digits
      nchar(input$zip) != 5 ~ "Zip code must be 5 digits",
      # Stop if year of birth is before 1900
      as.numeric(input$yob) < 1900 ~ "Year of birth must be after 1900",
      # Stop if phone number is not exactly 10 digits
      nchar(input$phone) != 10 ~ "Phone number must be 10 digits"
    )

    sd_server()
  }
}
```

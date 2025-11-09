# Create a 'Next' Button for Page Navigation

This function creates a 'Next' button for navigating to the specified
next page in a Surveydown survey. The button can be activated by
clicking or by pressing the Enter key when visible.

## Usage

``` r
sd_next(next_page = NULL, label = NULL)
```

## Arguments

- next_page:

  Character string. The ID of the next page to navigate to. This
  parameter is required.

- label:

  Character string. The label of the 'Next' button. Defaults to `NULL`,
  in which case the word `"Next"` will be used.

## Value

A 'shiny' tagList containing the 'Next' button UI element.

## Details

The function generates a 'shiny' action button that, when clicked or
when the Enter key is pressed, sets the input value to the specified
next page ID, facilitating page navigation within the Shiny application.
The button is styled to appear centered on the page and includes a class
for Enter key functionality.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example survey file
  survey_path <- system.file("examples", "sd_next.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {
    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```

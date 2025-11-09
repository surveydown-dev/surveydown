# Create a Redirect Element for 'shiny' Applications

This function creates a UI element that redirects the user to a
specified URL. It can be used in both reactive and non-reactive contexts
within 'shiny' applications.

## Usage

``` r
sd_redirect(
  id,
  url,
  button = TRUE,
  label = "Click here",
  delay = NULL,
  newtab = FALSE
)
```

## Arguments

- id:

  A character string of a unique id to be used to identify the redirect
  button in the survey body. In reactive contexts, this becomes the
  output ID, while the actual button gets the ID `id + "_btn"` to avoid
  input/output conflicts.

- url:

  A character string specifying the URL to redirect to.

- button:

  A logical value indicating whether to create a button (`TRUE`) or a
  text element (`FALSE`) for the redirect. Default is `TRUE`.

- label:

  A character string for the button or text label. Defaults to `NULL`,
  in which case the words `"Click here"` will be used.

- delay:

  An optional numeric value specifying the delay in seconds before
  automatic redirection. If `NULL` (default), no automatic redirection
  occurs.

- newtab:

  A logical value indicating whether to open the URL in a new tab
  (`TRUE`) or in the current tab (`FALSE`). Default is `FALSE`.

## Value

In a reactive context, creates an output with the specified ID that can
be displayed using
[`sd_output()`](https://pkg.surveydown.org/reference/sd_output.md). The
actual button element gets the ID `id + "_btn"` to prevent input/output
conflicts. In a non-reactive context, returns the redirect element
directly.

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Get path to example survey file
  survey_path <- system.file("examples", "sd_redirect.qmd",
                             package = "surveydown")

  # Copy to a temporary directory
  temp_dir <- tempdir()
  file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
  orig_dir <- getwd()
  setwd(temp_dir)

  # Define a minimal server
  server <- function(input, output, session) {

    # Reactive expression that generates a url with an id variable
    # parsed from the url
    url_redirect <- reactive({
      params <- sd_get_url_pars()
      id <- params["id"]
      return(paste0("https://www.google.com?id=", id))
    })

    # Create the redirect button
    sd_redirect(
      id = "redirect_url_pars",
      url = url_redirect(),
      button = TRUE,
      label = "Redirect"
    )

    sd_skip_if(
      input$screening_question == "end_1" ~ "end_page_1",
      input$screening_question == "end_1" ~ "end_page_2",
    )

    sd_server()
  }

  # Run the app
  shiny::shinyApp(ui = sd_ui(), server = server)

  # Clean up
  setwd(orig_dir)
}
```

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

  # Use sd_redirect() to create redirect in R chunks of survey.qmd:
  # sd_redirect(
  #   id     = "redirect",
  #   url    = "https://www.google.com",
  #   label  = "Redirect to Google",
  #   button = TRUE,
  #   newtab = TRUE
  # )

  # sd_redirect() also supports reactive redirections.
  # Find a working directory and start from a template:
  sd_create_survey(template = "external_redirect")
  # This creates survey.qmd and app.R - launch the survey using app.R
}
```

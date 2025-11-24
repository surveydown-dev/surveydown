# Create Navigation Buttons for Survey Pages

This function creates both 'Previous' and 'Next' buttons for navigating
between pages in a surveydown survey. The buttons are positioned on the
left (Previous) and right (Next) of the page. The Previous button allows
users to return to previously visited pages, while the Next button
maintains the standard forward navigation behavior.

## Usage

``` r
sd_nav(
  page_next = NULL,
  label_previous = NULL,
  label_next = NULL,
  show_previous = NULL,
  show_next = TRUE
)
```

## Arguments

- page_next:

  Character string. The ID of the next page to navigate to when the Next
  button is clicked. If `NULL`, the survey will navigate to the default
  next page in sequence.

- label_previous:

  Character string. The label for the 'Previous' button. Defaults to
  `NULL`, which uses "← Previous" (or the translated equivalent).

- label_next:

  Character string. The label for the 'Next' button. Defaults to `NULL`,
  which uses "Next →" (or the translated equivalent).

- show_previous:

  Logical. Whether to show the Previous button. Set to `FALSE` for the
  first page where there is no previous page to navigate to. If `NULL`
  (default), uses the `show-previous` setting from YAML or
  [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md).

- show_next:

  Logical. Whether to show the Next button. Set to `FALSE` to hide the
  Next button. Defaults to `TRUE`.

## Value

A 'shiny' tagList containing the navigation buttons UI elements.

## Details

The function generates two 'shiny' action buttons:

- **Previous button:** Positioned on the left, navigates to the last
  visited page. Uses page history tracking to determine the previous
  page.

- **Next button:** Positioned on the right, navigates forward. Can be
  activated by clicking or pressing the Enter key when visible.

The buttons are styled to appear on opposite sides of the page using
flexbox layout, and include arrow symbols to indicate direction.

## See also

[`sd_next`](https://pkg.surveydown.org/reference/sd_next.md) for the
legacy single-button navigation (deprecated)

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Basic usage with both buttons
  sd_nav()

  # First page - hide Previous button
  sd_nav(show_previous = FALSE)

  # Last page - hide Next button
  sd_nav(show_next = FALSE)

  # Hide both navigation buttons
  sd_nav(show_previous = FALSE, show_next = FALSE)

  # Custom labels
  sd_nav(
    label_previous = "Go Back",
    label_next = "Continue"
  )

  # Specify next page explicitly
  sd_nav(page_next = "demographics")
}
```

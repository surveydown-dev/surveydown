# Add a Page Template to the Current Document

This function inserts a template for a surveydown page at the current
cursor position in the active RStudio document. It provides a basic
structure for a new page, including a title, content area, and a next
button. If the function call exists in the document, it will be removed
before inserting the template.

## Usage

``` r
sd_add_page(page_id = "page_id")
```

## Arguments

- page_id:

  A character string specifying the ID for the page. Defaults to
  "page_id".

## Value

This function does not return a value. It modifies the active document
as a side effect by inserting text and potentially removing a function
call.

## Details

IMPORTANT: This function should be run outside any division or R code
chunk in your 'Quarto' document. Running it inside a division or code
chunk may result in an incorrect page structure.

The function performs the following steps:

1.  Checks for and removes any existing `sd_add_page()` function call in
    the document.

2.  Inserts a template at the current cursor position.

The template includes:

- A div with class `'sd-page'` and the specified page ID

- A placeholder for the page title

- A placeholder for page contents

- An R code chunk with a placeholder for questions and a next button

Special page_id values:

- When page_id is "end", a thank-you page template with
  [`sd_close()`](https://pkg.surveydown.org/reference/sd_close.md) is
  inserted

## Examples

``` r
if (interactive()) {
  library(surveydown)

  # Insert a new page template with default ID
  sd_add_page()

  # Insert a new page template with custom ID
  sd_add_page(page_id = "welcome")

  # Insert an end/thank you page
  sd_add_page(page_id = "end")
}
```

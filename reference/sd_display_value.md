# Display the value of a survey question

This function is depreciated - use
[`sd_output()`](https://pkg.surveydown.org/reference/sd_output.md)
instead.

## Usage

``` r
sd_display_value(id, display_type = "inline", wrapper = NULL, ...)
```

## Arguments

- id:

  The ID of the question to display

- display_type:

  The type of display. Can be `"inline"` (default), `"text"`,
  `"verbatim"`, or `"ui"`.

- wrapper:

  A function to wrap the output

- ...:

  Additional arguments passed to the wrapper function

## Value

A 'shiny' UI element displaying the question's value

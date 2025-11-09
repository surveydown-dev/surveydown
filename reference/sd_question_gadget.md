# Show a Shiny gadget for selecting a question type

This function displays a Shiny gadget that allows the user to select a
question type from a dropdown menu. Once submitted, it calls
sd_add_question() with the specified type.

## Usage

``` r
sd_question_gadget(chunk = FALSE)
```

## Arguments

- chunk:

  Logical. If `TRUE`, the code will be generated with the R code chunk
  wrapper. Defaults to `FALSE`.

## Value

The selected question type (invisibly).

# Define forward skip conditions for survey pages (Deprecated)

This function is deprecated. Please use
[`sd_skip_if()`](https://pkg.surveydown.org/reference/sd_skip_if.md)
instead.

This function is used to define conditions under which certain pages in
the survey should be skipped ahead to (forward only). It takes one or
more formulas where the left-hand side is the condition and the
right-hand side is the target page ID.

## Usage

``` r
sd_skip_forward(...)
```

## Arguments

- ...:

  One or more formulas defining skip conditions. The left-hand side of
  each formula should be a condition based on input values, and the
  right-hand side should be the ID of the page to skip to if the
  condition is met. Only forward skipping (to pages later in the
  sequence) is allowed.

## Value

A list of parsed conditions, where each element contains the condition
and the target page ID.

# Generate a Random Completion Code

This function generates a random completion code with a specified number
of digits. The code is returned as a character string.

## Usage

``` r
sd_completion_code(digits = 6)
```

## Arguments

- digits:

  An integer specifying the number of digits in the completion code.
  Must be a positive integer. Default is 6.

## Value

A character string representing the random completion code.

## Examples

``` r
library(surveydown)

sd_completion_code()  # generates a 6-digit code
#> [1] "258447"
sd_completion_code(digits = 8)  # generates an 8-digit code
#> [1] "94415312"
sd_completion_code(digits = 4)  # generates a 4-digit code
#> [1] "9645"
sd_completion_code(digits = 10)  # generates a 10-digit code
#> [1] "2941037505"
```

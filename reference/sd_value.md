# Access question values from survey responses (alias)

This is an alias for
[`sd_values()`](https://pkg.surveydown.org/reference/sd_values.md).

## Usage

``` r
sd_value(..., as_numeric = NULL, as_vector = NULL)
```

## Arguments

- ...:

  One or more question IDs to retrieve. Each can be provided as an
  unquoted name (e.g., `age`) or a quoted string (e.g., `"age"`).

- as_numeric:

  Logical or NULL. Controls numeric type conversion:

  |                  |                                                                                                                        |
  |------------------|------------------------------------------------------------------------------------------------------------------------|
  | Value            | Behavior                                                                                                               |
  | `NULL` (default) | Auto-detect: checks if value looks numeric, converts if so. Non-numeric values stay as character.                      |
  | `TRUE`           | Force convert: always calls [`as.numeric()`](https://rdrr.io/r/base/numeric.html). Non-convertible values become `NA`. |
  | `FALSE`          | Never convert: returns value as-is, no detection.                                                                      |

- as_vector:

  Logical or NULL. Controls splitting of pipe-separated values:

  |                  |                                                             |
  |------------------|-------------------------------------------------------------|
  | Value            | Behavior                                                    |
  | `NULL` (default) | Auto-detect: checks if pipe symbol exists, splits if found. |
  | `TRUE`           | Force split: always splits on pipe.                         |
  | `FALSE`          | Never split: returns value as-is, no detection.             |

## Value

If a single question ID is provided, returns the value of that question.
If multiple question IDs are provided, returns an unnamed vector of
values. Returns `NULL` for any question ID that doesn't exist.

## Details

This function provides a functional interface to access question values
from the `all_data` reactive values list. It is equivalent to using
`all_data$question_id` but allows programmatic access using a string
variable or unquoted name. Multiple question IDs can be provided to
retrieve multiple values at once.

## See also

[`sd_values()`](https://pkg.surveydown.org/reference/sd_values.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  library(surveydown)

  server <- function(input, output, session) {
    # Single value access - all equivalent:
    age1 <- all_data$age
    age2 <- sd_value(age)        # Unquoted (recommended)
    age3 <- sd_value("age")      # Quoted (also works)

    # Multiple values at once:
    values <- sd_value(age, name, country)
    # Returns values with auto-detection (numeric converted, pipes split)

    # Default behavior (NULL) auto-detects and converts numeric values:
    age <- sd_value(age)
    # If age is "25", returns 25 (numeric)
    # If age is "hello", returns "hello" (character, not convertible)

    # Force numeric conversion (may produce NA):
    val <- sd_value(some_field, as_numeric = TRUE)
    # "123" becomes 123, "hello" becomes NA

    # Never convert to numeric (e.g., ZIP codes with leading zeros):
    zip <- sd_value(zip_code, as_numeric = FALSE)
    # "01234" stays as "01234", not converted to 1234

    # Default behavior (NULL) auto-detects pipe and splits:
    fruits <- sd_value(fav_fruits)
    # If value is "apple|banana|orange", returns c("apple", "banana", "orange")
    # If value is "apple", returns "apple" (no pipe, no split)

    # Force split (even if no pipe present):
    vals <- sd_value(some_field, as_vector = TRUE)

    # Never split (keep as single string):
    raw <- sd_value(fav_fruits, as_vector = FALSE)
    # "apple|banana|orange" stays as "apple|banana|orange"

    # Check number of selections:
    if (length(sd_value(fav_fruits)) > 3) {
      # User selected more than 3 fruits
    }

    # Check if specific option was selected:
    if ("apple" %in% sd_value(fav_fruits)) {
      # User selected apple
    }

    sd_server(db = db)
  }
} # }
```

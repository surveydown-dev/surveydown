# Set password for surveydown survey

This function sets your surveydown password, which is used to access the
'PostgreSQL' data (e.g. Supabase). The password is saved in a
`.Renviron` file and adds `.Renviron` to `.gitignore`.

## Usage

``` r
sd_set_password(password)
```

## Arguments

- password:

  Character string. The password to be set for the database connection.

## Value

None. The function is called for its side effects.

## Details

The function performs the following actions:

1.  Creates a `.Renviron` file in the root directory if it doesn't
    exist.

2.  Adds or updates the `SURVEYDOWN_PASSWORD` entry in the `.Renviron`
    file.

3.  Adds `.Renviron` to `.gitignore` if it's not already there.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Set a temporary password for demonstration
  temp_password <- paste0(sample(letters, 10, replace = TRUE), collapse = "")

  # Set the password
  sd_set_password(temp_password)

  # After restarting R, verify the password was set
  cat("Password is :", Sys.getenv('SURVEYDOWN_PASSWORD'))
} # }
```

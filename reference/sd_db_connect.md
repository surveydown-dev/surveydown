# Connect to database

Establish a connection to the database using settings from .env file.
This function creates a connection pool for efficient database access
and provides options for local data storage when needed.

## Usage

``` r
sd_db_connect(env_file = ".env", ignore = FALSE, gssencmode = "auto")
```

## Arguments

- env_file:

  Character string. Path to the env file. Defaults to ".env"

- ignore:

  Logical. If `TRUE`, data will be saved to a local CSV file instead of
  the database. Defaults to `FALSE`.

- gssencmode:

  Character string. The GSS encryption mode for the database connection.
  Defaults to `"auto"`. Options are:

  - `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if
    GSSAPI negotiation fails

  - `"prefer"`: Uses GSSAPI encryption if available, plain connection
    otherwise

  - `"disable"`: Disables GSSAPI encryption entirely NOTE: If you have
    verified all connection details are correct but still cannot access
    the database, try setting this to `"disable"`.

## Value

A list containing the database connection pool (`db`) and table name
(`table`), or `NULL` if ignore is `TRUE` or if connection fails

## Examples

``` r
if (interactive()) {
  # Connect using settings from .env
  db <- sd_db_connect()

  # Use local storage instead of database
  db <- sd_db_connect(ignore = TRUE)

  # Close connection when done
  if (!is.null(db)) {
    pool::poolClose(db$db)
  }
}
```

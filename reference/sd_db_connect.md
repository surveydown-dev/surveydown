# Connect to a PostgreSQL database for storing survey responses

Establishes a connection pool to a PostgreSQL database (e.g. Supabase)
using credentials from a `.env` file. The survey operating mode
(`"database"`, `"preview"`, or `"local"`) is controlled via the `mode`
key under `survey-settings` in `survey.qmd`, not by this function.

## Usage

``` r
sd_db_connect(env_file = ".env", ignore = NULL, gssencmode = "auto")
```

## Arguments

- env_file:

  Character string. Path to the env file. Defaults to `".env"`.

- ignore:

  Logical. Deprecated. Use `mode: preview` in `survey.qmd` YAML instead.
  If `TRUE`, returns `NULL` with a deprecation warning. Defaults to
  `NULL`.

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

A list containing:

- `db`: The database connection pool

- `table`: The database table name

Returns `NULL` if the database connection fails or `ignore = TRUE`.

## Examples

``` r
if (interactive()) {
  db <- sd_db_connect()

  # Close connection when done
  if (!is.null(db$db)) {
    pool::poolClose(db$db)
  }
}
```

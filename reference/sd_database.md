# Connect to a 'PostgreSQL' Database with Automatic Cleanup

This function establishes a connection pool to a 'PostgreSQL' database
(e.g. Supabase) and sets up automatic cleanup when the 'shiny' session
ends.

## Usage

``` r
sd_database(
  host = NULL,
  dbname = NULL,
  port = NULL,
  user = NULL,
  table = NULL,
  password = Sys.getenv("SURVEYDOWN_PASSWORD"),
  gssencmode = "prefer",
  ignore = FALSE,
  min_size = 1,
  max_size = Inf
)
```

## Arguments

- host:

  Character string. The host address of the PostgreSQL database.

- dbname:

  Character string. The name of the PostgreSQL database.

- port:

  Integer. The port number for the PostgreSQL database connection.

- user:

  Character string. The username for the PostgreSQL database connection.

- table:

  Character string. The name of the table to interact with in the
  Supabase database.

- password:

  Character string. The password for the PostgreSQL database connection.
  NOTE: While you can provide a hard-coded password here, we do NOT
  recommend doing so for security purposes. Instead, you should
  establish a password with
  [`surveydown::sd_set_password()`](https://pkg.surveydown.org/reference/sd_set_password.md),
  which will create a local `.Renviron` file that stores your password
  as a `SURVEYDOWN_PASSWORD` environment variable. The `password`
  argument uses this as the default value, so if you set a password
  properly with
  [`surveydown::sd_set_password()`](https://pkg.surveydown.org/reference/sd_set_password.md),
  then you can safely ignore using the `password` argument here.

- gssencmode:

  Character string. The GSS encryption mode for the database connection.
  Defaults to `"prefer"`. NOTE: If you have verified all connection
  details are correct but still cannot access the database, consider
  setting this to `"disable"`. This can be necessary if you're on a
  secure connection, such as a VPN.

- ignore:

  Logical. If `TRUE`, data will be saved to a local CSV file instead of
  the database. Defaults to `FALSE`.

- min_size:

  Integer. The minimum number of connections in the pool. Defaults to 1.

- max_size:

  Integer. The maximum number of connections in the pool. Defaults to
  `Inf`.

## Value

A list containing the database connection pool (`db`) and the table name
(`table`), or `NULL` if in ignore mode or if there's an error.

## Examples

``` r
if (interactive()) {
  # Assuming SURVEYDOWN_PASSWORD is set in .Renviron
  db <- sd_database(
    host   = "aws-0-us-west-1.pooler.supabase.com",
    dbname = "postgres",
    port   = "6---",
    user   = "postgres.k----------i",
    table  = "your-table-name",
    ignore = FALSE
  )

  # Print the structure of the connection
  str(db)

  # Close the connection pool when done
  if (!is.null(db)) {
    pool::poolClose(db$db)
  }
}
```

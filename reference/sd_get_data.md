# Fetch data from a database table with automatic reactivity detection

This function retrieves all data from a specified table in a database.
It automatically detects whether it's being used in a reactive context
(e.g., within a 'shiny' application) and behaves accordingly. In a
reactive context, it returns a reactive expression that automatically
refreshes the data at specified intervals.

## Usage

``` r
sd_get_data(db, table = NULL, refresh_interval = NULL)
```

## Arguments

- db:

  A list containing database connection details created using
  [`sd_db_config()`](https://pkg.surveydown.org/reference/sd_db_config.md).
  Must have elements:

  - `db`: A `DBI` database connection object

  - `table`: A string specifying the name of the table to query

- table:

  Character string. Database table name to obtain data from, overrides
  the table provided in the `db` argument. Defaults to `NULL`.

- refresh_interval:

  Numeric. The time interval (in seconds) between data refreshes when in
  a reactive context. Default is `NULL`, meaning the data will not
  refresh.

## Value

In a non-reactive context, returns a data frame containing all rows and
columns from the specified table. In a reactive context, returns a
reactive expression that, when called, returns the most recent data from
the specified database table.

## Examples

``` r
# Non-reactive context example
if (FALSE) { # \dontrun{
  library(surveydown)

  # Assuming you have a database connection called db created using
  # sd_database(), you can fetch data with:

  data <- sd_get_data(db)
  head(data)

  # Reactive context example (inside a surveydown app)

  server <- function(input, output, session) {
    data <- sd_get_data(db, refresh_interval = 10)

    output$data_table <- renderTable({
      data()  # Note the parentheses to retrieve the reactive value
    })
  }
} # }
```

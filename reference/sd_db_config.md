# Configure database settings

Set up or modify database configuration settings in a .env file. These
settings are used to establish database connections for storing survey
responses.

## Usage

``` r
sd_db_config(
  host = NULL,
  dbname = NULL,
  port = NULL,
  user = NULL,
  table = NULL,
  password = NULL,
  interactive = NULL
)
```

## Arguments

- host:

  Character string. Database host

- dbname:

  Character string. Database name

- port:

  Character string. Database port

- user:

  Character string. Database user

- table:

  Character string. Table name

- password:

  Character string. Database password

- interactive:

  Logical. Whether to use interactive setup. Defaults to TRUE if no
  parameters provided

## Value

Invisibly returns a list of the current configuration settings

## See also

- [`sd_db_connect()`](https://pkg.surveydown.org/reference/sd_db_connect.md)
  to connect to the database

## Examples

``` r
if (interactive()) {
  # Interactive setup
  sd_db_config()

  # Update specific settings
  sd_db_config(table = "new_table")

  # Update multiple settings
  sd_db_config(
    host = "new_host",
    port = "5433",
    table = "new_table"
  )
}

```

# Configure database settings

Set up or modify database configuration settings in a .env file. These
settings are used to establish database connections for storing survey
responses.

## Usage

``` r
sd_db_config(
  url = NULL,
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

- url:

  Character string. A full PostgreSQL connection URL, e.g.
  `"postgresql://user:password@host:port/dbname"`. If provided, the URL
  is parsed to extract `host`, `port`, `dbname`, `user`, and `password`.
  If the password portion is a placeholder (e.g. `[YOUR-PASSWORD]`), you
  will be prompted to enter it. Individual parameters override values
  parsed from the URL.

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

  # Quick setup from Supabase connection URL
  sd_db_config(url = "postgresql://postgres.ref:password@host:6543/postgres")

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

#' Connect to a Supabase Database with Automatic Cleanup
#'
#' This function establishes a connection pool to a Supabase database and sets
#' up automatic cleanup when the Shiny session ends.
#'
#' @param host Character string. The host address of the Supabase database.
#' @param dbname Character string. The name of the Supabase database.
#' @param port Integer. The port number for the Supabase database connection.
#' @param user Character string. The username for the Supabase database
#' connection.
#' @param table Character string. The name of the table to interact with in
#' the Supabase database.
#' @param password Character string. The password for the Supabase database
#' connection. NOTE: While you can provide a hard-coded password here, we do
#' NOT recommend doing so for security purposes. Instead, you should establish
#' a password with `surveydown::sd_set_password()`, which will create a local
#' .Renviron file that stores you password as a SURVEYDOWN_PASSWORD environment
#' variable. The `password` argument uses this as the default value, so if you
#' set a password properly with `surveydown::sd_set_password()`, then you can
#' safely ignore using the `password` argument here.
#' @param gssencmode Character string. The GSS encryption mode for the database
#' connection. Defaults to `"prefer"`. NOTE: If you have verified all
#' connection details are correct but still cannot access the database,
#' consider setting this to `"disable"`. This can be necessary if you're on a
#' secure connection, such as a VPN.
#' @param ignore Logical. If TRUE, data will be saved to a local CSV file instead of the database. Defaults to FALSE.
#' @param min_size Integer. The minimum number of connections in the pool. Defaults to 1.
#' @param max_size Integer. The maximum number of connections in the pool. Defaults to Inf.
#'
#' @return A list containing the database connection pool (`db`) and the table name (`table`),
#'   or NULL if in ignore mode or if there's an error.
#'
#' @examples
#' \dontrun{
#'   # Assuming SURVEYDOWN_PASSWORD is set in .Renviron
#'   db_connection <- sd_database(
#'     host       = "aws-0-us-west-1.pooler.supabase.com",
#'     dbname     = "postgres",
#'     port       = "6---",
#'     user       = "postgres.k----------i",
#'     table = "your-table-name",
#'     ignore      = FALSE
#'   )
#'
#'   # Explicitly providing the password
#'   db_connection <- sd_database(
#'     host       = "aws-0-us-west-1.pooler.supabase.com",
#'     dbname     = "postgres",
#'     port       = "6---",
#'     user       = "postgres.k----------i",
#'     table = "your-table-name",
#'     password   = "your-password",
#'     ignore      = FALSE
#'   )
#' }
#'
#' @export
sd_database <- function(
    host       = NULL,
    dbname     = NULL,
    port       = NULL,
    user       = NULL,
    table      = NULL,
    password   = Sys.getenv("SURVEYDOWN_PASSWORD"),
    gssencmode = "prefer",
    ignore     = FALSE,
    min_size   = 1,
    max_size   = Inf
) {

    if (ignore) {
        message("Database connection ignored. Saving data to local CSV file.")
        return(NULL)
    }

    # Authentication/Checks for NULL Values
    if (is.null(host) | is.null(dbname) | is.null(port) | is.null(user) | is.null(table)) {
        message("One or more of the required parameters are NULL, so the database is NOT connected; writing to local data.csv file instead.")
        return(NULL)
    }

    if (!nchar(password)) {
        stop("Please define your password using surveydown::sd_set_password()")
    }

    tryCatch({
        pool <- pool::dbPool(
            RPostgres::Postgres(),
            host = host,
            dbname = dbname,
            port = port,
            user = user,
            password = password,
            gssencmode = gssencmode,
            minSize = min_size,
            maxSize = max_size
        )

        # Set up automatic cleanup when the Shiny session ends
        shiny::onStop(function() {
            pool::poolClose(pool)
        })

        message("Successfully connected to the database.")
        return(list(db = pool, table = table))
    }, error = function(e) {
        stop(paste("Error: Failed to connect to the database.",
                   "Details:", conditionMessage(e),
                   "\nPlease check your connection details:",
                   "\n- host:    ", host,
                   "\n- dbname:  ", dbname,
                   "\n- port:    ", port,
                   "\n- user:    ", user,
                   "\n- password:", password,
                   "\nTo update password, please use surveydown::sd_set_password().",
                   "\nIf you have verified all connection details are correct but still cannot access the database, consider setting the 'gssencmode' parameter to 'disable' in the sd_database() function."))
    })
}

#' Fetch data from a database table with optional reactivity
#'
#' This function retrieves all data from a specified table in a database.
#' When used in a Shiny application, it can optionally return a reactive
#' expression that automatically refreshes the data at specified intervals.
#'
#' @param db A list containing database connection details. Must have elements:
#'   \itemize{
#'     \item db: A DBI database connection object
#'     \item table: A string specifying the name of the table to query
#'   }
#' @param reactive Logical. If `TRUE`, returns a reactive expression for use in the server.
#'   If `FALSE` (default), returns the data directly.
#' @param refresh_interval Numeric. The time interval (in seconds) between data refreshes
#'   when in reactive mode. Default is `5` seconds. Ignored if reactive is `FALSE`.
#'
#' @return If reactive is `FALSE`, returns a data frame containing all rows and columns
#'   from the specified table. If reactive is TRUE, returns a reactive expression that,
#'   when called, returns the most recent data from the specified database table.
#'
#' @export
#'
#' @examples
#' # Examples here
sd_get_data <- function(db, reactive = FALSE, refresh_interval = 5) {
    if (is.null(db)) {
        warning("Database is not connected, db is NULL")
        return(NULL)
    }
    fetch_data <- function() {
        pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbReadTable(conn, db$table)
        })
    }
    if (reactive) {
        return(shiny::reactive({
            shiny::invalidateLater(refresh_interval * 1000)
            fetch_data()
        }))
    } else {
        return(fetch_data())
    }
}

# Convert to SQL
r_to_sql_type <- function(r_type) {
    switch(toupper(r_type),
           CHARACTER = "TEXT",
           INTEGER = "TEXT",
           DOUBLE = "TEXT",
           LOGICAL = "TEXT",
           FACTOR = "TEXT",
           "TEXT")
}

create_table <- function(data_list, db, table) {
    # Create column definitions
    col_def <- sapply(names(data_list), function(col_name) {
        r_type <- typeof(data_list[[col_name]])
        sql_type <- r_to_sql_type(r_type)
        paste0('"', col_name, '" ', sql_type)
    })

    # Ensure session_id is the first column and set as PRIMARY KEY
    col_def <- c('"session_id" TEXT PRIMARY KEY', col_def[names(col_def) != "session_id"])

    # Join column definitions
    col_def_str <- paste(col_def, collapse = ", ")

    create_table_query <- paste0(
        'CREATE TABLE IF NOT EXISTS "', table, '" (', col_def_str, ")"
    )

    pool::poolWithTransaction(db, function(conn) {
        # Create the table
        DBI::dbExecute(conn, create_table_query)

        # Enable Row Level Security
        DBI::dbExecute(conn, paste0('ALTER TABLE "', table, '" ENABLE ROW LEVEL SECURITY;'))
    })

    message("Table created (or already exists) in your Supabase database.")
}

check_and_add_columns <- function(data_local, db, table) {
    pool::poolWithTransaction(db, function(conn) {
        existing_cols <- DBI::dbListFields(conn, table)
        new_cols <- setdiff(names(data_local), existing_cols)
        for (col in new_cols) {
            r_type <- typeof(data_local[[col]])
            sql_type <- r_to_sql_type(r_type)
            query <- paste0('ALTER TABLE "', table, '" ADD COLUMN "', col, '" ', sql_type, ';')
            DBI::dbExecute(conn, query)
        }
    })
}

database_uploading <- function(data_list, db, table) {
    if(is.null(db)) {
        return(warning("Databasing is not in use"))
    }

    tryCatch({
        pool::poolWithTransaction(db, function(conn) {
            # Get the actual columns in the table
            existing_cols <- DBI::dbListFields(conn, table)

            # Filter data_list to only include existing columns
            data_list <- data_list[names(data_list) %in% existing_cols]

            # Prepare the update query
            cols <- names(data_list)
            update_cols <- setdiff(cols, "session_id")

            # Create value string, properly escaping and quoting values
            values <- sapply(data_list, function(x) {
                if (is.character(x)) {
                    paste0("'", gsub("'", "''", x), "'")
                } else if (is.numeric(x)) {
                    as.character(x)
                } else {
                    "NULL"
                }
            })
            values_str <- paste(values, collapse = ", ")

            update_set <- paste(sapply(update_cols, function(col) {
                paste0('"', col, '" = EXCLUDED."', col, '"')
            }), collapse = ", ")

            update_query <- paste0(
                'INSERT INTO "', table, '" ("', paste(cols, collapse = '", "'), '") ',
                'VALUES (', values_str, ') ',
                'ON CONFLICT (session_id) DO UPDATE SET ',
                update_set
            )

            # Print the query for debugging
            print(update_query)

            # Execute the query
            DBI::dbExecute(conn, update_query)
        })
    }, error = function(e) {
        warning("Error in database operation: ", e$message)
        print(e)  # Print the full error for debugging
    })
}

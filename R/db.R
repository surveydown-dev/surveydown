#' Connect to a supabase Database
#'
#' This function establishes a connection to a supabase database using the provided
#' connection details.
#'
#' @param host Character string. The host address of the supabase database.
#' @param dbname Character string. The name of the supabase database.
#' @param port Integer. The port number for the supabase database connection.
#' @param user Character string. The username for the supabase database connection.
#' @param table Character string. The name of the table to interact with in the supabase database.
#' @param password Character string. The password for the supabase database connection.
#'   Defaults to the value of the SURVEYDOWN_PASSWORD environment variable.
#' @param gssencmode Character string. The GSS encryption mode for the database connection. Defaults to "prefer".
#' @param pause Logical. If TRUE, data will be saved to a local CSV file instead of the database. Defaults to FALSE.
#'
#' @details The function checks for the presence of all required parameters and attempts to
#'   establish a connection to the supabase database. If successful, it returns a list containing
#'   the database connection object and the table name. The user must have created the specified
#'   table in supabase beforehand. If pause mode is enabled, the function returns NULL and data
#'   will be saved to a local CSV file. The password is obtained from the SURVEYDOWN_PASSWORD
#'   environment variable by default, but can be overridden by explicitly passing a value.
#'
#' @return A list containing the database connection object (`db`) and the table name (`table`),
#'   or NULL if in pause mode.
#'
#' @note The user must create their own table inside supabase in order to make additions.
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
#'     pause      = FALSE
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
#'     pause      = FALSE
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
        pause      = FALSE
) {

    if (pause) {
        message("Database connection paused. Saving data to local CSV file.")
        return(NULL)
    }

    # Authentication/Checks for NULL Values
    if (
        is.null(host) |
        is.null(dbname) |
        is.null(port) |
        is.null(user) |
        is.null(table)
    ) {
        message(
            "One or more of the required parameters are NULL, so the database is NOT connected; writing to local data.csv file instead."
        )
        return(NULL)
    }

    if (!nchar(password)) {
        stop("Please define your password using surveydown::sd_set_password()")
    }

    # < Code to handle supabase authentication here >
    #User Must create their own table inside of supabase in order to make additions.
    tryCatch(
        {
            db <- pool::dbPool(
                RPostgres::Postgres(),
                host = host,
                dbname = dbname,
                port = port,
                user = user,
                password = password,
                gssencmode = gssencmode,
                minSize = 1,
                maxSize = Inf
            )
            message("Successfully connected to the database.")
            return(list(db = db, table = table))
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

# Create a new table in the database
create_table <- function(db, table, df) {
    # Loop through the column names
    col_def <- ""

    #Create the col_definitions based on the type
    for (col_name in colnames(df)) {
        r_type <- typeof(df[[col_name]])
        sql_type <- r_to_sql_type(r_type)
        col_def <- paste0(col_def, "\"", col_name, "\" ", sql_type, ", ")
    }

    # Remove the trailing comma and space
    col_def <- substr(col_def, 1, nchar(col_def) - 2)

    create_table_query <- paste0(
        'CREATE TABLE "', table, '" (', col_def, ")"
    )
    pool::poolWithTransaction(db$db, function(conn) {
        # Create the table
        DBI::dbExecute(conn, create_table_query)

        # Enable Row Level Security
        DBI::dbExecute(conn, paste0('ALTER TABLE "', table, '" ENABLE ROW LEVEL SECURITY;'))
    })
    return(message("Database should appear on your supabase Account (Can take up to a minute.)"))
}

# Upload survey data to the database
database_uploading <- function(df, db, table) {
    if(is.null(db)) {
        return(warning("Databasing is not in use"))
    }

    tryCatch({
        pool::poolWithTransaction(db, function(conn) {
            # Check if table exists
            table_exists <- DBI::dbExistsTable(conn, table)

            if (!table_exists) {
                create_table(list(db = db, table = table), table, df)
            } else {
                # Check for new columns and ensure correct order
                existing_cols <- DBI::dbListFields(conn, table)
                new_cols <- setdiff(names(df), existing_cols)

                # Add new columns if any
                for (col in new_cols) {
                    r_type <- typeof(df[[col]])
                    sql_type <- r_to_sql_type(r_type)
                    query <- paste0('ALTER TABLE "', table, '" ADD COLUMN "', col, '" ', sql_type, ';')
                    DBI::dbExecute(conn, query)
                }
            }

            # Read existing data
            data <- DBI::dbReadTable(conn, table)

            matching_rows <- df[df$session_id %in% data$session_id, ]

            if (nrow(matching_rows) > 0) {
                delete_query <- paste0('DELETE FROM "', table, '" WHERE session_id = $1')
                for (session_id in matching_rows$session_id) {
                    DBI::dbExecute(conn, delete_query, params = list(session_id))
                }
                DBI::dbWriteTable(conn, table, matching_rows, append = TRUE, row.names = FALSE)
            } else {
                DBI::dbWriteTable(conn, table, df, append = TRUE, row.names = FALSE)
            }
        })
    }, error = function(e) {
        warning("Error in database operation: ", e$message)
    })
}

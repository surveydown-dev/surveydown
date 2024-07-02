## Establish database ----

#' Connect to a SupaBase Database
#'
#' Establishes a connection to a SupaBase database and if successful returns a list containing
#' the database connection object and the table name (db$db, db$table_name).
#'
#' The parameters' host, db_name, port, and user are derived from the "connect"
#' option on the SupaBase Option.
#'
#' @param host Character. The host address of the SupaBase database.
#' @param db_name Character. Always "postgres".
#' @param port Character. The port number to connect to the database.
#' @param user Character. The username to connect to the database.
#' @param table_name Character. The name of the table to connect to.
#' @param password Character. Your SupaBase account password.
#' @return A list containing the database connection object and the table name.
#' @export
#' @examples
#' \dontrun{
#' db <- sd_database(
#'   host = "aws-0-example-server.pooler.supabase.com",
#'   db_name = "postgres",
#'   port = "6543",
#'   user = "postgres.example",
#'   table_name = "Example_Name",
#'   password = Sys.getenv("SUPABASE_PASSWORD")
#' )
#' }
#'
sd_database <- function(host, db_name, port, user, table_name, password) {

    # Authentication/Checks for NULL Values
    if (is.null(host) || is.null(db_name) || is.null(port) || is.null(user)) {
        stop("Error: One or more required parameters (config, host, db_name, port, user) are NULL.")
    }

    if (!nchar(password)) {
        stop("You must provide your SupaBase password to access the database")
    }

    tryCatch(
        {
            db <-  DBI::dbConnect(
                RPostgres::Postgres(),
                host     = host,
                dbname   = db_name,
                port     = port,
                user     = user,
                password = password
            )
            message("Successfully connected to the database.")
            return(list(db = db, table_name = table_name))
        }, error = function(e) {
            stop("Error: Failed to connect to the database. Please check your connection details.")
        })
}

## Updating Database ----

transform_data <- function(question_vals, timestamp_vals, session_id) {

  # Replace NULLs with empty string, and
  # convert vectors to comma-separated strings
  for (i in seq_len(length(question_vals))) {
    # Check for NULL and replace with an empty string
    val <- question_vals[[i]]
    if (is.null(val)) {
      question_vals[[i]] <- ""
    } else if (length(val) > 1) {
      # Convert vectors to comma-separated strings
      question_vals[[i]] <- paste(question_vals[[i]], collapse = ", ")
    }
  }

  responses <- as.data.frame(question_vals)

  # Add session_id and timestamps
  data <- cbind(session_id, responses, as.data.frame(timestamp_vals))

  return(data)
}

### Database Uploading ----

# Database Creation Section

#Needed to change from R type to SQL type
r_to_sql_type <- function(r_type) {
    switch(toupper(r_type),
           CHARACTER = "TEXT",
           INTEGER = "TEXT",
           DOUBLE = "TEXT",
           LOGICAL = "TEXT",
           FACTOR = "TEXT",
           "TEXT")
}

create_table <- function(db, table_name, df) {
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
        'CREATE TABLE "', table_name, '" (', col_def, ")"
    )
    DBI::dbExecute(db, create_table_query)
    #A precaution to enable RLS
    DBI::dbExecute(db, paste0('ALTER TABLE \"', table_name, '\" ENABLE ROW LEVEL SECURITY;'))
    return(message("Database should appear on your SupaBase Account (Can take up to a minute.)"))
}

database_uploading <- function(df, db, table_name) {
    if(is.null(db)) {
        return(warning("Databasing is not in use"))
    }
    # Establish the database connection
    data <- tryCatch(DBI::dbReadTable(db, table_name), error = function(e) NULL)

    #This actually checks if its empty and will create a brand new table name of your choice
    if (is.null(data)) {
        create_table(db, table_name, df)
    }
    #Table Editing Section
    #Checking For Matching Session_Id's
    matching_rows <- df[df$session_id %in% data$session_id, ]

    if (nrow(matching_rows) > 0) {
        # Delete existing rows in the database table with matching session_id values from df
        DBI::dbExecute(db, paste0('DELETE FROM \"', table_name, '\" WHERE session_id IN (', paste(shQuote(matching_rows$session_id), collapse = ", "), ')'))
        # Append the new non-matching rows to the database table
        DBI::dbWriteTable(db, table_name, matching_rows, append = TRUE, row.names = FALSE)
    } else { #If there are no matching rows we just append the new row.
        DBI::dbWriteTable(db, table_name, df, append = TRUE, row.names = FALSE)
    }
}

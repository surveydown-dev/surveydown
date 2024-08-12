#' Connect to a supabase Database
#'
#' This function establishes a connection to a supabase database using the provided
#' connection details.
#'
#' @param host Character string. The host address of the supabase database.
#' @param db_name Character string. The name of the supabase database.
#' @param port Integer. The port number for the supabase database connection.
#' @param user Character string. The username for the supabase database connection.
#' @param table_name Character string. The name of the table to interact with in the supabase database.
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
#' @return A list containing the database connection object (`db`) and the table name (`table_name`),
#'   or NULL if in pause mode.
#'
#' @note The user must create their own table inside supabase in order to make additions.
#'
#' @examples
#' \dontrun{
#'   # Assuming SURVEYDOWN_PASSWORD is set in .Renviron
#'   db_connection <- sd_database(
#'     host       = "aws-0-us-west-1.pooler.supabase.com",
#'     db_name    = "postgres",
#'     port       = "6---",
#'     user       = "postgres.k----------i",
#'     table_name = "your-table-name",
#'     pause      = FALSE
#'   )
#'
#'   # Explicitly providing the password
#'   db_connection <- sd_database(
#'     host       = "aws-0-us-west-1.pooler.supabase.com",
#'     db_name    = "postgres",
#'     port       = "6---",
#'     user       = "postgres.k----------i",
#'     table_name = "your-table-name",
#'     password   = "your-password",
#'     pause      = FALSE
#'   )
#' }
#'
#' @export

sd_database <- function(
        host       = NULL,
        db_name    = NULL,
        port       = NULL,
        user       = NULL,
        table_name = NULL,
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
        is.null(db_name) |
        is.null(port) |
        is.null(user) |
        is.null(table_name)
    ) {
        message(
            "One or more of the required parameters are NULL, so the database is NOT connected; writing to local data.csv file instead."
        )
        return(NULL)
    }

    if (!nchar(password)) {
        stop("You must provide your supabase password to access the database")
    }

    # < Code to handle supabase authentication here >
    #User Must create their own table inside of supabase in order to make additions.
    tryCatch(
        {
            db <-  DBI::dbConnect(
                RPostgres::Postgres(),
                host       = host,
                dbname     = db_name,
                port       = port,
                user       = user,
                password   = password,
                gssencmode = gssencmode
            )
            message("Successfully connected to the database.")
            return(list(db = db, table_name = table_name))
        }, error = function(e) {
            stop(paste("Error: Failed to connect to the database.",
                       "Details:", conditionMessage(e),
                       "\nPlease check your connection details:)",
                       "\n- host:    ", host,
                       "\n- dbname:  ", db_name,
                       "\n- port:    ", port,
                       "\n- user:    ", user,
                       "\n- password:", password))
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
    return(message("Database should appear on your supabase Account (Can take up to a minute.)"))
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
    } else {
        # Check for new columns
        existing_cols <- DBI::dbListFields(db, table_name)
        new_cols <- setdiff(names(df), existing_cols)

        # Add new columns if any
        for (col in new_cols) {
            r_type <- typeof(df[[col]])
            sql_type <- r_to_sql_type(r_type)
            query <- paste0('ALTER TABLE "', table_name, '" ADD COLUMN "', col, '" ', sql_type, ';')
            DBI::dbExecute(db, query)
        }
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

#' Set Supabase Password
#'
#' This function sets the supabase password in the .Renviron file and adds .Renviron to .gitignore.
#'
#' @param password Character string. The password to be set for Supabase connection.
#'
#' @details The function performs the following actions:
#'   1. Creates a .Renviron file in the root directory if it doesn't exist.
#'   2. Adds or updates the SURVEYDOWN_PASSWORD entry in the .Renviron file.
#'   3. Adds .Renviron to .gitignore if it's not already there.
#'
#' @return None. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#'   sd_set_password("your_SURVEYDOWN_PASSWORD")
#' }
#'
#' @export

sd_set_password <- function(password) {
    # Define the path to .Renviron file
    renviron_path <- file.path(getwd(), ".Renviron")

    # Check if .Renviron file exists, if not create it
    if (!file.exists(renviron_path)) {
        file.create(renviron_path)
    }

    # Read existing content
    existing_content <- readLines(renviron_path)

    # Check if SURVEYDOWN_PASSWORD is already defined
    password_line_index <- grep("^SURVEYDOWN_PASSWORD=", existing_content)

    # Prepare the new password line
    new_password_line <- paste0("SURVEYDOWN_PASSWORD=", password)

    # If SURVEYDOWN_PASSWORD is already defined, replace it; otherwise, append it
    if (length(password_line_index) > 0) {
        existing_content[password_line_index] <- new_password_line
    } else {
        existing_content <- c(existing_content, new_password_line)
    }

    # Write the updated content back to .Renviron
    writeLines(existing_content, renviron_path)

    # Add .Renviron to .gitignore if not already there
    gitignore_path <- file.path(getwd(), ".gitignore")
    if (file.exists(gitignore_path)) {
        gitignore_content <- readLines(gitignore_path)
        if (!".Renviron" %in% gitignore_content) {
            # Remove any trailing empty lines
            while (length(gitignore_content) > 0 && gitignore_content[length(gitignore_content)] == "") {
                gitignore_content <- gitignore_content[-length(gitignore_content)]
            }
            # Add .Renviron to the end without an extra newline
            gitignore_content <- c(gitignore_content, ".Renviron")
            writeLines(gitignore_content, gitignore_path)
        }
    } else {
        writeLines(".Renviron", gitignore_path)
    }

    message("Password set successfully and .Renviron added to .gitignore.")
}

## Establish database ----

sd_database <- function(host, db_name, port, user, table_name, password) {

  # Authentication/Checks for NULL Values
  if (is.null(host) || is.null(db_name) || is.null(port) || is.null(user)) {
    stop("Error: One or more required parameters (config, host, db_name, port, user) are NULL.")
  }

  if (!nzchar(password)) {
    stop("You must provide your SupaBase password to access the database")
  }

  # < Code to handle SupaBase authentication here >
  #User Must create their own table inside of Supabase in order to make additions.
  tryCatch(
    {
       db <-  dbConnect(
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

# Needed to change from R type to SQL type
r_to_sql_type <- function(r_type) {
  switch(toupper(r_type),
         CHARACTER = "TEXT",
         INTEGER = "TEXT",
         DOUBLE = "TEXT",
         LOGICAL = "TEXT",
         FACTOR = "TEXT",
         "TEXT")
}

create_table <- function(db, table_name, column_definitions) {
  create_table_query <- paste0(
    "CREATE TABLE ", table_name, " (", column_definitions, ")"
  )
  DBI::dbExecute(db, create_table_query)
  #A precaution to enable RLS
  DBI::dbExecute(db, paste0("ALTER TABLE ", table_name, " ENABLE ROW LEVEL SECURITY;"))
  return(message("Database should appear on your SupaBase Account (Can take up to a minute.)"))
}

database_uploading <- function(df, db, table_name) {
  if(is.null(db)) {
    return(warning("Databasing is not in use"))
  }
  # Loop through the column names
  col_def <- ""

  #Create the col_definitions based on the type
  for (col_name in colnames(df)) {
    r_type <- typeof(df[[col_name]])
    sql_type <- r_to_sql_type(r_type)
    col_def <- paste0(col_def, col_name, " ", sql_type, ", ")
  }

  # Remove the trailing comma and space
  col_def <- substr(col_def, 1, nzchar(col_def) - 2)

  # Establish the database connection
  data <- tryCatch(DBI::dbReadTable(db, table_name), error = function(e) NULL)

  #This actually checks if its empty and will create a brand new table name of your choice
  if(is.null(data)) {
    create_table(db, table_name, col_def)
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

# Database Editing Section based on a change in column parameters

  #So here I will have to write a loop so see what variables changed and from here we add and delete whats necessary

  #For this loop we will check the datatypes for each var and change accordingly. Instead of using the col_def for table_query
  #We might be able to use it for the alter table command

  # ALTER TABLE table_name
  # ALTER TABLE column_name datatype;

  # Addition
  # ALTER TABLE table_name
  # ADD column_name datatype;
  #
  #
  # Deletion
  # ALTER TABLE table_name
  # DROP COLUMN column_name;

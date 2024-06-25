library(tidyverse)
library(here)
library(gargle)
library(googlesheets4)

#SupaBase Packages
library(DBI)
library(RPostgreSQL)

# usethis::edit_r_environ()


################################################################################

#Primary Solution Service Key Auth - Needs Service Email and
credentials_path <- "Service.json"

setup_auth <- function() {
  gs4_auth(path = credentials_path)
}

setup_auth()

# Sheet ID
ssID <- "1cNZeKg_BjN6fTDPOZtSFk8kxt7hVDsSOCo84xu4eW_U"


################################################################################

#Secondary Solution DeAuth - Only Needs google Sheet URL, Must be public and able to edit.

#gs4_deauth()
#sheet_url <- "URL or Sheet ID goes here"

################################################################################


# Sheet Reading/Editing/Checking Section

# Read the sheet
Sheet <- read_sheet(ss = ssID, range = "A:A")



rows_to_delete <- which(Sheet$session_id %in% df$session_id)

if(nrow(Sheet) == 0) {
  range_write(ssID, data = df)
}

if(length(rows_to_delete) == 0) { #This is meant for first time addition of a row
  range_write(ssID, data = df, range = paste("A", as.character(nrow(Sheet) + 2), sep = ""), col_names = FALSE)
} else {
  range_write(ssID, data = df, range = paste("A", as.character(rows_to_delete[1] + 1), sep = ""), col_names = FALSE) #We have to add one so we can avoid pasting the header
}
################################################################################
#SupaBase Working database alternative

# User and password should be set according to your Supabase settings
#psql -h adresshere.supabase.com -p portNumberHere -d postgres -U usernamehere

#Will have to put all of these variables below into your r_enviorn for security reasons

#Sys.getenv()
#usethis::edit_r_environ()

#Command + Shift + 0 to restart R for changes to take effect if using a mac.

df <- read_csv(here('data.csv'))

colnames(df) <- tolower(colnames(df))

db <-
  dbConnect(
    RPostgres::Postgres(),
    host = "aws-0-us-west-1.pooler.supabase.com",
    dbname = "postgres",
    port = "6543",
    user = "postgres.kjxxpyqplqvtqqxrxmzi",
    password = Sys.getenv("SUPABASE_PASSWORD")
  )

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
  DBI::dbExecute(db, paste0("ALTER TABLE ", table_name, " ENABLE ROW LEVEL SECURITY;"))
  return(message("Database should appear on your SupaBase Account."))
}

database_uploading <- function(df, db, table_name) {
  db <- sd_database()

  if(is.null(db$db)) {
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
  col_def <- substr(col_def, 1, stringr::str_length(col_def) - 2)

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
#New section that works to modify the table based on column changes.
#This will be split into 3 sections, additions-deletions-type changes.


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





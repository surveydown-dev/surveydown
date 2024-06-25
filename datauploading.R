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
credentials_path <- "surveydown/Service.json"

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
df <- read_csv(here('data.csv'))

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


db <-
  dbConnect(
    RPostgres::Postgres(),
    host = "aws-0-us-west-1.pooler.supabase.com",
    dbname = "postgres",
    port = "6543",
    user = "postgres.kjxxpyqplqvtqqxrxmzi",
    password = Sys.getenv("SupaPass")
  )


# Attempt to connect to the database
data1 <- dbReadTable(db, "scratch")

column_names <- c("session_id", "user_id", "timestamp", "event_type", "event_details")

# Create an empty tibble (data frame) with the specified column names
column_definitions <- "
  session_id TEXT,
  user_id TEXT,
  timestamp TIMESTAMP,
  event_type TEXT,
  event_details TEXT
"


r_to_sql_type <- function(r_type) {
  switch(str_to_upper(r_type),
         CHARACTER = "TEXT",
         INTEGER = "INTEGER",
         DOUBLE = "REAL",
         LOGICAL = "BOOLEAN",
         FACTOR = "TEXT",
        )
}


# Loop through the column names
# Initialize an empty string to hold the column definitions
col_def <- ""


for (col_name in colnames(df)) {
  r_type <- typeof(df[[col_name]])
  sql_type <- r_to_sql_type(r_type)
  col_def <- paste0(col_def, col_name, " ", sql_type, ", ")
}

# Remove the trailing comma and space
col_def <- substr(col_def, 2, str_length(col_def) - 2)




# Define the function to create the table
create_table <- function(db, table_name, column_definitions) {
  create_table_query <- paste0(
    "CREATE TABLE ", table_name, " (", column_definitions, ")"
  )
  dbExecute(db, create_table_query)
  dbExecute(db, paste0("ALTER TABLE ", table_name, " ENABLE ROW LEVEL SECURITY;"))
}

# Check for non-matching session_id values
matching_rows <- df[df$session_id %in% data$session_id, ]

if (nrow(matching_rows) > 0) {
  # Delete existing rows in the database table with matching session_id values from df
  dbExecute(db, paste0('DELETE FROM \"', db_tableName, '\" WHERE session_id IN (', paste(shQuote(matching_rows$session_id), collapse = ", "), ')'))

  # Append the new non-matching rows to the database table
  dbWriteTable(db, "Actual", matching_rows, append = TRUE, row.names = FALSE)
} else {
  dbWriteTable(db, "Actual", df, append = TRUE, row.names = FALSE)
}

# Disconnect from the database
dbDisconnect(db)

db_tableName <- "Actual"
#Writing to SupaBase
#dbWriteTable(db, "Actual", df, append = TRUE, row.names = FALSE)

#Disconnect the database right after the .csv is sent
dbDisconnect(db)


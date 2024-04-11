library(tidyverse)
library(here)
library(googlesheets4)

df <- read_csv(here('data.csv'))

credentials <- jsonlite::read_json("secret.json")


# Replace these values with your actual credentials
#client_id <- credentials$web$client_id
#client_secret <- credentials$web$client_secret
#refresh_token <- credentials$web$token_uri
#
# Authenticate with Google Sheets using OAuth credentials
#gs4_auth(
#  email = gargle::gargle_oauth_email(),
#  path = NULL,
#  subject = NULL,
#  scopes = "spreadsheets",
#  cache = gargle::gargle_oauth_cache(),
#  use_oob = gargle::gargle_oob_default(),
#  token = NULL
#)




#Everything Below is meant for Row editing


sheet_url <- "https://docs.google.com/spreadsheets/d/12f5iLaXlXqkWz3pWs6fLLPX6PfiiVxuQ_BzJqKfgacQ/edit#gid=798378927"

Sheet <- read_sheet(sheet_url, range = "A:A")

#Spread1 <- gs4_create("Sheets1", sheets = df) #Creation / Only needs to be done once

rows_to_delete <- which(Sheet$session_id %in% df$session_id)

if(length(rows_to_delete) == 0) { #This is meant for first time addition of a row
  range_write(sheet_url, data = df, range = paste("A", as.character(nrow(Sheet) + 2), sep = ""), col_names = FALSE)
} else {
  range_write(sheet_url, data = df, range = paste("A", as.character(rows_to_delete[1] + 1), sep = ""), col_names = FALSE) #We have to add one so we can avoid pasting the header
}








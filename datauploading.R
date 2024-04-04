library(tidyverse)
library(here)
library(googlesheets4)

df <- read_csv(here('data.csv'))

(original_client <- gs4_oauth_client())

(original_api_key <- gs4_api_key())

path_to_json <- system.file(
  "extdata", "secret.json",
  package = "gargle"
)
gs4_auth_configure(path = path_to_json)




sheet_url <- "https://docs.google.com/spreadsheets/d/19S2CJ73N7ISJz8HyO7oclxvKV_N_OAEl5wJE_yQ8VGg/edit#gid=339310633"

Sheet <- read_sheet(sheet_url, range = "A:A")

#Spread1 <- gs4_create("Sheets1", sheets = df) #Creation / Only needs to be done once

rows_to_delete <- which(Sheet$session_id %in% df$session_id)

range_write(sheet_url, data = df, range = paste("A", as.character(rows_to_delete[1] + 1), sep = ""), col_names = FALSE) #We have to add one so we can avoid pasting the header








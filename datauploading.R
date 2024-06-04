library(tidyverse)
library(here)
library(googlesheets4)

df <- read_csv(here('surveydown/data.csv'))







#Primary Solution Service Key Auth - Needs Service Email and
credentials_path <- "documents/GitHub/surveydown/Service.json"





#URl reading/Sheet Editing

sheet_url <- "https://docs.google.com/spreadsheets/d/12f5iLaXlXqkWz3pWs6fLLPX6PfiiVxuQ_BzJqKfgacQ/edit#gid=798378927" #Delete this once I get the TryCatch working



Sheet <- read_sheet(sheet_url, range = "A:A")

#
#create_object <- function() {
#  stop()
#}
#
#
## Sheet ID
#ssID <- "1cNZeKg_BjN6fTDPOZtSFk8kxt7hVDsSOCo84xu4eW_U"
#tryCatch(
#  {
#    sheet_data <- read_sheet(NULL, range = "A:A") #I have to now replace this NULL with get_sheet_url
#  },
#  error = function(e) {
#    Spread1 <- gs4_create("Sheets1", sheets = df)
#  }
#)
#




rows_to_delete <- which(Sheet$session_id %in% df$session_id)

if(nrow(Sheet) == 0) {
  range_write(ssID, data = df)
}

if(length(rows_to_delete) == 0) { #This is meant for first time addition of a row
  range_write(ssID, data = df, range = paste("A", as.character(nrow(Sheet) + 2), sep = ""), col_names = FALSE)
} else {
  range_write(ssID, data = df, range = paste("A", as.character(rows_to_delete[1] + 1), sep = ""), col_names = FALSE) #We have to add one so we can avoid pasting the header
}








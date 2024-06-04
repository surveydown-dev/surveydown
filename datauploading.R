library(tidyverse)
library(here)
library(googlesheets4)
library(gargle)



################################################################################

#Primary Solution Service Key Auth - Needs Service Email and
credentials_path <- "documents/GitHub/surveydown/Service.json"

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
df <- read_csv(here('documents/GitHub/surveydown/data.csv'))

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








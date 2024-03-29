library(tidyverse)
library(here)
library(googlesheets4)

df <- read_csv(here('surveydown', 'data.csv'))

#So would it be better for the creator to create their own sheet,
#Then add it here so the csv results for the survey can just be appened.
#Instead of creating a new survery everytime with gs4_create
#we do sheet_append()


#Practice Creation

Spread1 <- gs4_create("Sheets1", sheets = df) #Creation 

Sheet <- read_sheet(Spread1, range = "A:A")

rows_to_delete <- which(Sheet$session_id %in% df$session_id)

range_write(Spread1, data = df, range = paste0("A", as.character(rows_to_delete)))

#Things to Do, make it so other people can actually edit the rows instead of having direct
#access like I do



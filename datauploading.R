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

Spread1 %>% 
  sheet_append(df) #Appends survey result


library(tidyverse)
library(surveydown)

# Change dplyr settings so I can view all columns
options(dplyr.widtkh = Inf)

# This code obtains the raw data table from the supabase database
# This is done just once, then stored locally
# We clean the data the next file (3_data_cleaning.R)

# The settings below are specific to the supabase database we made
# for this demo. If you want to replicate this, you'll need to change
# these settings to your supabase database
db <- sd_database(
  host   = "aws-0-us-east-1.pooler.supabase.com",
  dbname = "postgres",
  port   = "5678",
  user   = "postgres.axzkymswaxcasjdflkurrj",
  table  = 'my_table'
)

df <- sd_get_data(db)
write_csv(df, here::here('data', 'data_raw.csv'))

# Load libraries
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)

# Change dplyr settings so I can view all columns
options(dplyr.widtkh = Inf)


### Obtain data_raw from the supabase database
### This is done just once, then stored locally
#
# db <- sd_database(
#   host   = "aws-0-us-east-1.pooler.supabase.com",
#   dbname = "postgres",
#   port   = "6543",
#   user   = "postgres.tnafzwpqajxvcjlqlhkl",
#   table  = "pilot_survey",
#   gssencmode = "disable"
# )
#
# df <- sd_get_data(db)
# write_csv(df, here::here('data', 'data_raw.csv'))


# Import raw data
data_raw <- read_csv(here("data", "data_raw.csv"))

# Format and join the three surveys -------

# Some special variables:
# session_id = a unique ID for the Run - should be the same across all surveys
# time_start = time stamp when survey was started
# time_end   = time stamp when survey ended
# time_p_*** = Time page *** was reached
# time_q_*** = Time question *** was last answered

# Compute time values for each page
data <- data_raw %>%
  mutate(
    # Compute time through whole survey
    time_start = ymd_hms(time_start, tz = "EST"),
    time_end =  ymd_hms(time_end, tz = "EST"),
    time_total = as.numeric(time_end - time_start, units = "secs"),
    # Compute time through just the cbc questions
    time_p_cbc_q1 =  ymd_hms(time_p_cbc_q1, tz = "EST"),
    time_p_cbc_q6 =  ymd_hms(time_p_cbc_q6, tz = "EST"),
    time_cbc_total = as.numeric(time_p_cbc_q6 - time_p_cbc_q1, units = "secs")
  ) %>%
  # Select important columns
  select(
    session_id, time_total, time_cbc_total, respID, screenout, starts_with("cbc"),
    apple_knowledge_1:feedback
  )

head(data)



# Filter out bad responses ---------

nrow(data)

# Drop people who got screened out
data <- data %>%
    filter(!is.na(screenout), screenout == "red")
nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>%
  filter(!is.na(cbc_q1)) %>%
  filter(!is.na(cbc_q2)) %>%
  filter(!is.na(cbc_q3)) %>%
  filter(!is.na(cbc_q4)) %>%
  filter(!is.na(cbc_q5)) %>%
  filter(!is.na(cbc_q6))
nrow(data)

# Drop anyone who answered the same question for all choice questions
data <- data %>%
  mutate(cbc_all_same =
     (cbc_q1 == cbc_q2) &
     (cbc_q2 == cbc_q3) &
     (cbc_q3 == cbc_q4) &
     (cbc_q4 == cbc_q5) &
     (cbc_q5 == cbc_q6)
  ) %>%
  filter(!cbc_all_same) %>%
  select(-cbc_all_same)
nrow(data)

# Drop respondents who went too fast
data <- data %>%
    mutate(
        # Convert time to minutes
        time_min_total = time_total / 60,
        time_min_cbc = time_cbc_total / 60
    )

# Look at summary of completion times
summary(data$time_min_total)
summary(data$time_min_cbc)

# Drop anyone who finished the choice question section in under 0.1 minutes
data <- data %>%
    filter(time_min_total >= 0.1)
nrow(data)


# Create choice data ---------

# First convert the data to long format
choice_data <- data %>%
    pivot_longer(
        cols = cbc_q1:cbc_q6,
        names_to = "qID",
        values_to = "choice") %>%
    # Convert the qID variable and choice column to a number
    mutate(
      qID = parse_number(qID),
      choice = parse_number(choice)
    )

head(choice_data)

# Read in choice questions and join it to the choice_data
survey <- read_csv("choice_questions.csv")
choice_data <- choice_data %>%
    left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen
choice_data <- choice_data %>%
    mutate(choice = ifelse(choice == altID, 1, 0)) %>%
    # Drop unused variables
    select(-image)

head(choice_data)

# Create new values for respID & obsID
nRespondents <- nrow(data)
nAlts <- max(survey$altID)
nQuestions <- max(survey$qID)
choice_data$respID <- rep(seq(nRespondents), each = nAlts*nQuestions)
choice_data$obsID <- rep(seq(nRespondents*nQuestions), each = nAlts)

# Reorder columns - it's nice to have the "ID" variables first
choice_data <- choice_data %>%
    select(ends_with("ID"), "choice", everything())

head(choice_data)

# Save cleaned data for modeling
write_csv(choice_data, here("data", "choice_data.csv"))

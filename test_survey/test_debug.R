#!/usr/bin/env Rscript

# Load the package
library(surveydown)

# Test database connection
cat("Testing database connection...\n")
db <- sd_db_connect()
cat("db is NULL:", is.null(db), "\n")

if (is.null(db)) {
  cat("Database connection is NULL - this explains why session persistence isn't working!\n")
  cat("When db is NULL, sd_store_value() will always use the new sampled value.\n")
} else {
  cat("Database connection successful\n")
  cat("db$table:", db$table, "\n")
}
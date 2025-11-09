# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

**surveydown** is an R package that creates markdown-based programmable
surveys using Quarto, Shiny, and PostgreSQL. It enables researchers to
define surveys in plain text (survey.qmd) and deploy them as interactive
Shiny applications with data stored in PostgreSQL databases.

## Core Architecture

The package follows a dual-file survey structure: - `survey.qmd`: Quarto
document containing survey content (pages, questions, etc.) - `app.R`:
Shiny application with global settings, database configuration, and
server logic

### Key Components

- **UI System** (`R/ui.R`):
  [`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md) creates the
  Shiny UI, reading theme and progress bar settings from survey.qmd YAML
  headers
- **Server Logic** (`R/server.R`):
  [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
  handles conditional display, progress tracking, page navigation, and
  database operations
- **Configuration** (`R/config.R`): `run_config()` parses survey.qmd
  files and extracts survey structure
- **Database Interface** (`R/db.R`): PostgreSQL integration with support
  for Supabase
- **Utilities** (`R/util.R`): Helper functions for survey processing and
  validation

### Survey Flow

1.  Survey content is defined in a Quarto document (`survey.qmd`)
2.  `run_config()` parses the .qmd file and extracts pages/questions
    into `_survey/` folder
3.  [`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md) creates
    the Shiny interface with theme and progress bar settings
4.  [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
    manages user interactions, conditional logic, and data storage

## Development Commands

### Package Development

``` r
# Load package for development
devtools::load_all()

# Generate documentation
devtools::document()

# Install package locally
devtools::install(force = TRUE)

# Run package checks
devtools::check()

# Build pkgdown documentation site
pkgdown::build_site()
```

### Testing

``` r
# Run tests (currently minimal)
library(testthat)
library(surveydown)
# test_check("surveydown")  # Commented out in testthat.R
```

### Example Survey

Test the package using the example survey in `test_survey/app.R`:

``` r
# From test_survey/ directory
shiny::runApp()
```

## Database Integration

The package uses PostgreSQL databases with built-in Supabase support.
Database connections are configured via: -
[`sd_db_config()`](https://pkg.surveydown.org/reference/sd_db_config.md):
Store database credentials in .env file -
[`sd_db_connect()`](https://pkg.surveydown.org/reference/sd_db_connect.md):
Establish database connection - Set `ignore = TRUE` for local testing
without database storage

## Key Functions

- [`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md): Generate
  survey UI from survey.qmd
- [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md):
  Main server logic with conditional display, progress tracking
- [`sd_skip_if()`](https://pkg.surveydown.org/reference/sd_skip_if.md):
  Define conditional skip logic
- [`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md):
  Database connection configuration
- `run_config()`: Parse and process survey content

## File Structure

- `R/`: Core package source code (ui.R, server.R, config.R, db.R,
  util.R, translation.R)
- `inst/`: Package resources and example files
- `man/`: Generated documentation
- `test_survey/`: Example survey application
- `tests/`: Test suite (currently minimal)
- `build.R`: Development build script

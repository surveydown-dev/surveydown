# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

surveydown is an R package for creating markdown-based programmable surveys using R, Quarto, Shiny, and PostgreSQL databases. The package allows users to define surveys in Quarto documents (`survey.qmd`) and render them as Shiny applications (`app.R`) with database-backed response storage.

## Architecture

### Core Components

- **R/**: Main package functions
  - `ui.R`: Creates Shiny UI from Quarto survey files, handles themes and progress bars
  - `server.R`: Server-side logic for conditional display, navigation, database operations
  - `db.R`: Database connection and operations (PostgreSQL/Supabase)
  - `config.R`: Configuration management and environment setup
  - `dashboard.R`: Admin dashboard for viewing survey responses
  - `translation.R`: Multi-language support system

- **inst/template/**: Survey template files
  - `app.R`: Template Shiny application structure
  - `survey.qmd`: Example Quarto survey document

- **inst/js/**: JavaScript modules for client-side functionality
  - Auto-scroll, progress updates, cookies, countdown timers, navigation, required question highlighting

### Survey Structure

Surveys consist of two main files:
1. `survey.qmd`: Quarto document with survey content, questions, and pages
2. `app.R`: Shiny application with database config, conditional logic, and server setup

Questions are defined using `sd_question()` functions within Quarto code chunks, and pages are wrapped in `::: {.sd_page id=page_id}` divs.

## Development Commands

### Package Development
```r
# Load package for development
devtools::load_all()

# Generate documentation from roxygen2 comments
devtools::document()

# Run tests
devtools::test()

# Check package for CRAN compliance
devtools::check()

# Install package locally
devtools::install()

# Build documentation site
pkgdown::build_site()
```

### Testing
Tests are located in `tests/` but currently minimal. The main test runner:
```r
library(testthat)
library(surveydown)
test_check("surveydown")
```

## Key Functions

- `sd_ui()`: Creates survey UI from Quarto files
- `sd_server()`: Main server logic with conditional display/skip logic
- `sd_question()`: Defines survey questions with various types (text, select, numeric, etc.)
- `sd_database()`: Database connection setup
- `sd_skip_forward()`: Conditional page skipping logic
- `sd_show_if()`: Conditional question display logic

## Dependencies

Main dependencies include:
- `shiny`: Web application framework
- `quarto`: Document rendering
- `RPostgres`/`DBI`: Database connectivity
- `bslib`: Bootstrap theming
- `shinyjs`: JavaScript interactions
- `pool`: Database connection pooling

## Database Integration

The package is designed to work with PostgreSQL databases, with special support for Supabase. Database configuration is managed through `.env` files and the `sd_db_config()` function.

## Required Question Highlighting

The package includes functionality to highlight unanswered required questions when users attempt to navigate to the next page. Key components:

- **CSS classes**: `.required-question-highlight` styles in `surveydown.css`
- **JavaScript**: `highlight_required.js` handles client-side highlighting and scrolling
- **Server logic**: `get_unanswered_required()` function identifies specific unanswered questions
- **Integration**: Questions are highlighted when validation fails on Next button click

The system automatically clears highlights when users start answering questions or successfully navigate pages.
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**surveydown** is an R package that creates markdown-based programmable surveys using Quarto, Shiny, and PostgreSQL. It enables researchers to define surveys in plain text (survey.qmd) and deploy them as interactive Shiny applications with data stored in PostgreSQL databases.

## Core Architecture

### Dual-File Survey Structure

Every surveydown survey consists of two required files:
- `survey.qmd`: Quarto document containing survey content (pages, questions, conditional logic)
- `app.R`: Shiny application with database configuration and server logic

### Build Process and the `_survey/` Folder

The package follows a **parse-once, use-many** pattern:

1. **Parse Phase** (triggered by `sd_ui()` and `run_config()`):
   - `survey.qmd` is rendered to HTML via Quarto
   - HTML is parsed to extract pages, questions, and structure
   - Results are saved to `_survey/` folder as:
     - `survey.html`: Rendered HTML content
     - `head.rds`: Extracted head content (CSS, JS dependencies)
     - `pages.rds`: Page structure with IDs, questions, and navigation
     - `questions.yml`: Question metadata (types, options, labels)
     - `settings.yml`: Server configuration (auto-generated, do not edit)
     - `translations.yml`: UI text translations

2. **Cache Invalidation**:
   - Files are re-parsed only when `survey.qmd`, `app.R`, or translation YAML files are modified
   - The `survey_files_need_updating()` and `survey_needs_updating()` functions check timestamps
   - This prevents unnecessary re-rendering during development

3. **Runtime**:
   - Shiny loads pre-parsed content from `_survey/` folder
   - This makes survey loading fast and separates content from logic

### Key Components

- **UI System** (`R/ui.R`):
  - `sd_ui()` creates the Shiny UI
  - Reads theme, progress bar, and footer settings from survey.qmd YAML
  - Manages Quarto rendering and HTML extraction
  - Handles resource path setup for CSS/JS

- **Server Logic** (`R/server.R`):
  - `sd_server()` is the main server function
  - Implements conditional display via `sd_show_if()`
  - Implements skip logic via `sd_skip_if()`
  - Tracks progress and manages page navigation
  - Handles database writes and session management
  - Supports cookie-based session restoration

- **Configuration** (`R/config.R`):
  - `run_config()` parses survey.qmd and extracts structure
  - `extract_html_pages()` processes page divs with `.sd-page` or `.sd_page` class
  - `extract_question_structure_html()` builds question metadata
  - Settings YAML management with precedence: `sd_server()` args > YAML header > defaults
  - `detect_sd_server_params()` parses app.R to detect overrides

- **Database Interface** (`R/db.R`):
  - PostgreSQL integration with connection pooling
  - `sd_database()` configures connection in app.R
  - `sd_db_config()` manages credentials in `.env` file
  - `sd_db_connect()` establishes connection (supports `ignore = TRUE` for local testing)
  - Built-in Supabase support

- **Utilities** (`R/util.R`):
  - File existence checks (`check_files_missing()`)
  - Markdown-to-HTML conversion for question labels/options
  - Resource path management in `.onAttach()`
  - Validation functions for IDs and required questions

- **Translation** (`R/translation.R`):
  - Multi-language support for UI text
  - Default translations: English, German, Spanish, French, Italian, Chinese
  - Users can override via `translations.yml` file
  - Language selection via `system_language` parameter

### Survey Flow

1. User defines survey content in `survey.qmd` with markdown and R code chunks
2. User defines server configuration in `app.R` (database, skip logic, etc.)
3. On app launch:
   - `sd_ui()` checks for changes and renders/parses survey.qmd if needed
   - Content is cached in `_survey/` folder
   - Shiny UI is generated with theme and progress bar
4. During session:
   - `sd_server()` manages page navigation and conditional logic
   - Responses are stored in PostgreSQL database or local CSV
   - Progress bar updates based on answered questions
5. On completion:
   - Optional rating question via `rate_survey = TRUE`
   - Session data is finalized and written to database

### Question System

Questions are defined in survey.qmd using shiny input functions with markdown support:

- Multiple choice: `mc`, `mc_buttons`, `mc_multiple`, `mc_multiple_buttons`
- Text inputs: `text`, `textarea`, `numeric`
- Selections: `select`, `slider`, `slider_numeric`
- Date inputs: `date`, `daterange`
- Matrix questions: Multiple questions in table format
- Custom questions: `sd_question_custom()` for reactive outputs (e.g., leaflet maps)

The question structure is extracted from HTML and stored in `questions.yml` with:
- `type`: Question type
- `label`: Question text (may be empty for reactive questions)
- `options`: Named list of choices (for select/mc questions)
- `is_matrix`: Boolean indicator

### Configuration Precedence

Settings can be defined in multiple places with this precedence (highest to lowest):

1. **`sd_server()` function parameters** in app.R (highest priority)
2. **YAML header** in survey.qmd (e.g., `use-cookies: false`)
3. **Default values** hardcoded in the package

The `settings.yml` file in `_survey/` reflects the final resolved configuration.

## Development Commands

### Package Development

```r
# Load package for development (run from package root)
devtools::load_all()

# Generate documentation from roxygen comments
devtools::document()

# Install package locally
devtools::install(force = TRUE)

# Run package checks (CRAN compliance)
devtools::check()

# Build pkgdown documentation site
pkgdown::build_site()

# Complete build workflow (see build.R)
source("build.R")
```

### Testing

```r
# Run tests (currently minimal - test_check is commented out)
library(testthat)
library(surveydown)
# test_check("surveydown")  # Tests need development
```

### Example Survey Development

```r
# From test_survey/ directory
setwd("test_survey")
shiny::runApp()

# Or from package root
shiny::runApp("test_survey")
```

The `test_survey/` folder contains a working example with a custom leaflet map question.

### Database Setup for Testing

```r
# Configure database credentials (creates/updates .env file)
sd_db_config()

# Test with local CSV storage (no database)
db <- sd_db_connect(ignore = TRUE)

# Connect to actual PostgreSQL database
db <- sd_db_connect(
  host = "your-host",
  dbname = "your-db",
  port = "5432",
  user = "your-user",
  password = "your-password",
  table = "responses"
)
```

## Important Architecture Patterns

### ID System

- **Page IDs**: Defined in `:::  {.sd-page id=page_name}` divs
- **Question IDs**: First parameter to shiny input functions
- **Reserved IDs**: `session_id`, `time_start`, `time_end`, `exit_survey_rating`, `current_page`, `browser`, `ip_address`
- All IDs must be unique across pages and questions
- `check_ids()` validates at startup

### Conditional Logic

Two types of conditional behavior:

1. **Skip Logic** (`sd_skip_if()`): Jumps to a different page based on answer
   - Targets must be valid page IDs
   - Defined in server function

2. **Show/Hide Logic** (`sd_show_if()`): Conditionally displays questions or pages
   - Targets can be question IDs or page IDs
   - Initially hidden elements use `display: none;` style
   - Defined in server function

### Required Questions

Three ways to mark questions as required:

1. `all_questions_required = TRUE` in `sd_server()` (excludes matrix parent questions)
2. `required_questions = c("q1", "q2")` in `sd_server()`
3. `required-questions: [q1, q2]` in survey.qmd YAML header

Required questions show an asterisk and block page navigation until answered.

### Resource Management

The package includes static resources in `inst/`:
- `css/`: surveydown.css, default_theme.css
- `js/`: auto_scroll.js, cookies.js, highlight_unanswered.js, keep_alive.js, etc.

These are automatically added to Shiny's resource path in `.onAttach()`.

## Key Design Decisions

1. **Files must be named exactly**: `survey.qmd` and `app.R` (enforced by `check_files_missing()`)
2. **Mixed page classes not allowed**: Use either `.sd-page` OR `.sd_page`, not both
3. **Matrix subquestions are auto-generated**: Don't manually create IDs like `matrix_q_row1`
4. **Progress never decreases**: Based on last answered question index, not percentage complete
5. **Settings YAML is read-only**: Generated by the system, manual edits will be overwritten
6. **Translation language selection**: Uses `system_language` (not `language` which is deprecated)

## File Structure Notes

- `R/`: Core package source (6 main files: config.R, db.R, server.R, translation.R, ui.R, util.R)
- `inst/`: Package resources (js/, css/, examples/)
- `man/`: Generated documentation (do not edit manually)
- `test_survey/`: Working example with custom leaflet map
- `tests/`: Test suite (currently minimal, needs development)
- `build.R`: Development build script for complete workflow
- `.env`: Database credentials (git-ignored, created by `sd_db_config()`)
- `_survey/`: Generated cache folder (git-ignored, auto-created)

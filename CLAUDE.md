# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Overview

**surveydown** is an R package for creating markdown-based programmable
surveys using Quarto, Shiny, and PostgreSQL databases. The package
enables researchers to define surveys in plain text (`.qmd` and `.R`
files), making them version-controllable, reproducible, and
collaborative.

The core workflow: 1. Users design surveys in `survey.qmd` (Quarto
markdown with R code chunks) 2. Users configure app behavior in `app.R`
(Shiny app with database settings) 3. Package renders the survey to a
Shiny app that stores responses in PostgreSQL (Supabase recommended)

## Development Commands

### Package Development

``` r
# Load package during development
devtools::load_all()

# Generate documentation from roxygen comments
devtools::document()

# Install package locally
devtools::install(force = TRUE)

# Run R CMD check
devtools::check()
```

### Documentation

``` r
# Build pkgdown site
pkgdown::build_site()
```

### Testing

Survey functionality is typically tested manually using the
`test_survey/` directory, which contains a full survey example with
`app.R` and `survey.qmd` files. To test changes:

1.  Make changes to package source files in `R/`
2.  Run `devtools::load_all()` to reload the package
3.  Navigate to `test_survey/` and run the survey app
4.  Verify changes work as expected

## Architecture

### Core Components

The package has a clear separation of concerns across several R files:

- **`R/ui.R`**: Contains
  [`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md) - the main
  UI function that:
  - Renders the `survey.qmd` file using Quarto
  - Extracts and processes HTML content
  - Creates the Shiny UI with CSS/JS dependencies
  - Manages theme settings, progress bar, and footer configuration
- **`R/server.R`**: Contains
  [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md) -
  the main server function that:
  - Handles page navigation and skip logic
  - Manages question state (answered/required)
  - Updates progress bar based on completion
  - Stores responses to database or local CSV
  - Implements conditional display logic via
    [`sd_show_if()`](https://pkg.surveydown.org/reference/sd_show_if.md)
  - Handles exit survey functionality
- **`R/config.R`**: Contains `run_config()` - the configuration
  processor that:
  - Parses the rendered `survey.html` file
  - Extracts pages and questions into structured data
  - Validates IDs and required questions
  - Manages the `_survey/` folder cache system
  - Creates `settings.yml` with theme-settings, survey-settings, and
    system-messages
- **`R/db.R`**: Database functions including:
  - [`sd_db_config()`](https://pkg.surveydown.org/reference/sd_db_config.md):
    Interactive database configuration setup
  - [`sd_db_connect()`](https://pkg.surveydown.org/reference/sd_db_connect.md):
    PostgreSQL connection management (Supabase support)
  - [`sd_database()`](https://pkg.surveydown.org/reference/sd_database.md):
    Database object creation
  - Functions for storing and retrieving survey responses
- **`R/util.R`**: Utility functions for:
  - Question type implementations
    ([`sd_question()`](https://pkg.surveydown.org/reference/sd_question.md))
  - Page navigation
    ([`sd_nav()`](https://pkg.surveydown.org/reference/sd_nav.md),
    [`sd_close()`](https://pkg.surveydown.org/reference/sd_close.md))
  - Conditional logic helpers
  - Markdown to HTML conversion
  - Resource path management
- **`R/messages.R`**: Multi-language system message support

### The `_survey/` Folder

When a survey runs, the package creates a `_survey/` folder that caches
parsed survey content:

- `survey.html`: Rendered HTML from `survey.qmd`
- `head.rds`: Extracted HTML head content
- `pages.rds`: Structured page data
- `questions.yml`: Question metadata and structure
- `settings.yml`: Complete configuration (theme-settings,
  survey-settings, system-messages)

This caching system prevents re-rendering on every app reload. Changes
to `survey.qmd` or `app.R` trigger re-parsing.

### Frontend Assets

- **`inst/js/`**: JavaScript modules for:

  - `auto_scroll.js`: Auto-scroll to next question
  - `cookies.js`: Cookie-based session management
  - `highlighting.js`: Unanswered question highlighting
  - `interaction.js`: UI interactions
  - `progressbar.js`: Progress tracking
  - `clipboard.js`: Copy functionality
  - `countdown.js`: Countdown timers
  - `keep_alive.js`: Session persistence

- **`inst/css/`**: Stylesheet files:

  - `surveydown.css`: Main package styles
  - `default_theme.css`: Default theme

- **`inst/template/`**: Default survey template with `survey.qmd` and
  `app.R`

- **`inst/examples/`**: Example `.qmd` files demonstrating different
  features

### Question Types

The
[`sd_question()`](https://pkg.surveydown.org/reference/sd_question.md)
function supports multiple question types defined in `R/util.R`: - `mc`:
Single choice (radio buttons) - `mc_multiple`: Multiple choice
(checkboxes) - `mc_buttons`: Single choice as buttons - `select`:
Dropdown select - `text`: Text input - `textarea`: Multi-line text -
`numeric`: Number input - `slider`: Slider input - `date`: Date picker -
`daterange`: Date range picker

Matrix questions and custom questions are also supported.

### Conditional Logic

Three main functions control survey flow (defined in `R/server.R` and
implemented in `R/util.R`): -
[`sd_skip_if()`](https://pkg.surveydown.org/reference/sd_skip_if.md):
Skip to a page based on conditions -
[`sd_show_if()`](https://pkg.surveydown.org/reference/sd_show_if.md):
Show/hide questions based on conditions -
[`sd_stop_if()`](https://pkg.surveydown.org/reference/sd_stop_if.md):
Prevent navigation if conditions aren’t met

### Progress Tracking

Progress calculation is based on the last answered question index. The
progress bar never decreases, even when earlier questions are answered
later. This is intentional to avoid confusing respondents.

## Key Design Patterns

1.  **Two-file survey structure**: Every survey consists of:

    - `survey.qmd`: Survey content (questions, pages, text)
    - `app.R`: App configuration (database, conditional logic, global
      settings)

2.  **Parse-once, cache-always**: Survey content is parsed from rendered
    HTML once, then cached in `_survey/` folder. Subsequent runs load
    from cache unless files change.

3.  **Reactive Shiny integration**: Since surveys are Shiny apps,
    designers can leverage Shiny’s reactive programming for dynamic
    content.

4.  **Database-first with CSV fallback**: Primary mode uses PostgreSQL
    (Supabase), but `ignore = TRUE` mode saves to local CSV for testing.

5.  **YAML-driven configuration**: Most
    [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
    parameters can be set in the `survey.qmd` YAML header. Function
    arguments override YAML settings.

## Common Development Patterns

### Adding a New Question Type

1.  Add rendering logic in
    [`sd_question()`](https://pkg.surveydown.org/reference/sd_question.md)
    function in `R/util.R`
2.  Handle the input in
    [`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
    reactive observers
3.  Update question structure parsing in `R/config.R` if needed
4.  Add example in `inst/examples/`

### Modifying Server Behavior

Server-side logic lives in `R/server.R`. The
[`sd_server()`](https://pkg.surveydown.org/reference/sd_server.md)
function is called within the user’s server function in `app.R`. Most
functionality is implemented via Shiny observers that react to user
input.

### Adding JavaScript Functionality

1.  Create a new `.js` file in `inst/js/`
2.  Include it in the UI via
    [`sd_ui()`](https://pkg.surveydown.org/reference/sd_ui.md) in
    `R/ui.R`
3.  Communicate between R and JS using Shiny’s session messaging

### Working with Database

Database functions in `R/db.R` use the `pool` package for connection
pooling and `RPostgres` for PostgreSQL. The package supports both direct
connection and ignore mode (local CSV).

## Important Notes

- **NAMESPACE is auto-generated**: Edit roxygen comments in R files,
  then run `devtools::document()`. Never edit NAMESPACE directly.

- **Version 1.0.0 changes**: Recent major update introduced
  [`sd_nav()`](https://pkg.surveydown.org/reference/sd_nav.md)
  (replacing
  [`sd_next()`](https://pkg.surveydown.org/reference/sd_next.md)),
  previous button support, shorthand page syntax (`--- page_id`), and
  auto-injection of navigation buttons.

- **Testing locally**: Use `db <- sd_db_connect(ignore = TRUE)` in
  `app.R` to test without database connection. Responses save to
  `preview_data.csv`.

- **Multi-language support**: System messages support English, German,
  Spanish, French, Italian, and Simplified Chinese via the
  `system_language` parameter.

- **Resource paths**: The `.onAttach()` function in `R/util.R` registers
  resource paths for `_survey`, `images`, `css`, `js`, and `www` folders
  using
  [`shiny::addResourcePath()`](https://rdrr.io/pkg/shiny/man/resourcePaths.html).

## Documentation

- Main documentation site: <https://surveydown.org> (built with pkgdown)
- Package site: <https://pkg.surveydown.org>
- PLOS One publication: <https://doi.org/10.1371/journal.pone.0331002>
- Example templates: <https://github.com/surveydown-dev> (look for
  “template” repos)

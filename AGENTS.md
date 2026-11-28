# AGENTS.md

This file provides guidance to coding agents working in this repository.

## IMPORTANT: Before and After Editing Code

**Before editing code**, terminate the Shiny session if there is one running.

**After completing improvements**, do the following:

1. **Clean test artifacts**: If there is a `test_survey/` folder, delete the `_survey/` folder and `preview_data.csv` file inside it:

   ```bash
   rm -rf test_survey/_survey test_survey/preview_data.csv
   ```

2. **Install the package**: run this R script:
   ```r
   devtools::install(force = TRUE)
   ```

## Branches and Releases

- **Branch names**: `vX.Y.Z-job`, where `X.Y.Z` is the version the branch will release and `job` is a short kebab-case description of the work (e.g. `v1.3.1-efficiency-improvements`). Cut new branches from an up-to-date `main`, and set `Version` in `DESCRIPTION` to `X.Y.Z` on the branch.
- **A bare `vX.Y.Z` is a tag name, never a branch name.** A branch always carries the `-job` suffix, so branch and tag names never collide.
- **Existing branches keep their names.** Do not rename or delete branches on GitHub. The old branches `v0.8.1`, `v0.8.2`, `v0.8.3`, `v0.8.4` and `v1.0.1` predate the suffix rule, so those five versions have no tags.
- **Tags**: every version that lands on `main` gets a tag `vX.Y.Z` and a GitHub release (`gh release create`) titled `vX.Y.Z`, with that version's section of `NEWS.md` as notes. A version submitted to CRAN is tagged at the commit recorded in `CRAN-SUBMISSION` once CRAN accepts it, and its title is `vX.Y.Z (CRAN)`. Any other version is tagged at the last commit on `main` that carries it.
- **Never move or delete a tag after it is pushed.**

## Overview

**surveydown** is an R package for creating markdown-based programmable surveys using Quarto, Shiny, and PostgreSQL databases. The package enables researchers to define surveys in plain text (`.qmd` and `.R` files), making them version-controllable, reproducible, and collaborative.

The core workflow:
1. Users design surveys in `survey.qmd` (Quarto markdown with R code chunks)
2. Users configure app behavior in `app.R` (Shiny app with database settings)
3. Package renders the survey to a Shiny app that stores responses in PostgreSQL (Supabase recommended)

## Development Commands

### Package Development

```r
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

```r
# Build pkgdown site
pkgdown::build_site()
```

### Testing

**Automated Tests:**

```r
# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-reserved-ids.R")
```

Tests are located in `tests/testthat/` and use the `testthat` framework. Current test coverage includes:
- `test-reserved-ids.R`: Validation of reserved IDs and ID checking logic
- `test-all-data.R`: Testing for the `all_data` reactive values list functionality
- `test-db-modes.R`: `sd_db_connect()` signature, deprecated `ignore` argument, and `get_session_data()` CSV routing
- `test-blank-value.R`: `is_blank_value()`, the check for an empty stored answer, with answers that are not strings (a `date` question stores a `Date`)

**Manual Testing:**

Survey functionality is typically tested manually using the `test_survey/` directory, which contains a full survey example with `app.R` and `survey.qmd` files. To test changes:

1. Make changes to package source files in `R/`
2. Run `devtools::load_all()` to reload the package
3. Navigate to `test_survey/` and run the survey app
4. Verify changes work as expected

## Architecture

### Core Components

The package has a clear separation of concerns across several R files:

- **`R/ui.R`**: Contains `sd_ui()` - the main UI function that:
  - Renders the `survey.qmd` file using Quarto
  - Extracts and processes HTML content
  - Creates the Shiny UI with CSS/JS dependencies
  - Manages theme settings, progress bar, and footer configuration

- **`R/server.R`**: Contains `sd_server()` - the main server function that:
  - Handles page navigation and skip logic
  - Manages question state (answered/required)
  - Updates progress bar based on completion
  - Stores responses to database or local CSV
  - Implements conditional display logic via `sd_show_if()`
  - Handles exit survey functionality

- **`R/config.R`**: Contains `run_config()` - the configuration processor that:
  - Parses the rendered `survey.html` file
  - Extracts pages and questions into structured data
  - Validates IDs and required questions
  - Manages the `_survey/` folder cache system
  - Creates `settings.yml` with theme-settings, survey-settings, and system-messages

- **`R/db.R`**: Database functions including:
  - `sd_db_config()`: Interactive database configuration setup
  - `sd_db_connect()`: PostgreSQL connection management (Supabase support). Accepts `env_file`, `ignore` (deprecated), and `gssencmode`. The survey mode (`database`/`preview`/`local`) is controlled via `survey-settings` in `survey.qmd` YAML, not by this function.
  - `sd_database()`: Database object creation (deprecated, use `sd_db_connect()`)
  - Functions for storing and retrieving survey responses

- **`R/util.R`**: Utility functions for:
  - Question type implementations (`sd_question()`)
  - Page navigation (`sd_nav()`, `sd_close()`)
  - Conditional logic helpers
  - Markdown to HTML conversion
  - Resource path management

- **`R/messages.R`**: Multi-language system message support

### The `_survey/` Folder

When a survey runs, the package creates a `_survey/` folder that caches parsed survey content:

- `survey.html`: Rendered HTML from `survey.qmd`
- `head.rds`: Extracted HTML head content, with its fonts taken out (see `fonts/`)
- `fonts/`: The fonts the render embedded (the theme's typeface, Quarto's and Font Awesome's icon fonts), one file per font, named by its content. `move_fonts_out()` in `R/ui.R` writes them and points the head to them; `sd_ui()` serves the folder at `sd-fonts/`. A browser fetches a font only when the page draws with it.
- `pages.rds`: Structured page data
- `questions.yml`: Question metadata and structure
- `settings.yml`: Complete configuration (theme-settings, survey-settings, system-messages)

This caching system prevents re-rendering on every app reload. Changes to `survey.qmd` or `app.R` trigger re-parsing.

The `settings.yml` structure has 3 sections:
- `theme-settings`: Visual configuration (barcolor, etc.)
- `survey-settings`: Behavior (`required`, `all-required`, `shuffled`, `all-shuffled`, `start-page`, `rate-survey`, `show-previous`, etc.)
- `system-messages`: Multi-language UI text

### Frontend Assets

- **`inst/js/`**: JavaScript modules for:
  - `accent.js`: Mixes the accent toward the text color on a theme whose accent can hardly be told from its page (slate), so selections stay visible
  - `auto_scroll.js`: Auto-scroll to next question
  - `cookies.js`: Cookie-based session management
  - `highlighting.js`: Unanswered question highlighting
  - `interaction.js`: UI interactions
  - `progressbar.js`: Progress tracking
  - `clipboard.js`: Copy functionality
  - `countdown.js`: Countdown timers
  - `keep_alive.js`: Session persistence
  - `layout.js`: Layout that depends on measured widths: even lines of choice buttons, navigation buttons that shrink to icons when their labels do not fit, the matrix's sideways scroll and the mark on its column headers while they are stuck to the top of the window, and the footer's slots and height, and marks choice buttons with pictures or several lines as cards. It sets classes (`sd-rows`, `sd-nav-tight`, `sd-fits`, `sd-stuck`, `sd-footer-stack`, `sd-cards`, …) that `surveydown.css` styles.

- **`inst/css/`**: Stylesheet files:
  - `surveydown.css`: Structural styles for every theme. All colors come from the tokens at the top of the file (`--sd-paper`, `--sd-ink`, `--sd-accent`, …), which derive from the Quarto theme's `--bs-body-bg`, `--bs-body-color` and `--bs-primary`. Use the tokens; do not hard-code colors, or dark bootswatch themes break. The survey designer sets one color, the accent; the link color (`--sd-link`) and the code color (`--sd-code`) are derived from it by turning its hue.
  - Do not write a `:has()` on the root or with nothing before it, or one followed by a combinator (`A:has(B) C`): Chrome then restyles elements across the page at every answer. What the page knows when it is built goes in a class (`sd-required` on a required card or matrix row, `sd-over-footer` on a bottom progress bar, `sd-banner` on the body while a mode banner shows), and a sibling selector (`input:checked + span`) serves where the markup allows. See `optimize.md`.
  - `default_theme.css`: Loaded only with `theme: default`. Holds the typeface.
  - The Lua filter embeds the CSS from the **installed** package when `survey.qmd` renders, so a CSS change shows up only after `devtools::install()` and a fresh render (delete `_survey/`).
  - The page carries both Bootstrap 3 (from Shiny) and Bootstrap 5 (from Quarto). Rules for tables, buttons and form controls must outrank both.
  - Check every style change at phone width (575px and below has its own rules) and with a dark theme (`theme: darkly`).

- **`inst/template/`**: Default survey template with `survey.qmd` and `app.R`

- **`inst/lua/`**: Lua filter (`include-resources.lua`) for Quarto rendering

### Question Types

The `sd_question()` function supports multiple question types defined in `R/util.R`:
- `mc`: Single choice (radio buttons)
- `mc_multiple`: Multiple choice (checkboxes)
- `mc_buttons`: Single choice as buttons
- `mc_multiple_buttons`: Multiple choice as buttons
- `select`: Dropdown select
- `text`: Text input
- `textarea`: Multi-line text
- `numeric`: Number input
- `slider`: Slider input
- `date`: Date picker
- `daterange`: Date range picker

Matrix questions and custom questions are also supported.

### Conditional Logic

Three main functions control survey flow (defined in `R/server.R` and implemented in `R/util.R`):
- `sd_skip_if()`: Skip to a page based on conditions
- `sd_show_if()`: Show/hide questions based on conditions
- `sd_stop_if()`: Prevent navigation if conditions aren't met

### Option/Row Randomization (v1.1.0+)

Questions can have their options or rows randomized:
- **MC-type questions** (`mc`, `mc_buttons`, `mc_multiple`, `mc_multiple_buttons`): Options are shuffled
- **Matrix-type questions**: Rows (sub-questions) are shuffled

Configuration in `survey.qmd` YAML:
```yaml
survey-settings:
  shuffled: [question_id1, question_id2]  # Specific questions to shuffle
  all-shuffled: yes  # Or shuffle all eligible questions
```

#### Partial Shuffling (v1.1.0+)

You can specify which positions to shuffle, leaving the rest in their original order:

```yaml
survey-settings:
  shuffled:
    - artist: 1-5              # Shuffle positions 1-5, leave 6-7 fixed
    - fruit: [1, 2, 4, 7]      # Shuffle only these specific positions
    - swift: [1-5, 8-10]       # Shuffle positions 1-5 and 8-10, leave 6-7 fixed
    - michael_jackson          # No indices = shuffle all (original behavior)
    - car_preference: [1, 3]   # For matrix: shuffle rows 1 and 3 only
```

Supported index formats:
- Range string: `"1-5"` → positions 1, 2, 3, 4, 5
- Integer vector: `[1, 2, 4, 7]` → those specific positions
- Mixed: `[1-5, 8-10]` → combines both ranges
- Omitted: shuffle all positions (backward compatible)

Implementation in `R/config.R`:
- `parse_shuffle_indices()`: Parses index specifications (ranges, arrays)
- `parse_shuffled_yaml()`: Converts YAML to named list with indices
- `determine_shuffled_questions()`: Returns named list where values are indices or NULL (all)
- `get_mc_question_ids()` / `get_matrix_question_ids()`: Filter eligible questions

### Accessing Question Values

There are two primary ways to access question values in server logic:

#### 1. Using `all_data` (direct access)

The `sd_server()` function creates a reactive values list called `all_data` that provides reliable access to question responses.

**Why use `all_data`?**
- Works for all questions, not just those on the current page
- Automatically includes restored values from database/cookies after page refresh
- Handles navigation backward/forward correctly
- More robust than direct `input$` access

**Usage:**
```r
# Direct access with $ notation
age_value <- all_data$age

# In conditional logic
sd_skip_if(
  all_data$age < 18 ~ "parental_consent",
  all_data$employed == "yes" ~ "employment_questions"
)

sd_show_if(
  all_data$has_children == "yes" ~ "num_children"
)

# In custom reactive expressions
output$custom_message <- renderText({
  paste("Hello", all_data$name, "from", all_data$country)
})
```

**Implementation details:**
- Created in `sd_server()` in `R/server.R`
- Synced via a high-priority observer
- Excludes timestamp fields and reserved IDs
- Exposed to parent environment so it's available before `sd_server()` is called

#### 2. Using `sd_values()` and `sd_value()` (functional access)

For functional/programmatic access to question values, use `sd_values()` (or its alias `sd_value()`). These functions are defined in `R/db.R`.

**When to use `sd_values()`:**
- When you need programmatic access using string variables
- When retrieving multiple values at once
- When you prefer function syntax over `$` notation

**Usage:**
```r
# Single value - all equivalent:
age1 <- all_data$age
age2 <- sd_values(age)        # Unquoted (recommended)
age3 <- sd_values("age")      # Quoted (also works)
age4 <- sd_value(age)         # Alias for sd_values()

# Multiple values at once (returns unnamed vector):
values <- sd_values(age, name, country)
# Returns c("5", "Pinocchio", "UK")

# Mixed quoted/unquoted:
values <- sd_values(age, "name", country)

# In conditional logic:
if (sd_values(age) < 18) {
  # Do something
}

# Check multiple values at once:
if (all(sd_values(vehicle_complex, buy_vehicle) == c("no", "no"))) {
  # Both are "no"
}
```

**Key differences:**
- `sd_values()` with single argument returns a single value (for backward compatibility)
- `sd_values()` with multiple arguments returns an unnamed vector
- Both access the same `all_data` reactive values list under the hood

#### 3. Storing Custom Values with `sd_store_value()`

Use `sd_store_value()` to save computed values or URL parameters to the database:

```r
# Store a computed value
sd_store_value(calculated_score, "score")

# Store URL parameters
sd_store_value(sd_get_url_pars()$source, "referral_source")
```

Note: Cannot use reserved IDs (see Reserved IDs section below).

### Progress Tracking

Progress calculation is based on the last answered question index. The progress bar never decreases, even when earlier questions are answered later. This is intentional to avoid confusing respondents.

## Key Design Patterns

1. **Two-file survey structure**: Every survey consists of:
   - `survey.qmd`: Survey content (questions, pages, text)
   - `app.R`: App configuration (database, conditional logic, global settings)

2. **Parse-once, cache-always**: Survey content is parsed from rendered HTML once, then cached in `_survey/` folder. Subsequent runs load from cache unless files change.

3. **Reactive Shiny integration**: Since surveys are Shiny apps, designers can leverage Shiny's reactive programming for dynamic content.

4. **Database-first with CSV fallback**: Primary mode uses PostgreSQL (Supabase). Set `mode: preview` or `mode: local` in the `survey.qmd` YAML to save responses to a local CSV instead.

5. **YAML-driven configuration**: Most `sd_server()` parameters can be set in the `survey.qmd` YAML header. Function arguments override YAML settings.

## Common Development Patterns

### Adding a New Question Type

1. Add rendering logic in `sd_question()` function in `R/util.R`
2. Handle the input in `sd_server()` reactive observers
3. Update question structure parsing in `R/config.R` if needed
4. Update template in `inst/template/` if needed

### Modifying Server Behavior

Server-side logic lives in `R/server.R`. The `sd_server()` function is called within the user's server function in `app.R`. Most functionality is implemented via Shiny observers that react to user input.

### Adding JavaScript Functionality

1. Create a new `.js` file in `inst/js/`
2. Add it to the `js_files` list in `inst/lua/include-resources.lua`, which embeds it when `survey.qmd` renders
3. Communicate between R and JS using Shiny's session messaging

### Working with Database

Database functions in `R/db.R` use the `pool` package for connection pooling and `RPostgres` for PostgreSQL. The package supports both direct connection and ignore mode (local CSV).

## Optimization

`optimize.md` holds how an optimization pass is done (best speed, least work for the respondent's browser and the server, nothing lost), the rules passes have left, and a log of every pass. A pass starts from the log's last entry and looks only at what changed since. Read it before changing anything that runs on every page load, every answer, or on a timer.

## Important Notes

- **NAMESPACE and `man/` are auto-generated**: Edit roxygen comments in R files, then run `devtools::document()`. Never edit NAMESPACE or any file in `man/` directly.

- **Breaking changes (v1.1.0)**:
  - `sd_server()` now **only accepts `db`** as its parameter; all other settings must be in YAML header of `survey.qmd`
  - YAML keys renamed: `required-questions` → `required`, `all-questions-required` → `all-required`
  - `matrix_option_width` parameter removed from `sd_question()` (use `matrix_question_width` only)
  - `sd_create_messages()` function deprecated

- **Version 1.0+ features**:
  - `sd_nav()` (replacing `sd_next()`) with previous button support
  - Shorthand page syntax (`--- page_id`)
  - Auto-injection of navigation buttons
  - Reserved IDs system to prevent conflicts with internal metadata fields

- **Reserved IDs**: The following IDs are reserved and cannot be used for pages, questions, or in `sd_store_value()`:
  - `session_id`, `time_start`, `time_end`, `exit_survey_rating`, `current_page`, `browser`, `ip_address`
  - Use `get_reserved_ids()` (internal function) to get the complete list
  - The `check_ids()` function in `R/config.R` validates page and question IDs during survey parsing

- **Testing locally**: Set `mode: preview` under `survey-settings` in `survey.qmd` to test without a database connection. Responses save to `preview_data.csv`. The deprecated `ignore = TRUE` argument in `sd_db_connect()` still works but emits a warning.

- **Multi-language support**: System messages support English, German, Spanish, French, Italian, and Simplified Chinese via the `system_language` parameter.

- **Resource paths**: The `.onAttach()` function in `R/util.R` registers resource paths for `_survey`, `images`, `css`, `js`, and `www` folders using `shiny::addResourcePath()`.

## Documentation

- Main documentation site: https://surveydown.org (built with pkgdown)
- Package site: https://pkg.surveydown.org
- PLOS One publication: https://doi.org/10.1371/journal.pone.0331002
- Example templates: https://github.com/surveydown-dev (look for "template" repos)

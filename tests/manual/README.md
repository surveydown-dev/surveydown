# Manual browser tests

Scripts in this folder drive a real (headless) Chrome browser against a
running survey app to verify interactive behavior that unit tests can't
cover (show/hide reactivity, page navigation, etc.). They are NOT run by
`devtools::test()` or R CMD check.

## Requirements

- The `chromote` package (`install.packages("chromote")`)
- Google Chrome installed

The survey apps being tested are bundled in `tests/manual/apps/`, so the
tests are self-contained and run on any machine.

## Usage

With the working directory set to the package root, run all tests at once:

```r
source("tests/manual/run-all.R")
```

Or run a single test:

```r
source("tests/manual/browser-test-conditional-showing.R")
```

Each script launches the survey app in a background R process, walks
through the survey in headless Chrome, prints `[PASS]` / `[FAIL]` lines
for each check, saves screenshots to `tests/manual/screenshots/` (not
tracked in git), and shuts the app down when finished.

## Contents

- `run-all.R` — runs every `browser-test-*.R` script (each in its own R
  process) and prints an aggregate summary. Runs the scripts in parallel
  if the `mirai` package is installed, sequentially otherwise.
- `helpers.R` — shared helpers (app launch, browser session, click/type,
  visibility and text checks, SweetAlert dismissal, teardown). Sourced by
  every test script.
- `browser-test-conditional-showing.R` — `sd_show_if()`: all condition
  styles (`sd_value()`, multi-input, `as.numeric()`, custom functions,
  conditional pages, cross-page `input$`/`all_data$` conditions).
- `browser-test-conditional-skipping.R` — `sd_skip_if()`: simple and
  two-question skip conditions, required-question blocking.
- `browser-test-conditional-stopping.R` — `sd_stop_if()`: stop messages,
  priority over required-question warnings, per-page applicability.
- `browser-test-data-storage.R` — preview-mode data storage: answers
  every question type (including the `slider_numeric` two-handle range
  variant), then verifies every value in `preview_data.csv`
  (pipe-joined multi-selects, slider label-to-value mapping, matrix
  sub-columns, `sd_store_value()`, session metadata, one row per session).
- `browser-test-restoration.R` — cookies and state restoration: page
  refresh returns to the same page with answers restored, sessions resume
  with the same `session_id` (single CSV row), and a fresh browser starts
  a new session with a distinct `session_id`.
- `browser-test-preview-db.R` — `mode: preview` with a (fake) database
  connection: the connection must never be used, responses go to
  `preview_data.csv`, and the banner says the database is connected but
  not used.
- `browser-test-warm-start.R` — launches the same app twice: cold (full
  render) and warm (`_survey/` cache intact), verifying the warm start
  skips `quarto_inspect()`/rendering/re-parsing while serving the same
  progress bar color, footer, and a working survey.
- `apps/` — bundled survey apps the tests run against.

IMPORTANT: every test script must use a unique port in the 8120-8129
range (currently 8123-8129), so `run-all.R` can run the apps in parallel
without conflicts. Cookie-enabled apps share the browser-wide localhost
cookie jar, which is fine here because `run-all.R` gives each script its
own R process (and therefore its own headless browser).

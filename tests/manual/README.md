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
- `apps/` — bundled survey apps the tests run against.

back-to-back without conflicts.
IMPORTANT: every test script must use a unique port in the 8120-8129
range (currently 8123-8125), so `run-all.R` can run the apps in parallel
without conflicts.

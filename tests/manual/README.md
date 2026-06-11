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

Run a script from the package root, e.g.:

```r
source("tests/manual/browser-test-conditional-showing.R")
```

Each script launches the survey app in a background R process, walks
through the survey in headless Chrome, prints `[PASS]` / `[FAIL]` lines
for each check, saves screenshots to `tests/manual/screenshots/` (not
tracked in git), and shuts the app down when finished.

## Contents

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

The scripts use different ports (8123-8125), so they can be run
back-to-back without conflicts.

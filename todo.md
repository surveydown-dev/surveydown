# Work plan: efficiency-improvements branch

Running list of agreed improvements (with Claude Code). Items 1-5 are done;
this file exists so the plan survives interruptions. Excluded from the
package build via .Rbuildignore.

## Workflow agreements

- Work happens on the `efficiency-improvements` branch.
- After each task: update NEWS.md, reinstall the package, run unit tests
  (`devtools::test()`), then PAUSE. John runs the manual browser tests
  (`source("tests/manual/run-all.R")` from the package root) and commits
  before the next task starts.
- Claude does not run the manual browser tests; John runs them and reports.
- Manual tests live in `tests/manual/` (self-contained bundled apps,
  chromote-based, parallel via mirai). Every test script needs a unique
  port in 8120-8129 (currently 8123-8128 used).

## Completed

1. [done] Session-scoped caching of parsed `_survey/settings.yml`
   (`get_messages()` / `get_settings_yml()` cache in `session$userData`).
2. [done] show_if observer no longer depends on every input; depends on
   statically extracted question refs + dynamic registration + the
   `sd_page_rendered` signal (visibility re-applied on each page render).
3. [done] Consolidated per-question observers: deleted dead
   `*_manual_range` / `*_autosave_timestamp` observer sets; one observer
   per question; `*_value` / `*_label_option` / `*_label_question` outputs
   registered once, reading from `all_data`.
4. [done] Local CSV in-process cache (`get_local_data()` mtime-keyed cache
   + `write_local_data()`); no read-modify-write from disk per save;
   removed duplicate `get_local_data()` definition.
5. [done] Debounced the cookie-sync observer (500ms; was firing per
   keystroke).
6. [done] Inline per-question JS for mc_buttons / mc_multiple_buttons /
   numeric replaced with delegated handlers in interaction.js (one
   registration per page load; numeric spinner injected on shiny:bound;
   input carries class "sd-numeric" added via htmltools::tagQuery).
   Data-storage test extended with a spinner-click check (new mousedown()
   helper). NEEDS: manual test run + commit.

Also done along the way (already on the branch):
- Bug fix: `mode: preview`/`local` never use the database even with a
  valid connection (banner + console messages explain; `sd_store_value()`
  also guarded via `is_csv_mode()`).
- Bug fix: cookied-session restoration crash in preview/local mode
  (`local_csv_file` defined after `handle_sessions()` call).
- Console now always prints the operating mode per session (all modes).
- `sd_reactive()` no longer warns when the failure is just "inputs not
  answered yet" (uses `extract_question_refs()`, now a package-level
  function with unit tests).
- Docs: `website/docs/reactivity.qmd` rewritten for the sd_reactive
  best-practice pattern (sd_value inline; no observe(); Option-3 callout
  for many derived values). Website repo commits separately.
- Manual browser test suite: tests/manual/ with run-all.R runner +
  bundled apps + helpers; covers show_if, skip_if, stop_if, data storage
  (all 13 question types + range slider), cookies/restoration,
  preview-with-db.

7. [done] Skip `quarto_inspect()` when `_survey/` cache is fresh.
   sd_ui() reads barcolor/barposition/footer from cached settings.yml via
   read_cached_theme_settings() (falls back to full inspect path);
   settings.yml no longer rewritten on warm starts, so run_config() stops
   re-parsing pages every startup. BONUS bug fix: survey_needs_updating()
   now also re-renders when the installed package is newer than the cached
   render (stale embedded JS/CSS). New warm-start browser test (port 8129,
   apps/warm_start) launches the app cold then warm and asserts no
   re-render/re-parse plus correct barcolor/footer from cache.
   NEEDS: manual test run + commit.

## Remaining tasks

(efficiency tasks 1-7 all complete)

## Widget improvements (approved earlier, after tasks 6-7)

8. [done] `matrix_multiple` question type (checkbox matrix). Renders via
   the matrix builder with mc_multiple sub-questions; downstream machinery
   (is_matrix detection, required, shuffling, restoration) worked
   automatically because it keys off the .matrix-question HTML class.
   CSS .checkbox parallels added. Data-storage app/test extended (2 new
   checks). NEEDS: manual test run + commit.
9. [done] Slider restyle: .irs--shiny styling in surveydown.css (slim
   theme-colored track, circular handle, value bubble). Minor tick marks
   hidden via CSS; labeled MAJOR ticks at main breaks kept (grid/ticks
   defaults unchanged after iteration with John). Min/max labels hidden
   when a labeled grid is shown (redundant). CSS mirrored to
   website/css/surveydown.css. Website slider screenshots regenerated
   via new website/make-screenshots.R (rerun it whenever question
   styling changes); slider.png example changed to a neutral question
   with the handle at the middle. noUiSlider alternative (#10) not
   needed. NEEDS: commit (package + website repos).
   NOTE: the website repo keeps its own trimmed copy of surveydown.css
   (website/css/surveydown.css) used by docs pages that live-render
   questions (e.g., question-types.qmd) - mirror any question styling
   changes there (matrix checkbox + slider rules synced 2026-06-12).
11. Clean numeric input: replace per-question spinner/validation JS with a
    shared component (pairs with task 6).
13. Rating / NPS question types (stars; 0-10 scale styled like mc_buttons).
14. Image-choice question type (mc with CSS card layout).
15. Per-type question registry refactor (one place per type defining
    render/restore/format) - do before adding several new types.

## Deferred / parked

- 12. Ranking question type: parked to avoid a `sortable` package
  dependency; revisit at the end (maybe vendor SortableJS directly).
- SurveyJS integration: explicitly out of scope for now.
- 10. noUiSlider replacement: only if #9 proves insufficient.

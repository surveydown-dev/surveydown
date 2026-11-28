# Optimization

How surveydown is kept fast and light: how an optimization pass is done, the rules passes have left, how each number is taken, and the log of every pass, which is what makes the next pass incremental.

A survey costs work in three places: the respondent's browser (what it downloads, and what it computes at each answer and while it waits), the server (what it computes per respondent and per answer), and the survey designer's machine (the render and the start of the app). A pass looks at all three.

## Rules

- **A pass follows every round of changes.** Once a set of updates is in, a pass looks at them before the next set begins, since what is new is where waste comes in.
- **A pass has three aims:** the best speed; the least work for the computers involved (the respondent's CPU, memory, network and battery, the server's CPU per respondent, the designer's wait at a render); and nothing the survey does lost. The third is never traded for the other two: a change that saves time and changes what a respondent sees or can do is not made.
- **Measured before and after,** the same way twice, with the methods under Measures. The first guess is often wrong. The numbers go in the log.
- **Incremental.** A pass starts from the log's last entry and looks only at what changed since the commit that entry names (`git diff <commit>..`), plus anything an entry left for later. A pass that is not logged did not happen as far as the next one knows.
- **The page's head goes out with every page load, so nothing heavy is embedded in it.** The render embeds every resource (`--embed-resources`), and `sd_ui()` sends the rendered head with the page. Fonts are taken out into `_survey/fonts` and served at `sd-fonts/` (`move_fonts_out()` in `R/ui.R`), so a browser fetches a font only when the page draws with it, and keeps it. Anything else large that the render embeds gets the same treatment.
- **One way of doing a job.** A script that does what a stylesheet already does is not included: Font Awesome's script drew the icons its stylesheet draws, and cost 1.5 MB in every page and a watch over every change to the page.
- **`:has()` only on the element it styles, and that element small.** A `:has()` on the root or with nothing before it is checked again at every change anywhere in the page. A `:has()` followed by a combinator (`A:has(B) C`) puts C's features in one list Chrome shares across all such rules, and every element of the page matching any of them is restyled whenever some element's `:has()` state may have changed: at every answer. What the page knows when it is built goes in a class instead (`sd-required` on a required card or matrix row, set where the mark is shown; `sd-over-footer` on a bottom progress bar; `sd-banner` on the body while a mode banner shows; `sd-cards` on choice buttons with pictures, set by `layout.js`), and a sibling selector (`input:checked + span`) serves where the markup allows.
- **The render asks R once.** Starting R takes a tenth of a second or more; the Lua filter asks for the package's folder once and builds every path from it.
- **Nothing is written to the respondent's console** but errors.
- **The server sends only what changed.** An answer sends the busy and idle signals and the progress; a page change sends the page. A new observer, output or message on the answer path is measured against that.

## A pass

1. Read the log's last entry: the commit it covers to, and what it left.
2. List what is new since (`git log` and `git diff` from that commit): the files, and which of the three places they run in.
3. Find the hot paths in what is new. In the browser: what each page load downloads (the head, the page, fonts, images); what runs at each answer (handlers, messages, style recalculation, layout); what wakes while nothing happens (timers, observers, polling). On the server: what runs per session and per answer (observers, outputs, messages, reads and writes of the data). On the designer's machine: the render and the start of the app, cold and warm.
4. Measure each suspect as it is now (Measures).
5. Fix the cause, in the place every caller goes through.
6. Measure again, in the same way.
7. Check that nothing is lost: `devtools::test()`, `devtools::check()`, `source("tests/manual/run-all.R")`, every official template walked at 1000, 390 and 320 px wide with a check for sideways overflow, the bootswatch themes, and each changed part looked at in the browser in every state it has.
8. Write the log entry.

## Measures

The test survey (`test_survey/`) in preview mode, the installed package, headless Chrome driven from R with `chromote`. Each number is the median of three runs.

- **Page weight:** the bytes of `GET /`, plain and with `Accept-Encoding: gzip`; what fills the head, read from `_survey/head.rds`.
- **Load:** a fresh browser with its cache off. Bytes are the sum of `Network.loadingFinished`'s `encodedDataLength`; the first page has shown when the first question is laid out. Taken unthrottled and on a 4G-like link (`Network.emulateNetworkConditions`: 10 Mbit/s down, 5 up, 40 ms latency). The browser's work is the change in `Performance.getMetrics` (`TaskDuration`, `ScriptDuration`, `RecalcStyleDuration`, `LayoutDuration`, `JSHeapUsedSize`).
- **Walk:** on the Question Types page, the first option of each choice group clicked 60 ms apart, "hello" typed a key at a time into each text field, the page scrolled to the bottom a frame at a time, then Next twice; the change in `Performance.getMetrics` over it.
- **Style:** the same answers under a trace. Elements restyled: `UpdateLayoutTree` events' `elementCount` (category `devtools.timeline`). Cost by selector: `selector_stats` (category `disabled-by-default-blink.debug`). Why elements were restyled: category `disabled-by-default-devtools.timeline.invalidationTracking`.
- **Idle:** `Performance.getMetrics` over 10 s with nothing happening, and a sampling profile of the page's scripts (`Profiler`) to name what ran.
- **Start:** `sd_ui()` in `test_survey/` with no `_survey/` folder (cold: render and parse) and with it (warm).
- **Server:** `Rprof()` at 5 ms intervals while four respondents take the walk one after another; the websocket messages by kind (`Network.webSocketFrameReceived`).

## Log

Newest first. Each entry: the version and date, the commit range covered, what was looked at, what was found and done with numbers, and what was left for later.

### v1.4.0, 2026-11-28

The first logged pass. Covers the package as it stands at `b22accd` (the redesign), and the commit that adds this entry. Measured as under Measures, on a Mac (Apple silicon), Chrome headless.

| | Before | This pass |
|---|---|---|
| `GET /`, plain / gzip | 9.0 MB / 4.6 MB | 1.4 MB / 0.25 MB |
| First page, 4G-like link | 3.75 s, 4.69 MB transferred | 0.65 s, 0.81 MB (fonts included) |
| First page, unthrottled | 0.55 s; 161 ms of browser work; 6.3 MB of script heap | 0.26 to 0.31 s; 88 ms; 3.7 MB |
| Walk: all browser work | 381 to 440 ms | 319 ms |
| Walk: style recalculation | 141 to 159 ms (v1.3.0's stylesheet: 52 ms) | 88 ms |
| Walk: elements restyled | 4,898 (v1.3.0's stylesheet: 776) | 1,940 |
| Walk: selector matching | 25.2 ms | 6.5 ms |
| Cold start of the test survey | 7.5 s, 1.2 s of it 13 starts of R | 5.0 to 5.5 s |

Found and fixed:
- The head was 8.8 MB, sent with every page load: Font Awesome's stylesheet with its fonts embedded, each font three or four times under the families its stylesheet names (4.0 MB); Raleway in ten faces of the whole character set (1.8 MB); Font Awesome's script (1.5 MB); Bootstrap's icon font (0.3 MB). The fonts are now files, named by their content, so a font named three times is one file (Rules). Font Awesome's script is gone; its stylesheet draws the same icons, checked with an icon of each family. This predates the range.
- `:root:has(#progressbar.top)`, written in the redesign, was a third of all selector time, and the bare `:has(.footer)` and `:has(.sd-mode-banner)` rules from before it were checked on every element. The rules with a `:has()` followed by a combinator (the required mark's room, the image cards' selected state, the slider's grid labels, the choice buttons' card layout, the matrix's captions and row marks on phones, the banner and the bottom bar) made every answer restyle the labels, button groups, cards and list items across the page. Each is now a class or a sibling selector (Rules); the slider already marks a grid with `irs-with-grid`. Drawn the same, checked in the browser at 1000 and 390 px: the room by the mark, the selected image card and its check, the hidden slider labels, the bold caption under a chosen matrix option, the row mark on phones, card buttons, the footer above the banner.
- The Lua filter started R once for each of its 13 files, 1.2 s of every render.
- The highlighting script logged to the console at every highlight.

Measured and left as they are:
- On the server, four respondents took 4.6 s of R time in all, 0.12 s of it in surveydown's own functions; the rest is Shiny sending messages. An answer sends the busy and idle signals and the progress (under 1 KB); a page change sends the page (232 KB for Question Types, the largest).
- At idle the page wakes about ten times a second, 8 ms of work in 10 s: Shiny's slider library (ion.rangeSlider) checks each slider's width every 300 ms.
- `option_attr.js` watches every change to the page and looks for its popups at each; it did not show in the walk's script time (46 ms in all).

Checked that nothing is lost: `devtools::test()` (84 pass), `devtools::check()` (0 errors, warnings and notes), `tests/manual/run-all.R` (102 pass, 0 fail), the 17 templates walked at three widths (the same overflow report as before the pass: a table that scrolls in its own box at 320 px, and Google's page after the redirect template leaves), the bottom bar above the footer and the banner, and the fonts and icons drawn from their files.

Left for later:
- Style recalculation is still above v1.3.0's (1,940 elements restyled against 776). What is left: the cards' state rules and the option rows' `:has(input:checked)` (both on the element they style), and Quarto's own rules in Chrome's shared list (lists, code copy buttons).
- Raleway comes as ten TTF faces of about 135 KB each, since Google Fonts gives TTF to the render's downloader; a first visit takes four of them (about 540 KB). WOFF2 would be about a third.
- Whether the websocket compresses the page HTML sent at each page change was not measured.
- The slider library's polling, if it matters on phones.

### Before the log

Passes before this log were not written down by what they covered. The one in v1.3.0 (the `efficiency-improvements` branch, merged in #282) did, on the server: the parsed `settings.yml` cached per session; `sd_show_if()` observing only the questions its conditions name; one observer per question, with the dead ones deleted; the local CSV held in memory, keyed by the file's time; the cookie sync debounced to 500 ms; one delegated handler for button and numeric questions in place of a script per question; and a warm start that skips `quarto_inspect()` and the re-parse, with a re-render when the installed package is newer than the cache.

# Browser test for warm-cache app startup using chromote.
#
# Launches the bundled warm_start app twice:
#   Cold start: empty _survey/ -> full quarto_inspect + render path.
#   Warm start: _survey/ intact -> sd_ui() must skip quarto_inspect and
#     the render entirely, serve display settings (progress bar color,
#     footer) from the cached settings.yml, and run_config() must not
#     re-parse pages (previously settings.yml was rewritten every startup,
#     forcing a re-parse).
#
# Run from the package root:
#   source("tests/manual/browser-test-warm-start.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "warm_start")
port <- 8129
csv_file <- file.path(app_dir, "preview_data.csv")

# Shared checks for both launches
check_app_works <- function(label) {
  check(
    paste(label, "footer rendered"),
    body_has("Warm start test footer")
  )
  check(
    paste(label, "progress bar uses configured barcolor"),
    isTRUE(js(
      "getComputedStyle(document.getElementById('progress'))
         .backgroundColor.includes('255, 0, 0')"
    ))
  )
  click("input[name=\"fruit\"][value=\"banana\"]")
  click("#page1_next", wait = 2)
  check(
    paste(label, "survey completes"),
    body_has("End of the warm start test survey")
  )
}

# -- Cold start (full render) --------------------------------------------

launch_app(app_dir, port) # clean = TRUE: empty cache
new_session(port)
check("cold start rendered the survey", any(grepl("rendering survey files", app_log())))
check_app_works("cold:")
shot("warm_start_cold.png")

d <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
check("cold: response stored", nrow(d) == 1 && d$fruit[1] == "banana")

# -- Warm start (cache intact) -------------------------------------------

stop_app(port)
launch_app(app_dir, port, clean = FALSE) # keep _survey/ and CSV
new_session(port)

log_lines <- app_log()
check(
  "warm start did NOT re-render survey.qmd",
  !any(grepl("rendering survey files", log_lines))
)
check(
  "warm start did NOT re-parse survey contents",
  !any(grepl("re-parsing survey contents", log_lines))
)
check_app_works("warm:")
shot("warm_start_warm.png")

d <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
check(
  "warm: second response appended to existing CSV",
  nrow(d) == 2 && all(d$fruit == "banana")
)

teardown(app_dir, port)

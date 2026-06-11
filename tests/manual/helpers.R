# Shared helpers for the manual browser tests in tests/manual/.
# Source this file from a test script run at the package root, e.g.:
#   source(file.path("tests", "manual", "helpers.R"))
# See tests/manual/README.md for requirements.

library(chromote)

n_fail <- 0
b <- NULL

shot_dir <- if (dir.exists(file.path("tests", "manual"))) {
  file.path("tests", "manual", "screenshots")
} else {
  tempdir() # fallback if not run from the package root
}
dir.create(shot_dir, showWarnings = FALSE, recursive = TRUE)

app_url <- function(port) sprintf("http://127.0.0.1:%d/", port)

# Launch a survey app in a background R process and wait until it responds
# (the first run renders survey.qmd, which can take ~60s)
launch_app <- function(app_dir, port) {
  if (!dir.exists(app_dir)) {
    stop(
      "Run this script from the package root (app not found at ",
      app_dir, ")"
    )
  }
  cat("Launching app from", app_dir, "...\n")
  system(sprintf(
    "Rscript -e 'shiny::runApp(\"%s\", port = %d)' > /tmp/sd_browser_test_app.log 2>&1 &",
    normalizePath(app_dir), port
  ))
  up <- FALSE
  for (i in 1:60) {
    up <- tryCatch(
      {
        suppressWarnings(readLines(app_url(port), n = 1))
        TRUE
      },
      error = function(e) FALSE
    )
    if (up) break
    Sys.sleep(3)
  }
  if (!up) {
    stop("App did not start. See /tmp/sd_browser_test_app.log")
  }
  cat("App is up.\n")
}

# Open a fresh browser session on the app (a new Shiny session each call)
new_session <- function(port, wait = 6) {
  if (!is.null(b)) {
    try(b$close(), silent = TRUE)
  }
  b <<- ChromoteSession$new()
  b$Page$navigate(app_url(port))
  Sys.sleep(wait)
}

js <- function(code) {
  b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
}

wait_for <- function(sel, timeout = 20) {
  t0 <- Sys.time()
  while (as.numeric(difftime(Sys.time(), t0, units = "secs")) < timeout) {
    found <- js(sprintf("document.querySelector('%s') !== null", sel))
    if (isTRUE(found)) return(invisible(TRUE))
    Sys.sleep(0.3)
  }
  stop("Timeout waiting for: ", sel)
}

click <- function(sel, wait = 1.2) {
  wait_for(sel)
  js(sprintf("document.querySelector('%s').click(); true", sel))
  Sys.sleep(wait)
}

set_text <- function(id, value, wait = 1.2) {
  wait_for(paste0("#", id))
  js(sprintf(
    "var el = document.getElementById('%s');
     el.value = '%s';
     el.dispatchEvent(new Event('input',  {bubbles: true}));
     el.dispatchEvent(new Event('change', {bubbles: true}));
     true",
    id, value
  ))
  Sys.sleep(wait)
}

# Returns TRUE if the question container is visible
visible <- function(id) {
  wait_for(paste0("#container-", id))
  disp <- js(sprintf(
    "getComputedStyle(document.querySelector('#container-%s')).display",
    id
  ))
  disp != "none"
}

# Returns TRUE if an element matching the selector exists in the DOM
present <- function(sel) {
  isTRUE(js(sprintf("document.querySelector('%s') !== null", sel)))
}

# Returns TRUE if the page body text contains the given string (case-sensitive)
body_has <- function(text) {
  isTRUE(js(sprintf("document.body.innerText.includes('%s')", text)))
}

# Dismiss a SweetAlert popup (used for stop_if and required-question warnings)
dismiss_alert <- function(wait = 1) {
  js("var btn = document.querySelector('.swal2-confirm'); if (btn) btn.click(); true")
  Sys.sleep(wait)
}

check <- function(label, condition) {
  status <- if (isTRUE(condition)) "PASS" else "FAIL"
  if (status == "FAIL") n_fail <<- n_fail + 1
  cat(sprintf("[%s] %s\n", status, label))
}

shot <- function(name) {
  b$screenshot(file.path(shot_dir, name))
}

# Close the browser, stop the app, clean generated artifacts, print summary
teardown <- function(app_dir, port) {
  cat(sprintf(
    "--- Done: %s ---\n",
    if (n_fail == 0) "all checks passed" else paste(n_fail, "check(s) FAILED")
  ))
  cat("Screenshots saved to:", shot_dir, "\n")
  try(b$close(), silent = TRUE)
  system(sprintf("pkill -f 'shiny::runApp.*%d'", port))
  unlink(file.path(app_dir, "_survey"), recursive = TRUE)
  unlink(file.path(app_dir, "preview_data.csv"))
  unlink(file.path(app_dir, "survey_files"), recursive = TRUE)
}

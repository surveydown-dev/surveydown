# Shared helpers for the manual browser tests in tests/manual/.
# Source this file from a test script run at the package root, e.g.:
#   source(file.path("tests", "manual", "helpers.R"))
# See tests/manual/README.md for requirements.

library(chromote)

n_fail <- 0
b <- NULL
browser_proc <- NULL

shot_dir <- if (dir.exists(file.path("tests", "manual"))) {
  file.path("tests", "manual", "screenshots")
} else {
  tempdir() # fallback if not run from the package root
}
dir.create(shot_dir, showWarnings = FALSE, recursive = TRUE)

app_url <- function(port) sprintf("http://127.0.0.1:%d/", port)

# Launch a survey app in a background R process and wait until it responds
# (the first run renders survey.qmd, which can take ~60s). Set clean =
# FALSE to keep existing artifacts (e.g., to test a warm _survey/ cache).
launch_app <- function(app_dir, port, clean = TRUE) {
  if (!dir.exists(app_dir)) {
    stop(
      "Run this script from the package root (app not found at ",
      app_dir, ")"
    )
  }
  # Kill any stale app process holding this test port (e.g., left behind
  # by an earlier crashed run), otherwise the new app fails to bind and we
  # silently talk to the zombie process
  system(sprintf("pkill -f 'shiny::runApp.*%d'", port), ignore.stderr = TRUE)
  Sys.sleep(1)

  if (clean) {
    # Start from a clean slate: remove artifacts a previous (interrupted
    # or manual) run may have left behind. Stale response CSVs contaminate
    # row counts, and a stale _survey/ cache can carry parsed content from
    # an older package version.
    unlink(file.path(app_dir, "_survey"), recursive = TRUE)
    unlink(file.path(app_dir, "preview_data.csv"))
    unlink(file.path(app_dir, "local_data.csv"))
    unlink(file.path(app_dir, "survey_files"), recursive = TRUE)
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

# Stop the app process WITHOUT cleaning artifacts (for warm-restart tests)
stop_app <- function(port, wait = 1) {
  system(sprintf("pkill -f 'shiny::runApp.*%d'", port), ignore.stderr = TRUE)
  Sys.sleep(wait)
}

# Read the current app process log (one shared log per test script)
app_log <- function() {
  readLines("/tmp/sd_browser_test_app.log", warn = FALSE)
}

# Open a fresh browser session on the app (a new Shiny session each call).
# With fresh_browser = TRUE, a brand-new browser process is started, which
# has an empty cookie jar -- this simulates closing and reopening the
# browser. The default reuses the same browser, so cookies persist across
# sessions (like opening a new tab).
new_session <- function(port, wait = 6, fresh_browser = FALSE) {
  if (!is.null(b)) {
    try(b$close(), silent = TRUE)
  }
  if (fresh_browser) {
    browser_proc <<- chromote::Chromote$new()
    b <<- browser_proc$new_session()
  } else {
    b <<- ChromoteSession$new()
  }
  b$Page$navigate(app_url(port))
  Sys.sleep(wait)
}

# Reload the current page (cookies persist; simulates a page refresh)
reload <- function(wait = 8) {
  b$Page$reload()
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

# Dispatch a real mousedown event (for handlers bound to mousedown rather
# than click, e.g. the numeric input's spinner buttons)
mousedown <- function(sel, wait = 1.2) {
  wait_for(sel)
  js(sprintf(
    "document.querySelector('%s').dispatchEvent(
       new MouseEvent('mousedown', {bubbles: true})
     );
     true",
    sel
  ))
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

# Set a select question (handles both selectize and plain select inputs)
set_select <- function(id, value, wait = 1.2) {
  wait_for(paste0("#", id))
  js(sprintf(
    "var el = $('#%s')[0];
     if (el.selectize) { el.selectize.setValue('%s'); }
     else { $(el).val('%s').trigger('change'); }
     true",
    id, value, value
  ))
  Sys.sleep(wait)
}

# Set a text slider (type = 'slider') to the 1-based position among its options
set_slider <- function(id, position, wait = 1.2) {
  wait_for(paste0("#", id))
  js(sprintf(
    "var $el = $('#%s');
     $el.data('ionRangeSlider').update({from: %d});
     $el.trigger('change');
     true",
    id, position - 1
  ))
  Sys.sleep(wait)
}

# Set a numeric slider (type = 'slider_numeric') to the given value
set_slider_numeric <- function(id, value, wait = 1.2) {
  wait_for(paste0("#", id))
  js(sprintf(
    "var $el = $('#%s');
     $el.data('ionRangeSlider').update({from: %s});
     $el.trigger('change');
     true",
    id, value
  ))
  Sys.sleep(wait)
}

# Set a numeric range slider (type = 'slider_numeric' with a length-2
# default) to the given from/to values
set_slider_range <- function(id, from, to, wait = 1.2) {
  wait_for(paste0("#", id))
  js(sprintf(
    "var $el = $('#%s');
     $el.data('ionRangeSlider').update({from: %s, to: %s});
     $el.trigger('change');
     true",
    id, from, to
  ))
  Sys.sleep(wait)
}

# Set a date question (type = 'date') to 'yyyy-mm-dd'.
# Shiny renames the bootstrap datepicker plugin to bsDatepicker, and its
# input binding listens for the 'changeDate' event (not 'change').
set_date <- function(id, value, wait = 1.2) {
  wait_for(paste0("#", id, " input"))
  js(sprintf(
    "var $inp = $('#%s input').first();
     if ($inp.bsDatepicker) { $inp.bsDatepicker('update', '%s'); }
     else { $inp.val('%s'); }
     $inp.trigger('changeDate');
     true",
    id, value, value
  ))
  Sys.sleep(wait)
}

# Set a date range question (type = 'daterange') to 'yyyy-mm-dd' start/end
set_daterange <- function(id, start, end, wait = 1.2) {
  wait_for(paste0("#", id, " input"))
  js(sprintf(
    "var inps = $('#%s input');
     var vals = ['%s', '%s'];
     inps.each(function(i) {
       var $inp = $(this);
       if ($inp.bsDatepicker) { $inp.bsDatepicker('update', vals[i]); }
       else { $inp.val(vals[i]); }
       $inp.trigger('changeDate');
     });
     true",
    id, start, end
  ))
  Sys.sleep(wait)
}

# Returns the current value of the first element matching the selector
input_val <- function(sel) {
  wait_for(sel)
  js(sprintf("document.querySelector('%s').value", sel))
}

# Current state of an ionRangeSlider: 'from'/'to' are numeric positions,
# 'from_value'/'to_value' are the display labels for text sliders
slider_state <- function(id, field = "from") {
  wait_for(paste0("#", id))
  js(sprintf("$('#%s').data('ionRangeSlider').result.%s", id, field))
}

# Returns TRUE if the first element matching the selector is checked
is_checked <- function(sel) {
  isTRUE(js(sprintf(
    "var el = document.querySelector('%s'); el !== null && el.checked", sel
  )))
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

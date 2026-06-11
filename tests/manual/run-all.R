# Run all manual browser tests.
#
# With the working directory set to the package root, run:
#   source("tests/manual/run-all.R")
#
# Each browser-test-*.R script runs in its own R process. If the `mirai`
# package is installed, all scripts run in parallel (total wall time is
# roughly that of the slowest test); otherwise they run sequentially.
# Output is printed per script once it finishes, followed by an aggregate
# summary. See tests/manual/README.md for requirements.
#
# NOTE: every test script must use a unique port (see README) so the apps
# can run concurrently.

scripts <- sort(list.files(
  file.path("tests", "manual"),
  pattern = "^browser-test-.*\\.R$",
  full.names = TRUE
))
if (length(scripts) == 0) {
  stop("No browser-test-*.R scripts found. Run from the package root.")
}

# Run one test script in a subprocess and collect its results
run_script <- function(script) {
  log_file <- tempfile(fileext = ".log")
  system2("Rscript", shQuote(script), stdout = log_file, stderr = log_file)
  output <- readLines(log_file, warn = FALSE)
  unlink(log_file)
  list(
    output = output,
    pass = sum(grepl("^\\[PASS\\]", output)),
    fail = sum(grepl("^\\[FAIL\\]", output)),
    # A script that crashed before its teardown never prints the Done marker
    completed = any(grepl("^--- Done", output))
  )
}

scripts <- stats::setNames(scripts, basename(scripts))

if (requireNamespace("mirai", quietly = TRUE)) {
  cat(sprintf(
    "Running %d test scripts in parallel (mirai)...\n", length(scripts)
  ))
  mirai::daemons(length(scripts))
  results <- mirai::mirai_map(scripts, run_script)[mirai::.progress]
  mirai::daemons(0)
} else {
  cat(sprintf(
    "Running %d test scripts sequentially (install 'mirai' to parallelize)...\n",
    length(scripts)
  ))
  results <- lapply(scripts, run_script)
}

# Print each script's output
for (name in names(results)) {
  cat("\n==============================================================\n")
  cat(name, "\n")
  cat("==============================================================\n")
  cat(results[[name]]$output, sep = "\n")
}

# Clean up any app process a crashed script left behind (test ports only)
if (any(!sapply(results, `[[`, "completed"))) {
  system("pkill -f 'shiny::runApp.*812[0-9]'", ignore.stderr = TRUE)
}

cat("\n==============================================================\n")
cat("Summary\n")
cat("==============================================================\n")
total_pass <- 0
total_fail <- 0
for (name in names(results)) {
  r <- results[[name]]
  total_pass <- total_pass + r$pass
  total_fail <- total_fail + r$fail
  status <- if (!r$completed) {
    "CRASHED"
  } else if (r$fail > 0) {
    "FAILED"
  } else {
    "ok"
  }
  cat(sprintf("%-45s %2d pass, %2d fail  [%s]\n", name, r$pass, r$fail, status))
}
cat(sprintf(
  "\nTotal: %d pass, %d fail%s\n",
  total_pass,
  total_fail,
  if (any(!sapply(results, `[[`, "completed"))) " (some scripts crashed)" else ""
))

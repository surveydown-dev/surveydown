# Browser test: mode preview must never use the database, using chromote.
#
# The bundled preview_with_db app passes a fake (non-NULL) database object
# to sd_server() with mode: preview. Verifies that:
#   - the banner says the database is connected but not used
#   - the survey works end-to-end (no code path touches the connection;
#     using the fake object as a pool would crash the app)
#   - responses are stored in preview_data.csv
#
# Run from the package root:
#   source("tests/manual/browser-test-preview-db.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "preview_with_db")
port <- 8128

launch_app(app_dir, port)
new_session(port)

check(
  "banner says database connected but not used",
  body_has("Database connected but not used")
)
shot("preview_db_banner.png")

click("input[name=\"fruit\"][value=\"banana\"]")
click("#page1_next", wait = 2)
check("survey completes without using the fake connection", body_has("End of the preview-with-db test survey"))

csv_file <- file.path(app_dir, "preview_data.csv")
check("responses stored in preview_data.csv", file.exists(csv_file))
if (file.exists(csv_file)) {
  d <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
  check("answer stored in CSV", nrow(d) == 1 && d$fruit[1] == "banana")
} else {
  check("answer stored in CSV", FALSE)
}

teardown(app_dir, port)

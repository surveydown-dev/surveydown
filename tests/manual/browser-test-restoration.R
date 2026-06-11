# Browser test for cookies and session/state restoration using chromote.
#
# Uses the bundled restoration app (use-cookies: true) to verify:
#   Phase A (page refresh, cookies persist):
#     - after a refresh, the respondent returns to the page they were on
#     - answers on the current page are restored from the cookie
#     - answers on previous pages are restored (via Previous button)
#     - the session is RESUMED: same session_id, still one CSV row
#   Phase B (browser close, fresh cookie jar):
#     - a new browser starts a fresh session on page 1 with empty inputs
#     - a second CSV row appears with a distinct session_id
#
# Run from the package root:
#   source("tests/manual/browser-test-restoration.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "restoration")
port <- 8127
csv_file <- file.path(app_dir, "preview_data.csv")

launch_app(app_dir, port)

read_data <- function() {
  if (!file.exists(csv_file)) {
    return(data.frame())
  }
  utils::read.csv(csv_file, stringsAsFactors = FALSE)
}

# -- Phase A: page refresh restores state --------------------------------

new_session(port)
click("input[name=\"fruit\"][value=\"banana\"]")
set_text("name", "Alice")
click("#page1_next", wait = 2)
set_text("age", "30")

reload() # refresh the page; cookies persist

check("A. returned to page2 after refresh", present("#container-age"))
check("A. current-page answer restored after refresh", input_val("#age") == "30")
shot("restore_A_page2.png")

click("#page2_prev", wait = 2)
check("A. previous page mc answer restored", is_checked("input[name=\"fruit\"][value=\"banana\"]"))
check("A. previous page text answer restored", input_val("#name") == "Alice")
shot("restore_A_page1.png")

d <- read_data()
check("A. still a single CSV row after refresh (session resumed)", nrow(d) == 1)
check("A. CSV kept page-1 answers", d$fruit[1] == "banana" && d$name[1] == "Alice")
session_a <- d$session_id[1]

# Finish the survey so page-2 data is saved
click("#page1_next", wait = 2)
check("A. page-2 answer still present after navigating forward", input_val("#age") == "30")
click("#page2_next", wait = 2)
check("A. reached end page", body_has("End of the restoration test survey"))

d <- read_data()
check("A. page-2 answer saved to CSV", as.character(d$age[1]) == "30")

# -- Phase B: fresh browser = new session --------------------------------

new_session(port, fresh_browser = TRUE)

check("B. fresh browser starts on page1", present("#container-fruit"))
check("B. mc input empty in fresh session", !is_checked("input[name=\"fruit\"][value=\"banana\"]"))
check("B. text input empty in fresh session", input_val("#name") == "")
shot("restore_B_fresh.png")

click("input[name=\"fruit\"][value=\"apple\"]")
set_text("name", "Bob")
click("#page1_next", wait = 2)
set_text("age", "25")
click("#page2_next", wait = 2)

d <- read_data()
check("B. two CSV rows after a second session", nrow(d) == 2)
check(
  "B. distinct session_ids per session",
  length(unique(d$session_id)) == 2
)
row_b <- d[d$session_id != session_a, ]
check(
  "B. second session stored its own answers",
  nrow(row_b) == 1 && row_b$fruit[1] == "apple" && row_b$name[1] == "Bob"
)

teardown(app_dir, port)

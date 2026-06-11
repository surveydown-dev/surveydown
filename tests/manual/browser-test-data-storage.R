# Browser test for data storage (preview mode / local CSV) using chromote.
#
# Walks the bundled data_storage app, answering every supported question
# type, then reads preview_data.csv and verifies that every value was
# stored correctly: raw values, pipe-joined multi-selects, slider
# label-to-value mapping, matrix sub-question columns, sd_store_value(),
# session metadata, and one-row-per-session.
#
# Run from the package root:
#   source("tests/manual/browser-test-data-storage.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "data_storage")
port <- 8126

launch_app(app_dir, port)
new_session(port)

# Page 1: mc, mc_multiple, text, numeric, select
click("input[name=\"fruit\"][value=\"banana\"]")
click("input[name=\"colors\"][value=\"red\"]")
click("input[name=\"colors\"][value=\"blue\"]")
set_text("name", "Alice")
set_text("age", "30")
set_select("size", "m")
shot("storage_page1.png")
click("#page1_next", wait = 2)

# Page 2: mc_buttons, mc_multiple_buttons, textarea, slider, slider_numeric
click("#pet input[value=\"dog\"]")
click("#drinks input[value=\"coffee\"]")
click("#drinks input[value=\"tea\"]")
set_text("comments", "hello world")
set_slider("satisfaction", 4) # 4th option = 'satisfied'
set_slider_numeric("rating", 7)
shot("storage_page2.png")
click("#page2_next", wait = 2)

# Page 3: date, daterange, matrix
set_date("birthday", "2024-06-15")
set_daterange("trip", "2024-06-01", "2024-06-30")
click("input[name=\"quality_speed\"][value=\"good\"]")
click("input[name=\"quality_support\"][value=\"bad\"]")
shot("storage_page3.png")
click("#page3_next", wait = 2)

check("reached end page", body_has("End of the data storage test survey"))

# -- Verify the stored CSV -----------------------------------------------

csv_file <- file.path(app_dir, "preview_data.csv")
check("preview_data.csv exists", file.exists(csv_file))

d <- utils::read.csv(csv_file, stringsAsFactors = FALSE)
val <- function(col) {
  if (!col %in% names(d)) return(NA_character_)
  as.character(d[[col]][1])
}

check("exactly one row stored for the session", nrow(d) == 1)
check("mc stored", val("fruit") == "banana")
check("mc_multiple stored pipe-joined", val("colors") == "red|blue")
check("text stored", val("name") == "Alice")
check("numeric stored", val("age") == "30")
check("select stored", val("size") == "m")
check("mc_buttons stored", val("pet") == "dog")
check("mc_multiple_buttons stored pipe-joined", val("drinks") == "coffee|tea")
check("textarea stored", val("comments") == "hello world")
check("slider stored mapped value (label -> value)", val("satisfaction") == "satisfied")
check("slider_numeric stored", val("rating") == "7")
check("date stored", val("birthday") == "2024-06-15")
check("daterange stored pipe-joined", val("trip") == "2024-06-01|2024-06-30")
check("matrix row 1 stored", val("quality_speed") == "good")
check("matrix row 2 stored", val("quality_support") == "bad")
check("sd_store_value stored", val("stored_test") == "abc123")
check("session_id present", !is.na(val("session_id")) && nzchar(val("session_id")))
check("time_start present", !is.na(val("time_start")) && nzchar(val("time_start")))
check(
  "question timestamp present (time_q_fruit)",
  !is.na(val("time_q_fruit")) && nzchar(val("time_q_fruit"))
)

teardown(app_dir, port)

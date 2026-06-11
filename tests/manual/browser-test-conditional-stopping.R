# Browser test for conditional stopping (sd_stop_if) using chromote.
#
# Verifies against the bundled conditional_stopping app that:
#   - stop conditions block navigation and show their message
#   - stop conditions take priority over required-question warnings
#   - the required-question warning appears once stops pass
#   - per-page applicability (the phone condition only fires on page 2)
#   - navigation proceeds once all conditions pass
#
# Run from the package root:
#   source("tests/manual/browser-test-conditional-stopping.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "conditional_stopping")
port <- 8125

launch_app(app_dir, port)
new_session(port)

# 1. Nothing answered: stop messages have priority over the required warning
click("#page1_next", wait = 2)
check("1. zip stop message shown (priority over required)", body_has("Zip code must be 5 digits"))
check("1. required warning NOT shown alongside stops", !body_has("Please answer all required questions"))
shot("stop_1_zip.png")
dismiss_alert()
check("1. still on page1", present("#container-zip"))

# 2. Valid zip, invalid yob
set_text("zip", "12345")
set_text("yob", "1850")
click("#page1_next", wait = 2)
check("2. yob stop message shown", body_has("Year of birth must be after 1900"))
check("2. zip message no longer shown", !body_has("Zip code must be 5 digits"))
dismiss_alert()

# 3. Stops pass, but required pet question unanswered
set_text("yob", "1990")
click("#page1_next", wait = 2)
check("3. required warning shown once stops pass", body_has("Please answer all required questions"))
shot("stop_3_required.png")
dismiss_alert()
check("3. still on page1", present("#container-zip"))

# 4. Everything valid: advance to page2
click("input[name=\"pet_preference\"][value=\"cat\"]")
click("#page1_next", wait = 2)
check("4. advanced to page2 once valid", present("#container-phone"))

# 5. Invalid phone blocks on page2
set_text("phone", "123")
click("#page2_next", wait = 2)
check("5. phone stop message shown", body_has("Phone number must be 10 digits"))
shot("stop_5_phone.png")
dismiss_alert()
check("5. still on page2", present("#container-phone"))

# 6. Valid phone: reach the end page
set_text("phone", "1234567890")
click("#page2_next", wait = 2)
check("6. reached end page with valid phone", body_has("end of the survey template"))
shot("stop_6_end.png")

teardown(app_dir, port)

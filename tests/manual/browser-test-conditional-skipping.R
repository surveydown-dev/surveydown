# Browser test for conditional skipping (sd_skip_if) using chromote.
#
# Verifies against the bundled conditional_skipping app that:
#   - required questions block navigation with a warning
#   - a simple condition skips to the screenout page
#   - non-matching answers proceed normally to the end page
#   - a complex (two-question) condition skips to the screenout page
#
# Run from the package root:
#   source("tests/manual/browser-test-conditional-skipping.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "conditional_skipping")
port <- 8124

launch_app(app_dir, port)

# Scenario A: required question blocks, then simple skip to screenout
new_session(port)
click("#welcome_next", wait = 2)
click("#basic_skipping_next", wait = 2) # nothing answered yet
check("A. required warning shown when unanswered", body_has("Please answer all required questions"))
shot("skip_A_required.png")
dismiss_alert()
check("A. still on basic_skipping page", present("#container-vehicle_simple"))
click("input[name=\"vehicle_simple\"][value=\"no\"]")
click("#basic_skipping_next", wait = 2)
check("A. skipped to screenout after No", body_has("Screenout page"))
shot("skip_A_screenout.png")

# Scenario B: no skip, mixed answers on complex page, normal end
new_session(port)
click("#welcome_next", wait = 2)
click("input[name=\"vehicle_simple\"][value=\"yes\"]")
click("#basic_skipping_next", wait = 2)
check("B. proceeded to complex page after Yes", present("#container-vehicle_complex"))
click("input[name=\"vehicle_complex\"][value=\"yes\"]")
click("input[name=\"buy_vehicle\"][value=\"no\"]")
click("#complex_skipping_next", wait = 2)
check("B. normal end page with mixed answers", body_has("normal end page"))
shot("skip_B_end.png")

# Scenario C: complex skip (both No) to screenout
new_session(port)
click("#welcome_next", wait = 2)
click("input[name=\"vehicle_simple\"][value=\"yes\"]")
click("#basic_skipping_next", wait = 2)
click("input[name=\"vehicle_complex\"][value=\"no\"]")
click("input[name=\"buy_vehicle\"][value=\"no\"]")
click("#complex_skipping_next", wait = 2)
check("C. skipped to screenout when both No", body_has("Screenout page"))
shot("skip_C_screenout.png")

teardown(app_dir, port)

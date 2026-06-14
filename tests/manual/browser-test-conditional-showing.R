# Browser test for conditional showing (sd_show_if) using chromote.
#
# Verifies that sd_show_if() reacts correctly for every condition style in
# the bundled conditional_showing app: sd_value(), multi-input AND,
# as.numeric() comparisons, custom functions wrapping input, conditional
# pages, and cross-page conditions written with input$ and all_data$ syntax.
#
# Run from the package root:
#   source("tests/manual/browser-test-conditional-showing.R")
# See tests/manual/README.md for requirements.

source(file.path("tests", "manual", "helpers.R"))

app_dir <- file.path("tests", "manual", "apps", "conditional_showing")
port <- 8123

launch_app(app_dir, port)
new_session(port)

# Page: welcome
click("#welcome_next", wait = 2)

# Page 1: basic_showif (same-page show/hide via sd_value)
check("1. other-question hidden initially", !visible("penguins_simple_other"))
click("input[name=\"penguins_simple\"][value=\"other\"]")
check("1. other-question shown after choosing Other", visible("penguins_simple_other"))
shot("1_basic_shown.png")
click("input[name=\"penguins_simple\"][value=\"adelie\"]")
check("1. other-question hidden again after choosing Adelie", !visible("penguins_simple_other"))
click("#basic_showif_next", wait = 2)

# Page 2: custom_showif (two-input AND condition)
click("input[name=\"penguins_complex\"][value=\"other\"]")
check("2. complex-other hidden with only Other chosen", !visible("penguins_complex_other"))
click("input[name=\"show_other\"][value=\"show\"]")
check("2. complex-other shown after Other + Show", visible("penguins_complex_other"))
shot("2_complex_shown.png")
click("input[name=\"show_other\"][value=\"hide\"]")
check("2. complex-other hidden after switching to Hide", !visible("penguins_complex_other"))
click("input[name=\"show_other\"][value=\"show\"]") # leave shown, answer it
set_text("penguins_complex_other", "rockhopper")
click("#custom_showif_next", wait = 2)

# Page 3: numeric condition (as.numeric(sd_value()) > 1)
set_text("car_number", "1")
check("3. ev question hidden with car_number = 1", !visible("ev_ownership"))
set_text("car_number", "3")
check("3. ev question shown with car_number = 3", visible("ev_ownership"))
shot("3_numeric_shown.png")
set_text("car_number", "1")
check("3. ev question hidden again with car_number = 1", !visible("ev_ownership"))
click("#numeric_show_if_next", wait = 2)

# Page 4: multiple inputs (mc_multiple_buttons)
click("#fav_fruits input[value=\"apple\"]")
check("4. apple/banana question shown after picking Apple", visible("apple_or_banana"))
shot("4_multi_shown.png")
click("#fav_fruits input[value=\"apple\"]") # unselect apple
click("#fav_fruits input[value=\"peach\"]")
check("4. apple/banana question hidden with only Peach", !visible("apple_or_banana"))
click("#multi_show_if_next", wait = 2)

# Page 5: custom function condition more_than_one_pet(input)
set_text("pet_number", "1")
check("5. pet-type hidden with pet_number = 1", !visible("pet_type"))
set_text("pet_number", "2")
check("5. pet-type shown with pet_number = 2 (custom function)", visible("pet_type"))
shot("5_custom_fn_shown.png")
click("#custom_function_next", wait = 2)

# Page 6: conditional page showing
click("input[name=\"pet_preference\"][value=\"cat\"]")
click("#conditional_page_next", wait = 2)
check("6. cat page shown after choosing Cat", body_has("This is the cat page"))
shot("6_cat_page.png")
click("#cat_page_next", wait = 2)
check("6. dog page skipped, landed on cross_page", present("#container-snow_preference"))

# Page 7: cross-page conditions (input$ and all_data$ syntax)
click("input[name=\"snow_preference\"][value=\"yes\"]")
click("#cross_page_next", wait = 2)
check("7. input$-target shown on next page after Yes", visible("snow_activity"))
check("7. all_data$-target shown on next page after Yes", visible("snow_memory"))
shot("7_crosspage_shown.png")

# Go back, flip the answer, return
click("#cross_page_targets_prev", wait = 2)
click("input[name=\"snow_preference\"][value=\"no\"]")
click("#cross_page_next", wait = 2)
check("7. input$-target hidden after switching to No", !visible("snow_activity"))
check("7. all_data$-target hidden after switching to No", !visible("snow_memory"))
shot("7_crosspage_hidden.png")

teardown(app_dir, port)

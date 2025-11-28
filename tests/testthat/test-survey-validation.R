test_that("Survey file validation works correctly", {
  # Test 1: Missing survey.qmd
  temp_dir <- create_test_survey(custom_app = generate_test_app())

  old_wd <- getwd()
  setwd(temp_dir)

  expect_false(surveydown:::survey_file_exists())

  setwd(old_wd)
  cleanup_test_survey(temp_dir)

  # Test 2: Missing app.R
  temp_dir <- create_test_survey(custom_qmd = generate_test_qmd())

  setwd(temp_dir)
  expect_false(surveydown:::app_file_exists())

  setwd(old_wd)
  cleanup_test_survey(temp_dir)

  # Test 3: Both files present
  temp_dir <- create_test_survey(
    custom_qmd = generate_test_qmd(),
    custom_app = generate_test_app()
  )

  setwd(temp_dir)
  expect_true(surveydown:::survey_file_exists())
  expect_true(surveydown:::app_file_exists())

  setwd(old_wd)
  cleanup_test_survey(temp_dir)
})

test_that("Page fence validation detects errors", {
  # Test: Mismatched page fences
  bad_qmd <- c(
    "---",
    "title: Test",
    "format: html",
    "---",
    "",
    "::: {#page1 .sd-page}",
    "Content",
    # Missing closing fence
    "",
    "::: {#page2 .sd-page}",
    "More content",
    ":::"
  )

  temp_dir <- tempfile()
  dir.create(temp_dir)
  writeLines(bad_qmd, file.path(temp_dir, "survey.qmd"))
  writeLines(generate_test_app(), file.path(temp_dir, "app.R"))

  validation <- validate_survey_files(temp_dir)

  expect_true("Mismatched page fence blocks" %in% validation$errors)

  cleanup_test_survey(temp_dir)
})

test_that("Survey templates can be created", {
  skip_on_cran()
  skip_if_offline()

  # Test default template
  temp_dir <- tempfile()

  expect_no_error(
    sd_create_survey(template = "default", path = temp_dir, ask = FALSE)
  )

  expect_true(file.exists(file.path(temp_dir, "survey.qmd")))
  expect_true(file.exists(file.path(temp_dir, "app.R")))

  cleanup_test_survey(temp_dir)
})

test_that("Invalid template names are rejected", {
  temp_dir <- tempfile()

  expect_error(
    sd_create_survey(template = "invalid_template_name", path = temp_dir, ask = FALSE),
    "Invalid template"
  )
})

test_that("Test helper functions work correctly", {
  # Test generate_test_qmd
  qmd_content <- generate_test_qmd(
    yaml_header = list(title = "Custom Title")
  )

  expect_true(any(grepl("Custom Title", qmd_content)))
  expect_true(any(grepl("#welcome", qmd_content)))
  expect_true(any(grepl(".sd-page", qmd_content)))

  # Test generate_test_app
  app_content <- generate_test_app(
    server_params = list(show_previous = TRUE, use_cookies = FALSE)
  )

  expect_true(any(grepl("library\\(surveydown\\)", app_content)))
  expect_true(any(grepl("show_previous = TRUE", app_content)))
  expect_true(any(grepl("use_cookies = FALSE", app_content)))
})
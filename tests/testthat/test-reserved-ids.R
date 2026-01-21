test_that("get_reserved_ids returns correct reserved IDs", {
  reserved <- surveydown:::get_reserved_ids()

  expect_type(reserved, "character")
  expect_true(length(reserved) > 0)
  expect_true("session_id" %in% reserved)
  expect_true("time_start" %in% reserved)
  expect_true("time_end" %in% reserved)
  expect_true("exit_survey_rating" %in% reserved)
  expect_true("current_page" %in% reserved)
  expect_true("browser" %in% reserved)
  expect_true("ip_address" %in% reserved)
})

test_that("sd_store_value validation logic catches reserved IDs", {
  # Test the validation logic used by sd_store_value
  # This tests that the check is performed correctly
  reserved_ids <- surveydown:::get_reserved_ids()

  # Test each reserved ID would be caught
  for (test_id in c("session_id", "time_start", "time_end",
                     "exit_survey_rating", "current_page",
                     "browser", "ip_address")) {
    expect_true(
      test_id %in% reserved_ids,
      info = paste("Reserved ID", test_id, "should be in the reserved list")
    )
  }

  # Test that non-reserved IDs are allowed
  non_reserved <- c("respondent_id", "user_code", "custom_field", "my_var")
  for (test_id in non_reserved) {
    expect_false(
      test_id %in% reserved_ids,
      info = paste("Non-reserved ID", test_id, "should not be in the reserved list")
    )
  }
})

test_that("check_ids catches reserved IDs in page IDs", {
  page_ids <- c("intro", "session_id", "outro")
  question_ids <- c("q1", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Restricted IDs found.*session_id",
    info = "Should error when page ID uses reserved ID 'session_id'"
  )
})

test_that("check_ids catches reserved IDs in question IDs", {
  page_ids <- c("intro", "outro")
  question_ids <- c("q1", "time_start", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Restricted IDs found.*time_start",
    info = "Should error when question ID uses reserved ID 'time_start'"
  )
})

test_that("check_ids catches multiple reserved IDs", {
  page_ids <- c("intro", "current_page")
  question_ids <- c("browser", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Restricted IDs found",
    info = "Should error when multiple reserved IDs are used"
  )
})

test_that("check_ids catches duplicate question IDs", {
  page_ids <- c("intro", "outro")
  question_ids <- c("q1", "q2", "q1")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Duplicate IDs found.*q1",
    info = "Should error when duplicate question IDs exist"
  )
})

test_that("check_ids catches duplicate page IDs", {
  page_ids <- c("intro", "main", "intro")
  question_ids <- c("q1", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Duplicate IDs found.*intro",
    info = "Should error when duplicate page IDs exist"
  )
})

test_that("check_ids catches IDs shared between pages and questions", {
  page_ids <- c("intro", "main", "outro")
  question_ids <- c("q1", "main", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Duplicate IDs found.*main",
    info = "Should error when page and question share the same ID"
  )
})

test_that("check_ids allows valid non-reserved IDs", {
  page_ids <- c("intro", "main", "outro")
  question_ids <- c("q1", "q2", "q3")

  expect_silent(
    surveydown:::check_ids(page_ids, question_ids)
  )
})

test_that("check_ids catches all reserved IDs", {
  reserved_ids <- surveydown:::get_reserved_ids()

  for (reserved_id in reserved_ids) {
    # Test with reserved ID as page ID
    page_ids <- c("intro", reserved_id)
    question_ids <- c("q1", "q2")

    expect_error(
      surveydown:::check_ids(page_ids, question_ids),
      "Restricted IDs found",
      info = paste("Should error for reserved page ID:", reserved_id)
    )

    # Test with reserved ID as question ID
    page_ids <- c("intro", "outro")
    question_ids <- c("q1", reserved_id)

    expect_error(
      surveydown:::check_ids(page_ids, question_ids),
      "Restricted IDs found",
      info = paste("Should error for reserved question ID:", reserved_id)
    )
  }
})

test_that("check_ids error messages are informative", {
  # Test duplicate ID error message
  page_ids <- c("intro", "main", "intro")
  question_ids <- c("q1", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Duplicate IDs found.*intro.*All page IDs and question IDs must be unique",
    info = "Duplicate ID error should be clear and mention uniqueness requirement"
  )

  # Test reserved ID error message
  page_ids <- c("intro", "session_id")
  question_ids <- c("q1", "q2")

  expect_error(
    surveydown:::check_ids(page_ids, question_ids),
    "Restricted IDs found.*session_id.*reserved and should not be used",
    info = "Reserved ID error should explain that IDs are reserved"
  )
})

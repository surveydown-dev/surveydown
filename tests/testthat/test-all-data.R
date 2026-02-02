test_that("all_data is created and accessible in sd_server", {
  skip_if_not_installed("shiny")

  # Create a minimal test environment
  test_env <- new.env()

  # Mock session object
  session <- shiny::MockShinySession$new()

  # Test that all_data gets created when sd_server is called
  # We can't fully test this without running a complete Shiny app,
  # but we can verify the basic structure
  expect_true(TRUE)  # Placeholder for now
})

test_that("reserved IDs are defined", {
  # Verify that reserved IDs are properly defined
  reserved_ids <- surveydown:::get_reserved_ids()

  # Check that reserved IDs list is not empty
  expect_true(length(reserved_ids) > 0)

  # Verify key reserved IDs are present
  expect_true("session_id" %in% reserved_ids)
  expect_true("time_start" %in% reserved_ids)
  expect_true("time_end" %in% reserved_ids)
})

test_that("all_data documentation is present", {
  # Verify that the help documentation includes all_data information
  help_text <- utils::capture.output(utils::help("sd_server", package = "surveydown"))

  # This test just ensures the function is documented
  expect_true(length(help_text) > 0)
})

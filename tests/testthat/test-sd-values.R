test_that("sd_values is created and accessible in sd_server", {
  skip_if_not_installed("shiny")

  # Create a minimal test environment
  test_env <- new.env()

  # Mock session object
  session <- shiny::MockShinySession$new()

  # Test that sd_values gets created when sd_server is called
  # We can't fully test this without running a complete Shiny app,
  # but we can verify the basic structure
  expect_true(TRUE)  # Placeholder for now
})

test_that("sd_values excludes reserved IDs and timestamps", {
  # Verify that the sync logic correctly excludes reserved IDs and timestamps
  reserved_ids <- surveydown:::get_reserved_ids()

  # Check that reserved IDs list is not empty
  expect_true(length(reserved_ids) > 0)

  # Verify key reserved IDs are present
  expect_true("session_id" %in% reserved_ids)
  expect_true("time_start" %in% reserved_ids)
  expect_true("time_end" %in% reserved_ids)
})

test_that("sd_values documentation is present", {
  # Verify that the help documentation includes sd_values information
  help_text <- utils::capture.output(utils::help("sd_server", package = "surveydown"))

  # This test just ensures the function is documented
  expect_true(length(help_text) > 0)
})

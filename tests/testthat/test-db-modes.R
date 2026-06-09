# Tests for db mode changes: mode removed from sd_db_connect(),
# ignore deprecation, and get_session_data() csv_file parameter

test_that("sd_db_connect() no longer accepts a mode argument", {
  expect_error(
    sd_db_connect(mode = "preview"),
    "unused argument"
  )
})

test_that("sd_db_connect(ignore = TRUE) returns NULL with deprecation warning", {
  expect_warning(
    result <- sd_db_connect(ignore = TRUE),
    "deprecated"
  )
  expect_null(result)
})

test_that("sd_db_connect(ignore = FALSE) warns about deprecation", {
  # Any non-NULL value of ignore triggers the deprecation warning
  expect_warning(
    sd_db_connect(ignore = FALSE),
    "deprecated"
  )
})

test_that("get_session_data() returns NULL when db is NULL and no CSV exists", {
  result <- surveydown:::get_session_data(
    db = NULL,
    search_session_id = "test-session-123"
  )
  expect_null(result)
})

test_that("get_session_data() reads from csv_file when db is NULL", {
  tmp <- tempfile(fileext = ".csv")
  data <- data.frame(
    session_id = "abc123",
    q1 = "answer",
    stringsAsFactors = FALSE
  )
  utils::write.csv(data, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- surveydown:::get_session_data(
    db = NULL,
    search_session_id = "abc123",
    csv_file = tmp
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$q1, "answer")
})

test_that("get_session_data() returns zero-row frame for non-matching session", {
  tmp <- tempfile(fileext = ".csv")
  data <- data.frame(
    session_id = "abc123",
    q1 = "answer",
    stringsAsFactors = FALSE
  )
  utils::write.csv(data, tmp, row.names = FALSE)
  on.exit(unlink(tmp))

  result <- surveydown:::get_session_data(
    db = NULL,
    search_session_id = "no-match",
    csv_file = tmp
  )
  expect_equal(nrow(result), 0L)
})

test_that("get_session_data() respects csv_file routing (preview vs local)", {
  # Create two separate CSV files simulating preview_data.csv and local_data.csv
  preview_file <- tempfile(fileext = ".csv")
  local_file <- tempfile(fileext = ".csv")

  preview_data <- data.frame(
    session_id = "session-preview",
    answer = "from_preview",
    stringsAsFactors = FALSE
  )
  local_data <- data.frame(
    session_id = "session-local",
    answer = "from_local",
    stringsAsFactors = FALSE
  )
  utils::write.csv(preview_data, preview_file, row.names = FALSE)
  utils::write.csv(local_data, local_file, row.names = FALSE)
  on.exit({
    unlink(preview_file)
    unlink(local_file)
  })

  result_preview <- surveydown:::get_session_data(
    db = NULL,
    search_session_id = "session-preview",
    csv_file = preview_file
  )
  result_local <- surveydown:::get_session_data(
    db = NULL,
    search_session_id = "session-local",
    csv_file = local_file
  )

  expect_equal(result_preview$answer, "from_preview")
  expect_equal(result_local$answer, "from_local")
})

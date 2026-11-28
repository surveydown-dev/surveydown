test_that("is_blank_value treats NULL, empty, NA and \"\" as blank", {
  expect_true(is_blank_value(NULL))
  expect_true(is_blank_value(character(0)))
  expect_true(is_blank_value(NA))
  expect_true(is_blank_value(""))
})

test_that("is_blank_value accepts answers that are not strings", {
  # A date question stores a Date; comparing it with "" gives NA
  expect_false(is_blank_value(as.Date("2026-11-28")))
  expect_false(is_blank_value(5))
  expect_false(is_blank_value("a|b"))
  expect_false(is_blank_value(c("a", "b")))
})

context("skipif logic")
library(surveydown)

test_that("skipif is properly formatted", {
  expect_error(sd_config(skip_if = 'foo'))
})

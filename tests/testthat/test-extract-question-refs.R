test_that("extract_question_refs finds input$ and all_data$ references", {
  f <- surveydown:::extract_question_refs
  expect_equal(f(quote(input$age > 5)), "age")
  expect_equal(
    sort(f(quote(all_data$a == "x" & input$b == "y"))),
    c("a", "b")
  )
})

test_that("extract_question_refs finds sd_value and sd_values references", {
  f <- surveydown:::extract_question_refs
  expect_equal(
    f(quote(sd_value("first") * sd_value(second))),
    c("first", "second")
  )
  expect_equal(
    sort(f(quote(all(sd_values(a, b) == c("no", "no"))))),
    c("a", "b")
  )
  expect_equal(f(quote(nchar(sd_value("zip")) != 5)), "zip")
})

test_that("extract_question_refs finds references in nested calls", {
  f <- surveydown:::extract_question_refs
  expect_equal(
    f(quote(paste("x", as.numeric(input$count) + 1))),
    "count"
  )
})

test_that("extract_question_refs returns empty for plain locals", {
  f <- surveydown:::extract_question_refs
  expect_equal(f(quote(n1 * n2)), character(0))
  expect_equal(f(quote(TRUE)), character(0))
})

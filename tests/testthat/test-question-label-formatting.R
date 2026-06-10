# Regression tests for preserving inline label formatting (e.g. **bold**) when a
# question is option-shuffled. Shuffled mc/mc_multiple questions are re-rendered
# on the client from the parsed question structure, so the structure must carry
# the label's inline HTML (`label_html`) in addition to the plain-text `label`.

test_that("extract_question_structure_html keeps label plain and label_html formatted", {
  html <- rvest::read_html(paste0(
    '<div class="question-container" data-question-id="q_fmt">',
    '<div id="q_fmt" class="form-group shiny-input-checkboxgroup shiny-input-container">',
    '<label class="control-label" id="q_fmt-label" for="q_fmt">',
    '<p>Select <strong>all</strong> that apply:</p>',
    '</label>',
    '<div class="shiny-options-group">',
    '<div class="checkbox"><label>',
    '<input type="checkbox" name="q_fmt" value="a"/><span>Option A</span>',
    '</label></div>',
    '<div class="checkbox"><label>',
    '<input type="checkbox" name="q_fmt" value="b"/><span>Option B</span>',
    '</label></div>',
    '</div>',
    '</div>',
    '</div>'
  ))

  structure <- surveydown:::extract_question_structure_html(html)
  expect_true("q_fmt" %in% names(structure))
  q <- structure[["q_fmt"]]

  # Plain-text label drops all markup (it feeds the `<id>_label_question` text
  # output, which would otherwise show literal <strong> tags).
  expect_equal(q$label, "Select all that apply:")
  expect_false(grepl("<strong>", q$label, fixed = TRUE))

  # label_html preserves the inline formatting for the shuffled re-render.
  expect_match(q$label_html, "Select", fixed = TRUE)
  expect_match(q$label_html, "<strong>all</strong>", fixed = TRUE)
  expect_match(q$label_html, "that apply", fixed = TRUE)
})

test_that("label_html falls back to the plain label when there is no formatting", {
  html <- rvest::read_html(paste0(
    '<div class="question-container" data-question-id="q_plain">',
    '<div id="q_plain" class="form-group shiny-input-radiogroup shiny-input-container">',
    '<label class="control-label" id="q_plain-label" for="q_plain">',
    '<p>Plain label</p>',
    '</label>',
    '</div>',
    '</div>'
  ))

  q <- surveydown:::extract_question_structure_html(html)[["q_plain"]]
  expect_equal(q$label, "Plain label")
  expect_equal(q$label_html, "Plain label")
})

test_that("markdown_to_html passes inline HTML through, so re-rendered labels stay formatted", {
  # generate_shuffled_mc_html() feeds label_html back through sd_question(),
  # which runs markdown_to_html(). That must preserve the <strong> tag.
  out <- as.character(
    surveydown:::markdown_to_html("Select <strong>all</strong> that apply:")
  )
  expect_match(out, "<strong>all</strong>", fixed = TRUE)
})

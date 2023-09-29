# The question function to render a Shiny input based on question type
question <- function(type, id, label, choices = NULL, conditional = NULL) {
  if (is.null(conditional) || (is.reactive(conditional) && isTRUE(conditional()))) {
    switch(type,
      text = textInput(id, label),
      select = selectInput(id, label, choices),
      checkbox = checkboxGroupInput(id, label, choices),
      # Add more input types as needed
    )
  } else {
    return(NULL)
  }
}

# Example function to get color choices for a respondent
get_color_choices_for_respondent <- function(id) {
  # In a real-world scenario, you'd load this from a database or another source
  responses <- list(
    '1' = c("Red", "Blue"),
    '2' = c("Orange", "Green")
  )
  responses[[id]]
}

prep_survey <- function(qmd_file) {
  # Capture the HTML output
  captured_output <- capture.output(quarto::quarto_render(qmd_file, output_format = "revealjs"))

  # Extract the section where HTML starts and ends
  start_idx <- which(grepl("<!DOCTYPE html>", captured_output))
  end_idx <- which(grepl("</html>", captured_output))

  # If we don't find the markers, return an error
  if (length(start_idx) == 0 || length(end_idx) == 0) {
    stop("Failed to identify the start or end of the HTML content.")
  }

  # Combine the output to form the full HTML content
  full_html <- paste(captured_output[start_idx:end_idx], collapse = "\n")

  # Parse the resulting HTML
  parsed_html <- xml2::read_html(full_html)
  sections <- parsed_html %>% rvest::html_nodes("section")
  html_sections <- lapply(sections, as.character)

  return(html_sections)
}

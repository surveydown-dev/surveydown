question <- function(type, id, prompt, choices = NULL) {
  switch(type,
         text = shiny::textInput(id, prompt),
         checkbox = shiny::checkboxGroupInput(id, prompt, choices),
         radio = shiny::radioButtons(id, prompt, choices),
         stop(paste("Unknown type:", type))
  )
}

parse_survey_pages <- function(file_path) {
  # Split the document into pages using `knitr::split_file()`
  pages_split <- knitr:::split_file(file_path)

  pages <- lapply(pages_split, function(page_file) {
    # Extract R code using `knitr::purl()`
    r_code <- knitr::purl(page_file, quiet = TRUE)
    r_code_content <- readLines(r_code, warn = FALSE)
    unlink(r_code)  # delete the temporary .R file

    # Evaluate the R code
    eval(parse(text = r_code_content))
  })

  # Clean up temporary split files
  file.remove(pages_split)

  return(pages)
}

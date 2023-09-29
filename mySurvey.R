# Module UI
surveyUI <- function(id) {
  ns <- NS(id)
  pages <- parse_survey_pages('survey.qmd')
  total_pages <- length(pages)

  # Create div containers for each page
  page_containers <- lapply(1:total_pages, function(i) {
    div(id = ns(paste0("page_", i)),
        do.call(shiny::tagList, lapply(pages[[i]], function(el) el))
    )
  })

  tagList(
    page_containers,
    div(
      actionButton(ns('prev_button'), 'Previous', style = 'margin-right: 10px;'),
      actionButton(ns('next_button'), 'Next')
    )
  )
}

# Module Server
surveyServer <- function(input, output, session) {
  ns <- session$ns
  pages <- parse_survey_pages('survey.qmd')
  total_pages <- length(pages)
  current_page <- reactiveVal(1)

  responses <- reactiveVal(list())

  # Initially, show the first page and hide others
  observe({
    for (i in 1:total_pages) {
      if (i == current_page()) {
        shinyjs::show(paste0("page_", i))
      } else {
        shinyjs::hide(paste0("page_", i))
      }
    }
  })

  observeEvent(input$next_button, {
    if (current_page() < total_pages) {
      current_page(current_page() + 1)
    }
  })

  observeEvent(input$prev_button, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })

  return(list(
    getResponses = responses
  ))
}

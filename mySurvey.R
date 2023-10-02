surveyUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$iframe(src = "www/survey.html", width="100%", height="800px", id = ns("survey_frame"), frameborder = "0", scrolling = "no"),

    # Add navigation buttons below the iframe
    div(
      id = ns("navigation"),
      actionButton(inputId = ns("prev_btn"), label = "Previous", class = "btn-primary"),
      actionButton(inputId = ns("next_btn"), label = "Next", class = "btn-primary")
    )
  )
}

surveyPageServer <- function(input, output, session) {
  observeEvent(input$next_btn, {
    session$sendCustomMessage(type = "iframeMessage", message = "next")
  })

  observeEvent(input$prev_btn, {
    session$sendCustomMessage(type = "iframeMessage", message = "previous")
  })
}

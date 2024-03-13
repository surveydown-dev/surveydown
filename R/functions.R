library(shiny)
library(shinyjs)

# UI functions ----

# Modified version of sd_question to include a class assignment
sd_question <- function(
  name,
  type,
  required = FALSE,
  label    = "label",
  option   = NULL,
  dependence = NULL,
  dependence_value = NULL
) {
  output <- NULL

  if (type ==  "select") {
    output <- shiny::selectizeInput(
      inputId = name,
      label = label,
      choices = option,
    )
  } else if (type == "mc") {
    output <- shiny::radioButtons(
      inputId = name,
      label = label,
      choices = option
    )
  } else if (type == "text") {
    output <- shiny::textInput(
      inputId = name,
      label = label,
      placeholder = option,
    )
  }

  return(output)
}

sd_next <- function(pageId, label = 'Next') {
  actionButton(inputId = paste0("next", pageId), label)
}

# Server functions ----

sd_server <- function(question_ids, n_pages, input, session) {

  # DB operations ----

  # Define a reactive expression that combines all registered questions
  input_vals <- reactive({
    sapply(
      question_ids,
      function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE)
  })

  # Use observe to react whenever 'input_vals' changes
  # If it changes, update the DB
  observe({

    # Capture the current state of inputs
    vals <- input_vals()

    # Transform to data frame, handling uninitialized inputs appropriately
    data <- transform_data(vals, question_ids, session)

    # Save data
    readr::write_csv(data, 'data.csv')

  })

  # Page Navigation ----

  observe({
    # This loop creates an observer for each "next" button dynamically
    for (i in 1:(n_pages - 1)) {
      local({
        current_page <- i
        observeEvent(input[[paste0("next", current_page)]], {
          shinyjs::hide(paste0("page-", current_page))
          shinyjs::show(paste0("page-", current_page + 1))
        })
      })
    }
  })

}

transform_data <- function(vals, question_ids, session) {

  data <- data.frame(
    session_id = session$token,
    timestamp = Sys.time()
  )

  # Assuming vals is a list of inputs from input_vals()
  names(vals) <- question_ids
  responses <- as.data.frame(vals)

  # Add session_id and timestamp
  data <- cbind(data, responses)

  return(data)
}

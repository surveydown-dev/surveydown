library(shiny)
library(shinyjs)
library(tibble)

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
      choices = option
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
      placeholder = option
    )
  }

  return(output)
}

# Server functions ----

sd_server <- function(input, session, question_ids, n_pages, skip_logic = NULL) {

  # DB operations ----

  # Define a reactive expression that combines all registered questions
  input_vals <- reactive({
    temp <- sapply(
      question_ids,
      function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE
    )
    names(temp) <- question_ids
    temp
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
    # Loop creates an observer for each "next" button dynamically
    for (i in 1:(n_pages - 1)) {
      local({
        current_page <- i
        observeEvent(input[[paste0("next", current_page)]], {

          # Default next page logic
          next_page <- current_page + 1

          # Check for skip logic
          if (!is.null(skip_logic)) {
            vals <- input_vals()
            for (j in 1:nrow(skip_logic)) {
              rule <- skip_logic[j,]
              if (vals[[rule$question_id]] == rule$response_value) {
                next_page <- rule$target_page
                break # Skip logic applies, no need to check further rules
              }
            }
          }

          # Execute page navigation
          shinyjs::hide(paste0("page-", current_page))
          shinyjs::show(paste0("page-", next_page))

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

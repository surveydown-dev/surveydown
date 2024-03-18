library(shiny)
library(shinyjs)

# UI functions ----

updateSurveyStructure <- function(questionId) {
  filePath <- "attr_question_ids.json"

  # Check if the file exists and read it; if not, create a new structure
  if (file.exists(filePath)) {
    surveyStructure <- jsonlite::fromJSON(filePath)
  } else {
    surveyStructure <- list(questionIds = vector("list"))
  }

  # Add the new question ID to the list
  surveyStructure$questionIds <- c(surveyStructure$questionIds, questionId)

  # Save the updated structure back to the file
  jsonlite::write_json(surveyStructure, filePath)
}


sd_question <- function(
  name,
  type,
  required = FALSE,
  label    = "label",
  option   = NULL
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

  # Update the survey structure JSON with the new question ID
  updateSurveyStructure(name)

  return(output)

}

# Server functions ----

sd_server <- function(
  input,
  session,
  skip_logic = NULL,
  showif = NULL
) {

  # Get page_count and question_ids from json objects
  page_count <- jsonlite::fromJSON("attr_page_count.json")$pageCount
  question_ids <- jsonlite::fromJSON("attr_question_ids.json")$questionIds

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

          # Assume default next page logic
          next_page <- current_page + 1

          # Apply skip logic if specified
          if (!is.null(skip_logic)) {
            vals <- input_vals()

            # Iterate through each skip rule
            for (j in 1:nrow(skip_logic)) {
              rule <- skip_logic[j, ]
              question_response <- vals[[rule$question_id]]

              # If a skip condition is met, update next_page accordingly
              if (
                !is.null(question_response) &
                (question_response == rule$response_value) &
                (current_page != rule$target_page)
              ) {
                next_page <- rule$target_page
                break # Found a matching skip rule, no need to check further
              }
            }
          }

          # Execute page navigation, considering skip logic adjustments
          shinyjs::hide(paste0("page-", current_page))
          shinyjs::show(paste0("page-", next_page))

        })
      })
    }
  })

  # showif conditions ----

  # Implement conditional display logic if 'showif' is provided
  if (!is.null(showif)) {
    observe({
      for (i in 1:nrow(showif)) {
        handle_showif_condition(showif[i,], input)
      }
    })
  }

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

# Function to handle the showif conditional display of questions
handle_showif_condition <- function(condition, input) {

  # Listen for changes to the dependent question
  observeEvent(input[[condition$question_dependence]], {

    # Check if the response matches the condition for displaying the question
    if (input[[condition$question_dependence]] == condition$response_value) {
      shinyjs::show(condition$question_id)
    } else {
      shinyjs::hide(condition$question_id)
    }
  }, ignoreNULL = TRUE)
}

library(shiny)
library(shinyjs)

# UI functions ----

sd_question <- function(
  name,
  type,
  required = FALSE,
  label    = "label",
  option   = NULL,
  label_null = NULL
) {

  output <- NULL

  if (is.null(label_null)) {
    label_null <- 'Choose an option...'
  }

  if (type ==  "select") {
    option <- c('', option)
    names(option)[1] <- label_null

    output <- shiny::selectInput(
      inputId = name,
      label = label,
      choices = option,
      multiple = FALSE,
      selected = FALSE,
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId = name,
      label = label,
      choices = option,
      selected = FALSE
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId = name,
      label = label,
      choices = option,
      selected = FALSE
    )

  } else if (type == "text") {

    output <- shiny::textInput(
      inputId = name,
      label = label,
      placeholder = option
    )

  } else if (type == "numeric") {

    output <- shiny::numericInput(
      inputId = name,
      label = label,
      value = NULL
    )

  }

  return(output)

}

# Server functions ----

sd_server <- function(
  input,
  session,
  question_ids = NULL,
  skip_logic = NULL,
  showif = NULL
) {

  # showif conditions ----

  # Implement conditional display logic if 'showif' is provided
  if (!is.null(showif)) {

    # By default, hide all conditional questions
    for (i in 1:nrow(showif)) {
      shinyjs::hide(showif[i,]$question_id)
    }

    # Then look for whether the showif condition is met to show the question
    observe({
      for (i in 1:nrow(showif)) {
        row <- showif[i,]

        # Listen for changes to the dependent question
        observeEvent(input[[row$question_dependence]], {

          # Check if the response matches the condition for displaying the question
          if (input[[row$question_dependence]] == row$response_value) {
            shinyjs::show(row$question_id)
          } else {
            shinyjs::hide(row$question_id)
          }
        }, ignoreNULL = TRUE)

      }
    })
  }

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

  # Read the pages information into a data frame
  page_df <- jsonlite::fromJSON(".survey_pages.json")$pages
  page_count <- nrow(page_df)
  observe({
    for (i in 1:(page_count - 1)) {
      local({

        # Capture the current iteration's variables in a local environment
        current_page <- page_df$name[i]
        next_page <- page_df$name[i + 1]

        observeEvent(input[[paste0("next-", current_page)]], {

          # Logic to determine the correct next page, considering skip logic
          actual_next_page <- next_page
          if (!is.null(skip_logic)) {
            vals <- input_vals()

            for (j in 1:nrow(skip_logic)) {
              rule <- skip_logic[j, ]
              question_response <- vals[[rule$question_id]]

              if (!is.null(question_response) &&
                  question_response == rule$response_value &&
                  current_page != rule$target_page) {
                actual_next_page <- rule$target_page
                break # Found a matching skip rule, no need to check further
              }
            }
          }

          # Execute page navigation
          shinyjs::hide(current_page)
          shinyjs::show(actual_next_page)
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

  # Replace any NULL values with ''
  for (i in 1:length(vals)) {
    if (is.null(vals[[i]])) {
      vals[[i]] <- ''
    }
  }

  # Assuming vals is a list of inputs from input_vals()
  names(vals) <- question_ids
  responses <- as.data.frame(vals)

  # Add session_id and timestamp
  data <- cbind(data, responses)

  return(data)
}

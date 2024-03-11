library(shiny)
library(googlesheets4)
library(shinyjs)

# UI functions

# Function to add a question ID to the registry
registerQuestion <- function(id) {
  questionRegistry[[id]] <<- TRUE
}

# Function to get all registered question IDs
getRegisteredQuestions <- function(questionRegistry) {
  return(names(questionRegistry))
}

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

  # Register the question ID
  registerQuestion(name)

  return(output)
}

sd_next <- function(pageId, label = 'Next') {
  actionButton(inputId = paste0("next", pageId), label)
}

# Server functions

sd_server <- function(db_url, questionRegistry, input, session) {

  # Get all registered question IDs
  question_ids <- getRegisteredQuestions(questionRegistry)

  # Define a reactive expression that combines all registered questions
  input_vals <- reactive({
    sapply(
      question_ids,
      function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE)
  })

  # Use observe to react whenever 'input_vals' changes
  observe({
    vals <- input_vals()
    update_google_sheet(db_url, input, session)
  })

  # Page Navigation ----

  total_pages <- 4  # Update this based on your actual number of pages

  observe({
    # This loop creates an observer for each "next" button dynamically
    for (i in 1:(total_pages - 1)) {
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

# Define a function to update Google Sheets
update_google_sheet <- function(db_url, input, session) {
  data <- get_data(input, session)
  sheet <- read_sheet(db_url)

  # Update the sheet row based on the session ID
  if (data$session_id %in% sheet$session_id) {
    sheet[which(sheet$session_id == data$session_id),] <- data
    write_sheet(ss = db_url, data = sheet, sheet = 'data')
  } else {
    # If session ID is missing, initialize the row
    sheet_append(ss = db_url, data = data, sheet = 'data')
  }
}

get_data <- function(input, session) {
  return(data.frame(
    session_id = session$token,
    timestamp = Sys.time(),
    color = ifelse(is.null(input$color), '', input$color),
    animal = ifelse(is.null(input$animal) || input$animal == '', 'NA', input$animal),
    education = ifelse(is.null(input$education), '', input$education)
  ))
}

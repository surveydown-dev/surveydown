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

sd_next <- function(next_page = NULL, label = "Next") {
  if (is.null(next_page)) {
    stop("You must specify the current_page for the 'Next' button.")
  }

  shiny::actionButton(
    inputId = make_next_button_id(next_page),
    label = label,
    onclick = sprintf("Shiny.setInputValue('next_page', '%s');", next_page)
  )
}

make_next_button_id <- function(next_page) {
  return(paste0("next-", next_page))
}

# Server functions ----

sd_server <- function(
  input,
  session,
  skip_if = NULL,
  skip_if_complex = NULL,
  show_if = NULL
) {

  # Get survey metadata

  page_metadata <- get_page_metadata()
  page_names <- names(page_metadata)
  question_ids <- unname(unlist(page_metadata))

  # Conditional display (show_if conditions) ----

  if (!is.null(show_if)) { handle_show_if_logic(input, show_if) }

  # Page Navigation ----

  observe({
    for (i in 2:length(page_metadata)) {
      local({

        # Define current and next page based on iteration
        next_page <- page_names[i]

        observeEvent(input[[make_next_button_id(next_page)]], {

          # Update next page with any basic skip logic
          if (!is.null(skip_if)) {
            next_page <- handle_basic_skip_logic(input, skip_if, next_page)
          }

          # Update next page with any complex skip logic
          if (!is.null(skip_if_complex)) {
            next_page <- handle_complex_skip_logic(input, skip_if_complex, next_page)
          }

          # Execute page navigation
          shinyjs::runjs('hideAllPages();') # Hide all pages
          shinyjs::show(next_page) # Show next page
        })
      })
    }
  })

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

}

get_page_metadata <- function() {

  pages <- get_page_nodes()
  page_names <- unlist(lapply(pages, function(x) rvest::html_attr(x, "id")))

  # Initialize a list to hold the results
  page_metadata <- list()

  # Iterate over each page and extract the Shiny widget IDs
  for (i in seq_len(length(page_names))) {

    # Extract the page ID
    page_id <- page_names[i]

    # Find all containers that might have an ID
    containers <- pages[i] |> rvest::html_nodes(".shiny-input-container")

    # Initialize a vector to store widget IDs for this page
    widget_ids <- character()

    # Iterate over containers to extract IDs
    for (container in containers) {
      # Direct ID from the container
      container_id <- rvest::html_attr(container, "id")
      if (!is.na(container_id)) {
        widget_ids <- c(widget_ids, container_id)
      }

      # IDs from input/select/textarea elements within the container
      input_ids <- container |>
        rvest::html_nodes("input, select, textarea") |>
        rvest::html_attr("id")
      widget_ids <- c(widget_ids, input_ids[!is.na(input_ids)])
    }

    # Store the results
    page_metadata[[page_id]] <- unique(widget_ids)
  }

  return(page_metadata)
}

get_page_nodes <- function() {

  # Get the list of .qmd files in the current working directory
  qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)

  # Check if there is exactly one .qmd file
  if (length(qmd_files) == 1) {
    qmd_file_name <- qmd_files[1]
    html_file_name <- sub("\\.qmd$", ".html", qmd_file_name)

    # Use the derived HTML file name to read the document with rvest
    pages <- rvest::read_html(html_file_name) |>
      rvest::html_nodes(".sd-page")
    return(pages)
  }

  stop("Error: Expected exactly one .qmd file in the directory.")

}

handle_show_if_logic <- function(input, show_if) {

  # Initially hide all conditional questions
  lapply(show_if, function(x) shinyjs::hide(x$target_question))

  # Iterate over each show_if rule
  lapply(show_if, function(rule) {
    observeEvent(input[[rule$dependent_question]], {
      # Check if the condition is met to show/hide the question
      if (rule$condition(input)) {
        shinyjs::show(rule$target_question)
      } else {
        shinyjs::hide(rule$target_question)
      }
    }, ignoreNULL = TRUE)
  })
}

handle_basic_skip_logic <- function(input, skip_if, next_page) {

  # Ensure skip_if is a tibble or data frame
  if (!is.data.frame(skip_if)) {
    stop("skip_if must be a data frame or tibble.")
  }

  for (i in 1:nrow(skip_if)) {
    rule <- skip_if[i,]
    val <- input[[rule$question_id]]
    if (!is.null(val)) {
      if (val == rule$target_value) {
        return(rule$target_page)
      }
    }
  }

  return(next_page)
}

handle_complex_skip_logic <- function(input, skip_if_complex, next_page) {

  # Loop through each skip logic condition
  for (j in 1:length(skip_if_complex)) {
    rule <- skip_if_complex[[j]]

    # Evaluate the condition
    condition_result <- rule$condition(input)

    # Check if the condition is met (and not logical(0))
    if (length(condition_result) > 0 && condition_result) {
      return(rule$target_page)
    }
  }

  return(next_page)
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

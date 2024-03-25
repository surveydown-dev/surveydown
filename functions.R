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

sd_next <- function(current_page = NULL, label = "Next") {
  if (is.null(current_page)) {
    stop("You must specify the current_page for the 'Next' button.")
  }

  shiny::actionButton(
    # Sanitize target_page for use as an ID
    inputId = paste0("next-", gsub("[^[:alnum:]]", "", current_page)),
    label = label,
    onclick = sprintf("Shiny.setInputValue('next_page', '%s');", current_page)
  )
}

# Server functions ----

sd_server <- function(
  input,
  session,
  skip_logic = NULL,
  showif = NULL
) {

  # Get survey metadata

  page_metadata <- get_page_metadata()
  page_names <- names(page_metadata)
  page_count <- length(page_metadata)
  question_ids <- unname(unlist(page_metadata))

  # showif conditions ----

  if (!is.null(showif)) { handle_showif_logic(input, showif) }

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
    for (i in 1:(page_count - 1)) {
      local({

        # Define current and next page based on iteration
        current_page <- page_names[i]
        next_page <- page_names[i + 1]

        # Generate the button ID expected for this navigation step
        button_id <- paste0("next-", current_page)

        observeEvent(input[[button_id]], {

          # Update next page with any skip logic
          next_page <- handle_skip_logic(
            input, skip_logic, current_page, next_page
          )
          # Execute page navigation
          shinyjs::hide(current_page)
          shinyjs::show(next_page)
        })
      })
    }
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

handle_showif_logic <- function(input, showif) {

  # Initially hide all conditional questions
  lapply(showif, function(x) shinyjs::hide(x$target_question))

  # Iterate over each showif rule
  lapply(showif, function(rule) {
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

handle_skip_logic <- function(input, skip_logic, current_page, next_page) {
  if (is.null(skip_logic)) { return(next_page) }
  # Loop through each skip logic condition
  for (j in 1:length(skip_logic)) {
    rule <- skip_logic[[j]]
    condition_func <- rule$condition
    target_page <- rule$target_page
    # Check if the condition is met & and not already on the target page
    if (condition_func(input) & current_page != target_page) {
      return(target_page)
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

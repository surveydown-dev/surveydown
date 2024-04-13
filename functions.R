library(shiny)
library(shinyjs)

shiny::shinyOptions(bootstrapTheme = bslib::bs_theme(version = 5L))

# UI ----

sd_question <- function(
  name,
  type,
  required   = FALSE,
  label      = "label",
  option     = NULL,
  label_null = NULL,
  direction  = "horizontal",
  individual = FALSE,
  justified  = FALSE,
  width      = '100%',
  status     = "default"
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
      label = markdown_to_html(label),
      choices = option,
      multiple = FALSE,
      selected = FALSE,
      width = width
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId = name,
      label = markdown_to_html(label),
      choices = option,
      selected = FALSE,
      width = width
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId = name,
      label = markdown_to_html(label),
      choices = option,
      selected = FALSE,
      width = width
    )

  } else if (type == "mc_buttons") {

    output <- shinyWidgets::radioGroupButtons(
      inputId = name,
      label = markdown_to_html(label),
      choices = list_name_md_to_html(option),
      selected = character(0),
      width = width,
      status = status
    )

  } else if (type == "mc_multiple_buttons") {

    output <- shinyWidgets::checkboxGroupButtons(
      inputId = name,
      label = markdown_to_html(label),
      choices = list_name_md_to_html(option),
      direction = direction,
      individual = individual,
      justified = justified,
      width = width
    )

  } else if (type == "text") {

    output <- shiny::textInput(
      inputId = name,
      label = markdown_to_html(label),
      placeholder = option,
      width = width
    )

  } else if (type == "numeric") {

    output <- shiny::numericInput(
      inputId = name,
      label = markdown_to_html(label),
      value = NULL,
      width = width
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
    style = "display: block; margin: auto;",
    onclick = sprintf("Shiny.setInputValue('next_page', '%s');", next_page)
  )
}

make_next_button_id <- function(next_page) {
  return(paste0("next-", next_page))
}

# Convert markdown to HTML

markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(shiny::HTML(markdown::renderMarkdown(text = text)))
}

# Convert list names from markdown to HTML
# Only works for mc_buttons and mc_multiple_buttons.

list_name_md_to_html <- function(list) {
  list_names_md <- names(list)
  list_names_html <- lapply(list_names_md, function(name) {
    html_name <- markdown_to_html(name)
    plain_name <- gsub("<[/]?p>|\\n", "", html_name)
    return(plain_name)
  })
  names(list) <- unlist(list_names_html)
  return(list)
}

# Config ----

sd_config <- function(
    db_url = NULL,
    db_key = NULL,
    db_create = NULL,
    skip_if = NULL,
    skip_if_custom = NULL,
    show_if = NULL,
    show_if_custom = NULL,
    preview = FALSE
) {

  # Get survey metadata

  page_structure <- get_page_structure()
  config <- list(
    page_structure = page_structure,
    page_ids = names(page_structure),
    question_ids = unname(unlist(page_structure))
  )

  # Check skip_if and show_if inputs

  check_skip_show(config, skip_if, skip_if_custom, show_if, show_if_custom)

  # Establish data base if not in preview mode

  if (!preview) {
    db_url <- establish_database(config, db_key, db_url, db_create)
  }

  # Store remaining config settings

  config$db_url <- db_url
  config$skip_if <- skip_if
  config$skip_if_custom <- skip_if_custom
  config$show_if <- show_if
  config$show_if_custom <- show_if_custom
  config$preview <- preview

  return(config)
}

## Page metadata ----

get_page_structure <- function() {

  pages <- get_page_nodes()
  page_ids <- unlist(lapply(pages, function(x) rvest::html_attr(x, "id")))

  # Initialize a list to hold the results
  page_structure <- list()

  # Iterate over each page and extract the Shiny widget IDs
  for (i in seq_len(length(page_ids))) {

    # Extract the page ID
    page_id <- page_ids[i]

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
    page_structure[[page_id]] <- unique(widget_ids)
  }

  return(page_structure)
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

  stop("Error: {surveydown} requires that only one .qmd file in the directory.")

}

## Config checks ----

check_skip_show <- function(
    config, skip_if, skip_if_custom, show_if, show_if_custom
) {
  # Placeholder for now - need to check for:
  # - That the skip_if and show_if arguments are data frames
  # - If the names of skip_if and show_if are "question_id", "question_value", and "target"
  # - That the "question_id" values in both show_if and skip_if are in config$question_ids
  # - That the "target" values in show_if are in config$question_ids
  # - That the "target" values in skip_if are in config$page_ids
  return(TRUE)
}

## Establish database ----

establish_database <- function(config, db_key, db_url = NULL, db_create = TRUE) {

  # Authentication

  if (is.null(db_key)) {
    stop("You must provide a db_key to authenticate your Google account")
  }

  # < Code to handle google authentication here >

  # Default behavior is to create a new google sheet
  # If the user provides a url to an existing sheet, then don't create one

  if (!is.null(db_url)) {
    db_create <- FALSE
  }

  # Create a new google sheet if db_create is still TRUE

  if (db_create) {

    # < Code to create the new google sheet here>
    # This should end with the url to the new sheet being stored

    db_url <- "url_to_new_sheet" # Replace with url to new sheet

  } else {

    # < Code to run checks that the names of the google sheet match those in config$question_ids >

  }

  # We return the db_url because if the user didn't provide one,
  # we need to use the url to the newly created sheet, otherwise
  # just return the one that the user provided

  return(db_url)

}

# Server ----

sd_server <- function(input, session, config) {

  # Create local objects from config file

  page_structure <- config$page_structure
  page_ids <- config$page_ids
  question_ids <- config$question_ids
  show_if <- config$show_if
  db_url <- config$db_url
  skip_if <- config$skip_if
  skip_if_custom <- config$skip_if_custom
  show_if <- config$show_if
  show_if_custom <- config$show_if_custom
  preview <- config$preview

  # Conditional display (show_if conditions) ----

  if (!is.null(show_if)) {
    handle_basic_show_if_logic(input, show_if)
  }

  if (!is.null(show_if_custom)) {
    handle_custom_show_if_logic(input, show_if_custom)
  }

  # Page Navigation ----

  observe({
    for (i in 2:length(page_structure)) {
      local({

        # Define current and next page based on iteration
        next_page <- page_ids[i]

        observeEvent(input[[make_next_button_id(next_page)]], {

          # Update next page with any basic skip logic
          if (!is.null(skip_if)) {
            next_page <- handle_basic_skip_logic(input, skip_if, next_page)
          }

          # Update next page with any custom skip logic
          if (!is.null(skip_if_custom)) {
            next_page <- handle_custom_skip_logic(input, skip_if_custom, next_page)
          }

          # Execute page navigation
          shinyjs::runjs('hideAllPages();') # Hide all pages
          shinyjs::show(next_page) # Show next page
        })
      })
    }
  })

  # DB operations ----

  # Update data base if not in preview mode

  if (!preview) {
    update_database(input, question_ids, db_url)
  }

}

## show_if ----

handle_basic_show_if_logic <- function(input, show_if) {

  # Ensure skip_if is a tibble or data frame
  if (!is.data.frame(show_if)) {
    stop("skip_if must be a data frame or tibble.")
  }

  # Initially hide all conditional questions
  for (i in 1:nrow(show_if)) {
    shinyjs::hide(show_if[i,]$target)
  }

  # Iterate over each show_if rule
  for (i in 1:nrow(show_if)) {
    rule <- show_if[i,]
    observeEvent(input[[rule$question_id]], {
      # Check if the condition is met to show/hide the question
      val <- input[[rule$question_id]]
      if (!is.null(val) & (val == rule$question_value)) {
        shinyjs::show(rule$target)
      } else {
        shinyjs::hide(rule$target)
      }
    }, ignoreNULL = TRUE)
  }

}

handle_custom_show_if_logic <- function(input, show_if_custom) {

  # Initially hide all conditional questions
  lapply(show_if_custom, function(x) shinyjs::hide(x$target))

  # Iterate over each show_if rule
  lapply(show_if_custom, function(rule) {
    observeEvent(input[[rule$dependent_question]], {
      # Check if the condition is met to show/hide the question
      if (rule$condition(input)) {
        shinyjs::show(rule$target)
      } else {
        shinyjs::hide(rule$target)
      }
    }, ignoreNULL = TRUE)
  })
}

## skip_if ----

handle_basic_skip_logic <- function(input, skip_if, next_page) {

  # Ensure skip_if is a tibble or data frame
  if (!is.data.frame(skip_if)) {
    stop("skip_if must be a data frame or tibble.")
  }

  for (i in 1:nrow(skip_if)) {
    rule <- skip_if[i,]
    val <- input[[rule$question_id]]
    if (!is.null(val)) {
      if (val == rule$question_value) {
        return(rule$target)
      }
    }
  }

  return(next_page)
}

handle_custom_skip_logic <- function(input, skip_if_custom, next_page) {

  # Loop through each skip logic condition
  for (j in 1:length(skip_if_custom)) {
    rule <- skip_if_custom[[j]]

    # Evaluate the condition
    condition_result <- rule$condition(input)

    # Check if the condition is met (and not logical(0))
    if (length(condition_result) > 0 && condition_result) {
      return(rule$target)
    }
  }

  return(next_page)
}

## Database ----

update_database <- function(input, question_ids, db_url) {

  # Define a reactive expression for each question_id

  input_vals <- reactive({
    temp <- sapply(
      question_ids,
      function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE
    )
    names(temp) <- question_ids
    temp
  })

  # Use observe to react whenever 'input_vals' changes
  # If it changes, update the database

  observe({

    # Capture the current state of inputs
    vals <- input_vals()

    # Transform to data frame, handling uninitialized inputs appropriately
    data <- transform_data(vals, question_ids, session)

    # Save data
    readr::write_csv(data, 'data.csv')

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

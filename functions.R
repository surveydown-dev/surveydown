library(shiny)
library(shinyjs)

shiny::shinyOptions(bootstrapTheme = bslib::bs_theme(version = 5L))

# UI ----

sd_question <- function(
  name,
  type,
  label,
  cols         = "80",
  direction    = "horizontal",
  status       = "default",
  width        = "100%",
  height       = "100px",
  selected     = NULL,
  label_select = "Choose an option...",
  grid         = TRUE,
  individual   = TRUE,
  justified    = FALSE,
  force_edges  = TRUE,
  option       = NULL,
  placeholder  = NULL,
  required     = FALSE,
  resize       = NULL
) {

  output <- NULL

  if (type ==  "select") {
    option <- c("", option)
    names(option)[1] <- label_select

    output <- shiny::selectInput(
      inputId  = name,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      multiple = FALSE,
      selected = FALSE
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId  = name,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId  = name,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_buttons") {

    output <- shinyWidgets::radioGroupButtons(
      inputId  = name,
      label    = markdown_to_html(label),
      choices  = list_name_md_to_html(option),
      selected = character(0),
      width    = width,
      status   = status
    )

  } else if (type == "mc_multiple_buttons") {

    output <- shinyWidgets::checkboxGroupButtons(
      inputId    = name,
      label      = markdown_to_html(label),
      choices    = list_name_md_to_html(option),
      direction  = direction,
      individual = individual,
      justified  = justified,
      width      = width
    )

  } else if (type == "text") {

    output <- shiny::textInput(
      inputId     = name,
      label       = markdown_to_html(label),
      width       = width,
      placeholder = option
    )

  } else if (type == "textarea") {

    output <- shiny::textAreaInput(
      inputId     = name,
      label       = markdown_to_html(label),
      width       = width,
      height      = height,
      cols        = cols,
      value       = NULL,
      rows        = "6",
      placeholder = placeholder,
      resize      = resize
    )

  } else if (type == "numeric") {

    output <- shiny::numericInput(
      inputId = name,
      label   = markdown_to_html(label),
      width   = width,
      value   = NULL
    )

  } else if (type == "slider") {

    output <- shinyWidgets::sliderTextInput(
      inputId      = name,
      label        = markdown_to_html(label),
      width        = width,
      choices      = option,
      selected     = selected,
      force_edges  = force_edges,
      grid         = grid,
      animate      = FALSE,
      hide_min_max = FALSE,
      from_fixed   = FALSE,
      to_fixed     = FALSE,
      from_min     = NULL,
      from_max     = NULL,
      to_min       = NULL,
      to_max       = NULL,
      pre          = NULL,
      post         = NULL,
      dragRange    = TRUE
    )

  } else if (type == "date") {

    output <- shiny::dateInput(
      inputId            = name,
      label              = markdown_to_html(label),
      width              = width,
      value              = NULL,
      min                = NULL,
      max                = NULL,
      format             = "mm/dd/yyyy",
      startview          = "month",
      weekstart          = 0,
      language           = "en",
      autoclose          = TRUE,
      datesdisabled      = NULL,
      daysofweekdisabled = NULL
    )

  } else if (type == "daterange") {

    output <- shiny::dateRangeInput(
      inputId   = name,
      label     = markdown_to_html(label),
      width     = width,
      start     = NULL,
      end       = NULL,
      min       = NULL,
      max       = NULL,
      format    = "mm/dd/yyyy",
      startview = "month",
      weekstart = 0,
      language  = "en",
      separator = "-",
      autoclose = TRUE
    )

  }

  # Wrap the output in a div with a custom data attribute to facilitate
  # question_id scraping later
  output_div <- shiny::tags$div(
    id = paste("container-", name),
    `data-question-id` = name,   # Custom attribute to identify the question_id
    class = "question-container", # Additional CSS class for styling or scripts
    output
  )

  return(output_div)

}

sd_next <- function(next_page = NULL, label = "Next") {
  if (is.null(next_page)) {
    stop("You must specify the current_page for the 'Next' button.")
  }

  shiny::actionButton(
    inputId = make_next_button_id(next_page),
    label   = label,
    style   = "display: block; margin: auto;",
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
    skip_if = NULL,
    skip_if_custom = NULL,
    show_if = NULL,
    show_if_custom = NULL,
    preview = FALSE,
    start_page = NULL
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
    db_url <- establish_database(config, db_key, db_url)
  }

  # Check that start_page (if used) points to an actual page

  if (!is.null(start_page)) {
    if (! start_page %in% config$page_ids) {
      stop(
        "The specified start_page does not exist - check that you have ",
        "not mis-spelled the name"
      )
    }
  }

  # Store remaining config settings

  config$db_url <- db_url
  config$skip_if <- skip_if
  config$skip_if_custom <- skip_if_custom
  config$show_if <- show_if
  config$show_if_custom <- show_if_custom
  config$preview <- preview
  config$start_page <- start_page

  return(config)
}

## Page structure ----

get_page_structure <- function() {

  # Get all page nodes
  page_nodes <- get_page_nodes()
  page_ids <- page_nodes |> rvest::html_attr("id")

  # Initialize a list to hold the results
  page_structure <- list()

  # Iterate over each page node to get the question_ids
  for (i in seq_along(page_nodes)) {
    page_id <- page_ids[i]
    page_node <- page_nodes[i]

    # Extract all question IDs within this page
    question_ids <- page_node |>
      rvest::html_nodes("[data-question-id]") |>
      rvest::html_attr("data-question-id")

    # Store the question IDs for this page
    page_structure[[page_id]] <- question_ids
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

establish_database <- function(config, db_key, db_url = NULL) {

  # Authentication

  if (is.null(db_key)) {
    stop("You must provide a db_key to authenticate your Google account")
  }

  # < Code to handle google authentication here >

  # Default behavior is to create a new google sheet if the user does not
  # provide a db_url

  if (is.null(db_url)) {

    # < Code to create the new google sheet here>
    # Will need to initialize the sheet with column names for
    # every question_id in config$question_ids

    # This should end with the url to the new sheet being stored

    db_url <- "url_to_new_sheet" # Replace with url to new sheet

  } else {

    # < Code to run checks that the column names in the provided google sheet
    # match those in config$question_ids >

    # < Code to update the provided google sheet with any newly added
    # question_ids if they are missing from the sheet >

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
  start_page <- config$start_page

  # Create a local session_id variable
  session_id <- session$token

  # Initialize object for storing timestamps

  timestamps <- reactiveValues(data = initialize_timestamps(page_ids))

  # Conditional display (show_if conditions) ----

  if (!is.null(show_if)) {
    handle_basic_show_if_logic(input, show_if)
  }

  if (!is.null(show_if_custom)) {
    handle_custom_show_if_logic(input, show_if_custom)
  }

  # Page Navigation ----

  # Start from start_page (if specified)

  if (!is.null(start_page)) {
    shinyjs::runjs("hideAllPages();")
    shinyjs::show(start_page)
  }

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

          # Store the timestamp with the page_id as the key
          timestamps$data[[make_page_ts_name(next_page)]] <- get_utc_timestamp()

          # Execute page navigation
          shinyjs::runjs("hideAllPages();") # Hide all pages
          shinyjs::show(next_page) # Show next page
        })
      })
    }
  })

  # Database operations ----

  # Update data base if not in preview mode

  if (!preview) {

    # Define a reactive expression for each question_id value

    get_question_vals <- reactive({
      temp <- sapply(
        question_ids,
        function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE
      )
      names(temp) <- question_ids
      temp
    })

    # Define a reactive expression for the timestamp values

    get_time_stamps <- reactive({ timestamps$data })

    # Use observe to react whenever "input_vals" changes
    # If it changes, update the database

    observe({

      # Capture the current state of question values and timestamps
      question_vals <- get_question_vals()
      timestamp_vals <- get_time_stamps()

      # Transform to data frame, handling uninitialized inputs appropriately
      data <- transform_data(question_vals, timestamp_vals, session_id)

      # Save data - need to update this with writing to the googlesheet
      readr::write_csv(data, "data.csv")

    })

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

transform_data <- function(question_vals, timestamp_vals, session_id) {

  # Replace NULLs with empty string, and
  # convert vectors to comma-separated strings
  for (i in 1:length(question_vals)) {
    # Check for NULL and replace with an empty string
    if (is.null(question_vals[[i]])) {
      question_vals[[i]] <- ""
    } else if (is.vector(question_vals[[i]])) {
      # Convert vectors to comma-separated strings
      question_vals[[i]] <- paste(question_vals[[i]], collapse = ", ")
    }
  }
  responses <- as.data.frame(question_vals)

  # Add session_id and timestamps
  data <- cbind(session_id, responses, as.data.frame(timestamp_vals))

  return(data)
}

# Other helpers ----

get_utc_timestamp <- function() {
  return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

initialize_timestamps <- function(page_ids) {

  timestamps <- list()

  # Store the time of the start (first page)
  timestamps[[make_page_ts_name(page_ids[1])]] <- get_utc_timestamp()

  # Store "" values for all remaining pages
  for (i in 2:length(page_ids)) {
    timestamps[[make_page_ts_name(page_ids[i])]] <- ""
  }

  return(timestamps)

}

make_page_ts_name <- function(page_id) {
  return(paste0("time_page_", page_id))
}

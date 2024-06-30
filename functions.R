library(shiny)
library(shinyjs)
library(DBI)
library(RPostgreSQL)

shiny::shinyOptions(bootstrapTheme = bslib::bs_theme(version = 5L))

# UI ----

sd_question <- function(
  type,
  id,
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
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      multiple = FALSE,
      selected = FALSE
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_buttons") {

    output <- shinyWidgets::radioGroupButtons(
      inputId  = id,
      label    = markdown_to_html(label),
      choices  = list_name_md_to_html(option),
      selected = character(0),
      width    = width,
      status   = status
    )

  } else if (type == "mc_multiple_buttons") {

    output <- shinyWidgets::checkboxGroupButtons(
      inputId    = id,
      label      = markdown_to_html(label),
      choices    = list_name_md_to_html(option),
      direction  = direction,
      individual = individual,
      justified  = justified,
      width      = width
    )

  } else if (type == "text") {

    output <- shiny::textInput(
      inputId     = id,
      label       = markdown_to_html(label),
      width       = width,
      placeholder = option
    )

  } else if (type == "textarea") {

    output <- shiny::textAreaInput(
      inputId     = id,
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
      inputId = id,
      label   = markdown_to_html(label),
      width   = width,
      value   = NULL
    )

  } else if (type == "slider") {

    output <- shinyWidgets::sliderTextInput(
      inputId      = id,
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
      inputId            = id,
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
      inputId   = id,
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
    id = paste("container-", id),
    `data-question-id` = id,   # Custom attribute to identify the question_id
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
    skip_if = NULL,
    skip_if_custom = NULL,
    show_if = NULL,
    show_if_custom = NULL,
    preview = FALSE,
    start_page = NULL,
    show_all_pages = FALSE
) {

  # Get survey metadata

  page_structure <- get_page_structure()
  question_structure <- get_question_structure()
  config <- list(
    page_structure = page_structure,
    question_structure = question_structure,
    page_ids       = names(page_structure),
    question_ids   = unname(unlist(page_structure))
  )

  # Check skip_if and show_if inputs

  check_skip_show(config, skip_if, skip_if_custom, show_if, show_if_custom)

  # Check that start_page (if used) points to an actual page

  if (!is.null(start_page)) {
    if (! start_page %in% config$page_ids) {
      stop(
        "The specified start_page does not exist - check that you have ",
        "not mis-spelled the id"
      )
    }
  }

  if (show_all_pages) {
    for (page in config$page_ids) {
      shinyjs::show(page)
    }
  }

  # Store remaining config settings

  config$skip_if <- skip_if
  config$skip_if_custom <- skip_if_custom
  config$show_if <- show_if
  config$show_if_custom <- show_if_custom
  config$preview <- preview
  config$start_page <- start_page
  config$show_all_pages <- show_all_pages

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

get_question_structure <- function() {

  # Should return a list where each item name is a question_id and
  # each item value is the set of question values (options), e.g.:
  # list(
  #   penguins = c('adelie', 'chinstrap', 'gentoo', 'other'),
  #   penguins_other = "",
  #   ...
  # )

  # This can be parsed out of the rendered html file

  question_nodes <- get_question_nodes()

}

get_question_nodes <- function() {

  # similar to get_page_nodes(), but the nodes should be the question_ids
  # also can update get_page_nodes() to find the html file directly rather than
  # the qmd file.

}

## Config checks ----

check_skip_show <- function(
    config, skip_if, skip_if_custom, show_if, show_if_custom
) {

  # Placeholder for now - need to check for:
  # - If the names of skip_if and show_if are "question_id", "question_value", and "target"
  # - That the "question_id" values in both show_if and skip_if are in config$question_ids
  # - That the "target" values in show_if are in config$question_ids
  # - That the "target" values in skip_if are in config$page_ids

  # Ensure skip_if and show_if are each a tibble or data frame
  if (!is.null(skip_if)) {
    if (!is.data.frame(skip_if)) {
      stop("skip_if must be a data frame or tibble.")
    }
  }
  if (!is.null(show_if)) {
    if (!is.data.frame(show_if)) {
      stop("show_if must be a data frame or tibble.")
    }
  }

  return(TRUE)
}

## Establish database ----

sd_database <- function(host, db_name, port, user, table_name, password) {


  # Authentication/Checks for NULL Values
  if (is.null(host) || is.null(db_name) || is.null(port) || is.null(user)) {
    stop("Error: One or more required parameters (config, host, db_name, port, user) are NULL.")
  }

  if (!nzchar(password)) {
    stop("You must provide your SupaBase password to access the database")
  }

  # < Code to handle SupaBase authentication here >
  #User Must create their own table inside of Supabase in order to make additions.
  tryCatch(
    {
       db <-  dbConnect(
          RPostgres::Postgres(),
          host     = host,
          dbname   = db_name,
          port     = port,
          user     = user,
          password = password
        )
      message("Successfully connected to the database.")
      return(list(db = db, table_name = table_name))
    }, error = function(e) {
      stop("Error: Failed to connect to the database. Please check your connection details.")
    })
}

# Server ----

sd_server <- function(input, session, config, db = NULL) {

  # Create local objects from config file

  page_structure <- config$page_structure
  page_ids <- config$page_ids
  question_ids <- config$question_ids
  show_if <- config$show_if
  skip_if <- config$skip_if
  skip_if_custom <- config$skip_if_custom
  show_if <- config$show_if
  show_if_custom <- config$show_if_custom
  preview <- config$preview
  start_page <- config$start_page

  # Create a local session_id variable
  session_id <- session$token

  # Initialize object for storing timestamps
  timestamps <- reactiveValues(data = initialize_timestamps(page_ids, question_ids))

  # Conditional display (show_if conditions) ----
  if (!is.null(show_if)) {
    handle_basic_show_if_logic(input, show_if)
  }

  if (!is.null(show_if_custom)) {
    handle_custom_show_if_logic(input, show_if_custom)
  }

  # Progress tracking ----

  # Initialize reactive value to track the maximum progress reached
  max_progress <- reactiveVal(0)

  # Initialize reactive value to track the number of questions answered
  answered_questions <- reactiveVal(0)

  # Initialize reactiveValues to store status information
  answer_status <- reactiveValues(
    processing_question = NULL,
    current_answers = NULL,
    current_answers_length = NULL,
    num_answered = NULL,
    progress = NULL
  )

  # Observing the question timestamps and progress bar
  observe({
    lapply(question_ids, function(id) {
      observeEvent(input[[id]], {
        # Updating question timestamps
        answer_status$processing_question <- list(id = id, value = input[[id]])
        if (is.na(timestamps$data[[make_ts_name("question", id)]])) {
          timestamps$data[[make_ts_name("question", id)]] <- get_utc_timestamp()
        }
        answered_position <- which(question_ids == id)
        current_progress <- answered_position / length(question_ids)
        if (current_progress > max_progress()) {
          max_progress(current_progress)
        }

        # Updating progress bar
        shinyjs::runjs(paste0("updateProgressBar(", max_progress() * 100, ");"))
      }, ignoreInit = TRUE)
    })
  })

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
        current_page <- page_ids[i-1]
        next_page <- page_ids[i]

        observeEvent(input[[make_next_button_id(next_page)]], {

          # Update next page with any basic skip logic
          if (!is.null(skip_if)) {
            next_page <- handle_basic_skip_logic(input, skip_if, current_page, next_page)
          }

          # Update next page with any custom skip logic
          if (!is.null(skip_if_custom)) {
            next_page <- handle_custom_skip_logic(input, skip_if_custom, current_page, next_page)
          }

          # Store the timestamp with the page_id as the key
          timestamps$data[[make_ts_name("page", next_page)]] <- get_utc_timestamp()

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
      df_local <- transform_data(question_vals, timestamp_vals, session_id)
      # Making everything a string because the db poops itself
      df_local[] <- lapply(df_local, as.character)


      # Update database
      if (is.null(db)) {
        warning('db is not connected, writing to local data.csv file instead')
        readr::write_csv(df_local, "data.csv")
      } else{
        database_uploading(df_local, db$db, db$table_name)
      }
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

handle_basic_skip_logic <- function(
  input, skip_if, current_page, next_page
) {

  for (i in 1:nrow(skip_if)) {
    rule <- skip_if[i,]
    val <- input[[rule$question_id]]
    if (!is.null(val)) {
      if ((val == rule$question_value) & (current_page != rule$target)) {
        return(rule$target)
      }
    }
  }

  return(next_page)
}

handle_custom_skip_logic <- function(
  input, skip_if_custom, current_page, next_page
) {

  # Loop through each skip logic condition
  for (j in 1:length(skip_if_custom)) {
    rule <- skip_if_custom[[j]]

    # Evaluate the condition
    condition_result <- rule$condition(input)

    # Check if the condition is met (and not logical(0))
    if (
      (length(condition_result) > 0) &
      (current_page != rule$target) &
      condition_result
    ) {
      return(rule$target)
    }
  }

  return(next_page)
}

## Database ----

transform_data <- function(question_vals, timestamp_vals, session_id) {

  # Replace NULLs with empty string, and
  # convert vectors to comma-separated strings
  for (i in seq_len(length(question_vals))) {
    # Check for NULL and replace with an empty string
    val <- question_vals[[i]]
    if (is.null(val)) {
      question_vals[[i]] <- ""
    } else if (length(val) > 1) {
      # Convert vectors to comma-separated strings
      question_vals[[i]] <- paste(question_vals[[i]], collapse = ", ")
    }
  }

  responses <- as.data.frame(question_vals)

  # Add session_id and timestamps
  data <- cbind(session_id, responses, as.data.frame(timestamp_vals))

  return(data)
}

### Database Uploading ----


#Database Creation Section

#Needed to change from R type to SQL type
r_to_sql_type <- function(r_type) {
  switch(toupper(r_type),
         CHARACTER = "TEXT",
         INTEGER = "TEXT",
         DOUBLE = "TEXT",
         LOGICAL = "TEXT",
         FACTOR = "TEXT",
         "TEXT")
}

create_table <- function(db, table_name, column_definitions) {
  create_table_query <- paste0(
    "CREATE TABLE ", table_name, " (", column_definitions, ")"
  )
  DBI::dbExecute(db, create_table_query)
  #A precaution to enable RLS
  DBI::dbExecute(db, paste0("ALTER TABLE ", table_name, " ENABLE ROW LEVEL SECURITY;"))
  return(message("Database should appear on your SupaBase Account (Can take up to a minute.)"))
}

database_uploading <- function(df, db, table_name) {
  if(is.null(db)) {
    return(warning("Databasing is not in use"))
  }
  # Loop through the column names
  col_def <- ""

  #Create the col_definitions based on the type
  for (col_name in colnames(df)) {
    r_type <- typeof(df[[col_name]])
    sql_type <- r_to_sql_type(r_type)
    col_def <- paste0(col_def, col_name, " ", sql_type, ", ")
  }

  # Remove the trailing comma and space
  col_def <- substr(col_def, 1, nzchar(col_def) - 2)

  # Establish the database connection
  data <- tryCatch(DBI::dbReadTable(db, table_name), error = function(e) NULL)

  #This actually checks if its empty and will create a brand new table name of your choice
  if(is.null(data)) {
    create_table(db, table_name, col_def)
  }

  #Table Editing Section
  #Checking For Matching Session_Id's
  matching_rows <- df[df$session_id %in% data$session_id, ]

  if (nrow(matching_rows) > 0) {
    # Delete existing rows in the database table with matching session_id values from df
    DBI::dbExecute(db, paste0('DELETE FROM \"', table_name, '\" WHERE session_id IN (', paste(shQuote(matching_rows$session_id), collapse = ", "), ')'))

    # Append the new non-matching rows to the database table
    DBI::dbWriteTable(db, table_name, matching_rows, append = TRUE, row.names = FALSE)
  } else { #If there are no matching rows we just append the new row.
    DBI::dbWriteTable(db, table_name, df, append = TRUE, row.names = FALSE)
  }
}
#Database Editing Section based on a change in column parameters

  #So here I will have to write a loop so see what variables changed and from here we add and delete whats necessary

  #For this loop we will check the datatypes for each var and change accordingly. Instead of using the col_def for table_query
  #We might be able to use it for the alter table command

  # ALTER TABLE table_name
  # ALTER TABLE column_name datatype;

  # Addition
  # ALTER TABLE table_name
  # ADD column_name datatype;
  #
  #
  # Deletion
  # ALTER TABLE table_name
  # DROP COLUMN column_name;


# Other helpers ----

get_utc_timestamp <- function() {
  return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

initialize_timestamps <- function(page_ids, question_ids) {
  timestamps <- list()

  # Initialize timestamps for pages
  timestamps[[make_ts_name("page", page_ids[1])]] <- get_utc_timestamp()
  for (i in 2:length(page_ids)) {
    timestamps[[make_ts_name("page", page_ids[i])]] <- NA
  }

  # Initialize timestamps for questions
  for (qid in question_ids) {
    timestamps[[make_ts_name("question", qid)]] <- NA
  }

  return(timestamps)
}

make_ts_name <- function(type, id) {
  if (type == "page") {
    return(paste0("time_p_", id))
  } else if (type == "question") {
    return(paste0("time_q_", id))
  }
}

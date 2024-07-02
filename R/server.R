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

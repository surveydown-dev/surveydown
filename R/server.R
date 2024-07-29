#' Server Logic for a surveydown survey
#'
#' This function defines the server-side logic for a Shiny application, handling various
#' operations such as conditional display, progress tracking, page navigation, and database
#' updates.
#'
#' @param input The Shiny input object.
#' @param session The Shiny session object.
#' @param config A list containing configuration settings for the application. Expected
#'        elements include `page_structure`, `page_ids`, `question_ids`, `show_if`, `skip_if`,
#'        `skip_if_custom`, `show_if_custom`, `preview`, and `start_page`.
#' @param db A list containing database connection information created using
#' sd_database() function. Expected elements include `db` and `table_name`.
#' Defaults to `NULL`.
#'
#' @details The function performs the following tasks:
#'   - Initializes local variables based on the provided configuration.
#'   - Sets up reactive values to track timestamps and progress.
#'   - Implements conditional display logic for UI elements based on `show_if` and `show_if_custom` conditions.
#'   - Tracks the progress of answered questions and updates the progress bar accordingly.
#'   - Handles page navigation within the Shiny application, including basic and custom skip logic.
#'   - Performs database operations to store responses, either to a specified database or a local CSV file if in preview mode.
#'
#' @examples
#' \dontrun{
#'   server <- function(input, output, session) {
#'     config <- list(
#'       page_structure = list(),
#'       page_ids = c("page1", "page2"),
#'       question_ids = c("q1", "q2"),
#'       show_if = NULL,
#'       skip_if = NULL,
#'       skip_if_custom = NULL,
#'       show_if_custom = NULL,
#'       preview = FALSE,
#'       start_page = "page1"
#'     )
#'     sd_server(input, session, config)
#'   }
#'   shinyApp(ui = ui, server = server)
#' }
#'
#' @export
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

    # Set up question_required in session$userData
    session$userData$question_required <- shiny::reactiveVal(list())

    # Initialize question_required based on UI elements
    shiny::observe({
        req_list <- lapply(question_ids, function(id) {
            req_status <- input[[paste0("container-", id, "-required")]]
            if (!is.null(req_status)) {
                as.logical(req_status)
            } else {
                FALSE  # default to not required if status is not found
            }
        })
        names(req_list) <- question_ids
        session$userData$question_required(req_list)
    })

    # Initialize object for storing timestamps
    timestamps <- shiny::reactiveValues(data = initialize_timestamps(page_ids, question_ids))

    # Conditional display (show_if conditions) ----
    if (!is.null(show_if)) {
        handle_basic_show_if_logic(input, show_if)
    }

    if (!is.null(show_if_custom)) {
        handle_custom_show_if_logic(input, show_if_custom)
    }

    # Progress Tracking ----

    # Initialize reactive value to track the maximum progress reached
    max_progress <- shiny::reactiveVal(0)

    # Initialize reactive value to track answered required questions
    answered_required <- shiny::reactiveValues()

    # Observing the question timestamps and progress bar
    shiny::observe({
        lapply(question_ids, function(id) {
            shiny::observeEvent(input[[id]], {
                # Updating question timestamps
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

                # Check if the question is required
                is_required <- session$userData$question_required()[[id]]

                # Track answered required questions
                if (is_required) {
                    answer <- input[[id]]
                    answered_required[[id]] <- if (is.null(answer)) FALSE
                    else if (is.character(answer)) nzchar(trimws(answer))
                    else if (is.numeric(answer)) !is.na(answer)
                    else if (is.logical(answer)) !is.na(answer)
                    else if (is.list(answer)) length(answer) > 0  # For checkbox groups
                    else FALSE
                }
            }, ignoreInit = TRUE, ignoreNULL = FALSE)
        })
    })

    # Page Navigation ----

    # Start from start_page (if specified)
    if (!is.null(start_page)) {
        shinyjs::runjs("hideAllPages();")
        shinyjs::show(start_page)
    }

    shiny::observe({
        for (i in 2:length(page_structure)) {
            local({
                # Define current and next page based on iteration
                current_page <- page_ids[i-1]
                next_page <- page_ids[i]

                shiny::observeEvent(input[[make_next_button_id(next_page)]], {
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

                    # Check if all required questions on the current page are answered
                    current_page_questions <- page_structure[[current_page]]

                    # Required questions
                    required_questions <- current_page_questions[sapply(current_page_questions, function(q) {
                        config$question_required[[q]]
                    })]

                    all_required_answered <- all(sapply(current_page_questions, function(q) {
                        is_required <- config$question_required[[q]]

                        if (!is_required) return(TRUE)  # If not required, consider it "answered"

                        # If required, check if it's answered
                        answer <- input[[q]]
                        is_answered <- if (is.null(answer)) FALSE
                        else if (is.character(answer)) nzchar(trimws(answer))
                        else if (is.numeric(answer)) !is.na(answer)
                        else if (is.logical(answer)) !is.na(answer)
                        else if (is.list(answer)) length(answer) > 0  # For checkbox groups
                        else FALSE
                        return(is_answered)
                    }))

                    if (isTRUE(all_required_answered)) {
                        # Execute page navigation
                        shinyjs::runjs("hideAllPages();") # Hide all pages
                        shinyjs::show(next_page) # Show next page
                    } else {
                        # Show error message
                        shinyjs::alert("Please answer all required questions before proceeding.")
                    }
                })
            })
        }
    })

    # Database operations ----

    # Update data base if not in preview mode

    if (!preview) {

        # Define a reactive expression for each question_id value

        get_question_vals <- shiny::reactive({
            temp <- sapply(
                question_ids,
                function(id) input[[id]], simplify = FALSE, USE.NAMES = TRUE
            )
            names(temp) <- question_ids
            temp
        })

        # Define a reactive expression for the timestamp values

        get_time_stamps <- shiny::reactive({ timestamps$data })

        # Use observe to react whenever "input_vals" changes
        # If it changes, update the database

        shiny::observe({

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
        shiny::observeEvent(input[[rule$question_id]], {
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
        shiny::observeEvent(input[[rule$dependent_question]], {
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

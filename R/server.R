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

# Server Function ----

sd_server <- function(input, session, config, db = NULL) {

    # Create local objects from config file
    page_structure <- config$page_structure
    page_ids       <- config$page_ids
    question_ids   <- config$question_ids
    show_if        <- config$show_if
    skip_if        <- config$skip_if
    skip_if_custom <- config$skip_if_custom
    show_if        <- config$show_if
    show_if_custom <- config$show_if_custom
    preview        <- config$preview
    start_page     <- config$start_page

    # Create a local session_id variable for Data Operations use
    session_id <- session$token

    # Progress Tracking ----

    # Initialize object for storing timestamps
    timestamps <- shiny::reactiveValues(data = initialize_timestamps(page_ids, question_ids))

    # Conditional display (show_if conditions)
    if (!is.null(show_if)) {
        handle_basic_show_if_logic(input, show_if)
    }

    if (!is.null(show_if_custom)) {
        handle_custom_show_if_logic(input, show_if_custom)
    }

    # Track the progress
    max_progress <- shiny::reactiveVal(0)
    shiny::observe({
        lapply(question_ids, function(id) {
            shiny::observeEvent(input[[id]], {
                if (is.na(timestamps$data[[make_ts_name("question", id)]])) {
                    timestamps$data[[make_ts_name("question", id)]] <- get_utc_timestamp()
                }
                current_progress <- which(question_ids == id) / length(question_ids)
                max_progress(max(max_progress(), current_progress))
                shinyjs::runjs(sprintf("updateProgressBar(%f);", max_progress() * 100))
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
        lapply(2:length(page_structure), function(i) {
            current_page <- page_ids[i-1]
            next_page <- page_ids[i]

            shiny::observeEvent(input[[make_next_button_id(next_page)]], {

                # Update next page based on skip logic
                next_page <- handle_skip_logic(input, skip_if, skip_if_custom, current_page, next_page)

                # Update timestamp for the next page
                timestamps$data[[make_ts_name("page", next_page)]] <- get_utc_timestamp()

                # Check if all required questions are answered
                current_page_questions <- page_structure[[current_page]]
                all_required_answered <- check_all_required(current_page_questions, config, input)
                if (all_required_answered) {
                    shinyjs::runjs("hideAllPages();")
                    shinyjs::show(next_page)
                } else {
                    shinyjs::alert("Please answer all required questions before proceeding.")
                }
            })
        })
    })

    # Database Operations ----

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

        # Define a reactive expression for the time stamp values
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

# Helper Functions ----

# show_if
handle_basic_show_if_logic <- function(input, show_if) {
    # Ensure show_if is a tibble or data frame
    if (!is.data.frame(show_if)) {
        stop("show_if must be a data frame or tibble.")
    }

    # Initially hide all conditional questions
    unique_targets <- unique(show_if$target)
    for (target in unique_targets) {
        shinyjs::hide(target)
    }

    # Group show_if rules by question_id and target
    show_if_grouped <- split(show_if, list(show_if$question_id, show_if$target))

    # Iterate over each group of show_if rules
    for (group in show_if_grouped) {
        question_id <- group$question_id[1]
        target <- group$target[1]
        question_values <- group$question_value

        shiny::observeEvent(input[[question_id]], {
            # Check if the condition is met to show/hide the question
            val <- input[[question_id]]
            if (!is.null(val) && val %in% question_values) {
                shinyjs::show(target)
            } else {
                shinyjs::hide(target)
            }
        }, ignoreNULL = TRUE)
    }
}

handle_custom_show_if_logic <- function(input, show_if_custom) {
    # Group show_if_custom rules by target
    show_if_custom_grouped <- split(show_if_custom, sapply(show_if_custom, function(x) x$target))

    # Initially hide all conditional questions
    unique_targets <- names(show_if_custom_grouped)
    for (target in unique_targets) {
        shinyjs::hide(target)
    }

    # Iterate over each group of show_if_custom rules
    for (group in show_if_custom_grouped) {
        target <- group[[1]]$target

        # Collect all dependent questions and conditions for this target
        dependent_questions <- unique(sapply(group, function(x) x$dependent_question))
        conditions <- lapply(group, function(x) x$condition)

        # Create a reactive expression to check all conditions
        check_conditions <- shiny::reactive({
            any(sapply(conditions, function(condition) condition(input)))
        })

        # Observe changes in any of the dependent questions
        shiny::observe({
            # Trigger the observer for changes in any dependent question
            for (question in dependent_questions) {
                input[[question]]
            }

            # Check if any condition is met to show/hide the question
            if (check_conditions()) {
                shinyjs::show(target)
            } else {
                shinyjs::hide(target)
            }
        })
    }
}

# skip_if
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

handle_skip_logic <- function(input, skip_if, skip_if_custom, current_page, next_page) {
    if (!is.null(skip_if)) {
        next_page <- handle_basic_skip_logic(input, skip_if, current_page, next_page)
    }
    if (!is.null(skip_if_custom)) {
        next_page <- handle_custom_skip_logic(input, skip_if_custom, current_page, next_page)
    }
    return(next_page)
}

# Answering progress of required questions
check_all_required <- function(questions, config, input) {
    all(vapply(questions, function(q) check_answer(q, config, input), logical(1)))
}

check_answer <- function(q, config, input) {
    if (!config$question_required[[q]]) return(TRUE)

    answer <- input[[q]]
    if (is.null(answer)) return(FALSE)

    if (is.character(answer)) return(any(nzchar(answer)))
    if (is.numeric(answer)) return(any(!is.na(answer)))
    if (inherits(answer, "Date")) return(any(!is.na(answer)))
    if (is.list(answer)) return(any(!sapply(answer, is.null)))

    return(TRUE)  # Default to true for unknown types
}

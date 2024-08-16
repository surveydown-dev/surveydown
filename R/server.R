# Global variable to store custom values
.sd_custom_values <- new.env()

#' Store a custom value for the survey
#'
#' This function allows storing additional values to be included in the survey data,
#' such as respondent IDs or other custom data. The values are stored in a special
#' environment (.sd_custom_values) and will be included when the survey data is saved.
#'
#' @param value The value to be stored.
#' @param name (Optional) The name for the value in the data.
#'             If not provided, the name of the `value` variable will be used.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#'   sd_store_value(respondentID)
#'   sd_store_value(respondentID, "respID")
#' }
#'
#' @export
sd_store_value <- function(value, name = NULL) {
    if (is.null(name)) {
        name <- deparse(substitute(value))
    }
    # Store the value in the global environment
    assign(name, value, envir = .sd_custom_values)
    invisible(NULL)
}

#' Server Logic for a surveydown survey
#'
#' @description
#' This function defines the server-side logic for a Shiny application used in surveydown.
#' It handles various operations such as conditional display, progress tracking,
#' page navigation, and database updates for survey responses.
#'
#' @param input The Shiny input object.
#' @param output The Shiny output object.
#' @param session The Shiny session object.
#' @param config A list containing configuration settings for the application.
#' @param db A list containing database connection information created using
#'        \code{\link{sd_database}} function. Defaults to \code{NULL}.
#'
#' @details
#' The \code{config} list should include the following elements:
#' \itemize{
#'   \item \code{page_structure}: A list defining the structure of survey pages.
#'   \item \code{page_ids}: A vector of page identifiers.
#'   \item \code{question_ids}: A vector of question identifiers.
#'   \item \code{show_if}: A data frame defining conditions for showing questions.
#'   \item \code{skip_if}: A data frame defining conditions for skipping pages.
#'   \item \code{skip_if_custom}: A list of custom skip conditions.
#'   \item \code{show_if_custom}: A list of custom show conditions.
#'   \item \code{start_page}: The identifier of the starting page.
#'   \item \code{question_required}: A vector of required question identifiers.
#'   \item \code{all_questions_required}: A logical indicating if all questions are required.
#' }
#'
#' The function performs the following tasks:
#' \itemize{
#'   \item Initializes variables and reactive values.
#'   \item Implements conditional display logic for questions.
#'   \item Tracks answered questions and updates the progress bar.
#'   \item Handles page navigation and skip logic.
#'   \item Manages required questions.
#'   \item Performs database operations or saves to a local CSV file in preview mode.
#' }
#'
#' @section Progress Bar:
#' The progress bar is updated based on the last answered question. It will jump to the
#' percentage corresponding to the last answered question and will never decrease,
#' even if earlier questions are answered later.
#'
#' @section Database Operations:
#' If \code{db} is provided, the function will update the database with survey responses.
#' If \code{db} is \code{NULL} (pause mode), responses will be saved to a local CSV file.
#'
#' @return
#' This function does not return a value; it sets up the server-side logic for the Shiny application.
#'
#' @examples
#' \dontrun{
#'   shinyApp(
#'     ui = sd_ui(),
#'     server = function(input, output, session) {
#'       sd_server(input, output, session, config = my_config, db = my_db)
#'     }
#'   )
#' }
#'
#' @seealso
#' \code{\link{sd_database}}, \code{\link{sd_question}}
#'
#' @export
#' @importFrom shiny reactive reactiveVal observeEvent
#' @importFrom shinyjs show hide runjs
#' @importFrom readr write_csv
sd_server <- function(input, output, session, config, db = NULL) {

    # Create a local session_id variable for Data Operations use
    session_id <- session$token

    # Create local objects from config file
    page_structure <- config$page_structure
    page_ids       <- config$page_ids
    question_ids   <- config$question_ids
    show_if        <- config$show_if
    skip_if        <- config$skip_if
    skip_if_custom <- config$skip_if_custom
    show_if        <- config$show_if
    show_if_custom <- config$show_if_custom
    start_page     <- config$start_page
    question_required <- config$question_required

    # Show asteriks for required questions
    session$onFlush(function() {
        shinyjs::runjs(sprintf(
            "console.log('Shiny initialized'); window.initializeRequiredQuestions(%s);",
            # jsonlite::toJSON(question_required) # Requires dependency
            vector_to_json_array(question_required)
        ))
    }, once = TRUE)

    # Progress Bar Tracking ----

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
    last_answered_question <- shiny::reactiveVal(0)

    shiny::observe({
        lapply(seq_along(question_ids), function(index) {
            id <- question_ids[index]
            shiny::observeEvent(input[[id]], {
                # Check if the question is answered (non-null and non-empty)
                is_answered <- !is.null(input[[id]]) &&
                    (if(is.list(input[[id]])) {
                        length(input[[id]]) > 0
                    } else if(is.character(input[[id]])) {
                        any(nchar(trimws(input[[id]])) > 0)
                    } else if(is.numeric(input[[id]])) {
                        !all(is.na(input[[id]]))
                    } else {
                        !isTRUE(all(is.na(input[[id]]))) && !identical(input[[id]], "")
                    })

                if (isTRUE(is_answered)) {
                    if (is.na(timestamps$data[[make_ts_name("question", id)]])) {
                        timestamps$data[[make_ts_name("question", id)]] <- get_utc_timestamp()
                    }

                    # Update the last answered question if this question has a higher index
                    if (index > last_answered_question()) {
                        last_answered_question(index)
                    }

                    # Calculate progress based on the last answered question
                    current_progress <- last_answered_question() / length(question_ids)
                    max_progress(max(max_progress(), current_progress))
                    shinyjs::runjs(sprintf("updateProgressBar(%f);", max_progress() * 100))
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

    # Create a reactive value for show_if
    show_if_reactive <- shiny::reactiveVal(show_if)

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

                all_required_answered <- check_all_required(
                    current_page_questions, question_required, input, show_if
                )

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

    pause_mode <- is.null(db)

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

    # Define a reactive expression for custom values
    get_custom_vals <- shiny::reactive({
        as.list(.sd_custom_values)
    })

    # Use observe to react whenever "input_vals" changes
    # If it changes, update the database

    shiny::observe({

        # Capture the current state of question values and timestamps
        question_vals <- get_question_vals()
        timestamp_vals <- get_time_stamps()
        custom_vals <- get_custom_vals()

        # Make values accessible in the UI
        for (id in names(question_vals)) {
            local({
                local_id <- id
                output[[paste0(local_id, "_value")]] <- shiny::renderText({
                    value <- question_vals[[local_id]]
                    if (is.null(value)) return("")
                    if (is.list(value)) value <- paste(value, collapse = ", ")
                    as.character(value)
                })
            })
        }

        # Transform to data frame, handling uninitialized inputs appropriately
        df_local <- transform_data(question_vals, timestamp_vals, session_id, custom_vals)

        # Making everything a string because the db poops itself
        df_local[] <- lapply(df_local, as.character)

        # Update database or write to CSV based on preview mode
        if (pause_mode) {
            readr::write_csv(df_local, "data.csv")
        } else {
            database_uploading(df_local, db$db, db$table_name)
        }
    })
}

#' Handle basic show-if logic
#'
#' @param input Shiny input object
#' @param show_if Data frame of show-if conditions
#' @keywords internal
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

#' Handle custom show-if logic
#'
#' @param input Shiny input object
#' @param show_if_custom List of custom show-if conditions
#' @keywords internal
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

#' Handle basic skip logic
#'
#' @param input Shiny input object
#' @param skip_if Data frame of skip-if conditions
#' @param current_page Current page identifier
#' @param next_page Next page identifier
#' @return Updated next page identifier
#' @keywords internal
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

#' Handle custom skip logic
#'
#' @param input Shiny input object
#' @param skip_if_custom List of custom skip-if conditions
#' @param current_page Current page identifier
#' @param next_page Next page identifier
#' @return Updated next page identifier
#' @keywords internal
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

#' Handle overall skip logic
#'
#' @param input Shiny input object
#' @param skip_if Data frame of basic skip-if conditions
#' @param skip_if_custom List of custom skip-if conditions
#' @param current_page Current page identifier
#' @param next_page Next page identifier
#' @return Updated next page identifier
#' @keywords internal
handle_skip_logic <- function(input, skip_if, skip_if_custom, current_page, next_page) {
    if (!is.null(skip_if)) {
        next_page <- handle_basic_skip_logic(input, skip_if, current_page, next_page)
    }
    if (!is.null(skip_if_custom)) {
        next_page <- handle_custom_skip_logic(input, skip_if_custom, current_page, next_page)
    }
    return(next_page)
}

#' Check if all required questions are answered
#'
#' @param questions Vector of question identifiers
#' @param questions_required Vector of required question identifiers
#' @param input Shiny input object
#' @param show_if Data frame of show-if conditions
#' @return Logical indicating if all required questions are answered
#' @keywords internal
check_all_required <- function(questions, questions_required, input, show_if) {
    all(vapply(questions, function(q) {
        tryCatch({
            if (!(q %in% questions_required)) return(TRUE)
            if (!is_question_visible(q, show_if, input)) return(TRUE)
            check_answer(q, input)
        }, error = function(e) {
            message("Error checking question ", q, ": ", e$message)
            return(FALSE)
        })
    }, logical(1)))
}

#' Check if a single question is answered
#'
#' @param q Question identifier
#' @param input Shiny input object
#' @return Logical indicating if the question is answered
#' @keywords internal
check_answer <- function(q, input) {
    answer <- input[[q]]
    if (is.null(answer)) return(FALSE)
    if (is.character(answer)) return(any(nzchar(answer)))
    if (is.numeric(answer)) return(any(!is.na(answer)))
    if (inherits(answer, "Date")) return(any(!is.na(answer)))
    if (is.list(answer)) return(any(!sapply(answer, is.null)))
    return(TRUE)  # Default to true for unknown types
}

#' Check if a question is visible
#'
#' @param q Question identifier
#' @param show_if Data frame of show-if conditions
#' @param input Shiny input object
#' @return Logical indicating if the question is visible
#' @keywords internal
is_question_visible <- function(q, show_if, input) {
    if (is.null(show_if) || nrow(show_if) == 0) return(TRUE)

    # Check if the question is a target in show_if
    is_target <- q %in% show_if$target
    if (!is_target) return(TRUE)

    # Get all corresponding rules for this target
    rules <- show_if[show_if$target == q, ]

    # Check if any condition is met
    any(sapply(1:nrow(rules), function(i) {
        input_value <- input[[rules$question_id[i]]]
        expected_value <- rules$question_value[i]

        # Handle different input types
        if (is.null(input_value)) {
            return(FALSE)
        } else if (is.list(input_value)) {
            return(expected_value %in% unlist(input_value))
        } else {
            return(input_value == expected_value)
        }
    }))
}

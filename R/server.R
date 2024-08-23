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
sd_server <- function(input, output, session, config, db = NULL) {

    # Initialize local variables ----

    # Create a local session_id variable for Data Operations use
    session_id <- session$token

    # Create local objects from config file
    page_structure <- config$page_structure
    page_ids       <- config$page_ids
    question_ids   <- config$question_ids
    skip_if        <- config$skip_if
    skip_if_custom <- config$skip_if_custom
    show_if        <- config$show_if
    show_if_custom <- config$show_if_custom
    start_page     <- config$start_page
    show_all_pages <- config$show_all_pages
    admin_page     <- config$admin_page
    question_required <- config$question_required

    # Initial page setting ----

    # Start from start_page (if specified)
    if (!is.null(start_page)) {
        shinyjs::show(start_page)
    } else {
        shinyjs::runjs("showFirstPage();")
    }

    # Show all pages if show_all_pages is TRUE
    if (show_all_pages) {
        for (page in page_ids) {
            shinyjs::show(page)
        }
    }

    # Keep-alive observer - this will be triggered every 60 seconds
    shiny::observeEvent(input$keepAlive, {
        cat("Session keep-alive at", format(Sys.time(), "%m/%d/%Y %H:%M:%S"), "\n")
    })

    # Show asterisks for required questions
    session$onFlush(function() {
        shinyjs::runjs(sprintf(
            "console.log('Shiny initialized'); window.initializeRequiredQuestions(%s);",
            # jsonlite::toJSON(question_required) # Requires dependency
            vector_to_json_array(question_required)
        ))
    }, once = TRUE)

    # Admin Page Logic ----

    if (admin_page) {
        admin_enable(input, output, session, db)
    }

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

    # Initialize progress bar update functionality
    load_js_file("update_progress.js")

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

                    # Use the custom message handler to update the progress bar
                    session$sendCustomMessage("updateProgressBar", max_progress() * 100)
                }
            }, ignoreInit = TRUE, ignoreNULL = FALSE)
        })
    })

    # Page Navigation ----

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

    # Define a function to get all stored values
    get_stored_vals <- function() {
        shiny::isolate({
            session <- shiny::getDefaultReactiveDomain()
            if (is.null(session)) {
            stop("sd_get_all_stored_values must be called from within a Shiny reactive context")
            }
            if (is.null(session$userData$stored_values)) {
            return(list())
            }
            session$userData$stored_values
        })
    }

    # Use observe to react whenever "input_vals" changes
    # If it changes, update the database

    shiny::observe({

        # Capture the current state of question values and timestamps
        question_vals <- get_question_vals()
        timestamp_vals <- get_time_stamps()
        stored_vals <- get_stored_vals()

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
        df_local <- transform_data(question_vals, timestamp_vals, session_id, stored_vals)

        # Making everything a string because the db poops itself
        df_local[] <- lapply(df_local, as.character)

        # Update database or write to CSV based on preview mode
        if (pause_mode) {
            utils::write.csv(df_local, "data.csv", row.names = FALSE)
        } else {
            database_uploading(df_local, db$db, db$table_name)
        }
    })
}

# Handle basic show-if logic
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

# Handle custom show-if logic
handle_custom_show_if_logic <- function(input, show_if_custom) {
  # Initially hide all conditional questions
  lapply(show_if_custom, function(x) shinyjs::hide(x$target))

  # Create a reactive expression for each condition
  condition_reactives <- lapply(show_if_custom, function(rule) {
    shiny::reactive({ rule$condition(input) })
  })

  # Create a single observer to handle all conditions
  shiny::observe({
    for (i in seq_along(show_if_custom)) {
      condition_result <- condition_reactives[[i]]()
      condition_met <- isTRUE(condition_result)

      if (condition_met) {
        shinyjs::show(show_if_custom[[i]]$target)
      } else {
        shinyjs::hide(show_if_custom[[i]]$target)
      }
    }
  })
}

# Handle basic skip logic
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

# Handle custom skip logic
handle_custom_skip_logic <- function(
    input, skip_if_custom, current_page, next_page
) {

    # Loop through each skip logic condition
    for (j in 1:length(skip_if_custom)) {
        rule <- skip_if_custom[[j]]

        # Evaluate the condition
        condition_result <- rule$condition(input)

        # Check if the condition is met
        if (isTRUE(condition_result) & (current_page != rule$target)) {
            return(rule$target)
        }
    }

    return(next_page)
}

# Handle overall skip logic
handle_skip_logic <- function(input, skip_if, skip_if_custom, current_page, next_page) {
    if (!is.null(skip_if)) {
        next_page <- handle_basic_skip_logic(input, skip_if, current_page, next_page)
    }
    if (!is.null(skip_if_custom)) {
        next_page <- handle_custom_skip_logic(input, skip_if_custom, current_page, next_page)
    }
    return(next_page)
}

# Check if all required questions are answered
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

# Check if a single question is answered
check_answer <- function(q, input) {
    answer <- input[[q]]
    if (is.null(answer)) return(FALSE)
    if (is.character(answer)) return(any(nzchar(answer)))
    if (is.numeric(answer)) return(any(!is.na(answer)))
    if (inherits(answer, "Date")) return(any(!is.na(answer)))
    if (is.list(answer)) return(any(!sapply(answer, is.null)))
    return(TRUE)  # Default to true for unknown types
}

# Check if a question is visible
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

admin_enable <- function(input, output, session, db) {
    # Add admin button
    shiny::insertUI(
        selector = "body",
        where = "afterBegin",
        ui = htmltools::tags$div(
            id = "admin-button-container",
            style = "position: fixed; top: 20px; left: 10px; z-index: 1000;",
            shiny::actionButton("admin_button", "Admin")
        )
    )

    # Add hidden admin section (only login page is rendered initially)
    shiny::insertUI(
        selector = "body",
        where = "beforeEnd",
        ui = shinyjs::hidden(
            htmltools::div(
                id = "admin-section",
                class = "admin-section",
                htmltools::div(
                    id = "login-page",
                    htmltools::h2("Admin Login"),
                    shiny::passwordInput("adminpw", "Password"),
                    shiny::actionButton("submitPw", "Log in")
                )
            )
        )
    )

    # Toggle admin section visibility
    shiny::observeEvent(input$admin_button, {
        shinyjs::runjs("hideAllPages();")
        shinyjs::show("admin-section")
        shinyjs::show("login-page")
    })

    # Password check and admin content reveal
    shiny::observeEvent(input$submitPw, {
        if (input$adminpw == Sys.getenv("SURVEYDOWN_PASSWORD")) {
            # Store login status in a session variable
            session$userData$isAdmin <- TRUE

            # Hide login page
            shinyjs::hide("login-page")

            # Render admin content dynamically
            shiny::insertUI(
                selector = "#admin-section",
                ui = htmltools::div(
                    id = "admin-content",
                    htmltools::h2("Admin Page"),
                    shiny::actionButton("pause_survey", "Pause Survey"),
                    shiny::actionButton("pause_db", "Pause DB"),
                    shiny::downloadButton("download_data", "Download Data"),
                    shiny::actionButton("back_to_survey", "Admin Logout and Back to Survey"),
                    htmltools::hr(),
                    htmltools::h3("Survey Data"),
                    DT::DTOutput("survey_data_table")
                )
            )
            output$survey_data_table <- DT::renderDT({
                data <- DBI::dbReadTable(db$db, db$table_name)
                DT::datatable(data, options = list(scrollX = TRUE))
            })
        } else {
            shiny::showNotification("Incorrect password", type = "error")
        }
    })

    # Back to survey button
    shiny::observeEvent(input$back_to_survey, {
        # Clear the admin session
        session$userData$isAdmin <- NULL

        # Hide admin-related content
        shinyjs::hide("admin-section")
        shinyjs::hide("login-page")

        shiny::removeUI(selector = "#admin-content")
        shiny::updateTextInput(session, "adminpw", value = "")
        shinyjs::runjs("showFirstPage();")
    })

    # Download Data button functionality
    output$download_data <- shiny::downloadHandler(
        filename = function() {
            paste0(db$table_name, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            # Read the table
            data <- sd_get_data(db)

            # Write to CSV
            utils::write.csv(data, file, row.names = FALSE)
        }
    )
}

# Transform survey data for database storage
transform_data <- function(question_vals, time_vals, session_id, stored_vals) {
    # Create current timestamp in UTC with the desired format
    current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")

    for (i in seq_len(length(question_vals))) {
        val <- question_vals[[i]]
        if (is.null(val)) {
            question_vals[[i]] <- ""
        } else if (length(val) > 1) {
            question_vals[[i]] <- paste(question_vals[[i]], collapse = ", ")
        }
    }

    responses <- as.data.frame(question_vals)

    stored_df <- data.frame(matrix(ncol = length(stored_vals), nrow = 1))
    colnames(stored_df) <- names(stored_vals)
    for (name in names(stored_vals)) {
        stored_df[[name]] <- ifelse(is.null(stored_vals[[name]]), "", stored_vals[[name]])
    }

    # Combine all data
    data <- cbind(
        time_start = current_time,
        session_id = session_id,
        stored_df,
        responses,
        stats::setNames(as.data.frame(time_vals), names(time_vals))
    )

    return(data)
}

#' Set Password
#'
#' This function sets the supabase password in the .Renviron file and adds .Renviron to .gitignore.
#'
#' @param password Character string. The password to be set for Supabase connection.
#'
#' @details The function performs the following actions:
#'   1. Creates a .Renviron file in the root directory if it doesn't exist.
#'   2. Adds or updates the SURVEYDOWN_PASSWORD entry in the .Renviron file.
#'   3. Adds .Renviron to .gitignore if it's not already there.
#'
#' @return None. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#'   sd_set_password("your_SURVEYDOWN_PASSWORD")
#' }
#'
#' @export
sd_set_password <- function(password) {
    # Define the path to .Renviron file
    renviron_path <- file.path(getwd(), ".Renviron")

    # Check if .Renviron file exists, if not create it
    if (!file.exists(renviron_path)) {
        file.create(renviron_path)
    }

    # Read existing content
    existing_content <- readLines(renviron_path)

    # Check if SURVEYDOWN_PASSWORD is already defined
    password_line_index <- grep("^SURVEYDOWN_PASSWORD=", existing_content)

    # Prepare the new password line
    new_password_line <- paste0("SURVEYDOWN_PASSWORD=", password)

    # If SURVEYDOWN_PASSWORD is already defined, replace it; otherwise, append it
    if (length(password_line_index) > 0) {
        existing_content[password_line_index] <- new_password_line
    } else {
        existing_content <- c(existing_content, new_password_line)
    }

    # Write the updated content back to .Renviron
    writeLines(existing_content, renviron_path)

    # Add .Renviron to .gitignore if not already there
    gitignore_path <- file.path(getwd(), ".gitignore")
    if (file.exists(gitignore_path)) {
        gitignore_content <- readLines(gitignore_path)
        if (!".Renviron" %in% gitignore_content) {
            # Remove any trailing empty lines
            while (length(gitignore_content) > 0 && gitignore_content[length(gitignore_content)] == "") {
                gitignore_content <- gitignore_content[-length(gitignore_content)]
            }
            # Add .Renviron to the end without an extra newline
            gitignore_content <- c(gitignore_content, ".Renviron")
            writeLines(gitignore_content, gitignore_path)
        }
    } else {
        writeLines(".Renviron", gitignore_path)
    }

    message("Password set successfully and .Renviron added to .gitignore.")
}

#' Store a value
#'
#' This function allows storing additional values to be included in the survey data,
#' such as respondent IDs or other data.
#'
#' @param value The raid value to be stored.
#' @param id (Optional) The id (name) of the value in the data.
#'             If not provided, the id of the `value` variable will be used.
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
sd_store_value <- function(value, id = NULL) {
    if (is.null(id)) {
        id <- deparse(substitute(value))
    }

    shiny::isolate({
        session <- shiny::getDefaultReactiveDomain()
        if (is.null(session)) {
            stop("sd_store_value must be called from within a Shiny reactive context")
        }
        if (is.null(session$userData$stored_values)) {
            session$userData$stored_values <- list()
        }
        session$userData$stored_values[[id]] <- value
    })

    invisible(NULL)
}

#' Create a copy of an input value
#'
#' This function creates a copy of an input value and makes it available as a new output.
#' The new output can then be displayed using sd_display_value().
#'
#' @param id The ID of the input value to copy
#' @param id_copy The ID for the new copy (must be different from id)
#'
#' @return NULL invisibly. This function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' sd_copy_value(id = "respondent_name", id_copy = "resp_name2")
#'
#' # Then in UI:
#' # sd_display_value("resp_name2")
#' }
#'
#' @export
sd_copy_value <- function(id, id_copy) {
    if (id == id_copy) {
        stop("The 'id_copy' must be different from the 'id'")
    }
    shiny::isolate({
        output <- shiny::getDefaultReactiveDomain()$output
        input <- shiny::getDefaultReactiveDomain()$input
        output_id <- paste0(id_copy, "_value")
        if (!is.null(output)) {
            output[[output_id]] <- shiny::renderText({ input[[id]] })
        } else {
            warning("sd_copy_value was not called within a Shiny reactive context")
        }
    })
    invisible(NULL)
}

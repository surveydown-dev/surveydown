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
#' @import shiny
#' @importFrom stats setNames
#' @importFrom shiny reactiveValuesToList observeEvent renderText
#'
#' @details
#' The \code{config} list should include the following elements:
#' \itemize{
#'   \item \code{pages}: A list of page objects containing page content and metadata.
#'   \item \code{head_content}: HTML content for the <head> section.
#'   \item \code{page_ids}: A vector of page identifiers.
#'   \item \code{question_ids}: A vector of question identifiers.
#'   \item \code{skip_if}: A list defining conditions for skipping pages.
#'   \item \code{show_if}: A list defining conditions for showing questions.
#'   \item \code{start_page}: The identifier of the starting page.
#'   \item \code{admin_page}: A logical indicating if an admin page should be included.
#'   \item \code{question_required}: A vector of required question identifiers.
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
#'   \item Sets up admin functionality if enabled in the configuration.
#' }
#'
#' @section Progress Bar:
#' The progress bar is updated based on the last answered question. It will jump to the
#' percentage corresponding to the last answered question and will never decrease,
#' even if earlier questions are answered later. The progress is calculated as the ratio
#' of the last answered question's index to the total number of questions.
#'
#' @section Database Operations:
#' If \code{db} is provided, the function will update the database with survey responses.
#' If \code{db} is \code{NULL} (ignore mode), responses will be saved to a local CSV file.
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
#' \code{\link{sd_database}}, \code{\link{sd_config}}
#'
#' @export
sd_server <- function(input, output, session, config, db = NULL) {

    # Initialize local functions ----

    # Function to update progress bar
    update_progress_bar <- function(index) {
        if (index > last_answered_question()) {
            last_answered_question(index)
            current_progress <- index / length(question_ids)
            max_progress(max(max_progress(), current_progress))
            session$sendCustomMessage("updateProgressBar", max_progress() * 100)
        }
    }

    # Function to update the data
    update_data <- function() {
        data_list <- latest_data()
        if (ignore_mode) {
            if (file.access('.', 2) == 0) {  # Check if current directory is writable
                tryCatch({
                    utils::write.csv(
                        as.data.frame(data_list, stringsAsFactors = FALSE),
                        "data.csv",
                        row.names = FALSE
                    )
                }, error = function(e) {
                    warning("Unable to write to data.csv")
                    message("Error details: ", e$message)
                })
            } else {
                message("Running in a non-writable environment.")
            }
        } else {
            database_uploading(data_list, db$db, db$table)
        }
    }

    # Initialize local variables ----

    # Tag start time and unique session_id
    time_start <- get_utc_timestamp()
    session_id <- session$token

    # Check if db is NULL (either left blank or specified with ignore = TRUE)
    ignore_mode <- is.null(db)

    # Create local objects from config file
    pages          <- config$pages
    head_content   <- config$head_content
    page_ids       <- config$page_ids
    question_ids   <- config$question_ids
    skip_if        <- config$skip_if
    show_if        <- config$show_if
    start_page     <- config$start_page
    admin_page     <- config$admin_page
    question_required <- config$question_required

    # Pre-compute timestamp IDs
    page_ts_ids     <- paste0("time_p_", page_ids)
    question_ts_ids <- paste0("time_q_", question_ids)
    all_ts_ids      <- c(page_ts_ids, question_ts_ids)

    # Initial settings ----

    # Keep-alive observer - this will be triggered every 60 seconds
    shiny::observeEvent(input$keepAlive, {
      cat("Session keep-alive at", format(Sys.time(), "%m/%d/%Y %H:%M:%S"), "\n")
    })

    # Create admin page if admin_page is TRUE
    if (isTRUE(config$admin_page)) admin_enable(input, output, session, db)

    # Add observers for show_if conditions
    if (!is.null(show_if)) { set_show_if_logic(input, show_if) }

    # Data tracking ----

    # Format static data
    static_list <- list(
        session_id = session_id,
        time_start = time_start
    )
    static_list <- c(static_list, get_stored_vals(session))

    # Initialize values for progressbar
    load_js_file("update_progress.js")
    max_progress <- shiny::reactiveVal(0)
    last_answered_question <- shiny::reactiveVal(0)

    # Initialize timestamps and question values
    timestamps <- initialize_timestamps(page_ts_ids, question_ts_ids, time_start)
    question_vals <- initialize_question_vals(question_ids)

    # Database table initialization
    if (!ignore_mode) {
        table_exists <- pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbExistsTable(conn, db$table)
        })
        time_ids <- c(all_ts_ids, 'time_last_interaction')
        initial_data <- c(
            static_list,
            setNames(lapply(question_ids, function(id) NA), question_ids),
            setNames(lapply(time_ids, function(id) NA), time_ids)
        )
        if (!table_exists) {
            create_table(initial_data, db$db, db$table)
        }
        # Check if there are any new columns, update DB accordingly
        check_and_add_columns(initial_data, db$db, db$table)
    }

    # Create a reactive expression for the latest data
    latest_data <- shiny::reactive({
        # Update timestamp of last interaction
        timestamps$time_last_interaction <- get_utc_timestamp()
        # Merge all lists
        data <- c(
            static_list,
            reactiveValuesToList(question_vals),
            reactiveValuesToList(timestamps)
        )
        # Ensure all elements are of length 1, use "" for empty or NULL values
        data <- lapply(data, function(x) {
            if (length(x) == 0 || is.null(x) || (is.na(x) && !is.character(x))) "" else as.character(x)[1]
        })
        data <- data[names(data) != ""]
        return(data)
    })

    # Main question observers ----
    # (one created for each question)

    lapply(seq_along(question_ids), function(index) {
        local_id <- question_ids[index]
        local_ts_id <- question_ts_ids[index]

        observeEvent(input[[local_id]], {
            # Update question value
            question_vals[[local_id]] <- format_question_value(input[[local_id]])

            # Update timestamp and progress if interacted
            if (!is.null(input[[paste0(local_id, "_interacted")]])) {
                timestamps[[local_ts_id]] <- get_utc_timestamp()
                update_progress_bar(index)
            }

            # Make value accessible in the UI
            output[[paste0(local_id, "_value")]] <- renderText({
                as.character(question_vals[[local_id]])
            })

            # Update data
            update_data()
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
    })

    # Page rendering ----

    # Create reactive values for the current page ID
    current_page_id <- shiny::reactiveVal(page_ids[1])

    # Start from start_page (if specified)
    if (!is.null(start_page)) {
        current_page_id(start_page)
    }

    get_current_page <- reactive({
        pages[[which(sapply(pages, function(p) p$id == current_page_id()))]]
    })

    # Render the current page
    output$main <- shiny::renderUI({
        current_page <- get_current_page()
        shiny::tagList(
            shiny::tags$head(shiny::HTML(head_content)),
            shiny::tags$div(
                class = "content",
                shiny::tags$div(
                    class = "page-columns page-rows-contents page-layout-article",
                    shiny::tags$div(
                        id = "quarto-content",
                        role = "main",
                        shiny::HTML(current_page$content)
                    )
                )
            )
        )
    })

    # Page navigation ----

    # Determine which page is next, then update current_page_id() to it
    shiny::observe({
        lapply(pages, function(page) {
            shiny::observeEvent(input[[page$next_button_id]], {

                # Get current and next pages
                current_page_id <- page$id
                next_page_id <- get_default_next_page(page, page_ids)

                # Determine next page based on the current page and skip logic
                next_page_id <- handle_skip_logic(
                    input, skip_if, current_page_id, next_page_id
                )

                if (!is.null(next_page_id)) {

                    # Check if all required questions are answered
                    all_required_answered <- check_all_required(
                        page, input, show_if
                    )

                    if (all_required_answered) {
                        # Update the current page ID, then update the data
                        current_page_id(next_page_id)
                        # Update timestamp for the next page
                        next_ts_id <- page_ts_ids[which(page_ids == next_page_id)]
                        timestamps[[next_ts_id]] <- get_utc_timestamp()
                        update_data()
                    } else {
                        shiny::showNotification(
                            "Please answer all required questions before proceeding.",
                            type = "error"
                        )
                    }
                }
            })
        })
    })

    shiny::observe({
        page <- get_current_page()
        if (is.null(page$next_page_id)) {
            update_progress_bar(length(question_ids))
        }
    })

    # Ensure final update on session end
    shiny::onSessionEnded(function() {
        shiny::isolate({
            update_data()
        })
    })

}

set_show_if_logic <- function(input, show_if) {
    # Create a reactive expression for each condition
    condition_reactives <- lapply(show_if, function(rule) {
        shiny::reactive({
            tryCatch({
                evaluate_condition(rule$condition, input)
            }, error = function(e) {
                warning(sprintf(
                    "Error in show_if condition for target '%s': %s",
                    rule$target, conditionMessage(e))
                )
                FALSE
            })
        })
    })

    # Create a single observer to handle all conditions
    shiny::observe({
        for (i in seq_along(show_if)) {
            condition_result <- condition_reactives[[i]]()
            condition_met <- isTRUE(condition_result)
            target <- show_if[[i]]$target

            if (condition_met) {
                shinyjs::runjs(sprintf("
                    $('#%s').closest('.question-container').show();
                    $('#%s').show();
                ", target, target))
            } else {
                shinyjs::runjs(sprintf("
                    $('#%s').closest('.question-container').hide();
                    $('#%s').hide();
                ", target, target))
            }
        }
    })
}

evaluate_condition <- function(condition, input) {
    tryCatch({
        eval(condition, envir = list(input = input))
    }, error = function(e) {
        stop(sprintf(
            "Error in evaluating condition: %s\nCondition: %s\nError: %s",
            deparse(condition), conditionMessage(e))
        )
    })
}

# Function to get all stored values
get_stored_vals <- function(session) {
    shiny::isolate({
        if (is.null(session)) {
            stop("get_stored_vals must be called from within a Shiny reactive context")
        }
        stored_vals <- session$userData$stored_values
        if (is.null(stored_vals)) { return(NULL) }

        # Format stored values as a list
        formatted_vals <- lapply(stored_vals, function(val) {
            if (is.null(val)) "" else val
        })

        return(formatted_vals)
    })
}

# Get Current UTC Timestamp
get_utc_timestamp <- function() {
    return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

# Initialize timestamps for pages and questions
initialize_timestamps <- function(page_ts_ids, question_ts_ids, time_start) {
    timestamps <- shiny::reactiveValues()

    # Initialize page timestamps
    for (i in seq_along(page_ts_ids)) {
        timestamps[[page_ts_ids[i]]] <- if (i == 1) get_utc_timestamp() else ""
    }

    # Initialize question timestamps
    for (ts_id in question_ts_ids) {
        timestamps[[ts_id]] <- ""
    }

    # Initialize time of last interaction
    timestamps[['time_last_interaction']] <- time_start

    return(timestamps)
}

initialize_question_vals <- function(question_ids) {
    vals <- shiny::reactiveValues()
    for (id in question_ids) {
        vals[[id]] <- ""  # Empty string instead of NA
    }
    return(vals)
}

# Helper function to format a single question value
format_question_value <- function(val) {
    if (is.null(val) || identical(val, NA) || identical(val, "NA")) {
        return("")
    } else if (length(val) > 1) {
        return(paste(val, collapse = ", "))
    } else {
        return(as.character(val))
    }
}

get_default_next_page <- function(page, page_ids) {
    if (is.null(page$next_page_id)) { return(NULL) }
    next_page_id <- page$next_page_id
    if (next_page_id == "") {
        # No next page specified, so just go to the next one
        index <- which(page_ids == page$id) + 1
        if (index <= length(page_ids)) {
            return(page_ids[index])
        } else {
            return(NULL)  # No next page if we're on the last page
        }
    }
    return(next_page_id)
}

handle_skip_logic <- function(input, skip_if, current_page_id, next_page_id) {
    if (is.null(next_page_id) | is.null(skip_if)) { return(next_page_id) }
    # Loop through each skip logic condition
    for (i in seq_along(skip_if)) {
        rule <- skip_if[[i]]

        # Evaluate the condition
        condition_result <- tryCatch({
            evaluate_condition(rule$condition, input)
        }, error = function(e) {
            warning(sprintf(
                "Error in skip_if condition for target '%s': %s",
                rule$target, conditionMessage(e))
            )
            FALSE
        })

        # Check if the condition is met
        if (isTRUE(condition_result) & (current_page_id != rule$target)) {
            return(rule$target)
        }
    }
    return(next_page_id)
}

# Check if all required questions are answered
check_all_required <- function(page, input, show_if) {
    questions <- page$questions
    questions_required <- page$required_questions
    results <- vapply(questions, function(q) {
        is_required <- q %in% questions_required
        is_visible <- is_question_visible(q, show_if, input)
        is_answered <- check_answer(q, input)

        if (!is_required) return(TRUE)
        if (!is_visible) return(TRUE)
        return(is_answered)
    }, logical(1))

    all_required_answered <- all(results)
    return(all_required_answered)
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

    # Check show_if conditions
    is_visible <- if (is.null(show_if)) TRUE else {
        !any(sapply(show_if, function(rule) {
            rule$target == q && !isTRUE(evaluate_condition(rule$condition, input))
        }))
    }

    return(is_visible)
}

admin_enable <- function(input, output, session, db) {
    #not fun to figure out, do not render the admin page at the start if you are
    #using an outright hide_pages js file
    show_admin_section <- function() {
        shinyjs::hide("quarto-content")
        shiny::insertUI(
            selector = "body",
            where = "beforeEnd",
            ui = htmltools::div(
                id = "admin-section",
                class = "admin-section",
                htmltools::div(
                    id = "login-page",
                    htmltools::h2("Admin Login"),
                    shiny::passwordInput("adminpw", "Password"),
                    shiny::actionButton("submitPw", "Log in"),
                    htmltools::br(),
                    htmltools::br(),
                    shiny::actionButton("back_to_survey_login", "Back to Survey")
                ),
                shinyjs::hidden(
                    htmltools::div(
                        id = "admin-content",
                        htmltools::h2("Admin Page"),
                        shiny::actionButton("pause_survey", "Pause Survey"),
                        shiny::actionButton("pause_db", "Pause DB"),
                        shiny::downloadButton("download_data", "Download Data"),
                        shiny::actionButton("back_to_survey_admin", "Back to Survey"),
                        htmltools::hr(),
                        htmltools::h3("Survey Data"),
                        DT::DTOutput("survey_data_table")
                    )
                )
            )
        )
    }

    # Observe for URL change
    url_reactive <- reactive({
        session$clientData$url_search
    })

    # Observe changes to the URL
    shiny::observe({
        url <- url_reactive()
        query <- parseQueryString(url)
        admin_param <- query[['admin']]
        if(!is.null(admin_param)) {
            show_admin_section()
        }
    })

    # Password check and admin content reveal
    shiny::observeEvent(input$submitPw, {
        if (input$adminpw == Sys.getenv("SURVEYDOWN_PASSWORD")) {
            session$userData$isAdmin <- TRUE
            shinyjs::hide("login-page")
            shinyjs::show("admin-content")

            output$survey_data_table <- DT::renderDT({
                data <- DBI::dbReadTable(db$db, db$table)
                DT::datatable(data, options = list(scrollX = TRUE))
            })
        } else {
            shiny::showNotification("Incorrect password", type = "error")
        }
    })

    # Function to return to survey
    return_to_survey <- function() {
        session$userData$isAdmin <- NULL
        shinyjs::hide("admin-section")
        shinyjs::show("quarto-content")
        shinyjs::runjs("showFirstPage();")
        shiny::updateQueryString("?", mode = "replace")
    }

    # Back to survey button on login page
    shiny::observeEvent(input$back_to_survey_login, {
        return_to_survey()
    })

    # Back to survey button on admin content page
    shiny::observeEvent(input$back_to_survey_admin, {
        return_to_survey()
    })

    #Pause Survey - Pause DB Section

    shiny::observeEvent(input$pause_survey, {
        #Code here that write to the table to change row value from 0 -> 1 and back if it happens
        data <- DBI::dbReadTable(db$db, paste0(db$table, "_admin_table"))
        #Read table value in, change it from true to false


        #Add in Sd_server if(survey_paused == TRUE)
        #Create and display a blank page that says the survey is pause


    })

    # Download Data button functionality
    output$download_data <- shiny::downloadHandler(
        filename = function() {
            paste0(db$table, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            data <- DBI::dbReadTable(db$db, db$table)
            utils::write.csv(data, file, row.names = FALSE)
        }
    )
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
        session$userData$stored_values[[id]] <- format_question_value(value)
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

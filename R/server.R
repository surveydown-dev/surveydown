#' Server Logic for a surveydown survey
#'
#' @description
#' This function defines the server-side logic for a Shiny application used in surveydown.
#' It handles various operations such as conditional display, progress tracking,
#' page navigation, database updates for survey responses, and admin functionality.
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
#'   \item \code{admin_page}: A logical indicating if an admin page should be included.
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
#'
#'   # With admin page enabled
#'   my_config <- sd_config(admin_page = TRUE)
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

    # Tag start time and unique session_id
    time_start <- get_utc_timestamp()
    session_id <- session$token

    # Check if db is NULL (either left blank or specified with ignore = TRUE)
    ignore_mode <- is.null(db)

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

    # Initial page settings ----

    # Start from start_page (if specified)
    if (!is.null(start_page)) {
        shinyjs::show(start_page)
    } else {
        shinyjs::runjs("showFirstPage();")
    }

    # Show all pages if show_all_pages is TRUE
    if (show_all_pages) lapply(page_ids, shinyjs::show)

    # Conditional display (show_if conditions)
    if (!is.null(show_if)) { basic_show_if_logic(input, show_if) }
    if (!is.null(show_if_custom)) { custom_show_if_logic(input, show_if_custom) }

    # Other settings ----

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

    # Create admin page if admin_page is TRUE
    if (isTRUE(config$admin_page)) {
        admin_enable(input, output, session, db)
    }

    # Data tracking ----

    # Initialize object for storing timestamps
    timestamps <- shiny::reactiveValues(
        data = initialize_timestamps(page_ids, question_ids)
    )

    # Create a reactive expression for obtaining all formatted question values
    get_question_vals <- shiny::reactive({
        result <- list()
        for (id in question_ids) {
            result[[id]] <- format_question_value(input[[id]])
        }
        return(result)
    })

    # Set the respondent_id
    respondent_id <- get_respondent_id(if (!ignore_mode) db else NULL)

    # Format static data
    stored_vals <- get_stored_vals(session)
    static_df <- cbind(
        data.frame(
            time_start    = time_start,
            respondent_id = respondent_id,
            session_id    = session_id
        )
    )
    if (!is.null(stored_vals)) {
        static_df <- cbind(static_df, stored_vals)
    }

    # Initialize values for progressbar
    load_js_file("update_progress.js")
    max_progress <- shiny::reactiveVal(0)
    last_answered_question <- shiny::reactiveVal(0)

    # Main question observer ----

    # Main observer for progress bar, timestamps, and database updating
    shiny::observe({
        question_values <- get_question_vals()
        time_last_interaction <- get_utc_timestamp()

        for (index in seq_along(question_ids)) {
            id <- question_ids[index]
            value <- question_values[[id]]

            # Make value accessible in the UI
            local({
                local_id <- id
                output[[paste0(local_id, "_value")]] <- shiny::renderText({
                    as.character(question_values[[local_id]])
                })
            })

            # If answered, update timestamp, progress bar, and database
            if (!is.null(input[[paste0(id, "_interacted")]])) {
                ts_name <- make_ts_name("question", id)
                if (is.na(timestamps$data[[ts_name]])) {
                    timestamps$data[[ts_name]] <- get_utc_timestamp()
                }
                if (index > last_answered_question()) {
                    last_answered_question(index)
                }
            }
        }

        # Calculate progress based on the last answered question
        current_progress <- last_answered_question() / length(question_ids)
        max_progress(max(max_progress(), current_progress))

        # Use the custom message handler to update the progress bar
        session$sendCustomMessage("updateProgressBar", max_progress() * 100)

        # Update database ----

        # Transform to data frame, handling uninitialized inputs appropriately
        df_local <- transform_data(static_df, question_values, timestamps$data, time_last_interaction)

        # Update database or write to CSV based on preview mode
        if (ignore_mode) {
            if (file.access('.', 2) == 0) {  # Check if current directory is writable
                tryCatch({
                    utils::write.csv(df_local, "data.csv", row.names = FALSE)
                }, error = function(e) {
                    warning("Unable to write to data.csv. The survey will run without data collection.")
                    message("Error details: ", e$message)
                })
            } else {
                message("Running in a non-writable environment. The survey will proceed without data collection.")
            }
        } else {
            database_uploading(df_local, db$db, db$table)
        }
    })

    # Main Page Observer ----
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
                current_page_questions <- page_structure[[current_page]]$questions
                all_required_answered <- check_all_required(
                    current_page_questions, question_required, input, show_if, show_if_custom
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
}

# Get Current UTC Timestamp
get_utc_timestamp <- function() {
    return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

# Initialize Timestamps for Pages and Questions
initialize_timestamps <- function(page_ids, question_ids) {
    timestamps <- list()

    timestamps[[make_ts_name("page", page_ids[1])]] <- get_utc_timestamp()
    for (i in 2:length(page_ids)) {
        timestamps[[make_ts_name("page", page_ids[i])]] <- NA
    }

    for (qid in question_ids) {
        timestamps[[make_ts_name("question", qid)]] <- NA
    }

    return(timestamps)
}

# Make Timestamp Name
make_ts_name <- function(type, id) {
    if (type == "page") {
        return(paste0("time_p_", id))
    } else if (type == "question") {
        return(paste0("time_q_", id))
    }
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

# Handle basic show-if logic
basic_show_if_logic <- function(input, show_if) {

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
custom_show_if_logic <- function(input, show_if_custom) {
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

# Handle overall skip logic
handle_skip_logic <- function(input, skip_if, skip_if_custom, current_page, next_page) {
    if (!is.null(skip_if)) {
        next_page <- basic_skip_logic(input, skip_if, current_page, next_page)
    }
    if (!is.null(skip_if_custom)) {
        next_page <- custom_skip_logic(input, skip_if_custom, current_page, next_page)
    }
    return(next_page)
}

# Handle basic skip logic
basic_skip_logic <- function(
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
custom_skip_logic <- function(
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

# Check if all required questions are answered
check_all_required <- function(
        questions, questions_required, input, show_if, show_if_custom
) {
    results <- vapply(questions, function(q) {
        is_required <- q %in% questions_required
        is_visible <- is_question_visible(q, show_if, show_if_custom, input)
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
is_question_visible <- function(q, show_if, show_if_custom, input) {
    # Check basic show_if conditions
    basic_visible <- if (is.null(show_if) || nrow(show_if) == 0) TRUE else {
        rules <- show_if[show_if$target == q, ]
        nrow(rules) == 0 || any(sapply(1:nrow(rules), function(i) {
            input_value <- input[[rules$question_id[i]]]
            expected_value <- rules$question_value[i]
            if (is.null(input_value)) FALSE
            else if (is.list(input_value)) expected_value %in% unlist(input_value)
            else input_value == expected_value
        }))
    }

    # Check custom show_if conditions
    custom_visible <- if (is.null(show_if_custom)) TRUE else {
        !any(sapply(show_if_custom, function(rule) {
            rule$target == q && !isTRUE(rule$condition(input))
        }))
    }

    # Return TRUE if the question is visible according to both conditions
    basic_visible && custom_visible
}

# Function to get all stored values
get_stored_vals <- function(session) {
    shiny::isolate({
        if (is.null(session)) {
            stop("get_stored_vals must be called from within a Shiny reactive context")
        }
        stored_vals <- session$userData$stored_values
        if (is.null(stored_vals)) { return(NULL) }

        # Format stored values
        stored_df <- data.frame(matrix(ncol = length(stored_vals), nrow = 1))
        colnames(stored_df) <- names(stored_vals)
        for (name in names(stored_vals)) {
            val <- stored_vals[[name]]
            stored_df[[name]] <- ifelse(is.null(val), "", val)
        }
        return(stored_df)
    })
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

# Get the next respondent ID
get_respondent_id <- function(db = NULL) {
    if (is.null(db)) return(1)

    tryCatch({
        if (DBI::dbExistsTable(db$db, db$table)) {
            query <- paste0("SELECT COALESCE(MAX(CAST(respondent_id AS INTEGER)), 0) FROM ", db$table)
            result <- DBI::dbGetQuery(db$db, query)
            new_id <- as.integer(result[[1]]) + 1
            return(new_id)
        } else {
            return(1)
        }
    }, error = function(e) {
        warning("Error in get_respondent_id: ", e$message)
        return(1)
    })
}

# Transform survey data for database storage
transform_data <- function(
        static_df, question_values, time_vals, time_last_interaction
) {

    # Initialize empty data frames (if no questions, no time values)
    question_values_df <- data.frame(row.names = 1)
    time_vals_df <- question_values_df

    # Convert question and time values to a data frame
    if (length(question_values) != 0) {
        question_values_df <- as.data.frame(
            lapply(question_values, function(x) rep(x, length.out = 1)),
            stringsAsFactors = FALSE
        )
    }
    if (length(time_vals) != 0) {
        time_vals_df <- as.data.frame(
            lapply(time_vals, function(x) rep(x, length.out = 1)),
            stringsAsFactors = FALSE
        )
    }

    # Create a single-row data frame for time_last_interaction
    time_last_interaction_df <- data.frame(time_last_interaction = time_last_interaction)

    # Combine all parts
    data <- cbind(
        static_df,
        question_values_df,
        time_last_interaction_df,
        time_vals_df
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

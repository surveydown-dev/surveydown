#' Server Logic for a surveydown survey
#'
#' @description
#' This function defines the server-side logic for a Shiny application used in surveydown.
#' It handles various operations such as conditional display, progress tracking,
#' page navigation, and database updates for survey responses.
#'
#' @param db A list containing database connection information created using
#' \code{\link{sd_database}} function. Defaults to \code{NULL}.
#' @param use_html Logical. By default, the `"survey.qmd"` file will be
#' rendered when the app launches, which can be slow. Users can render it
#' first into a html file and set `use_html = TRUE` to use the pre-rendered
#' file, which is faster when the app loads. Defaults to `FALSE`.
#' @param required_questions Vector of character strings. The IDs of questions that must be answered. Defaults to NULL.
#' @param all_questions_required Logical. If TRUE, all questions in the survey will be required. Defaults to FALSE.
#' @param start_page Character string. The ID of the page to start on. Defaults to NULL.
#' @param admin_page Logical. Whether to include an admin page for viewing and downloading survey data. Defaults to `FALSE`.
#'
#' @import shiny
#' @importFrom stats setNames
#' @importFrom shiny reactiveValuesToList observeEvent renderText
#'
#' @details
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
#'   library(surveydown)
#'   db <- sd_database()
#'
#'   shinyApp(
#'     ui = sd_ui(),
#'     server = function(input, output, session) {
#'       sd_server(db = db)
#'     }
#'   )
#' }
#'
#' @seealso
#' \code{\link{sd_database}}
#'
#' @export
sd_server <- function(
    db = NULL,
    use_html = FALSE,
    required_questions = NULL,
    all_questions_required = FALSE,
    start_page = NULL,
    admin_page = FALSE
) {

    # Get input, output, and session from the parent environment
    parent_env <- parent.frame()
    input <- get("input", envir = parent_env)
    output <- get("output", envir = parent_env)
    session <- get("session", envir = parent_env)

    # Tag start time and unique session_id
    time_start <- get_utc_timestamp()
    session_id <- session$token

    # Get any skip or show conditions
    show_if <- shiny::getDefaultReactiveDomain()$userData$show_if
    skip_if <- shiny::getDefaultReactiveDomain()$userData$skip_if

    # Run the configuration settings
    config <- run_config(
        use_html,
        required_questions,
        all_questions_required,
        start_page,
        admin_page,
        skip_if,
        show_if
    )

    # Set up show_if conditions
    show_if_results <- if (!is.null(show_if)) {
        set_show_if_conditions(show_if)
    } else {
        shiny::reactive(list())
    }
    # Create an observer to handle visibility
    shiny::observe({
        results <- show_if_results()
        for (target in names(results)) {
            if (results[[target]]) {
                shinyjs::show(target)
            } else {
                shinyjs::hide(target)
            }
        }
    })

    # Initialize local variables ----

    # Check if db is NULL (either left blank or specified with ignore = TRUE)
    ignore_mode <- is.null(db)

    # Create local objects from config file
    pages        <- config$pages
    head_content <- config$head_content
    page_ids     <- config$page_ids
    question_ids <- config$question_ids
    start_page   <- config$start_page
    admin_page   <- config$admin_page
    question_required <- config$question_required
    page_id_to_index <- setNames(seq_along(page_ids), page_ids)

    # Pre-compute timestamp IDs
    page_ts_ids     <- paste0("time_p_", page_ids)
    question_ts_ids <- paste0("time_q_", question_ids)
    start_page_ts_id <- page_ts_ids[which(page_ids == start_page)]
    all_ids <- c('time_end', question_ids, question_ts_ids, page_ts_ids)

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

    update_data <- function(data_list, changed_fields = NULL, time_last = FALSE) {
        if (length(changed_fields) == 0) {
            changed_fields = names(data_list)
        }
        if (time_last) {
            data_list[['time_end']] <- get_utc_timestamp()
        }

        admin_table <- admin_data$admin_table_state()
        is_survey_paused <- !is.null(admin_table) && !is.null(admin_table$pausesurvey) && admin_table$pausesurvey
        is_db_paused <- !is.null(admin_table) && !is.null(admin_table$pausedb) && admin_table$pausedb

        if (!is_survey_paused) {
            if (ignore_mode || is_db_paused) {
                # Write to CSV if in ignore mode or if DB is paused
                if (file.access('.', 2) == 0) {  # Check if current directory is writable
                    tryCatch({
                        utils::write.csv(
                            as.data.frame(data_list, stringsAsFactors = FALSE),
                            "data.csv",
                            row.names = FALSE,
                            append = file.exists("data.csv")
                        )
                    }, error = function(e) {
                        warning("Unable to write to data.csv")
                        message("Error details: ", e$message)
                    })
                } else {
                    message("Running in a non-writable environment.")
                }
            } else {
                # Upload to database if not in ignore mode and DB is not paused
                database_uploading(data_list, db$db, db$table, changed_fields)
            }
        } else {
            message("Survey is paused. Data not saved.")
        }

        # Reset changed_fields after updating the data
        changed_fields(character(0))
    }



    # Initial settings ----

    # Keep-alive observer - this will be triggered every 60 seconds
    shiny::observeEvent(input$keepAlive, {
      cat("Session keep-alive at", format(Sys.time(), "%m/%d/%Y %H:%M:%S"), "\n")
    })


    # Initialize values for progressbar
    load_js_file("update_progress.js")
    max_progress <- shiny::reactiveVal(0)
    last_answered_question <- shiny::reactiveVal(0)

    # Data tracking ----

    # Initialize the all_data reactive values
    initial_data <- get_initial_data(
        session, session_id, time_start, all_ids, start_page_ts_id
    )
    all_data <- do.call(shiny::reactiveValues, initial_data)

    # Initialize database table
    if (!ignore_mode) {
        table_exists <- pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbExistsTable(conn, db$table)
        })
        if (!table_exists) {
            create_table(initial_data, db$db, db$table)
        }
    }

    # Reactive expression that returns a list of the latest data
    latest_data <- shiny::reactive({
        # Convert reactiveValues to a regular list
        data <- reactiveValuesToList(all_data)

        # Ensure all elements are of length 1, use "" for empty or NULL values
        data <- lapply(data, function(x) {
            if (length(x) == 0 || is.null(x) || (is.na(x) && !is.character(x))) "" else as.character(x)[1]
        })

        data[names(data) != ""]
    })

    # Observer to update the data upon any change in the data
    observe({
        data <- latest_data()
        update_data(data, changed_fields())
    })

    # Reactive value to track which fields have changed
    changed_fields <- shiny::reactiveVal(character(0))

    # Main question observers ----
    # (one created for each question)

    lapply(seq_along(question_ids), function(index) {
        local_id <- question_ids[index]
        local_ts_id <- question_ts_ids[index]

        observeEvent(input[[local_id]], {
            # Update question value
            all_data[[local_id]] <- format_question_value(input[[local_id]])

            # Update tracker of which fields changed
            changed_fields(c(changed_fields(), local_id))

            # Update timestamp and progress if interacted
            if (!is.null(input[[paste0(local_id, "_interacted")]])) {
                all_data[[local_ts_id]] <- get_utc_timestamp()
                changed_fields(c(changed_fields(), local_ts_id))
                update_progress_bar(index)
            }

            # Make value accessible in the UI
            output[[paste0(local_id, "_value")]] <- renderText({
                as.character(all_data[[local_id]])
            })

            # Trigger show_if evaluation
            show_if_results()

            # Update data after a short delay
            shiny::invalidateLater(100)
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
    })

    # Admin setup ----

    admin_data <- if (admin_page) {
        admin_enable(input, output, session, db, current_page_id, admin_state, pages)
    } else {
        list(admin_make_content = function() {}, admin_table_state = reactiveVal(NULL))
    }

    is_admin_page <- reactive({
        query <- parseQueryString(session$clientData$url_search)
        !is.null(query[['admin']])
    })

    admin_state <- reactiveVal("login")

    # Page rendering ----

    # Create reactive values for the start page ID
    # (defaults to first page if NULL...see run_config() function)

    current_page_id <- reactiveVal(start_page)

    #Logic for getting the current page, if statements needed for the admin section
    get_current_page <- reactive({
        admin_table <- admin_data$admin_table_state()
        is_admin <- is_admin_page()
        current_id <- current_page_id()

        if (current_id == "pause-page" ||
            (!is.null(admin_table) &&
             !is.null(admin_table$pausesurvey) &&
             isTRUE(admin_table$pausesurvey) &&
             !isTRUE(is_admin))) {
            return(list(id = "pause-page", content = make_pause_page()))
        }


        if (isTRUE(is_admin) && isTrue(config$admin_page)) {
            if (admin_state() == "login") {
                return(list(id = "admin_login", content = admin_make_login()))
            } else if (admin_state() == "content") {
                return(list(id = "admin_content", content = admin_data$admin_make_content()))
            }
        }

        page_index <- which(sapply(pages, function(p) p$id == current_id))
        if (length(page_index) == 0) {
            return(pages[[1]])
        }
        pages[[page_index]]
    })

    #Main page rendering
    output$main <- renderUI({
        current_page <- get_current_page()
        page_content <- current_page$content
        print(current_page$id)
        if (! current_page$id %in% c("admin_login", "admin_content", "pause-page")) {
            page_content <- HTML(page_content)
        }
        tagList(
            tags$head(HTML(head_content)),
            tags$div(
                class = "content",
                tags$div(
                    class = "page-columns page-rows-contents page-layout-article",
                    tags$div(
                        id = "quarto-content",
                        role = "main",
                        page_content
                    )
                )
            )
        )
    })

    # Observer to handle PauseDB
    observe({
        admin_table <- admin_data$admin_table_state()
        if (!is.null(db) &&
            !is.null(admin_table) &&
            !is.null(admin_table$pausedb)) {
            db$ignore <- isTRUE(admin_table$pausedb)
            if (db$ignore) {
                message("Database operations are paused. Data will be saved to CSV.")
            } else {
                message("Database operations are active.")
            }
        } else if (!is.null(db)) {
            db$ignore <- FALSE
        }
    })

    # Page navigation ----

    check_required <- function(page) {
        all(vapply(page$required_questions, function(q) {
            is_visible <- is_question_visible(q)
            !is_visible || check_answer(q, input)
        }, logical(1)))
    }

    is_question_visible <- function(q) {
        results <- show_if_results()
        !q %in% names(results) || results[[q]]
    }

    # Determine which page is next, then update current_page_id() to it
    observe({
        lapply(pages, function(page) {
            observeEvent(input[[page$next_button_id]], {
                shiny::isolate({
                    current_page_id <- page$id
                    next_page_id <- get_default_next_page(page, page_ids, page_id_to_index)
                    next_page_id <- handle_skip_logic(input, skip_if, current_page_id, next_page_id)
                    if (!is.null(next_page_id) && check_required(page)) {
                        # Set the current page as the next page
                        current_page_id(next_page_id)

                        # Update the page time stamp
                        next_ts_id <- page_ts_ids[which(page_ids == next_page_id)]
                        all_data[[next_ts_id]] <- get_utc_timestamp()

                        # Update tracker of which fields changed
                        changed_fields(c(changed_fields(), next_ts_id))
                    } else if (!is.null(next_page_id)) {
                        shiny::showNotification(
                            "Please answer all required questions before proceeding.",
                            type = "error"
                        )
                    }
                })
            })
        })
    })

    # Observer to max out the progress bar when we reach the last page
    shiny::observe({
        page <- get_current_page()
        if (
            is.null(page$next_page_id) &
            (! page$id %in% c('pause-page', 'admin_login', 'admin_content'))
        ) {
            update_progress_bar(length(question_ids))
        }
    })

    # Ensure final update on session end
    shiny::onSessionEnded(function() {
        shiny::isolate({
            update_data(latest_data(), time_last = TRUE)
        })
    })

}

#' Define skip conditions for survey pages
#'
#' @description
#' This function is used to define conditions under which certain pages in the survey should be skipped.
#' It takes one or more formulas where the left-hand side is the condition and the right-hand side is the target page ID.
#'
#' @param ... One or more formulas defining skip conditions.
#'   The left-hand side of each formula should be a condition based on input values,
#'   and the right-hand side should be the ID of the page to skip to if the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the condition and the target page ID.
#'
#' @examples
#' \dontrun{
#' sd_skip_if(
#'   as.numeric(input$age < 18) ~ "underage_page",
#'   input$country != "USA" ~ "international_page"
#' )
#'}
#' @seealso \code{\link{sd_show_if}}
#'
#' @export
sd_skip_if <- function(...) {
    conditions <- parse_conditions(...)

    # Create a list in userData to store the skip_if targets
    shiny::isolate({
        session <- shiny::getDefaultReactiveDomain()
        if (is.null(session)) {
            stop("sd_skip_if must be called within a Shiny reactive context")
        }
        if (is.null(session$userData$skip_if)) {
            session$userData$skip_if <- list()
        }
        session$userData$skip_if$conditions <- conditions
        session$userData$skip_if$targets <- get_unique_targets(conditions)
    })
}

#' Define show conditions for survey questions
#'
#' @description
#' This function is used to define conditions under which certain questions in the survey should be shown.
#' It takes one or more formulas where the left-hand side is the condition and the right-hand side is the target question ID.
#' If called with no arguments, it will return NULL and set no conditions.
#'
#' @param ... One or more formulas defining show conditions.
#'   The left-hand side of each formula should be a condition based on input values,
#'   and the right-hand side should be the ID of the question to show if the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the condition and the target question ID.
#'   Returns NULL if no conditions are provided.
#'
#' @examples
#' \dontrun{
#' sd_show_if(
#'   input$has_pets == "yes" ~ "pet_details",
#'   input$employment == "employed" ~ "job_questions"
#' )
#' }
#'
#' @seealso \code{\link{sd_skip_if}}
#'
#' @export
sd_show_if <- function(...) {
  conditions <- parse_conditions(...)
  # Create a list in userData to store the show_if targets
  shiny::isolate({
    session <- shiny::getDefaultReactiveDomain()
    if (is.null(session)) {
      stop("sd_show_if must be called within a Shiny reactive context")
    }
    if (is.null(session$userData$show_if)) {
      session$userData$show_if <- list()
    }
    session$userData$show_if$conditions <- conditions
    session$userData$show_if$targets <- get_unique_targets(conditions)
  })
}

set_show_if_conditions <- function(show_if) {
    conditions <- show_if$conditions

    if (length(conditions) == 0) {
        return(shiny::reactive(list()))
    }

    # Group conditions by target
    grouped_conditions <- split(conditions, sapply(conditions, function(rule) rule$target))

    # Create a reactive expression for each group of conditions
    condition_reactives <- lapply(grouped_conditions, function(group) {
        shiny::reactive({
            results <- lapply(group, function(rule) {
                tryCatch({
                    evaluate_condition(rule)
                }, error = function(e) {
                    warning(sprintf(
                        "Error in show_if condition for target '%s', condition '%s': %s",
                        rule$target,
                        deparse(rule$condition),
                        conditionMessage(e)
                    ))
                    FALSE
                })
            })
            any(unlist(results))
        })
    })

    # Return a reactive that contains all condition results
    shiny::reactive({
        lapply(condition_reactives, function(r) r())
    })
}

get_unique_targets <- function(a) {
    return(unique(sapply(a, function(x) x$target)))
}

parse_conditions <- function(...) {
    conditions <- list(...)
    lapply(conditions, function(cond) {
        if (!inherits(cond, "formula")) {
            stop("Each condition must be a formula (condition ~ target)")
        }
        list(
            condition = cond[[2]],  # Left-hand side of the formula
            target = eval(cond[[3]])  # Right-hand side of the formula
        )
    })
}

evaluate_condition <- function(rule) {
    isTRUE(eval(
        rule$condition,
        envir = list(input = shiny::getDefaultReactiveDomain()$input)
    ))
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

get_initial_data <- function(
    session, session_id, time_start, all_ids, start_page_ts_id
) {
    # Initialize with static data
    data <- c(
        list(session_id = session_id, time_start = time_start),
        get_stored_vals(session)
    )

    # Initialize question & timestamp values
    for (id in all_ids) { data[[id]] <- "" }
    data[['time_start']] <- time_start
    data[[start_page_ts_id]] <- time_start
    data[['time_end']] <- ""

    return(data)
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

get_default_next_page <- function(page, page_ids, page_id_to_index) {
    if (is.null(page$next_page_id)) return(NULL)
    next_page_id <- page$next_page_id
    if (next_page_id == "") {
        index <- page_id_to_index[page$id] + 1
        if (index <= length(page_ids)) {
            return(page_ids[index])
        } else {
            return(NULL)
        }
    }
    return(next_page_id)
}

handle_skip_logic <- function(input, skip_if, current_page_id, next_page_id) {
    if (is.null(next_page_id) | is.null(skip_if)) { return(next_page_id) }

    # Loop through each skip logic condition
    conditions <- skip_if$conditions
    for (i in seq_along(conditions)) {
        rule <- conditions[[i]]

        # Evaluate the condition
        condition_result <- tryCatch({
            evaluate_condition(rule)
        }, error = function(e) {
            warning(sprintf(
                "Error in skip_if condition for target '%s': %s",
                rule$target, conditionMessage(e))
            )
            FALSE
        })

        # Check if the condition is met
        if (condition_result & (current_page_id != rule$target)) {
            return(rule$target)
        }
    }
    return(next_page_id)
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


# New admin enable section
admin_make_login <- function() {
    div(
        id = "admin-login",
        class = "sd-page",
        h2("Admin Login"),
        passwordInput("adminpw", "Password"),
        actionButton("admin_submit_pw", "Log in"),
        br(),
        br(),
        actionButton("admin_back_to_survey_login", "Back to Survey")
    )
}

make_pause_page <- function() {
    div(
        id = "pause-page",
        class = "sd-page",
        h2("Survey Paused"),
        p("The survey is currently paused. Please check back later.")
    )
}

admin_enable <- function(input, output, session, db, current_page_id, admin_state, start_page) {
    admin_table_state <- reactiveVal(NULL)

    # Function to get current admin state
    get_admin_state <- function() {
        if (is.null(db) || is.null(db$db) || is.null(db$table)) return(NULL)
        admin_table <- paste0(db$table, '_admin')
        tryCatch({
            result <- DBI::dbGetQuery(db$db, sprintf('SELECT "pausedb", "pausesurvey" FROM "%s" LIMIT 1', admin_table))
            if (nrow(result) == 0) {
                DBI::dbExecute(db$db, sprintf('INSERT INTO "%s" ("pausedb", "pausesurvey") VALUES (FALSE, FALSE)', admin_table))
                return(data.frame(PauseDB = FALSE, PauseSurvey = FALSE))
            }
            return(result)
        }, error = function(e) {
            message("Error fetching admin state: ", e$message)
            return(data.frame(PauseDB = FALSE, PauseSurvey = FALSE))
        })
    }

    # Initialize admin_table_state
    observe({
        admin_table_state(get_admin_state())
    })

    # Function to return to survey
    return_to_survey <- function() {
        current_state <- admin_table_state()
        admin_state("survey")
        if (current_state$pausesurvey) {
            # If survey is paused, go to pause page
            current_page_id("pause-page")
        } else {
            # If survey is not paused, go to first page
            current_page_id(pages[[1]]$id)
        }

        current_page_id(start_page)  # Set to the start page (usually first page)

        updateQueryString("?", mode = "replace")
    }

    # Observe for admin login attempt
    observeEvent(input$admin_submit_pw, {
        if (input$adminpw == Sys.getenv("SURVEYDOWN_PASSWORD")) {
            admin_state("content")

            # Populate the data table
            output$survey_data_table <- DT::renderDT({
                data <- DBI::dbReadTable(db$db, db$table)
                DT::datatable(data, options = list(scrollX = TRUE))
            })
        } else {
            showNotification("Incorrect password", type = "error")
        }
    })

    # Back to survey buttons
    observeEvent(input$admin_back_to_survey_login, {
        return_to_survey()
    })
    observeEvent(input$admin_back_to_survey_content, {
        return_to_survey()
    })

    # Function to update database
    update_db_state <- function(column, value) {
        admin_table <- paste0(db$table, '_admin')
        DBI::dbExecute(db$db, sprintf(
            'UPDATE "%s" SET "%s" = %s',
            admin_table, column, ifelse(value, "TRUE", "FALSE")
        ))
    }

    # Pause/Unpause Survey functionality
    observeEvent(input$admin_pause_survey, {
        update_db_state("pausesurvey", input$admin_pause_survey)
    })

    # Pause/Unpause DB functionality
    observeEvent(input$admin_pause_db, {
        update_db_state("pausedb", input$admin_pause_db)
    })

    # Download Data button functionality
    output$admin_download_data <- downloadHandler(
        filename = function() {
            paste0(db$table, "_", Sys.Date(), ".csv")
        },
        content = function(file) {
            data <- DBI::dbReadTable(db$db, db$table)
            write.csv(data, file, row.names = FALSE)
        }
    )

    #Creates the actual admin page
    admin_make_content <- reactive({
        current_state <- admin_table_state()
        div(
            id = "admin-content",
            class = "sd-page",
            h2("Admin Page"),
            shinyWidgets::switchInput(
                inputId = "admin_pause_survey",
                label = "Pause Survey",
                value = current_state$pausesurvey,
                labelWidth = "120px"
            ),
            shinyWidgets::switchInput(
                inputId = "admin_pause_db",
                label = "Pause DB",
                value = current_state$pausedb,
                labelWidth = "120px"
            ),
            downloadButton("admin_download_data", "Download Data"),
            actionButton("admin_back_to_survey_content", "Back to Survey"),
            hr(),
            h3("Survey Data"),
            DT::DTOutput("survey_data_table")
        )
    })

    # Return the modified admin_make_content function and admin_table_state
    return(list(
        admin_make_content = admin_make_content,
        admin_table_state = admin_table_state
    ))
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

#' Show the Saved Survey Password
#'
#' This function displays the password saved in the .Renviron file under the
#' SURVEYDOWN_PASSWORD variable. It includes a confirmation step to ensure
#' the user wants to display the password in the console. If no password is found,
#' it suggests using the sd_set_password() function to define a password.
#'
#' @return A character string containing the password if found and confirmed,
#'         or a message if no password is saved along with a suggestion to set one.
#'
#' @importFrom usethis ui_yeah ui_info ui_oops ui_todo
#'
#' @examples
#' \dontrun{
#'   sd_show_password()
#' }
#'
#' @export
sd_show_password <- function() {
  # Define the path to .Renviron file
  renviron_path <- file.path(getwd(), ".Renviron")

  # Check if .Renviron file exists
  if (!file.exists(renviron_path)) {
    usethis::ui_oops("No .Renviron file found. No password is saved.")
    usethis::ui_todo("Use sd_set_password() to define a password.")
    return(invisible(NULL))
  }

  # Read the content of .Renviron
  env_content <- readLines(renviron_path)

  # Find the line with SURVEYDOWN_PASSWORD
  password_line <- grep("^SURVEYDOWN_PASSWORD=", env_content, value = TRUE)

  if (length(password_line) == 0) {
    usethis::ui_oops("No password found in .Renviron file.")
    usethis::ui_todo("Use sd_set_password() to define a password.")
    return(invisible(NULL))
  }

  # Extract the password
  password <- sub("^SURVEYDOWN_PASSWORD=", "", password_line)

  # Confirm with the user
  if (usethis::ui_yeah("Are you sure you want to display your password in the console?")) {
    usethis::ui_info("Your saved password is: {password}")
  } else {
    usethis::ui_info("Password display cancelled.")
  }
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

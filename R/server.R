#' Server logic for a surveydown survey
#'
#' @description
#' This function defines the server-side logic for a 'shiny' application used in
#' surveydown. It handles various operations such as conditional display,
#' progress tracking, page navigation, database updates for survey responses,
#' and exit survey functionality.
#'
#' @param db A list containing database connection information created using
#'   `sd_database()` function. Defaults to `NULL`.
#' @param required_questions Vector of character strings. The IDs of questions
#'   that must be answered. Defaults to `NULL`.
#' @param all_questions_required Logical. If `TRUE`, all questions in the
#'   survey will be required. Defaults to `FALSE`.
#' @param start_page Character string. The ID of the page to start on.
#'   Defaults to `NULL`.
#' @param auto_scroll Logical. Whether to enable auto-scrolling to the next
#'   question after answering. Defaults to `FALSE`.
#' @param rate_survey Logical. If `TRUE`, shows a rating question when exiting
#'   the survey. If `FALSE`, shows a simple confirmation dialog.
#'   Defaults to `FALSE`.
#' @param language Set the language for the survey system messages. Include
#'   your own in a `translations.yml` file, or choose a built in one from
#'   the following list: English (`"en"`), German (`"de"`), Spanish (`"es"`),
#'   French (`"fr"`), Italian (`"it"`), Simplified Chinese (`"zh-CN"`).
#'   Defaults to `"en"`.
#' @param use_cookies Logical. If `TRUE`, enables cookie-based session management
#'   for storing and restoring survey progress. Defaults to `TRUE`.
#'
#' @details
#' The function performs the following tasks:
#' \itemize{
#'   \item Initializes variables and reactive values.
#'   \item Implements conditional display logic for questions.
#'   \item Tracks answered questions and updates the progress bar.
#'   \item Handles page navigation and skip logic.
#'   \item Manages required questions.
#'   \item Performs database operation.
#'   \item Controls auto-scrolling behavior based on the `auto_scroll` argument.
#'   \item Uses sweetalert for warning messages when required questions are not
#'         answered.
#'   \item Handles the exit survey process based on the `rate_survey` argument.
#' }
#'
#' @section Progress Bar:
#' The progress bar is updated based on the last answered question. It will jump
#' to the percentage corresponding to the last answered question and will never
#' decrease, even if earlier questions are answered later. The progress is
#' calculated as the ratio of the last answered question's index to the total
#' number of questions.
#'
#' @section Database Operations:
#' If `db` is provided, the function will update the database with survey
#' responses. If `db` is `NULL` (ignore mode), responses will be saved to a local
#' CSV file.
#'
#' @section Auto-Scrolling:
#' When `auto_scroll` is `TRUE`, the survey will automatically scroll to the
#' next question after the current question is answered. This behavior can be
#' disabled by setting `auto_scroll = FALSE`.
#'
#' @section Exit Survey:
#' When `rate_survey = TRUE`, the function will show a rating question when
#' the user attempts to exit the survey. When `FALSE`, it will show a simple
#' confirmation dialog. The rating, if provided, is saved with the survey data.
#'
#' @return
#' This function does not return a value; it sets up the server-side logic for
#' the 'shiny' application.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "basic_survey.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # sd_server() accepts these following parameters
#'     sd_server(
#'       db = NULL,
#'       required_questions = NULL,
#'       all_questions_required = FALSE,
#'       start_page = NULL,
#'       auto_scroll = FALSE,
#'       rate_survey = FALSE,
#'       language = "en",
#'       use_cookies = TRUE
#'     )
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso
#' `sd_database()`, `sd_ui()`
#'
#' @export
sd_server <- function(
        db                     = NULL,
        required_questions     = NULL,
        all_questions_required = FALSE,
        start_page             = NULL,
        auto_scroll            = FALSE,
        rate_survey            = FALSE,
        language               = "en",
        use_cookies            = TRUE
) {

    # 1. Initialize local variables ----

    # Get input, output, and session from the parent environment
    parent_env <- parent.frame()
    input      <- get("input", envir = parent_env)
    output     <- get("output", envir = parent_env)
    session    <- get("session", envir = parent_env)

    session$userData$db <- db

    # Tag start time
    time_start <- get_utc_timestamp()

    # Get any skip or show conditions
    show_if <- shiny::getDefaultReactiveDomain()$userData$show_if
    skip_if <- shiny::getDefaultReactiveDomain()$userData$skip_if

    # Run the configuration settings
    config <- run_config(
        required_questions,
        all_questions_required,
        start_page,
        skip_if,
        show_if,
        rate_survey,
        language
    )

    # Create local objects from config file
    pages              <- config$pages
    page_ids           <- config$page_ids
    question_ids       <- config$question_ids
    question_structure <- config$question_structure
    start_page         <- config$start_page
    question_required  <- config$question_required
    page_id_to_index   <- stats::setNames(seq_along(page_ids), page_ids)

    # Pre-compute timestamp IDs
    page_ts_ids      <- paste0("time_p_", page_ids)
    question_ts_ids  <- paste0("time_q_", question_ids)
    start_page_ts_id <- page_ts_ids[which(page_ids == start_page)]
    all_ids          <- c('time_end', question_ids, question_ts_ids, page_ts_ids)

    # Create current_page_id reactive value
    current_page_id <- shiny::reactiveVal(start_page)

    # Progress bar
    max_progress <- shiny::reactiveVal(0)
    last_answered_question <- shiny::reactiveVal(0)
    update_progress_bar <- function(index) {
        if (index > last_answered_question()) {
            last_answered_question(index)
            current_progress <- index / length(question_ids)
            max_progress(max(max_progress(), current_progress))
            session$sendCustomMessage("updateProgressBar", max_progress() * 100)
        }
    }

    # Initialize session handling and session_id
    session_id <- session$token
    session_id <- handle_sessions(session_id, db, session, input, time_start, start_page,
                                  current_page_id, question_ids, question_ts_ids,
                                  update_progress_bar, use_cookies)
    # Auto scroll
    session$sendCustomMessage("updateSurveydownConfig", list(autoScrollEnabled = auto_scroll))

    # Check if db is NULL (either blank or specified with ignore = TRUE)
    ignore_mode <- is.null(db)

    # Initialize translations list (from '_survey/translations.yml' file)
    translations <- get_translations()$translations

    # Keep-alive observer - this will be triggered every 60 seconds
    shiny::observeEvent(input$keepAlive, {
        cat("Session keep-alive at", format(Sys.time(), "%m/%d/%Y %H:%M:%S"), "\n")
    })

    # 2. show_if conditions ----

    # Reactive to store visibility status of all questions
    question_visibility <- shiny::reactiveVal(
        stats::setNames(rep(TRUE, length(question_ids)), question_ids)
    )

    # Observer to apply show_if conditions and update question_visibility
    shiny::observe({
        shiny::reactiveValuesToList(input)
        show_if_results <- set_show_if_conditions(show_if)()
        current_visibility <- question_visibility()
        for (target in names(show_if_results)) {
            current_visibility[target] <- show_if_results[[target]]
            if (show_if_results[[target]]) {
                shinyjs::show(paste0('container-', target))
            } else {
                shinyjs::hide(paste0('container-', target))
            }
        }
        question_visibility(current_visibility)
    })

    # 3. Update data ----

    update_data <- function(time_last = FALSE) {
        data_list <- latest_data()
        fields <- changed_fields()

        # Only update fields that have actually changed and have values
        if (length(fields) > 0) {
            # Filter out fields with empty values unless explicitly changed
            valid_fields <- character(0)
            for (field in fields) {
                if (!is.null(data_list[[field]]) && data_list[[field]] != "") {
                    valid_fields <- c(valid_fields, field)
                }
            }
            fields <- valid_fields
        } else {
            # On initial load or restoration, use all non-empty fields
            fields <- names(data_list)[sapply(data_list, function(x) !is.null(x) && x != "")]
        }

        if (time_last) {
            data_list[['time_end']] <- get_utc_timestamp()
            fields <- unique(c(fields, 'time_end'))
        }

        # Local data handling
        if (ignore_mode) {
            if (file.access('.', 2) == 0) {
                tryCatch({
                    # Read existing data
                    existing_data <- if (file.exists("preview_data.csv")) {
                        utils::read.csv("preview_data.csv", stringsAsFactors = FALSE)
                    } else {
                        data.frame()
                    }

                    # Convert current data_list to data frame
                    new_data <- as.data.frame(data_list, stringsAsFactors = FALSE)

                    # If there is existing data, update or append based on session_id
                    if (nrow(existing_data) > 0) {
                        # Find if this session_id already exists
                        session_idx <- which(existing_data$session_id == data_list$session_id)

                        if (length(session_idx) > 0) {
                            # Update existing session data
                            for (field in fields) {
                                if (field %in% names(existing_data)) {
                                    existing_data[session_idx, field] <- data_list[[field]]
                                } else {
                                    # Add new column with NAs, then update the specific row
                                    existing_data[[field]] <- NA
                                    existing_data[session_idx, field] <- data_list[[field]]
                                }
                            }
                            updated_data <- existing_data
                        } else {
                            # Ensure all columns from existing_data are in new_data
                            missing_cols <- setdiff(names(existing_data), names(new_data))
                            for (col in missing_cols) {
                                new_data[[col]] <- NA
                            }
                            # Ensure all columns from new_data are in existing_data
                            missing_cols <- setdiff(names(new_data), names(existing_data))
                            for (col in missing_cols) {
                                existing_data[[col]] <- NA
                            }
                            # Now both data frames should have the same columns
                            updated_data <- rbind(existing_data, new_data[names(existing_data)])
                        }
                    } else {
                        # If no existing data, use new data
                        updated_data <- new_data
                    }

                    # Write updated data back to file
                    utils::write.csv(
                        updated_data,
                        "preview_data.csv",
                        row.names = FALSE,
                        na = ""
                    )
                }, error = function(e) {
                    warning("Unable to write to preview_data.csv: ", e$message)
                    message("Error details: ", e$message)
                })
            } else {
                message("Running in a non-writable environment.")
            }
        } else {
            database_uploading(data_list, db$db, db$table, fields)
        }

        # Only reset changed fields that were actually processed
        changed_fields(setdiff(changed_fields(), fields))
    }

    # 4. Data tracking ----

    # First check and initialize table if needed
    if (!ignore_mode) {
        # Create a minimal initial data just for table creation
        min_initial_data <- list(
            session_id = character(0),
            time_start = character(0),
            time_end = character(0)
        )

        table_exists <- pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbExistsTable(conn, db$table)
        })
        if (!table_exists) {
            create_table(min_initial_data, db$db, db$table)
        }
    }

    # Now handle session and get proper initial data
    initial_data <- get_initial_data(
        session, session_id, time_start, all_ids, start_page_ts_id
    )
    all_data <- do.call(shiny::reactiveValues, initial_data)

    # Reactive expression that returns a list of the latest data
    latest_data <- shiny::reactive({
        # Convert reactiveValues to a regular list
        data <- shiny::reactiveValuesToList(all_data)

        # Ensure all elements are of length 1, use "" for empty or NULL values
        data <- lapply(data, function(x) {
            if (length(x) == 0 || is.null(x) || (is.na(x) && !is.character(x))) "" else as.character(x)[1]
        })

        data[names(data) != ""]
    })

    # Reactive value to track which fields have changed
    changed_fields <- shiny::reactiveVal(names(initial_data))

    # Expose all_data and changed_fields to session's userData for use by sd_store_value
    session$userData$all_data <- all_data
    session$userData$changed_fields <- changed_fields

    # Update checkpoint 1 - when session starts
    shiny::isolate({
        update_data()
    })

    # 5. Main question observers ----

    lapply(seq_along(question_ids), function(index) {
        local({
            local_id    <- question_ids[index]
            local_ts_id <- question_ts_ids[index]

            shiny::observeEvent(input[[local_id]], {
                # Tag event time and update value
                timestamp            <- get_utc_timestamp()
                value                <- input[[local_id]]
                formatted_value      <- format_question_value(value)
                all_data[[local_id]] <- formatted_value

                # Update timestamp and progress if interacted
                changed <- local_id
                if (!is.null(input[[paste0(local_id, "_interacted")]])) {
                    all_data[[local_ts_id]] <- timestamp
                    changed <- c(changed, local_ts_id)
                    update_progress_bar(index)
                }

                # Update tracker of which fields changed
                changed_fields(c(changed_fields(), changed))

                # Get question labels and values from question structure
                question_info  <- question_structure[[local_id]]
                label_question <- question_info$label
                options        <- question_info$options
                label_options  <- names(options)

                # For the selected value(s), get the corresponding label(s)
                if (length(options) == length(label_options)) {
                    names(options) <- label_options
                }
                label_option <- if (is.null(value) || length(value) == 0) {
                    ""
                } else {
                    options[options %in% value] |>
                        names() |>
                        paste(collapse = ", ")
                }

                # Store the values and labels in output
                output[[paste0(local_id, "_value")]] <- shiny::renderText({
                    formatted_value
                })
                output[[paste0(local_id, "_label_option")]] <- shiny::renderText({
                    label_option
                })
                output[[paste0(local_id, "_label_question")]] <- shiny::renderText({
                    label_question
                })
            },
            ignoreNULL = FALSE,
            ignoreInit = TRUE)
        })
    })

    # Observer to update cookies with answers
    shiny::observe({
        # Get current page ID
        page_id <- current_page_id()

        # Get all questions for current page
        page_questions <- names(input)[names(input) %in% question_ids]

        # Create answers list
        answers <- list()
        last_timestamp <- NULL
        max_index <- 0

        for (q_id in page_questions) {
            # Get question value
            val <- input[[q_id]]
            if (!is.null(val)) {
                answers[[q_id]] <- val

                # If question was interacted with, check its position
                if (!is.null(input[[paste0(q_id, "_interacted")]])) {
                    # Find this question's index in the overall sequence
                    current_index <- match(q_id, question_ids)
                    if (!is.na(current_index) && current_index > max_index) {
                        max_index <- current_index
                        last_timestamp <- list(
                            id = paste0("time_q_", q_id),
                            time = get_utc_timestamp()
                        )
                    }
                }
            }
        }

        # Send to client to update cookie
        if (length(answers) > 0 && !is.null(db)) {  # Only update cookies in db mode
            page_data <- list(
                answers = answers,
                last_timestamp = last_timestamp
            )
            session$sendCustomMessage("setAnswerData",
                                      list(pageId = page_id,
                                           pageData = page_data))
        }
    })

    # 6. Page rendering ----

    # Create reactive values for the start page ID
    get_current_page <- shiny::reactive({
        pages[[which(sapply(pages, function(p) p$id == current_page_id()))]]
    })

    # Render main page content when current page changes
    output$main <- shiny::renderUI({
        current_page <- get_current_page()
        shiny::tagList(
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

    # 7. Page navigation ----

    check_required <- function(page) {
        required_questions <- page$required_questions
        is_visible <- question_visibility()[required_questions]
        all(vapply(required_questions, function(q) {
            if (!is_visible[q]) return(TRUE)
            if (question_structure[[q]]$is_matrix) {
                all(sapply(question_structure[[q]]$row, function(r) check_answer(paste0(q, "_", r), input)))
            } else {
                check_answer(q, input)
            }
        }, logical(1)))
    }

    # Determine which page is next, then update current_page_id() to it
    shiny::observe({
        lapply(pages, function(page) {
            shiny::observeEvent(input[[page$next_button_id]], {
                shiny::isolate({
                    # Grab the time stamp of the page turn
                    timestamp <- get_utc_timestamp()

                    # Figure out page ids
                    current_page_id <- page$id
                    next_page_id <- get_default_next_page(page, page_ids, page_id_to_index)
                    next_page_id <- handle_skip_logic(input, skip_if, current_page_id, next_page_id)
                    if (!is.null(next_page_id) && check_required(page)) {
                        # Set the current page as the next page
                        current_page_id(next_page_id)

                        # Update the page time stamp
                        next_ts_id <- page_ts_ids[which(page_ids == next_page_id)]
                        all_data[[next_ts_id]] <- timestamp

                        # Save the current page to all_data
                        all_data[["current_page"]] <- next_page_id

                        # Update tracker of which fields changed
                        changed_fields(c(changed_fields(), next_ts_id, "current_page"))

                        # Update checkpoint 2 - upon going to the next page
                        update_data()
                    } else if (!is.null(next_page_id)) {
                        shinyWidgets::sendSweetAlert(
                            session = session,
                            title = translations[["warning"]],
                            text = translations[["required"]],
                            type = "warning"
                        )
                    }
                })
            })
        })
    })

    # Observer to max out the progress bar when we reach the last page
    shiny::observe({
        page <- get_current_page()
        if (is.null(page$next_page_id)) {
            update_progress_bar(length(question_ids))
        }
    })

    # 8. Survey rating and exit ----

    # Observer for the exit survey modal
    shiny::observeEvent(input$show_exit_modal, {
        if (rate_survey) {
            shiny::showModal(shiny::modalDialog(
                title = translations[["rating_title"]],
                sd_question(
                    type   = 'mc_buttons',
                    id     = 'survey_rating',
                    label  = glue::glue("{translations[['rating_text']]}:<br><small>({translations[['rating_scale']]})</small>"),
                    option = c(
                        "1" = "1",
                        "2" = "2",
                        "3" = "3",
                        "4" = "4",
                        "5" = "5"
                    )
                ),
                footer = shiny::tagList(
                    shiny::modalButton(translations[["cancel"]]),
                    shiny::actionButton("submit_rating", translations[["submit_exit"]])
                )
            ))
        } else {
            shiny::showModal(shiny::modalDialog(
                title = translations[["confirm_exit"]],
                translations[["sure_exit"]],
                footer = shiny::tagList(
                    shiny::modalButton(translations[["cancel"]]),
                    shiny::actionButton("confirm_exit", translations[["exit"]])
                )
            ))
        }
    })

    # Observer to handle the rating submission or exit confirmation
    shiny::observeEvent(input$submit_rating, {
        # Save the rating
        rating <- input$survey_rating
        all_data[['exit_survey_rating']] <- rating
        changed_fields(c(changed_fields(), 'exit_survey_rating'))
        # Update checkpoint 3 - when submitting rating
        shiny::isolate({
            update_data(time_last = TRUE)
        })
        # Close the modal and the window
        shiny::removeModal()
        session$sendCustomMessage("closeWindow", list())
    })

    shiny::observeEvent(input$confirm_exit, {
        # Update checkpoint 4 - when exiting survey
        shiny::isolate({
            update_data(time_last = TRUE)
        })
        # Close the modal and the window
        shiny::removeModal()
        session$sendCustomMessage("closeWindow", list())
    })

    # Update checkpoint 5 - when session ends
    shiny::onSessionEnded(function() {
        shiny::isolate({
            update_data(time_last = TRUE)
        })
    })
}

#' Define skip conditions for survey pages
#'
#' @description
#' This function is used to define conditions under which certain pages in the
#' survey should be skipped. It takes one or more formulas where the left-hand
#' side is the condition and the right-hand side is the target page ID.
#'
#' @param ... One or more formulas defining skip conditions.
#'   The left-hand side of each formula should be a condition based on input
#'   values, and the right-hand side should be the ID of the page to skip to if
#'   the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the
#' condition and the target page ID.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_skip_if.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # Skip to page based on input
#'     sd_skip_if(
#'       input$fav_fruit == "orange" ~ "orange_page",
#'       input$fav_fruit == "other" ~ "other_page"
#'     )
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso `sd_show_if()`
#'
#' @export
sd_skip_if <- function(...) {
    conditions <- parse_conditions(...)
    calling_env <- parent.frame()

    # Process each condition
    processed_conditions <- lapply(conditions, function(rule) {
        tryCatch({
            # Store the original condition for use with function calls
            rule$original_condition <- rule$condition

            # Extract any reactive expressions that might be called
            # We're storing the environment for potential evaluation later
            rule$calling_env <- calling_env

            # # For debugging
            # cat("Captured condition: ", deparse(rule$condition), "\n")

            return(rule)
        }, error = function(e) {
            warning("Error processing condition: ", e$message)
            return(rule)
        })
    })

    # Store in userData
    shiny::isolate({
        session <- shiny::getDefaultReactiveDomain()
        if (is.null(session)) {
            stop("sd_skip_if must be called within a Shiny reactive context")
        }
        if (is.null(session$userData$skip_if)) {
            session$userData$skip_if <- list()
        }
        session$userData$skip_if$conditions <- processed_conditions
        session$userData$skip_if$targets <- get_unique_targets(processed_conditions)
    })
}

#' Define show conditions for survey questions
#'
#' @description
#' This function is used to define conditions under which certain questions in the survey should be shown.
#' It takes one or more formulas where the left-hand side is the condition and the right-hand side is the target question ID.
#' If called with no arguments, it will return `NULL` and set no conditions.
#'
#' @param ... One or more formulas defining show conditions.
#'   The left-hand side of each formula should be a condition based on input values,
#'   and the right-hand side should be the ID of the question to show if the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the condition and the target question ID.
#'   Returns `NULL` if no conditions are provided.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_show_if.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     sd_show_if(
#'       # If "Other" is chosen, show the conditional question
#'       input$fav_fruit == "other" ~ "fav_fruit_other"
#'     )
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso `sd_skip_if()`
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

#' Set password for surveydown survey
#'
#' This function sets your surveydown password, which is used to access
#' the 'PostgreSQL' data (e.g. Supabase). The password is saved in a `.Renviron`
#' file and adds `.Renviron` to `.gitignore`.
#'
#' @param password Character string. The password to be set for the database
#'   connection.
#'
#' @details The function performs the following actions:
#'   1. Creates a `.Renviron` file in the root directory if it doesn't exist.
#'   2. Adds or updates the `SURVEYDOWN_PASSWORD` entry in the `.Renviron` file.
#'   3. Adds `.Renviron` to `.gitignore` if it's not already there.
#'
#' @return None. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#'   # Set a temporary password for demonstration
#'   temp_password <- paste0(sample(letters, 10, replace = TRUE), collapse = "")
#'
#'   # Set the password
#'   sd_set_password(temp_password)
#'
#'   # After restarting R, verify the password was set
#'   cat("Password is :", Sys.getenv('SURVEYDOWN_PASSWORD'))
#' }
#'
#' @export
sd_set_password <- function(password) {

    # v0.8.0
    .Deprecated("sd_db_config")

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
#' This function displays the password saved in the `.Renviron` file under the
#' `SURVEYDOWN_PASSWORD` variable. It includes a confirmation step to ensure
#' the user wants to display the password in the console. If no password is
#' found, it suggests using the `sd_set_password()` function to define a
#' password.
#'
#' @return A character string containing the password if found and confirmed,
#'   or a message if no password is saved along with a suggestion to set one.
#'
#' @examples
#' \dontrun{
#'   surveydown::sd_show_password()
#' }
#'
#' @export
sd_show_password <- function() {

    # v0.8.0
    .Deprecated("sd_db_config")

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

#' Store a value in the survey data
#'
#' This function allows storing additional values to be included in the survey
#' data, such as respondent IDs or other metadata.
#'
#' @param value The value to be stored. This can be any R object that can be
#'   coerced to a character string.
#' @param id (Optional) Character string. The id (name) of the value in the
#'   data. If not provided, the name of the `value` variable will be used.
#'
#' @return `NULL` (invisibly)
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_ui.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "basic_survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # Create a respondent ID to store
#'     respondentID <- 42
#'
#'     # Store the respondentID
#'     sd_store_value(respondentID)
#'
#'     # Store the respondentID as the variable "respID"
#'     sd_store_value(respondentID, "respID")
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
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

        # Initialize stored_values if it doesn't exist
        if (is.null(session$userData$stored_values)) {
            session$userData$stored_values <- list()
        }

        formatted_value <- format_question_value(value)
        session$userData$stored_values[[id]] <- formatted_value

        # Make value accessible in the UI
        output <- shiny::getDefaultReactiveDomain()$output
        output[[paste0(id, "_value")]] <- shiny::renderText({ formatted_value })

        # Get access to all_data and update it if available
        # This allows the stored value to be accessible through sd_output
        if (!is.null(session$userData$all_data)) {
            session$userData$all_data[[id]] <- formatted_value
            # Add to changed fields to trigger database update
            if (!is.null(session$userData$changed_fields)) {
                current_fields <- session$userData$changed_fields()
                session$userData$changed_fields(c(current_fields, id))
            }
        }
    })

    invisible(NULL)
}

#' Create a reactive value that is also stored in survey data
#'
#' This function creates a reactive value similar to Shiny's reactive() function,
#' but also automatically stores the calculated value in the survey data.
#'
#' @param id Character string. The id (name) of the value to be stored in the data.
#' @param expr An expression that calculates a value based on inputs
#' @param blank_na Logical. If TRUE, NA values are converted to empty strings. Default is TRUE.
#'
#' @return A reactive expression that can be called like a function
#'
#' @examples
#' \dontrun{
#' # In your server function:
#' product <- sd_reactive("product", {
#'   input$first_number * input$second_number
#' })
#'
#' # Use the reactive value elsewhere
#' output$result <- renderText({
#'   paste("The product is:", product())
#' })
#'
#' # In your survey.qmd file, display the value:
#' The product is: `r sd_output("product", type = "value")`.
#' }
#'
#' @export
sd_reactive <- function(id, expr, blank_na = TRUE) {
    # Validate id
    if (!is.character(id) || length(id) != 1) {
        stop("'id' must be a single character string")
    }

    # Capture the expression and its environment
    expr_call <- substitute(expr)
    expr_env <- parent.frame()

    # Create a reactive expression
    reactive_expr <- shiny::reactive({
        # Get current session
        session <- shiny::getDefaultReactiveDomain()
        if (is.null(session)) {
            warning("sd_reactive() must be called within a Shiny reactive context")
            return(NULL)
        }

        # Evaluate the expression in its original environment
        # This ensures that 'input' and other objects are found
        result <- eval(expr_call, envir = expr_env)

        # Store the value in the survey data
        if (is.null(result) || (length(result) == 1 && is.na(result))) {
            sd_store_value("", id)
            return(if (blank_na) "" else result)
        } else {
            sd_store_value(result, id)
            return(result)
        }
    })

    # Auto-trigger the evaluation once to ensure value is available
    # This creates an observer that will run once when the session initializes
    shiny::observeEvent(shiny::getDefaultReactiveDomain()$clientData, {
        # This forces the reactive to run once right away
        reactive_expr()
    }, once = TRUE)

    return(reactive_expr)
}

#' Create a copy of a value
#'
#' This function creates a copy of an input value and makes it available as a
#' new output. The new output can then be displayed using `sd_output()`.
#'
#' @param id Character string. The ID of the input value to copy.
#' @param id_copy Character string. The ID for the new copy (must be different
#'   from `id`).
#'
#' @return `NULL` invisibly. This function is called for its side effects.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_ui.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "sd_copy_value.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # Make a copy of the "name" variable to call its value a second time
#'     sd_copy_value(id = "name", id_copy = "name_copy")
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso `sd_output()` for displaying the copied value
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

#' Check if a question is answered
#'
#' This function checks if a given question has been answered by the user.
#' For matrix questions, it checks if all sub-questions (rows) are answered.
#'
#' @param question_id The ID of the question to check.
#' @return A logical value: `TRUE` if the question is answered, `FALSE`
#' otherwise.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_is_answered.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     sd_show_if(
#'       # If "apple_text" is answered, show the conditional question
#'       sd_is_answered("apple_text") ~ "other_fruit"
#'     )
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @export
sd_is_answered <- function(question_id) {
    # Get the Shiny session
    session <- shiny::getDefaultReactiveDomain()

    if (is.null(session)) {
        stop("sd_is_answered() must be called from within a Shiny reactive context")
    }

    # Access the input object from the session
    input <- session$input

    # Check if it's a matrix question (ends with a number)
    if (!grepl("_\\d+$", question_id)) {
        # It's potentially a matrix question, check all sub-questions
        sub_questions <- grep(paste0("^", question_id, "_"), names(input), value = TRUE)

        if (length(sub_questions) > 0) {
            # It's confirmed to be a matrix question
            return(all(sapply(sub_questions, function(sq) !is.null(input[[sq]]) && nzchar(input[[sq]]))))
        }
    }

    # For non-matrix questions or individual sub-questions
    if (is.null(input[[question_id]])) return(FALSE)

    if (is.list(input[[question_id]])) {
        # For questions that can have multiple answers (e.g., checkboxes)
        return(length(input[[question_id]]) > 0 && any(nzchar(unlist(input[[question_id]]))))
    } else {
        # For single-answer questions
        return(!is.null(input[[question_id]]) && nzchar(input[[question_id]]))
    }
}

# Helper functions ----

set_show_if_conditions <- function(show_if) {
    if (is.null(show_if) || length(show_if$conditions) == 0) {
        return(shiny::reactive(list()))
    }
    shiny::reactive({
        results <- lapply(show_if$conditions, function(rule) {
            result <- tryCatch({
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
            stats::setNames(list(result), rule$target)
        })
        do.call(c, results)
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

# evaluate_condition <- function(rule) {
#     isTRUE(eval(
#         rule$condition,
#         envir = list(input = shiny::getDefaultReactiveDomain()$input)
#     ))
# }

evaluate_condition <- function(rule) {
    # Create a safe evaluation environment that can handle reactive expressions
    session <- shiny::getDefaultReactiveDomain()
    eval_env <- list(input = session$input)

    # Try to evaluate using the original condition (which might have function calls)
    tryCatch({
        # Use both the original calling environment and the input
        result <- eval(rule$original_condition,
                       envir = rule$calling_env,
                       enclos = environment())
        return(isTRUE(result))
    }, error = function(e) {
        warning("Error in condition evaluation: ", e$message)
        return(FALSE)
    })
}

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

get_local_data <- function() {
    if (file.exists("preview_data.csv")) {
        tryCatch({
            return(utils::read.csv("preview_data.csv", stringsAsFactors = FALSE))
        }, error = function(e) {
            warning("Error reading preview_data.csv: ", e$message)
            return(NULL)
        })
    }
    return(NULL)
}

get_cookie_data <- function(session, current_page_id) {
    # Get stored answer data from input
    answer_data <- session$input$stored_answer_data

    if (is.null(answer_data) || !length(answer_data)) {
        return(NULL)
    }

    # Extract data for current page
    page_data <- answer_data[[current_page_id]]
    if (is.null(page_data)) {
        return(NULL)
    }

    # Return the full page data structure including answers and timestamps
    return(page_data)
}

restore_current_page_values <- function(restore_data, session, page_filter = NULL) {
    for (col in names(restore_data)) {
        # Skip special columns
        if (!col %in% c("session_id", "current_page", "time_start", "time_end")) {
            val <- restore_data[[col]]
            if (!is.null(val) && !is.na(val) && val != "") {
                session$sendInputMessage(col, list(value = val, priority = "event"))
            }
        }
    }
}

handle_data_restoration <- function(session_id, db, session, current_page_id, start_page,
                                    question_ids, question_ts_ids, progress_updater) {
    if (is.null(session_id)) return(NULL)

    # Get data based on source
    if (!is.null(db)) {
        all_data <- sd_get_data(db)
    } else {
        all_data <- get_local_data()
    }

    # If no data available, return NULL
    if (is.null(all_data)) return(NULL)

    restore_data <- all_data[all_data$session_id == session_id, ]

    if (nrow(restore_data) == 0) return(NULL)

    shiny::isolate({
        # 1. Restore page state (using restore_data)
        if ("current_page" %in% names(restore_data)) {
            restored_page <- restore_data[["current_page"]]
            if (!is.null(restored_page) && !is.na(restored_page) && nchar(restored_page) > 0) {
                current_page_id(restored_page)
            } else {
                current_page_id(start_page)
            }
        } else {
            current_page_id(start_page)
        }

        # Get cookie data after page state is set
        answer_data <- NULL
        if (!is.null(db)) {
            answer_data <- get_cookie_data(session, current_page_id())
        }

        # 2. Find the last answered question for progress bar
        last_index <- 0
        if (!is.null(db) && !is.null(answer_data) && !is.null(answer_data$last_timestamp)) {
            # Use last timestamp from cookie data in DB mode
            last_ts_id <- answer_data$last_timestamp$id
            # Find the index of this timestamp ID in our question_ts_ids
            last_index <- match(last_ts_id, question_ts_ids)
            if (is.na(last_index)) last_index <- 0
        } else {
            # Use restore_data for local CSV mode
            for (i in seq_along(question_ids)) {
                ts_id <- question_ts_ids[i]
                if (ts_id %in% names(restore_data)) {
                    ts_val <- restore_data[[ts_id]]
                    if (length(ts_val) == 1 && !is.null(ts_val) && !is.na(ts_val) && ts_val != "") {
                        last_index <- i
                    }
                }
            }
        }

        if (last_index > 0) {
            progress_updater(last_index)
        }

        # 3. Restore question values
        if (!is.null(db) && !is.null(answer_data) && !is.null(answer_data$answers)) {
            # Use answer data from cookies for current page
            for (col in names(answer_data$answers)) {
                val <- answer_data$answers[[col]]
                if (!is.null(val) && !identical(val, "")) {
                    session$sendInputMessage(col, list(value = val, priority = "event"))
                }
            }
        } else {
            # Fall back to restore_data
            restore_current_page_values(restore_data, session)
        }
    })
    return(restore_data)
}

handle_sessions <- function(session_id, db = NULL, session, input, time_start,
                            start_page, current_page_id, question_ids,
                            question_ts_ids, progress_updater, use_cookies = TRUE) {
    # Check 1: if db is null, don't use cookies
    if (is.null(db)) {
        use_cookies <- FALSE
    }

    # Check 2: Cookies enabled?
    if (!use_cookies) {
        return(session_id)
    }

    # Create a variable to store the final ID
    final_session_id <- session_id

    # Do the cookie check synchronously in a reactive context
    shiny::isolate({

        # Check 3: Cookie exists and is valid?
        stored_id <- shiny::reactiveValuesToList(input)$stored_session_id
        if (!is.null(stored_id) && nchar(stored_id) > 0 &&
            # Check 4: Either DB connection exists or preview_data.csv is writable
            (!is.null(db) || (file.exists("preview_data.csv") && file.access("preview_data.csv", 2) == 0))) {

            # Check 5: Session exists in DB or preview data?
            restore_data <- handle_data_restoration(
                stored_id, db, session, current_page_id,
                start_page, question_ids, question_ts_ids,
                progress_updater
            )

            if (!is.null(restore_data)) {

                # All checks passed - use stored session
                final_session_id <- stored_id
                session$sendCustomMessage("setCookie", list(sessionId = stored_id))
            } else {

                # Session not in DB - use new session
                session$sendCustomMessage("setCookie", list(sessionId = session_id))
            }
        } else {

            # No cookie or no DB connection - use new session
            session$sendCustomMessage("setCookie", list(sessionId = session_id))
        }
    })

    return(final_session_id)
}

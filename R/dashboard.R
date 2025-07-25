#' Launch Survey Dashboard
#'
#' Opens an interactive dashboard to view and analyze survey responses
#' using a Shiny dashboard interface.
#'
#' @description
#' This function creates a comprehensive dashboard for survey data analysis
#' with two main tabs:
#' - Dashboard: Displays survey statistics, response trends, and a full
#'   response data table
#' - Settings: Provides interface for updating database connection settings
#'
#' @details
#' The dashboard offers the following features:
#' - Summary value boxes showing total responses, daily average,
#'   completion rate, and average rating
#' - Response trend plot with daily and cumulative responses
#' - Downloadable survey responses data table
#' - Database connection configuration and testing
#'
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"auto"`. Options are:
#'   - `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if GSSAPI negotiation fails
#'   - `"prefer"`: Uses GSSAPI encryption if available, plain connection otherwise
#'   - `"disable"`: Disables GSSAPI encryption entirely
#'   Set to `"disable"` if you're having connection issues on a secure connection like a VPN.
#'
#' @return
#' Launches a Shiny application with the survey dashboard.
#' The function does not return a value; it is called for its side effects
#' of opening the dashboard interface.
#'
#' @examples
#' \dontrun{
#' # Launch the survey dashboard with default settings
#' sd_dashboard()
#'
#' # Launch with disabled GSS encryption (for VPN connections)
#' sd_dashboard(gssencmode = "disable")
#' }
#'
#' @export
sd_dashboard <- function(gssencmode = "auto") {
    if (file.exists(".env")) {
        dotenv::load_dot_env(".env")
    }

    ui <- bslib::page_fillable(
        bslib::page_navbar(
            title = "Survey Dashboard",
            bslib::nav_item(
                bslib::input_dark_mode(id = "dark_mode")
            )
        ),

        shiny::tags$head(
            shiny::tags$script(
                "
            function myFunction() {
                var x = document.getElementById('password');
                if (x.type === 'password') {
                    x.type = 'text';
                    document.getElementById('toggle_password').innerText = 'Hide';
                } else {
                    x.type = 'password';
                    document.getElementById('toggle_password').innerText = 'Show';
                }
            }
        "
            )
        ),

        bslib::navset_pill_list(
            widths = c(2, 10),

            #Dashboard Page
            bslib::nav_panel(
                title = "Dashboard",

                #DropDown Table Selection
                shiny::selectInput(
                    "table_select",
                    "Choose a database table to view:",
                    choices = c("Loading..." = ""),
                    width = "20%",
                    selectize = FALSE
                ),

                #Basic Information
                bslib::layout_column_wrap(
                    width = 1 / 3,
                    heights_equal = "row",
                    bslib::value_box(
                        title = "Total Responses",
                        value = shiny::textOutput("total_responses"),
                        showcase = shiny::icon("table")
                    ),
                    bslib::value_box(
                        title = "Daily Average",
                        value = shiny::textOutput("daily_average"),
                        showcase = shiny::icon("chart-line")
                    ),
                    bslib::value_box(
                        title = "Completion Rate",
                        value = shiny::textOutput("completion_rate"),
                        showcase = shiny::icon("check-circle")
                    )
                ),
                bslib::layout_column_wrap(
                    width = 1 / 2,
                    bslib::card(
                        full_screen = TRUE,
                        bslib::card_header("Cumulative Responses"),
                        shiny::plotOutput("cumulative_trend", height = "300px")
                    ),
                    bslib::card(
                        full_screen = TRUE,
                        bslib::card_header("Daily Responses"),
                        shiny::plotOutput("daily_trend", height = "300px")
                    )
                ),
                bslib::card(
                    full_screen = TRUE,
                    bslib::card_header(
                        class = "d-flex justify-content-between align-items-center",
                        "Survey Responses",
                        shiny::downloadButton(
                            "download_survey_data",
                            "Download CSV",
                            class = "btn-sm btn-secondary"
                        )
                    ),
                    DT::dataTableOutput("survey_data_table")
                )
            ),

            # Settings Page
            bslib::nav_panel(
                title = "DB Config",
                bslib::card(
                    bslib::card_header("Current Database Settings"),
                    bslib::card_body(
                        shiny::tags$div(
                            style = "display: grid; grid-template-columns: auto 1fr; gap: 10px; align-items: center;",
                            shiny::tags$strong("Host:"),
                            shiny::span(Sys.getenv("SD_HOST", "Not set")),
                            shiny::tags$strong("Port:"),
                            shiny::span(Sys.getenv("SD_PORT", "Not set")),
                            shiny::tags$strong("Database:"),
                            shiny::span(Sys.getenv("SD_DBNAME", "Not set")),
                            shiny::tags$strong("User:"),
                            shiny::span(Sys.getenv("SD_USER", "Not set")),
                            shiny::tags$strong("Default Table:"),
                            shiny::span(Sys.getenv("SD_TABLE", "Not set"))
                        )
                    )
                ),
                bslib::card(
                    bslib::card_header("Update Database Connection"),
                    bslib::card_body(
                        shiny::textInput(
                            "host",
                            "Host:",
                            value = Sys.getenv("SD_HOST", "localhost")
                        ),
                        shiny::textInput(
                            "port",
                            "Port:",
                            value = Sys.getenv("SD_PORT", "5432")
                        ),
                        shiny::textInput(
                            "dbname",
                            "Database Name:",
                            value = Sys.getenv("SD_DBNAME", "postgres")
                        ),
                        shiny::textInput(
                            "user",
                            "User:",
                            value = Sys.getenv("SD_USER", "username")
                        ),
                        shiny::div(
                            id = "password-container",
                            style = "position: relative;",
                            shiny::div(
                                style = "display: flex; align-items: center;",
                                shiny::passwordInput(
                                    "password",
                                    "Password:",
                                    value = Sys.getenv("SD_PASSWORD", "")
                                ),
                                shiny::div(
                                    style = "margin-left: 10px; margin-top: 1em;",
                                    shiny::actionButton(
                                        "toggle_password",
                                        "Show",
                                        class = "btn-sm btn-secondary",
                                        onclick = "myFunction()"
                                    )
                                )
                            )
                        ),
                        shiny::textInput(
                            "default_table",
                            "Development Table:",
                            value = Sys.getenv("SD_TABLE", "responses")
                        ),
                        shiny::div(
                            style = "margin-top: 20px;",
                            shiny::actionButton(
                                "test_connection",
                                "Test Connection",
                                class = "btn-primary",
                                style = "width: 300px;"
                            )
                        ),
                        shiny::textOutput("connection_status")
                    )
                )
            )
        )
    )

    #Server Block

    server <- function(input, output, session) {
        # Reactive values for connection status and database
        rv <- shiny::reactiveValues(
            connection_status = FALSE,
            current_db = NULL
        )

        # Initial connection check
        attempt_connection <- function(
            config = NULL,
            return_details = FALSE,
            gss_mode = gssencmode
        ) {
            # Helper function to try connection with specific gssencmode
            try_connection <- function(gss_mode) {
                if (is.null(config)) {
                    # Use default connection from .env with the specified gssencmode
                    sd_db_connect(gssencmode = gss_mode)
                } else {
                    # Use provided config with the specified gssencmode
                    pool <- try_db_connection(config, gss_mode)
                    list(db = pool)
                }
            }

            # First attempt with the specified gssencmode
            tryCatch(
                {
                    db <- try_connection(gss_mode)

                    if (!is.null(db)) {
                        rv$connection_status <- TRUE
                        rv$current_db <- db
                        if (return_details) {
                            return(list(
                                success = TRUE,
                                fallback_used = FALSE,
                                message = "Connection successful"
                            ))
                        } else {
                            return(TRUE)
                        }
                    }
                    if (return_details) {
                        return(list(
                            success = FALSE,
                            fallback_used = FALSE,
                            message = "Connection failed"
                        ))
                    } else {
                        return(FALSE)
                    }
                },
                error = function(e) {
                    error_msg <- as.character(e$message)

                    # Only try fallback if we're in "auto" mode and it's a GSSAPI error
                    if (is_gssapi_error(error_msg) && gss_mode == "auto") {
                        message(
                            "GSSAPI negotiation failed, retrying with gssencmode='disable'..."
                        )

                        tryCatch(
                            {
                                db <- try_connection("disable")

                                if (!is.null(db)) {
                                    rv$connection_status <- TRUE
                                    rv$current_db <- db
                                    message(
                                        "Connection successful with gssencmode='disable'"
                                    )
                                    if (return_details) {
                                        return(list(
                                            success = TRUE,
                                            fallback_used = TRUE,
                                            message = "Connection successful (GSSAPI disabled due to negotiation error)"
                                        ))
                                    } else {
                                        return(TRUE)
                                    }
                                }
                                if (return_details) {
                                    return(list(
                                        success = FALSE,
                                        fallback_used = TRUE,
                                        message = "Connection failed with both GSSAPI modes"
                                    ))
                                } else {
                                    return(FALSE)
                                }
                            },
                            error = function(e2) {
                                # Both attempts failed
                                rv$connection_status <- FALSE
                                rv$current_db <- NULL
                                warning(
                                    "Connection failed with both gssencmode='prefer' and 'disable': ",
                                    e2$message
                                )
                                if (return_details) {
                                    return(list(
                                        success = FALSE,
                                        fallback_used = TRUE,
                                        message = paste(
                                            "Connection failed with both GSSAPI modes:",
                                            e2$message
                                        )
                                    ))
                                } else {
                                    return(FALSE)
                                }
                            }
                        )
                    } else {
                        # Not a GSSAPI error or already using "disable", just fail normally
                        rv$connection_status <- FALSE
                        rv$current_db <- NULL
                        if (return_details) {
                            return(list(
                                success = FALSE,
                                fallback_used = FALSE,
                                message = error_msg
                            ))
                        } else {
                            return(FALSE)
                        }
                    }
                }
            )
        }

        # Initial connection attempt
        shiny::observe({
            attempt_connection()
        })

        shiny::observe({
            if (rv$connection_status && !is.null(rv$current_db)) {
                tryCatch(
                    {
                        tables <- pool::poolWithTransaction(
                            rv$current_db$db,
                            function(conn) {
                                all_tables <- DBI::dbListTables(conn)
                                all_tables[!grepl("^pg_", all_tables)]
                            }
                        )

                        # Set default table as first choice if available
                        default_table <- Sys.getenv("SD_TABLE", "")
                        if (default_table %in% tables) {
                            tables <- c(
                                default_table,
                                setdiff(tables, default_table)
                            )
                        }

                        shiny::updateSelectInput(
                            session,
                            "table_select",
                            choices = if (length(tables) > 0) {
                                tables
                            } else {
                                c("No tables found" = "")
                            }
                        )
                    },
                    error = function(e) {
                        shiny::updateSelectInput(
                            session,
                            "table_select",
                            choices = c("Connection error" = "")
                        )
                    }
                )
            } else {
                shiny::updateSelectInput(
                    session,
                    "table_select",
                    choices = c("No connection" = "")
                )
            }
        })

        # Handle test connection button
        shiny::observeEvent(
            input$test_connection,
            {
                # Close existing connection
                if (!is.null(rv$current_db) && !is.null(rv$current_db$db)) {
                    tryCatch(
                        {
                            pool::poolClose(rv$current_db$db)
                            rv$current_db <- NULL
                            rv$connection_status <- FALSE
                        },
                        error = function(e) {
                            warning("Error closing connection: ", e$message)
                        }
                    )
                }

                # Test new connection
                config <- list(
                    host = input$host,
                    port = input$port,
                    dbname = input$dbname,
                    user = input$user,
                    password = input$password
                )

                success <- attempt_connection(config)

                if (success) {
                    # Save to .env file
                    env_content <- paste(
                        "# Database connection settings for surveydown",
                        sprintf("SD_HOST=%s", input$host),
                        sprintf("SD_PORT=%s", input$port),
                        sprintf("SD_DBNAME=%s", input$dbname),
                        sprintf("SD_USER=%s", input$user),
                        sprintf("SD_PASSWORD=%s", input$password),
                        sprintf("SD_TABLE=%s", input$default_table),
                        sep = "\n"
                    )
                    writeLines(env_content, ".env")

                    # Update .gitignore
                    if (file.exists(".gitignore")) {
                        gitignore_content <- readLines(".gitignore")
                        if (!".env" %in% gitignore_content) {
                            write("\n.env", ".gitignore", append = TRUE)
                        }
                    } else {
                        write(".env", ".gitignore")
                    }

                    output$connection_status <- shiny::renderText(
                        "Connection successful & Parameters saved to .env file."
                    )
                } else {
                    output$connection_status <- shiny::renderText(
                        "Connection failed. Please check your settings."
                    )
                }
            },
            ignoreInit = TRUE
        )

        # Reactive survey data with error handling
        survey_data <- shiny::reactive({
            shiny::req(rv$connection_status)
            shiny::req(input$table_select)
            shiny::req(rv$current_db)

            tryCatch(
                {
                    data <- pool::poolWithTransaction(
                        rv$current_db$db,
                        function(conn) {
                            DBI::dbGetQuery(
                                conn,
                                sprintf(
                                    'SELECT * FROM "%s"',
                                    input$table_select
                                )
                            )
                        }
                    )
                    return(data)
                },
                error = function(e) {
                    warning("Error fetching survey data: ", e$message)
                    return(NULL)
                }
            )
        })

        # Downloadable CSV of survey data
        output$download_survey_data <- shiny::downloadHandler(
            filename = function() {
                paste0(input$table_select, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                data <- survey_data()
                utils::write.csv(data, file, row.names = FALSE)
            }
        )

        # Value Boxes
        output$total_responses <- shiny::renderText({
            shiny::req(survey_data())
            nrow(survey_data())
        })

        output$daily_average <- shiny::renderText({
            shiny::req(survey_data())
            data <- survey_data()
            start_times <- as.POSIXct(
                data$time_start,
                format = "%Y-%m-%d %H:%M:%S"
            )
            first_response <- min(start_times, na.rm = TRUE)
            last_response <- max(start_times, na.rm = TRUE)
            duration_days <- max(
                as.numeric(difftime(
                    last_response,
                    first_response,
                    units = "days"
                )),
                1
            )
            round(nrow(data) / duration_days, 1)
        })

        output$completion_rate <- shiny::renderText({
            shiny::req(survey_data())
            data <- survey_data()
            total_responses <- nrow(data)
            completed_responses <- sum(
                !is.na(data$time_end) & data$time_end != "",
                na.rm = TRUE
            )
            if (total_responses > 0) {
                sprintf("%.1f%%", (completed_responses / total_responses) * 100)
            } else {
                "0.0%"
            }
        })

        # Response Trend Plot
        output$cumulative_trend <- shiny::renderPlot(
            {
                data <- survey_data()
                # Get theme colors based on dark mode
                bg_color <- if (input$dark_mode == "dark") {
                    "#2c3e50"
                } else {
                    "#ffffff"
                }
                text_color <- if (input$dark_mode == "dark") {
                    "#ffffff"
                } else {
                    "#1a2226"
                }
                grid_color <- if (input$dark_mode == "dark") {
                    "gray30"
                } else {
                    "gray80"
                }
                line_color <- if (input$dark_mode == "dark") {
                    "#3498db"
                } else {
                    "#0062cc"
                }

                if (
                    is.null(data) ||
                        nrow(data) == 0 ||
                        !("time_start" %in% names(data))
                ) {
                    graphics::par(bg = bg_color, fg = text_color)
                    graphics::plot.new()
                    graphics::text(
                        0.5,
                        0.5,
                        "No data available to display",
                        col = text_color,
                        cex = 1.2
                    )
                    return()
                }

                dates <- try(as.Date(data$time_start))
                if (
                    inherits(dates, "try-error") ||
                        length(dates) == 0 ||
                        all(is.na(dates))
                ) {
                    graphics::par(bg = bg_color, fg = text_color)
                    graphics::plot.new()
                    graphics::text(
                        0.5,
                        0.5,
                        "Unable to process date data",
                        col = text_color,
                        cex = 1.2
                    )
                    return()
                }

                dates <- dates[!is.na(dates)]
                daily_counts <- table(dates)
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range
                all_counts[names(daily_counts)] <- daily_counts
                cumulative_responses <- cumsum(all_counts)

                # Plot setup with theme
                graphics::par(
                    bg = bg_color,
                    fg = text_color,
                    col.axis = text_color,
                    col.lab = text_color,
                    mar = c(4, 4, 2, 2)
                )

                # Add grid first
                graphics::plot(
                    date_range,
                    cumulative_responses,
                    type = "n",
                    xlab = "Date",
                    ylab = "Cumulative Responses"
                )
                graphics::grid(col = grid_color, lty = "dotted")

                # Add line and points
                graphics::lines(
                    date_range,
                    cumulative_responses,
                    col = line_color,
                    lwd = 2
                )
                graphics::points(
                    date_range,
                    cumulative_responses,
                    col = line_color,
                    pch = 16
                )
            },
            bg = "transparent"
        )

        output$daily_trend <- shiny::renderPlot(
            {
                data <- survey_data()
                # Get theme colors based on dark mode
                bg_color <- if (input$dark_mode == "dark") {
                    "#2c3e50"
                } else {
                    "#ffffff"
                }
                text_color <- if (input$dark_mode == "dark") {
                    "#ffffff"
                } else {
                    "#1a2226"
                }
                grid_color <- if (input$dark_mode == "dark") {
                    "gray30"
                } else {
                    "gray80"
                }
                bar_color <- if (input$dark_mode == "dark") {
                    "#3498db"
                } else {
                    "#0062cc"
                }
                bar_border <- if (input$dark_mode == "dark") {
                    "#00008B"
                } else {
                    "#00008B"
                }

                if (
                    is.null(data) ||
                        nrow(data) == 0 ||
                        !("time_start" %in% names(data))
                ) {
                    graphics::par(bg = bg_color, fg = text_color)
                    graphics::plot.new()
                    graphics::text(
                        0.5,
                        0.5,
                        "No data available to display",
                        col = text_color,
                        cex = 1.2
                    )
                    return()
                }

                dates <- try(as.Date(data$time_start))
                if (
                    inherits(dates, "try-error") ||
                        length(dates) == 0 ||
                        all(is.na(dates))
                ) {
                    graphics::par(bg = bg_color, fg = text_color)
                    graphics::plot.new()
                    graphics::text(
                        0.5,
                        0.5,
                        "Unable to process date data",
                        col = text_color,
                        cex = 1.2
                    )
                    return()
                }

                dates <- dates[!is.na(dates)]
                daily_counts <- table(dates)
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range
                all_counts[names(daily_counts)] <- daily_counts

                # Plot setup with theme
                graphics::par(
                    bg = bg_color,
                    fg = text_color,
                    col.axis = text_color,
                    col.lab = text_color,
                    mar = c(5, 4, 2, 2)
                ) # Increased bottom margin for date labels

                # Create barplot
                bp <- graphics::barplot(
                    all_counts,
                    col = bar_color,
                    border = bar_border,
                    xlab = "Date",
                    ylab = "Daily Responses",
                    xaxt = "n",
                    space = 0.2
                )

                # Add gridlines
                graphics::grid(col = grid_color, lty = "dotted")

                # Add x-axis with rotated labels
                graphics::axis(
                    1,
                    at = bp,
                    labels = format(date_range, "%b %d"),
                    las = 2,
                    col.axis = text_color,
                    cex.axis = 0.9
                )
            },
            bg = "transparent"
        )

        # Survey Data Table
        output$survey_data_table <- DT::renderDataTable({
            shiny::req(survey_data())
            data <- survey_data()
            DT::datatable(
                data,
                extensions = 'Scroller',
                options = list(
                    dom = 'Bfrtip',
                    scrollX = TRUE,
                    scrollY = '400px',
                    scroller = TRUE,
                    pageLength = 50
                ),
                class = 'cell-border stripe'
            )
        })

        # Mr.Gorbachev CLOSE DOWN THIS POOL

        #Its not working...Getting a reactive value access error

        #cleanup <- function() {
        #    if (!is.null(rv$current_db) && !is.null(rv$current_db$db)) {
        #        tryCatch({
        #            pool::poolClose(rv$current_db$db)
        #        }, error = function(e) {
        #            warning("Error closing database connection: ", e$message)
        #        })
        #    }
        #}
        #
        #shiny::onStop(cleanup)
    }

    # Run the Shiny app
    shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
}

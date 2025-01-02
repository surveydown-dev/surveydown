#' Launch Survey Dashboard
#'
#' @description
#' Opens a dashboard interface to view survey responses and summary statistics.
#' Includes database configuration, summary statistics, and response data.
#'
#' @return No return value, called for side effects (opens dashboard interface)
#' @export
sd_dashboard <- function() {
    # First load the environment variables
    if (file.exists(".env")) {
        dotenv::load_dot_env(".env")
    }

    # Try to connect to database
    local_db <- sd_db_connect()

    # Define UI
    ui <- miniUI::miniPage(
        shinyjs::useShinyjs(),
        miniUI::gadgetTitleBar("Survey Dashboard"),

        miniUI::miniContentPanel(
            style = "margin: 0 auto; max-width: 1200px;",

            # Styles
            shiny::tags$head(
                shiny::tags$style(shiny::HTML("
                    .info-box {
                        padding: 20px;
                        background-color: white;
                        border-radius: 5px;
                        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                        margin-bottom: 15px;
                    }
                    .info-box h4 {
                        margin-top: 0;
                        color: #2c3e50;
                    }
                    .info-box .value {
                        font-size: 24px;
                        font-weight: bold;
                        color: #3498db;
                    }
                    .info-box .meta {
                        margin-top: 10px;
                        color: #7f8c8d;
                        font-size: 0.9em;
                    }
                    .nav-tabs {
                        margin-bottom: 0 !important;
                    }
                    .tab-content {
                        padding-top: 0 !important;
                        background: white;
                        border-radius: 0 0 5px 5px;
                        border: 1px solid #ddd;
                        border-top: none;
                    }
                    .dataTables_wrapper {
                        padding: 20px;
                        background: white;
                        border-radius: 5px;
                        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                    }
                    .tabbable > .tab-content {
                        margin-top: 0;
                        padding-top: 0;
                    }
                    .tab-pane {
                        padding-top: 0;
                    }
                "))
            ),

            # Main tabset panel
            shiny::tabsetPanel(
                id = "mainTabs",

                # Summary Tab
                shiny::tabPanel(
                    "Summary",
                    shiny::br(),
                    shiny::selectInput(
                        "table_select",
                        "Select Survey Table:",
                        choices = if (!is.null(local_db)) {
                            pool::poolWithTransaction(local_db$db, function(conn) {
                                tables <- DBI::dbListTables(conn)
                                tables[!grepl("^pg_", tables)]
                            })
                        } else {
                            NULL
                        },
                        width = "100%"
                    ),
                    shiny::uiOutput("survey_stats"),
                    shiny::plotOutput("response_trend", height = "300px")
                ),

                # Data Tab
                shiny::tabPanel(
                    "Responses",
                    shiny::br(),
                    DT::DTOutput("survey_data_table")
                ),

                # DB Settings Tab
                shiny::tabPanel(
                    "Database Settings",
                    shiny::br(),
                    # Current settings display
                    shiny::div(
                        style = "margin-bottom: 30px; background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                        shiny::h4("Current Settings:", style = "margin-top: 0; margin-bottom: 15px;"),
                        shiny::div(
                            style = "display: grid; grid-template-columns: auto 1fr; gap: 10px; align-items: center;",
                            shiny::tags$strong("Host:", style = "color: #666;"),
                            shiny::span(Sys.getenv("SD_HOST", "Not set"), style = "color: #2c3e50;"),
                            shiny::tags$strong("Port:", style = "color: #666;"),
                            shiny::span(Sys.getenv("SD_PORT", "Not set"), style = "color: #2c3e50;"),
                            shiny::tags$strong("Database:", style = "color: #666;"),
                            shiny::span(Sys.getenv("SD_DBNAME", "Not set"), style = "color: #2c3e50;"),
                            shiny::tags$strong("User:", style = "color: #666;"),
                            shiny::span(Sys.getenv("SD_USER", "Not set"), style = "color: #2c3e50;"),
                            shiny::tags$strong("Password:", style = "color: #666;"),
                            shiny::span("*****", style = "color: #2c3e50;"),
                            shiny::tags$strong("GSS Mode:", style = "color: #666;"),
                            shiny::span(Sys.getenv("SD_GSSENCMODE", "Not set"), style = "color: #2c3e50;")
                        )
                    ),

                    shiny::hr(style = "margin: 20px 0;"),

                    shiny::h4("Update Settings:", style = "margin-bottom: 20px;"),

                    # Host
                    shiny::textInput(
                        "host",
                        "Host:",
                        value = Sys.getenv("SD_HOST", "localhost"),
                        placeholder = "e.g., localhost"
                    ),

                    # Port
                    shiny::textInput(
                        "port",
                        "Port:",
                        value = Sys.getenv("SD_PORT", "5432"),
                        placeholder = "e.g., 5432"
                    ),

                    # Database name
                    shiny::textInput(
                        "dbname",
                        "Database name:",
                        value = Sys.getenv("SD_DBNAME", "postgres"),
                        placeholder = "e.g., postgres"
                    ),

                    # User
                    shiny::textInput(
                        "user",
                        "User:",
                        value = Sys.getenv("SD_USER", "username"),
                        placeholder = "Database username"
                    ),

                    # Password input with show/hide toggle
                    shiny::div(
                        id = "password-container",
                        style = "position: relative;",
                        shiny::div(
                            style = "display: flex; align-items: center;",
                            shiny::div(
                                style = "flex-grow: 1;",
                                shiny::passwordInput(
                                    "password",
                                    "Password:",
                                    value = Sys.getenv("SD_PASSWORD", ""),
                                    placeholder = "Database password",
                                    width = "100%"
                                )
                            ),
                            shiny::div(
                                style = "margin-left: 10px; margin-top: 25px;",
                                shiny::actionButton(
                                    "toggle_password",
                                    "Show",
                                    class = "btn-sm btn-secondary",
                                    style = "padding: 2px 8px; font-size: 12px;"
                                )
                            )
                        )
                    ),

                    # GSS encryption mode
                    shiny::selectInput(
                        "gssencmode",
                        "GSS encryption mode:",
                        choices = c("prefer", "disable", "require"),
                        selected = Sys.getenv("SD_GSSENCMODE", "prefer")
                    ),

                    # Test connection button
                    shiny::div(
                        style = "margin-top: 20px;",
                        shiny::actionButton(
                            "test_connection",
                            "Test Connection",
                            class = "btn-primary",
                            style = "width: 100%;"
                        )
                    ),

                    # Connection status
                    shiny::div(
                        style = "margin-top: 10px; text-align: center;",
                        shiny::textOutput("connection_status")
                    )
                )
            )
        )
    )

    # Server logic
    server <- function(input, output, session) {
        # Reactive values
        connection_status <- shiny::reactiveVal(FALSE)

        # Initialize connection status if we have a working connection
        shiny::observe({
            if (!is.null(local_db)) {
                # Verify the connection is actually working
                tryCatch({
                    pool::poolWithTransaction(local_db$db, function(conn) {
                        tables <- DBI::dbListTables(conn)
                        if (length(tables) > 0) {
                            connection_status(TRUE)
                        }
                    })
                }, error = function(e) {
                    connection_status(FALSE)
                })
            }
        })

        # Password visibility toggle
        password_visible <- shiny::reactiveVal(FALSE)

        shiny::observeEvent(input$toggle_password, {
            password_visible(!password_visible())

            shiny::updateActionButton(session, "toggle_password",
                                      label = if (password_visible()) "Hide" else "Show"
            )

            if (password_visible()) {
                shiny::updateTextInput(session, "password",
                                       value = input$password,
                                       type = "text"
                )
            } else {
                shiny::updateTextInput(session, "password",
                                       value = input$password,
                                       type = "password"
                )
            }
        })

        # Test connection function
        test_connection <- function(config) {
            tryCatch({
                pool <- pool::dbPool(
                    RPostgres::Postgres(),
                    host = config$host,
                    dbname = config$dbname,
                    port = config$port,
                    user = config$user,
                    password = config$password,
                    gssencmode = config$gssencmode
                )
                pool::poolClose(pool)
                TRUE
            }, error = function(e) {
                FALSE
            })
        }

        # Test connection button observer
        shiny::observeEvent(input$test_connection, {
            # Close existing connection if it exists
            if (!is.null(local_db)) {
                pool::poolClose(local_db$db)
            }

            config <- list(
                host = input$host,
                port = input$port,
                dbname = input$dbname,
                user = input$user,
                password = input$password,
                gssencmode = input$gssencmode
            )

            success <- test_connection(config)
            if (success) {
                # Save configuration
                template <- paste(
                    "# Database connection settings for surveydown",
                    sprintf("SD_HOST=%s", input$host),
                    sprintf("SD_PORT=%s", input$port),
                    sprintf("SD_DBNAME=%s", input$dbname),
                    sprintf("SD_USER=%s", input$user),
                    sprintf("SD_PASSWORD=%s", input$password),
                    sprintf("SD_GSSENCMODE=%s", input$gssencmode),
                    sep = "\n"
                )

                writeLines(template, ".env")

                if (file.exists(".gitignore")) {
                    gitignore_content <- readLines(".gitignore")
                    if (!".env" %in% gitignore_content) {
                        write("\n.env", ".gitignore", append = TRUE)
                    }
                } else {
                    write(".env", ".gitignore")
                }

                # Connect with new settings
                local_db <<- sd_db_connect()

                # Update table list if connection successful
                if (!is.null(local_db)) {
                    tables <- pool::poolWithTransaction(local_db$db, function(conn) {
                        tables <- DBI::dbListTables(conn)
                        tables[!grepl("^pg_", tables)]
                    })

                    shiny::updateSelectInput(session, "table_select",
                                             choices = tables,
                                             selected = tables[1]
                    )
                }

                output$connection_status <- shiny::renderText("Connection successful!")
                connection_status(TRUE)
            } else {
                output$connection_status <- shiny::renderText("Connection failed. Please check your settings.")
                connection_status(FALSE)
            }
        })

        # Reactive data for dashboard
        survey_data <- shiny::reactive({
            req(input$table_select)
            req(connection_status())

            pool::poolWithTransaction(local_db$db, function(conn) {
                DBI::dbGetQuery(conn, sprintf('SELECT * FROM "%s"', input$table_select))
            })
        })

        # Generate survey statistics
        output$survey_stats <- shiny::renderUI({
            req(survey_data())
            data <- survey_data()

            if (nrow(data) == 0) {
                return(shiny::div(
                    class = "info-box",
                    "No data available for selected table"
                ))
            }

            # Calculate statistics
            total_respondents <- nrow(data)

            # Convert time_start to datetime and calculate time differences
            start_times <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M:%S")

            if (length(start_times) > 0) {
                first_response <- min(start_times, na.rm = TRUE)
                last_response <- max(start_times, na.rm = TRUE)

                # Calculate duration in days
                survey_duration <- difftime(last_response, first_response, units = "days")
                duration_days <- max(as.numeric(survey_duration), 1)

                # Calculate daily average
                daily_avg <- round(total_respondents / duration_days, 1)

                # Calculate completion rate if rating exists
                completion_rate <- if ("exit_survey_rating" %in% names(data)) {
                    rated_responses <- sum(!is.na(data$exit_survey_rating))
                    sprintf("%.1f%%", (rated_responses / total_respondents) * 100)
                } else {
                    "N/A"
                }

                # Calculate average rating if it exists
                avg_rating <- if ("exit_survey_rating" %in% names(data)) {
                    mean_rating <- mean(as.numeric(data$exit_survey_rating), na.rm = TRUE)
                    if (!is.nan(mean_rating)) sprintf("%.1f", mean_rating) else "N/A"
                } else {
                    "N/A"
                }

                # Format dates
                first_date <- format(first_response, "%Y-%m-%d")
                last_date <- format(last_response, "%Y-%m-%d")
            } else {
                first_date <- "No responses"
                last_date <- "No responses"
                daily_avg <- 0
                duration_days <- 0
                completion_rate <- "N/A"
                avg_rating <- "N/A"
            }

            shiny::fluidRow(
                shiny::column(
                    width = 3,
                    shiny::div(
                        class = "info-box",
                        shiny::h4("Total Responses"),
                        shiny::div(class = "value", total_respondents)
                    )
                ),
                shiny::column(
                    width = 3,
                    shiny::div(
                        class = "info-box",
                        shiny::h4("Daily Average"),
                        shiny::div(class = "value", daily_avg)
                    )
                ),
                shiny::column(
                    width = 3,
                    shiny::div(
                        class = "info-box",
                        shiny::h4("Completion Rate"),
                        shiny::div(class = "value", completion_rate)
                    )
                ),
                shiny::column(
                    width = 3,
                    shiny::div(
                        class = "info-box",
                        shiny::h4("Average Rating"),
                        shiny::div(class = "value", avg_rating)
                    )
                ),
                shiny::column(
                    width = 12,
                    shiny::div(
                        class = "info-box",
                        shiny::h4("Survey Timeline"),
                        shiny::div(
                            class = "meta",
                            shiny::p(
                                sprintf("Duration: %.1f days", duration_days),
                                style = "margin: 5px 0;"
                            ),
                            shiny::p(
                                sprintf("First Response: %s", first_date),
                                style = "margin: 5px 0;"
                            ),
                            shiny::p(
                                sprintf("Last Response: %s", last_date),
                                style = "margin: 5px 0;"
                            )
                        )
                    )
                )
            )
        })

        # Generate response trend plot
        output$response_trend <- shiny::renderPlot({
            req(survey_data())
            data <- survey_data()

            if (nrow(data) > 0 && "time_start" %in% names(data)) {
                # Convert time_start to Date
                dates <- as.Date(data$time_start)

                # Create daily counts
                daily_counts <- table(dates)

                # Create a complete sequence of dates
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range

                # Fill in actual counts
                all_counts[names(daily_counts)] <- daily_counts

                # Calculate cumulative responses
                cumulative_responses <- cumsum(all_counts)

                # Create the plot
                par(mar = c(4, 4, 2, 4))

                # Plot daily responses
                plot(date_range, all_counts,
                     type = "h",
                     col = "#3498db",
                     xlab = "Date",
                     ylab = "Daily Responses",
                     main = "Response Trend")

                # Add cumulative line on secondary y-axis
                par(new = TRUE)
                plot(date_range, cumulative_responses,
                     type = "l",
                     col = "#e74c3c",
                     xaxt = "n",
                     yaxt = "n",
                     xlab = "",
                     ylab = "")

                # Add secondary axis
                axis(4, col = "#e74c3c", col.axis = "#e74c3c")
                mtext("Cumulative Responses", side = 4, line = 2, col = "#e74c3c")

                # Add legend
                legend("topleft",
                       legend = c("Daily", "Cumulative"),
                       col = c("#3498db", "#e74c3c"),
                       lty = c(1, 1),
                       bg = "white")
            }
        })

        # Data table
        output$survey_data_table <- DT::renderDT({
            req(survey_data())
            data <- survey_data()

            DT::datatable(
                data,
                extensions = c('Buttons', 'Scroller'),
                options = list(
                    dom = 'Bfrtip',
                    buttons = list(
                        list(
                            extend = 'csv',
                            text = 'Download CSV',
                            filename = function() input$table_select
                        )
                    ),
                    scrollX = TRUE,
                    scrollY = '400px',
                    scroller = TRUE,
                    pageLength = 50
                ),
                class = 'cell-border stripe'
            )
        })

        # Handle the Done button
        shiny::observeEvent(input$done, {
            # Close connection and stop app
            if (!is.null(local_db)) {
                pool::poolClose(local_db$db)
            }
            shiny::stopApp()
        })
    }

    # Run the app
    shiny::runGadget(ui, server, viewer = shiny::browserViewer(browser = getOption("browser")))
}

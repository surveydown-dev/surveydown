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
#' @return
#' Launches a Shiny application with the survey dashboard.
#' The function does not return a value; it is called for its side effects
#' of opening the dashboard interface.
#'
#' @examples
#' \dontrun{
#' # Launch the survey dashboard
#' sd_dashboard()
#' }
#'
#' @import shiny
#' @import shinydashboard
#' @import DBI
#' @import pool
#' @importFrom DT renderDT datatable
#' @importFrom RPostgres Postgres
#' @importFrom graphics axis legend mtext par plot.new
#' @importFrom utils write.csv
#' @export
sd_dashboard <- function() {
    # First load the environment variables
    if (file.exists(".env")) {
        dotenv::load_dot_env(".env")
    }

    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(
            title = "Survey Dashboard"
        ),
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(

                # Add divider and centered "Tables" label
                shiny::tags$div(
                    class = "sidebar-menu",
                    style = "padding: 10px 0;",  # Add vertical padding
                    shiny::tags$li(
                        class = "header",
                        style = "background-color: #1a2226; color: #ffffff; text-align: center; padding: 10px 0; font-size: 12px;",
                        "Choose a table to view:"
                    ),
                    shiny::tags$li(
                        style = "padding: 10px 15px;",
                        shiny::selectInput(
                            "table_select",
                            NULL,
                            choices = c("No connection" = ""),  # Start with no connection message
                            width = "100%",
                            selectize = TRUE
                        )
                    )
                ),

                # Other menu items
                shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("dashboard")),
                shinydashboard::menuItem("Connection Settings", tabName = "settings", icon = shiny::icon("cog"))
            )
        ),
        shinydashboard::dashboardBody(
            shiny::tags$head(
                shiny::tags$style(shiny::HTML("
                    .info-box {
                        min-height: 90px;
                        display: flex;
                        flex-direction: column;
                        justify-content: center;
                        text-align: center;
                    }
                    .info-box-number {
                        font-size: 24px;
                        font-weight: bold;
                    }
                    .info-box-text {
                        text-transform: uppercase;
                        font-size: 12px;
                    }
                "))
            ),
            shinydashboard::tabItems(
                # Dashboard Tab
                shinydashboard::tabItem(tabName = "dashboard",
                                        # First fluid row with stats and graphs
                                        shiny::fluidRow(
                                            shiny::column(
                                                width = 2,
                                                # Value boxes
                                                shinydashboard::valueBoxOutput("total_responses", width = NULL),
                                                shinydashboard::valueBoxOutput("daily_average", width = NULL),
                                                shinydashboard::valueBoxOutput("completion_rate", width = NULL)
                                            ),
                                            # Right columns for graphs
                                            shiny::column(
                                                width = 5,
                                                shinydashboard::box(
                                                    width = NULL,
                                                    title = "Cumulative Responses",
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    shiny::plotOutput("cumulative_trend", height = "300px")
                                                )
                                            ),
                                            shiny::column(
                                                width = 5,
                                                shinydashboard::box(
                                                    title = "Daily Responses",
                                                    width = NULL,
                                                    status = "primary",
                                                    solidHeader = TRUE,
                                                    shiny::plotOutput("daily_trend", height = "300px")
                                                )
                                            )
                                        ),
                                        # Second fluid row with data table
                                        shiny::fluidRow(
                                            shinydashboard::box(
                                                title = "Survey Responses",
                                                width = 12,
                                                shiny::div(
                                                    style = "margin-bottom: 15px; display: flex; align-items: center;",
                                                    shiny::div(
                                                        style = "margin-right: auto;",  # This pushes it to the left
                                                        shiny::downloadButton("download_survey_data", "Download CSV")
                                                    )
                                                ),
                                                DT::dataTableOutput("survey_data_table")
                                            )
                                        )
                ),

                # Settings Tab
                shinydashboard::tabItem(tabName = "settings",
                                        shiny::fluidRow(
                                            shinydashboard::box(
                                                title = "Current Database Settings",
                                                width = 12,
                                                status = "primary",
                                                solidHeader = TRUE,
                                                shiny::div(
                                                    style = "display: grid; grid-template-columns: auto 1fr; gap: 10px;",
                                                    shiny::tags$strong("Host:"),
                                                    shiny::span(Sys.getenv("SD_HOST", "Not set")),
                                                    shiny::tags$strong("Port:"),
                                                    shiny::span(Sys.getenv("SD_PORT", "Not set")),
                                                    shiny::tags$strong("Database:"),
                                                    shiny::span(Sys.getenv("SD_DBNAME", "Not set")),
                                                    shiny::tags$strong("User:"),
                                                    shiny::span(Sys.getenv("SD_USER", "Not set")),
                                                    shiny::tags$strong("GSS Mode:"),
                                                    shiny::span(Sys.getenv("SD_GSSENCMODE", "Not set")),
                                                    shiny::tags$strong("Default Table:"),
                                                    shiny::span(Sys.getenv("SD_TABLE", "Not set"))
                                                )
                                            )
                                        ),

                                        shiny::fluidRow(
                                            shinydashboard::box(
                                                title = "Update Your Database Connection",
                                                width = 12,
                                                status = "info",
                                                solidHeader = TRUE,
                                                shiny::textInput("host", "Host:",
                                                                 value = Sys.getenv("SD_HOST", "localhost")),
                                                shiny::textInput("port", "Port:",
                                                                 value = Sys.getenv("SD_PORT", "5432")),
                                                shiny::textInput("dbname", "Database Name:",
                                                                 value = Sys.getenv("SD_DBNAME", "postgres")),
                                                shiny::textInput("user", "User:",
                                                                 value = Sys.getenv("SD_USER", "username")),
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
                                                                width = "100%"
                                                            )
                                                        ),
                                                        shiny::div(
                                                            style = "margin-left: 10px; margin-top: 25px;",
                                                            shiny::actionButton(
                                                                "toggle_password",
                                                                "Show",
                                                                class = "btn-sm btn-secondary",
                                                                style = "padding: 2px 8px; font-size: 12px;",
                                                                onclick = "myFunction()"
                                                            )
                                                        )
                                                    )
                                                ),
                                                shiny::selectInput("gssencmode", "GSS Encryption Mode",
                                                                   choices = c("prefer", "disable", "require"),
                                                                   selected = Sys.getenv("SD_GSSENCMODE", "prefer")),

                                                # New input for default table
                                                shiny::textInput("default_table", "Development Table:",
                                                                 value = Sys.getenv("SD_TABLE", "responses")),

                                                shiny::actionButton("test_connection", "Test Connection",
                                                                    class = "btn-primary"),
                                                shiny::textOutput("connection_status")
                                            )
                                        )
                )
            ),
            #Script for password, something I found online R solutions weren't working
            shiny::tags$script(HTML("
                function myFunction() {
                  var x = document.getElementById('password');
                  if (x.type === 'password') {
                    x.type = 'text';
                  } else {
                    x.type = 'password';
                  }
                }
                "))
        )
    )

    #Server Block

    server <- function(input, output, session) {
        # Reactive values for connection status
        rv <- shiny::reactiveValues(
            connection_status = FALSE,
            current_db = NULL
        )

        # Initial connection check
        attempt_connection <- function(config = NULL) {
            tryCatch({
                if (is.null(config)) {
                    # Use default connection from .env
                    db <- sd_db_connect()
                } else {
                    # Use provided config
                    pool <- pool::dbPool(
                        RPostgres::Postgres(),
                        host = config$host,
                        dbname = config$dbname,
                        port = config$port,
                        user = config$user,
                        password = config$password,
                        gssencmode = config$gssencmode
                    )
                    db <- list(db = pool)
                }

                if (!is.null(db)) {
                    rv$connection_status <- TRUE
                    rv$current_db <- db
                    return(TRUE)
                }
                return(FALSE)
            }, error = function(e) {
                rv$connection_status <- FALSE
                rv$current_db <- NULL
                return(FALSE)
            })
        }


        # Initial connection attempt
        shiny::observe({
            attempt_connection()
        })

        shiny::observe({
            if (rv$connection_status && !is.null(rv$current_db)) {
                tryCatch({
                    tables <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
                        all_tables <- DBI::dbListTables(conn)
                        all_tables[!grepl("^pg_", all_tables)]
                    })

                    # Set default table as first choice if available
                    default_table <- Sys.getenv("SD_TABLE", "")
                    if (default_table %in% tables) {
                        tables <- c(default_table, setdiff(tables, default_table))
                    }

                    shiny::updateSelectInput(session, "table_select",
                                             choices = if (length(tables) > 0) tables else c("No tables found" = "")
                    )
                }, error = function(e) {
                    shiny::updateSelectInput(session, "table_select",
                                             choices = c("Connection error" = "")
                    )
                })
            } else {
                shiny::updateSelectInput(session, "table_select",
                                         choices = c("No connection" = "")
                )
            }
        })

        # Initialize connection status based on local_db
        shiny::observeEvent(input$test_connection, {
            # First close any existing connection
            if (!is.null(rv$current_db) && !is.null(rv$current_db$db)) {
                tryCatch({
                    pool::poolClose(rv$current_db$db)
                    rv$current_db <- NULL
                    rv$connection_status <- FALSE
                }, error = function(e) {
                    warning("Error closing existing connection: ", e$message)
                })
            }

            config <- list(
                host = input$host,
                port = input$port,
                dbname = input$dbname,
                user = input$user,
                password = input$password,
                gssencmode = input$gssencmode
            )

            suppressMessages({
                success <- attempt_connection(config)
            })

            if (success) {
                # Save configuration to .env file
                template <- paste(
                    "# Database connection settings for surveydown",
                    sprintf("SD_HOST=%s", input$host),
                    sprintf("SD_PORT=%s", input$port),
                    sprintf("SD_DBNAME=%s", input$dbname),
                    sprintf("SD_USER=%s", input$user),
                    sprintf("SD_PASSWORD=%s", input$password),
                    sprintf("SD_GSSENCMODE=%s", input$gssencmode),
                    sprintf("SD_TABLE=%s", input$default_table),
                    sep = "\n"
                )
                writeLines(template, ".env")

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

                # Update table selection choices
                tables <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
                    all_tables <- DBI::dbListTables(conn)
                    all_tables[!grepl("^pg_", all_tables)]
                })

                # Set default table as first choice if available
                if (input$default_table %in% tables) {
                    tables <- c(input$default_table, setdiff(tables, input$default_table))
                }

                shiny::updateSelectInput(session, "table_select",
                                         choices = if (length(tables) > 0) tables else c("No tables found" = "")
                )
            } else {
                output$connection_status <- shiny::renderText(
                    "Connection failed. Please check your settings."
                )
            }
        }, ignoreInit = TRUE)

        # Reactive survey data with error handling
        survey_data <- shiny::reactive({
            shiny::req(rv$connection_status)
            shiny::req(input$table_select)
            shiny::req(rv$current_db)

            tryCatch({
                data <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
                    DBI::dbGetQuery(conn, sprintf('SELECT * FROM "%s"', input$table_select))
                })
                return(data)
            }, error = function(e) {
                warning("Error fetching survey data: ", e$message)
                return(NULL)
            })
        })

        # Downloadable CSV of survey data
        output$download_survey_data <- downloadHandler(
            filename = function() {
                paste0(input$table_select, "_", Sys.Date(), ".csv")
            },
            content = function(file) {
                data <- survey_data()
                write.csv(data, file, row.names = FALSE)
            }
        )


        # Value Boxes
        output$total_responses <- shinydashboard::renderValueBox({
            shiny::req(survey_data())
            data <- survey_data()
            shinydashboard::valueBox(
                nrow(data),
                "Total Responses",
                icon = shiny::icon("users"),
                color = "aqua"
            )
        })

        output$daily_average <- shinydashboard::renderValueBox({
            shiny::req(survey_data())
            data <- survey_data()

            start_times <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M:%S")
            first_response <- min(start_times, na.rm = TRUE)
            last_response <- max(start_times, na.rm = TRUE)

            duration_days <- max(as.numeric(difftime(last_response, first_response, units = "days")), 1)
            daily_avg <- round(nrow(data) / duration_days, 1)

            shinydashboard::valueBox(
                daily_avg,
                "Daily Average",
                icon = shiny::icon("chart-line"),
                color = "green"
            )
        })

        output$completion_rate <- shinydashboard::renderValueBox({
            shiny::req(survey_data())
            data <- survey_data()

            total_responses <- nrow(data)
            completed_responses <- sum(!is.na(data$time_end) & data$time_end != "", na.rm = TRUE)

            completion_rate <- if(total_responses > 0) {
                sprintf("%.1f%%", (completed_responses / total_responses) * 100)
            } else {
                "0.0%"
            }

            shinydashboard::valueBox(
                completion_rate,
                "Completion Rate",
                icon = shiny::icon("check-circle"),
                color = "yellow"
            )
        })

        # Response Trend Plot
        output$cumulative_trend <- shiny::renderPlot({
            # Check for data early
            data <- survey_data()
            if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
                plot.new()
                plot.window(xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "No data available to display", cex = 1.2)
                return()
            }

            withCallingHandlers({
                dates <- try(as.Date(data$time_start), silent = TRUE)
                if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "Unable to process date data", cex = 1.2)
                    return()
                }

                dates <- dates[!is.na(dates)]
                if (length(dates) == 0) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "No valid dates found in data", cex = 1.2)
                    return()
                }

                daily_counts <- table(dates)
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range
                all_counts[names(daily_counts)] <- daily_counts
                cumulative_responses <- cumsum(all_counts)

                # Plot cumulative responses
                par(mar = c(4, 4, 2, 2))
                plot(date_range, cumulative_responses,
                     type = "l",
                     col = "#e74c3c",
                     lwd = 2,
                     xlab = "Date",
                     ylab = "Cumulative Responses"
                     )

                points(date_range, cumulative_responses,
                       col = "#e74c3c",
                       pch = 16)

                grid()
            }, warning = function(w) {
                invokeRestart("muffleWarning")
            })
        })

        # Daily trend plot
        output$daily_trend <- shiny::renderPlot({
            data <- survey_data()
            if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
                plot.new()
                plot.window(xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "No data available to display", cex = 1.2)
                return()
            }

            withCallingHandlers({
                dates <- try(as.Date(data$time_start), silent = TRUE)
                if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "Unable to process date data", cex = 1.2)
                    return()
                }

                dates <- dates[!is.na(dates)]
                if (length(dates) == 0) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "No valid dates found in data", cex = 1.2)
                    return()
                }

                daily_counts <- table(dates)
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range
                all_counts[names(daily_counts)] <- daily_counts

                # Match the theme with cumulative plot but add spacing and border
                par(mar = c(4, 4.5, 2, 2))  # Increased left margin for y-axis
                bp <- barplot(all_counts,
                              col = "#3498db",
                              xlab = "Date",
                              ylab = "Daily Responses",
                              xaxt = "n",
                              space = 0.1,
                              border = "darkblue"
                )

                # Add x-axis with same format as cumulative plot
                axis(1,
                     at = bp,
                     labels = format(date_range, "%b %d"),
                     las = 1,
                     cex.axis = 0.9
                )
            }, warning = function(w) {
                invokeRestart("muffleWarning")
            })
        })


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

    }

    # Run the Shiny app
    shiny::shinyApp(ui, server)
}

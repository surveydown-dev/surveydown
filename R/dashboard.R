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
#' @export
sd_dashboard <- function() {
    # First load the environment variables
    if (file.exists(".env")) {
        dotenv::load_dot_env(".env")
    }

    # Try to connect to database
    local_db <- sd_db_connect()

    ui <- shinydashboard::dashboardPage(
        shinydashboard::dashboardHeader(title = "Survey Dashboard"),
        shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
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
                                        shiny::fluidRow(
                                            shinydashboard::box(
                                                width = 12,
                                                shiny::selectInput(
                                                    "table_select",
                                                    "Select Survey Table:",
                                                    choices = if (!is.null(local_db)) {
                                                        pool::poolWithTransaction(local_db$db, function(conn) {
                                                            DBI::dbListTables(conn)
                                                        })
                                                    } else {
                                                        NULL
                                                    },
                                                    width = "100%"
                                                )
                                            )
                                        ),

                                        # Summary Boxes
                                        shiny::fluidRow(
                                            shinydashboard::valueBoxOutput("total_responses", width = 3),
                                            shinydashboard::valueBoxOutput("daily_average", width = 3),
                                            shinydashboard::valueBoxOutput("completion_rate", width = 3),
                                            shinydashboard::valueBoxOutput("avg_rating", width = 3)
                                        ),

                                        # Response Trend and Timeline
                                        shinydashboard::box(
                                            title = "Response Trend",
                                            width = 8,
                                            shiny::fluidRow(
                                                shiny::column(
                                                    width = 12,
                                                    shiny::selectInput(
                                                        "plot_type",
                                                        "Plot Type:",
                                                        choices = c("Line Graph" = "line", "Bar Graph" = "bar"),
                                                        selected = "line"
                                                    )
                                                )
                                            ),
                                            shiny::plotOutput("response_trend", height = "300px")
                                        ),
                                        shiny::fluidRow(
                                            shinydashboard::box(
                                                title = "Survey Responses",
                                                width = 12,
                                                DT::dataTableOutput("survey_data_table"),
                                                shiny::downloadButton("download_survey_data", "Download CSV")
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
                                                    shiny::span(Sys.getenv("SD_GSSENCMODE", "Not set"))
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
        connection_status <- shiny::reactiveVal(FALSE)

        # Initialize connection status based on local_db
        shiny::observe({
            if (!is.null(local_db)) {
                # Test the initial connection
                config <- list(
                    host = Sys.getenv("SD_HOST", "localhost"),
                    port = Sys.getenv("SD_PORT", "5432"),
                    dbname = Sys.getenv("SD_DBNAME", "postgres"),
                    user = Sys.getenv("SD_USER", "username"),
                    password = Sys.getenv("SD_PASSWORD", ""),
                    gssencmode = Sys.getenv("SD_GSSENCMODE", "prefer")
                )

                success <- tryCatch({
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

                connection_status(success)
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

        # Test connection observer
        shiny::observeEvent(input$test_connection, {
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
                # Save configuration to .env file
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

                # Update .gitignore
                if (file.exists(".gitignore")) {
                    gitignore_content <- readLines(".gitignore")
                    if (!".env" %in% gitignore_content) {
                        write("\n.env", ".gitignore", append = TRUE)
                    }
                } else {
                    write(".env", ".gitignore")
                }

                output$connection_status <- shiny::renderText("Connection successful!")
                connection_status(TRUE)
            } else {
                output$connection_status <- shiny::renderText("Connection failed. Please check your settings.")
                connection_status(FALSE)
            }
        })

        # Reactive survey data with error handling
        survey_data <- shiny::reactive({
            shiny::req(input$table_select)
            shiny::req(connection_status())
            shiny::req(!is.null(local_db))

            tryCatch({
                data <- pool::poolWithTransaction(local_db$db, function(conn) {
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

            if ("exit_survey_rating" %in% names(data)) {
                rated_responses <- sum(!is.na(data$exit_survey_rating))
                completion_rate <- sprintf("%.1f%%", (rated_responses / nrow(data)) * 100)
            } else {
                completion_rate <- "N/A"
            }

            shinydashboard::valueBox(
                completion_rate,
                "Completion Rate",
                icon = shiny::icon("check-circle"),
                color = "yellow"
            )
        })

        output$avg_rating <- shinydashboard::renderValueBox({
            shiny::req(survey_data())
            data <- survey_data()

            if ("exit_survey_rating" %in% names(data)) {
                mean_rating <- mean(as.numeric(data$exit_survey_rating), na.rm = TRUE)
                avg_rating <- if (!is.nan(mean_rating)) sprintf("%.1f", mean_rating) else "N/A"
            } else {
                avg_rating <- "N/A"
            }

            shinydashboard::valueBox(
                avg_rating,
                "Avg. Rating",
                icon = shiny::icon("star"),
                color = "purple"
            )
        })

        # Response Trend Plot
        output$response_trend <- shiny::renderPlot({
            # Check for data early
            data <- survey_data()
            if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
                # Create an empty plot with a message
                plot.new()
                plot.window(xlim = c(0, 1), ylim = c(0, 1))
                text(0.5, 0.5, "No data available to display", cex = 1.2)
                return()
            }

            # Suppress warnings for the entire plotting process
            withCallingHandlers({
                # Convert time_start to Date - wrap in try to handle conversion errors
                dates <- try(as.Date(data$time_start), silent = TRUE)
                if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "Unable to process date data", cex = 1.2)
                    return()
                }

                # Remove NA dates
                dates <- dates[!is.na(dates)]
                if (length(dates) == 0) {
                    plot.new()
                    plot.window(xlim = c(0, 1), ylim = c(0, 1))
                    text(0.5, 0.5, "No valid dates found in data", cex = 1.2)
                    return()
                }

                # Create daily counts
                daily_counts <- table(dates)

                # Create a complete sequence of dates
                date_range <- seq(min(dates), max(dates), by = "day")
                all_counts <- integer(length(date_range))
                names(all_counts) <- date_range

                all_counts[names(daily_counts)] <- daily_counts
                cumulative_responses <- cumsum(all_counts)

                # Set up the plot area
                par(mar = c(4, 4, 2, 4))

                if (input$plot_type == "line") {
                    # Plot daily responses as a line
                    plot(date_range, all_counts,
                         type = "l",
                         col = "#3498db",
                         xlab = "Date",
                         ylab = "Daily Responses",
                         main = "Response Trend")

                    # Add points to the line
                    points(date_range, all_counts,
                           col = "#3498db",
                           pch = 16)

                } else {  # Bar graph
                    # Plot daily responses as bars
                    barplot_data <- barplot(all_counts,
                                            names.arg = date_range,
                                            col = "#3498db",
                                            xlab = "Date",
                                            ylab = "Daily Responses",
                                            main = "Response Trend",
                                            las = 2)  # Rotate x-axis labels

                    # Store the bar positions for the cumulative line
                    date_positions <- barplot_data
                }

                # Add cumulative line on secondary y-axis
                par(new = TRUE)
                if (input$plot_type == "line") {
                    plot(date_range, cumulative_responses,
                         type = "l",
                         col = "#e74c3c",
                         lwd = 2,
                         xaxt = "n",
                         yaxt = "n",
                         xlab = "",
                         ylab = "")
                } else {
                    plot(date_positions, cumulative_responses,
                         type = "l",
                         col = "#e74c3c",
                         lwd = 2,
                         xaxt = "n",
                         yaxt = "n",
                         xlab = "",
                         ylab = "")
                }

                # Add secondary axis
                axis(4, col = "#e74c3c", col.axis = "#e74c3c")
                mtext("Cumulative Responses", side = 4, line = 2, col = "#e74c3c")

                # Add legend
                if (input$plot_type == "line") {
                    legend_type <- c("Daily", "Cumulative")
                    legend_line <- c(1, 1)
                } else {
                    legend_type <- c("Daily (Bars)", "Cumulative")
                    legend_line <- c(0, 1)
                }

                legend("topleft",
                       legend = legend_type,
                       col = c("#3498db", "#e74c3c"),
                       lty = legend_line,
                       pch = if(input$plot_type == "line") c(16, NA) else c(NA, NA),
                       fill = if(input$plot_type == "bar") c("#3498db", NA) else c(NA, NA),
                       bg = "white")
            }, warning = function(w) {
                # Suppress all warnings
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

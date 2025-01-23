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
            shiny::tags$script("
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
        ")
        ),

        bslib::navset_pill_list(
            widths = c(2, 10),

            #Dashboard Page
            bslib::nav_panel(title = "Dashboard",

                        #DropDown Table Selection
                        shiny::selectInput(
                            "table_select",
                            "Choose a table to view:",
                            choices = c("Loading..." = ""),
                            selected = Sys.getenv("SD_TABLE", ""),
                            width = "20%",
                            selectize = FALSE
                        ),

                        #Basic Information
                        bslib::layout_column_wrap(
                            width = 1/3,
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
                            width = 1/2,
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
                                shiny::downloadButton("download_survey_data", "Download CSV",
                                                      class = "btn-sm btn-secondary")
                            ),
                            DT::dataTableOutput("survey_data_table")
                        )
            ),

            #Settings Page
            bslib::nav_panel(title = "Settings",
                         bslib::card(
                             bslib::card_header("Current Database Settings"),
                             bslib::card_body(
                                 shiny::tags$div(
                                     style = "display: grid; grid-template-columns: auto 1fr; gap: 10px; align-items: center;",
                                     shiny::tags$strong("Host:"), shiny::span(Sys.getenv("SD_HOST", "Not set")),
                                     shiny::tags$strong("Port:"), shiny::span(Sys.getenv("SD_PORT", "Not set")),
                                     shiny::tags$strong("Database:"), shiny::span(Sys.getenv("SD_DBNAME", "Not set")),
                                     shiny::tags$strong("User:"), shiny::span(Sys.getenv("SD_USER", "Not set")),
                                     shiny::tags$strong("GSS Mode:"), shiny::span(Sys.getenv("SD_GSSENCMODE", "Not set")),
                                     shiny::tags$strong("Default Table:"), shiny::span(Sys.getenv("SD_TABLE", "Not set"))
                                 )
                             )
                         ),
                         bslib::card(
                             bslib::card_header("Update Database Connection"),
                             bslib::card_body(
                                 shiny::textInput("host", "Host:", value = Sys.getenv("SD_HOST", "localhost")),
                                 shiny::textInput("port", "Port:", value = Sys.getenv("SD_PORT", "5432")),
                                 shiny::textInput("dbname", "Database Name:", value = Sys.getenv("SD_DBNAME", "postgres")),
                                 shiny::textInput("user", "User:", value = Sys.getenv("SD_USER", "username")),
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
                                             style = "margin-left: 10px; margin-top: 10px;",
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
                                 shiny::textInput("default_table", "Development Table:",
                                                  value = Sys.getenv("SD_TABLE", "responses")),
                                 shiny::div(
                                     style = "margin-top: 20px;",
                                     shiny::actionButton("test_connection", "Test Connection",
                                                         class = "btn-primary w-100")
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

        # Connection attempt function
        attempt_connection <- function(config = NULL) {
            tryCatch({
                if (is.null(config)) {
                    db <- sd_db_connect()
                } else {
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

        # Initial connection and table loading
        shiny::observe({
            # Try initial connection if not connected
            if (!rv$connection_status) {
                attempt_connection()
            }

            # Handle table selection based on connection status
            if (rv$connection_status && !is.null(rv$current_db)) {
                tryCatch({
                    # Get tables
                    tables <- pool::poolWithTransaction(rv$current_db$db, function(conn) {
                        DBI::dbListTables(conn)
                    })
                    filtered_tables <- tables[!grepl("^pg_", tables)]

                    # Get default table
                    default_table <- Sys.getenv("SD_TABLE", "")

                    if (default_table %in% filtered_tables) {
                        # Set default table first
                        filtered_tables <- c(default_table, setdiff(filtered_tables, default_table))
                        shiny::updateSelectInput(session, "table_select",
                                                 choices = filtered_tables,
                                                 selected = default_table
                        )
                    } else {
                        shiny::updateSelectInput(session, "table_select",
                                                 choices = if (length(filtered_tables) > 0) filtered_tables else c("No tables found" = "")
                        )
                    }
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

        # Handle test connection button
        shiny::observeEvent(input$test_connection, {
            # Close existing connection
            if (!is.null(rv$current_db) && !is.null(rv$current_db$db)) {
                tryCatch({
                    pool::poolClose(rv$current_db$db)
                    rv$current_db <- NULL
                    rv$connection_status <- FALSE
                }, error = function(e) {
                    warning("Error closing connection: ", e$message)
                })
            }

            # Test new connection
            config <- list(
                host = input$host,
                port = input$port,
                dbname = input$dbname,
                user = input$user,
                password = input$password,
                gssencmode = input$gssencmode
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
                    sprintf("SD_GSSENCMODE=%s", input$gssencmode),
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
        output$total_responses <- shiny::renderText({
            shiny::req(survey_data())
            nrow(survey_data())
        })


        output$daily_average <- shiny::renderText({
            shiny::req(survey_data())
            data <- survey_data()
            start_times <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M:%S")
            first_response <- min(start_times, na.rm = TRUE)
            last_response <- max(start_times, na.rm = TRUE)
            duration_days <- max(as.numeric(difftime(last_response, first_response, units = "days")), 1)
            round(nrow(data) / duration_days, 1)
        })

        output$completion_rate <- shiny::renderText({
            shiny::req(survey_data())
            data <- survey_data()
            total_responses <- nrow(data)
            completed_responses <- sum(!is.na(data$time_end) & data$time_end != "", na.rm = TRUE)
            if(total_responses > 0) {
                sprintf("%.1f%%", (completed_responses / total_responses) * 100)
            } else {
                "0.0%"
            }
        })

        # Response Trend Plot
        output$cumulative_trend <- shiny::renderPlot({
            data <- survey_data()
            # Get theme colors based on dark mode
            bg_color <- if (input$dark_mode == "dark") "#2c3e50" else "#ffffff"
            text_color <- if (input$dark_mode == "dark") "#ffffff" else "#1a2226"
            grid_color <- if (input$dark_mode == "dark") "gray30" else "gray80"
            line_color <- if (input$dark_mode == "dark") "#3498db" else "#0062cc"

            if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
                par(bg = bg_color, fg = text_color)
                plot.new()
                text(0.5, 0.5, "No data available to display", col = text_color, cex = 1.2)
                return()
            }

            dates <- try(as.Date(data$time_start))
            if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
                par(bg = bg_color, fg = text_color)
                plot.new()
                text(0.5, 0.5, "Unable to process date data", col = text_color, cex = 1.2)
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
            par(bg = bg_color, fg = text_color, col.axis = text_color, col.lab = text_color,
                mar = c(4, 4, 2, 2))

            # Add grid first
            plot(date_range, cumulative_responses, type = "n",
                 xlab = "Date", ylab = "Cumulative Responses")
            grid(col = grid_color, lty = "dotted")

            # Add line and points
            lines(date_range, cumulative_responses, col = line_color, lwd = 2)
            points(date_range, cumulative_responses, col = line_color, pch = 16)
        }, bg = "transparent")

        output$daily_trend <- shiny::renderPlot({
            data <- survey_data()
            # Get theme colors based on dark mode
            bg_color <- if (input$dark_mode == "dark") "#2c3e50" else "#ffffff"
            text_color <- if (input$dark_mode == "dark") "#ffffff" else "#1a2226"
            grid_color <- if (input$dark_mode == "dark") "gray30" else "gray80"
            bar_color <- if (input$dark_mode == "dark") "#3498db" else "#0062cc"
            bar_border <- if (input$dark_mode == "dark") "#00008B" else "#00008B"

            if (is.null(data) || nrow(data) == 0 || !("time_start" %in% names(data))) {
                par(bg = bg_color, fg = text_color)
                plot.new()
                text(0.5, 0.5, "No data available to display", col = text_color, cex = 1.2)
                return()
            }

            dates <- try(as.Date(data$time_start))
            if (inherits(dates, "try-error") || length(dates) == 0 || all(is.na(dates))) {
                par(bg = bg_color, fg = text_color)
                plot.new()
                text(0.5, 0.5, "Unable to process date data", col = text_color, cex = 1.2)
                return()
            }

            dates <- dates[!is.na(dates)]
            daily_counts <- table(dates)
            date_range <- seq(min(dates), max(dates), by = "day")
            all_counts <- integer(length(date_range))
            names(all_counts) <- date_range
            all_counts[names(daily_counts)] <- daily_counts

            # Plot setup with theme
            par(bg = bg_color, fg = text_color, col.axis = text_color, col.lab = text_color,
                mar = c(5, 4, 2, 2))  # Increased bottom margin for date labels

            # Create barplot
            bp <- barplot(all_counts,
                          col = bar_color,
                          border = bar_border,
                          xlab = "Date",
                          ylab = "Daily Responses",
                          xaxt = "n",
                          space = 0.2
            )

            # Add gridlines
            grid(col = grid_color, lty = "dotted")

            # Add x-axis with rotated labels
            axis(1,
                 at = bp,
                 labels = format(date_range, "%b %d"),
                 las = 2,
                 col.axis = text_color,
                 cex.axis = 0.9
            )
        }, bg = "transparent")

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
    shiny::shinyApp(ui, server)
}

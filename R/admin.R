#' Launch Admin App
#'
#' @description
#' Opens an admin interface to view survey responses and summary statistics
#'
#' @return No return value, called for side effects (opens admin interface)
#' @export
sd_admin <- function() {
    # Connect to database for initial table list
    local_db <- sd_db_connect()

    if (is.null(local_db)) {
        cli::cli_alert_warning("Database connection failed. Opening configuration dialog...")
        cli::cli_text("")

        # Launch the configuration gadget
        config <- db_config()

        # If user cancelled, stop
        if (is.null(config)) {
            cli::cli_alert_danger("Database configuration cancelled.")
            return(invisible())
        }

        # Try connecting again with new configuration
        local_db <- sd_db_connect()

        # If still failed, stop
        if (is.null(local_db)) {
            cli::cli_alert_danger("Database connection still failed after configuration.")
            return(invisible())
        }
    }

    # Get list of tables
    tables <- pool::poolWithTransaction(local_db$db, function(conn) {
        # Get all tables except system tables and admin tables
        tables <- DBI::dbListTables(conn)
        # Filter out pg_ tables (system) and _admin tables
        tables <- tables[!grepl("^pg_", tables)]
        return(tables)
    })

    # Define UI
    ui <- miniUI::miniPage(
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
                    margin-bottom: 20px;
                }
                .tab-content {
                    background: white;
                    padding: 20px;
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
            "))
        ),
        miniUI::gadgetTitleBar(
            "Survey Admin Dashboard",
            right = miniUI::miniTitleBarButton("done", "Close", primary = TRUE)
        ),
        miniUI::miniContentPanel(
            shiny::fluidRow(
                shiny::column(
                    width = 12,
                    shiny::selectInput(
                        "table_select",
                        "Select Survey Table:",
                        choices = tables,
                        selected = local_db$table,
                        width = "100%"
                    )
                )
            ),
            shiny::tabsetPanel(
                id = "tabs",
                # Summary Tab
                shiny::tabPanel(
                    "Summary",
                    shiny::div(
                        style = "margin: 20px 0;",
                        shiny::uiOutput("survey_stats")
                    ),
                    shiny::div(
                        style = "margin: 20px 0;",
                        shiny::plotOutput("response_trend", height = "300px")
                    )
                ),
                # Data Table Tab
                shiny::tabPanel(
                    "Responses",
                    shiny::div(
                        style = "margin: 20px 0;",
                        DT::DTOutput("survey_data_table")
                    )
                )
            )
        )
    )

    # Define server
    server <- function(input, output, session) {
        # Reactive table data
        survey_data <- shiny::reactive({
            pool::poolWithTransaction(local_db$db, function(conn) {
                DBI::dbGetQuery(conn, sprintf('SELECT * FROM "%s"', input$table_select))
            })
        })

        # Generate survey statistics
        output$survey_stats <- shiny::renderUI({
            data <- survey_data()

            # Calculate statistics
            total_respondents <- nrow(data)

            # Convert time_start to datetime and calculate time differences
            start_times <- as.POSIXct(data$time_start, format="%Y-%m-%d %H:%M:%S")

            if (length(start_times) > 0) {
                first_response <- min(start_times, na.rm = TRUE)
                last_response <- max(start_times, na.rm = TRUE)

                # Calculate duration in days (add 1 if same day)
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
            data <- survey_data()

            # Create the datatable with extended features
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
            pool::poolClose(local_db$db)
            shiny::stopApp(NULL)
        })

        # Close connection when session ends
        shiny::onSessionEnded(function() {
            pool::poolClose(local_db$db)
        })
    }

    # Run gadget
    shiny::runGadget(ui, server, viewer = shiny::browserViewer(browser = getOption("browser")))
}

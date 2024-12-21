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
        cli::cli_alert_warning("Failed to connect to the database - review your connection settings.")
        cli::cli_text("")
        db_fail_messages()
        stop()
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
        miniUI::gadgetTitleBar("Surveydown Admin Interface"),
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
                    "Survey Summary",
                    shiny::div(
                        style = "margin: 20px;",
                        shiny::uiOutput("survey_stats")
                    )
                ),
                # Data Table Tab
                shiny::tabPanel(
                    "Survey Data",
                    DT::DTOutput("survey_data_table")
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
            if(length(start_times) > 0) {
                first_response <- min(start_times, na.rm = TRUE)
                last_response <- max(start_times, na.rm = TRUE)

                # Calculate duration in days (add 1 if same day)
                survey_duration <- difftime(last_response, first_response, units = "days")
                duration_days <- max(as.numeric(survey_duration), 1)  # At least 1 day

                # Calculate daily average
                daily_avg <- round(total_respondents / duration_days, 1)

                # Format dates
                first_date <- format(first_response, "%Y-%m-%d")
                last_date <- format(last_response, "%Y-%m-%d")
            } else {
                first_date <- "No responses"
                last_date <- "No responses"
                daily_avg <- 0
                duration_days <- 0
            }

            # Create info boxes
            shiny::div(
                class = "survey-summary",
                style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px;",

                shiny::div(
                    style = "margin-bottom: 30px;",
                    shiny::h3(input$table_select, style = "margin-bottom: 20px;")
                ),

                shiny::div(
                    style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px;",

                    # Total Respondents
                    shiny::div(
                        style = "background: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        shiny::h4("Total Respondents"),
                        shiny::p(style = "font-size: 24px; font-weight: bold;", total_respondents)
                    ),

                    # Average Daily Responses
                    shiny::div(
                        style = "background: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        shiny::h4("Average Daily Responses"),
                        shiny::p(style = "font-size: 24px; font-weight: bold;", daily_avg)
                    ),

                    # Survey Duration
                    shiny::div(
                        style = "background: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                        shiny::h4("Survey Duration"),
                        shiny::p(style = "font-size: 24px; font-weight: bold;",
                                 if(duration_days == 1) "1 day" else sprintf("%.1f days", duration_days))
                    )
                ),

                # Additional Info
                shiny::div(
                    style = "margin-top: 20px; color: #666;",
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
        })

        # Data table
        output$survey_data_table <- DT::renderDT({
            DT::datatable(
                survey_data(),
                options = list(
                    scrollX = TRUE,
                    pageLength = 25,
                    dom = 'Bfrtip',
                    buttons = list(
                        list(
                            extend = 'csv',
                            text = 'Download CSV',
                            filename = input$table_select
                        )
                    )
                ),
                extensions = 'Buttons'
            )
        })

        # Close connection on exit
        shiny::onStop(function() {
            pool::poolClose(local_db$db)
        })
    }

    # Run gadget
    shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}

#' Launch Admin App
#'
#' Opens a Shiny gadget that provides administrative functionality for surveydown surveys,
#' including viewing survey responses and managing survey data.
#' @param table Character string. The name of the table to interact with.
#' Only used to override the `table` set in the current `.env` configuration
#' that was created using the `sd_db_config()` function. Defaults to `NULL`, in
#' which case the table specified in the `.env` file will be used.
#'
#' @return No return value, called for side effects (opens Shiny gadget)
#'
#' @export
sd_admin <- function(table = NULL) {
    if (is.null(table)) {
        local_db <- sd_db_connect()
    } else {
        local_db <- sd_db_connect(table = table)
    }

    if (is.null(local_db)) {
        cli::cli_alert_warning("Failed to connect to the database - review your connection settings.")
        cli::cli_text("")
        db_fail_messages()
        stop()
    }

    # Define UI
    ui <- miniUI::miniPage(
        miniUI::gadgetTitleBar("Surveydown Admin Interface"),
        miniUI::miniContentPanel(
            shiny::tabsetPanel(
                shiny::tabPanel(
                    "Survey Data",
                    DT::DTOutput("survey_data_table")
                )
            )
        )
    )

    # Define server
    server <- function(input, output, session) {
        # Create reactive to fetch data
        survey_data <- shiny::reactive({
            tryCatch({
                pool::poolWithTransaction(local_db$db, function(conn) {
                    # First check if table exists
                    table_exists <- DBI::dbExistsTable(conn, local_db$table)
                    if (!table_exists) {
                        stop(sprintf("Table '%s' does not exist in the database", local_db$table))
                    }

                    # Read the table data
                    query <- sprintf('SELECT * FROM "%s"', local_db$table)
                    DBI::dbGetQuery(conn, query)
                })
            }, error = function(e) {
                shiny::showNotification(
                    sprintf("Error reading data: %s", e$message),
                    type = "error",
                    duration = NULL
                )
                return(NULL)
            })
        })

        # Render data table
        output$survey_data_table <- DT::renderDT({
            data <- survey_data()

            if (is.null(data) || nrow(data) == 0) {
                return(DT::datatable(
                    data.frame(Message = "No data available"),
                    options = list(dom = 't'),
                    selection = 'none',
                    rownames = FALSE
                ))
            }

            DT::datatable(
                data,
                options = list(
                    scrollX = TRUE,
                    pageLength = 25,
                    dom = 'Bfrtip',
                    buttons = list(
                        list(
                            extend = 'copy',
                            text = 'Copy'
                        ),
                        list(
                            extend = 'csv',
                            text = 'CSV',
                            filename = local_db$table
                        ),
                        list(
                            extend = 'excel',
                            text = 'Excel',
                            filename = local_db$table
                        )
                    )
                ),
                extensions = 'Buttons'
            )
        })

        # Handle the Done button
        shiny::observeEvent(input$done, {
            pool::poolClose(local_db$db)
            shiny::stopApp()
        })
    }

    # Run the gadget
    shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}

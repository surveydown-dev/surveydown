#' Launch Admin Interface
#'
#' Opens a Shiny gadget that provides administrative functionality for surveydown surveys,
#' including viewing survey responses and managing survey data.
#'
#' @param db Optional. A database connection object created by sd_database().
#'   If provided, other database parameters will be ignored.
#' @param host Character string. The host address of the database.
#' @param dbname Character string. The name of the database.
#' @param port Integer. The port number for the database connection.
#' @param user Character string. The username for the database connection.
#' @param table Character string. The name of the table to interact with.
#' @param password Character string. The password for the database connection.
#'   Defaults to SURVEYDOWN_PASSWORD environment variable.
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to "prefer".
#'
#' @return No return value, called for side effects (opens Shiny gadget)
#'
#' @export
sd_admin <- function(
        db = NULL,
        host = NULL,
        dbname = NULL,
        port = NULL,
        user = NULL,
        table = NULL,
        password = Sys.getenv("SURVEYDOWN_PASSWORD"),
        gssencmode = "prefer"
) {
    # Database connection
    local_db <- NULL
    if (!is.null(db)) {
        # Use existing connection
        if (!"table" %in% names(db)) {
            stop("Invalid database connection object. Must be created using sd_database()")
        }
        local_db <- db
    } else {
        # Create new connection from parameters
        if (is.null(host) || is.null(dbname) || is.null(port) ||
            is.null(user) || is.null(table)) {
            stop("Either 'db' or all database connection parameters (host, dbname, port, user, table) must be provided")
        }

        local_db <- sd_database(
            host = host,
            dbname = dbname,
            port = port,
            user = user,
            table = table,
            password = password,
            gssencmode = gssencmode
        )
    }

    if (is.null(local_db)) {
        stop("Failed to connect to database. Please check your connection parameters.")
    }

    # UI
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

    # Server definition as shown on shiny gadgets
    server <- function(input, output, session) {
        # Create our table and adding download buttons like on adminV1
        output$survey_data_table <- DT::renderDT({
            data <- DBI::dbReadTable(local_db$db, local_db$table)
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

        # Adding a Done button to exit out of the gadget
        shiny::observeEvent(input$done, {
            if (!is.null(db)) {
                shiny::stopApp()
            } else {
                pool::poolClose(local_db$db)
                shiny::stopApp()
            }
        })
    }

    # Run the gadget
    shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}

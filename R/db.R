#' Connect to a 'PostgreSQL' Database with Automatic Cleanup
#'
#' This function establishes a connection pool to a 'PostgreSQL' database
#' (e.g. Supabase) and sets up automatic cleanup when the 'shiny' session ends.
#'
#' @param host Character string. The host address of the Supabase database.
#' @param dbname Character string. The name of the Supabase database.
#' @param port Integer. The port number for the Supabase database connection.
#' @param user Character string. The username for the Supabase database
#'   connection.
#' @param table Character string. The name of the table to interact with in
#'   the Supabase database.
#' @param password Character string. The password for the Supabase database
#'   connection. NOTE: While you can provide a hard-coded password here, we do
#'   NOT recommend doing so for security purposes. Instead, you should establish
#'   a password with `surveydown::sd_set_password()`, which will create a local
#'   `.Renviron` file that stores your password as a `SURVEYDOWN_PASSWORD`
#'    environment
#'   variable. The `password` argument uses this as the default value, so if you
#'   set a password properly with `surveydown::sd_set_password()`, then you can
#'   safely ignore using the `password` argument here.
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"prefer"`. NOTE: If you have verified all
#'   connection details are correct but still cannot access the database,
#'   consider setting this to `"disable"`. This can be necessary if you're on a
#'   secure connection, such as a VPN.
#' @param ignore Logical. If `TRUE`, data will be saved to a local CSV file
#'  instead of the database. Defaults to `FALSE`.
#' @param min_size Integer. The minimum number of connections in the pool.
#'  Defaults to 1.
#' @param max_size Integer. The maximum number of connections in the pool.
#'  Defaults to `Inf`.
#'
#' @return A list containing the database connection pool (`db`) and the table
#'  name (`table`), or `NULL` if in ignore mode or if there's an error.
#'
#' @examples
#' if (interactive()) {
#'   # Assuming SURVEYDOWN_PASSWORD is set in .Renviron
#'   db <- sd_database(
#'     host   = "aws-0-us-west-1.pooler.supabase.com",
#'     dbname = "postgres",
#'     port   = "6---",
#'     user   = "postgres.k----------i",
#'     table  = "your-table-name",
#'     ignore = FALSE
#'   )
#'
#'   # Print the structure of the connection
#'   str(db)
#'
#'   # Close the connection pool when done
#'   if (!is.null(db)) {
#'     pool::poolClose(db$db)
#'   }
#' }
#' @export
sd_database <- function(
    host       = NULL,
    dbname     = NULL,
    port       = NULL,
    user       = NULL,
    table      = NULL,
    password   = Sys.getenv("SURVEYDOWN_PASSWORD"),
    gssencmode = "prefer",
    ignore     = FALSE,
    min_size   = 1,
    max_size   = Inf
) {

    if (ignore) {
        message("Database connection ignored. Saving data to local CSV file.")
        return(NULL)
    }

    # Authentication/Checks for NULL Values
    if (is.null(host) | is.null(dbname) | is.null(port) | is.null(user) | is.null(table)) {
        message("One or more of the required arguments in sd_database() are NULL, so the database is NOT connected; writing responses to local data.csv file *for previewing purposes only*.")
        return(NULL)
    }

    if (!nchar(password)) {
        stop("Please define your password using surveydown::sd_set_password().\n If you just did this, RESTART the R session to make sure the password environment variable is loaded.")
    }

    tryCatch({
        pool <- pool::dbPool(
            RPostgres::Postgres(),
            host = host,
            dbname = dbname,
            port = port,
            user = user,
            password = password,
            gssencmode = gssencmode,
            minSize = min_size,
            maxSize = max_size
        )

        # Set up automatic cleanup when the Shiny session ends
        shiny::onStop(function() {
            pool::poolClose(pool)
        })

        message("Successfully connected to the database.")
        return(list(db = pool, table = table))
    }, error = function(e) {
        stop(paste("Error: Failed to connect to the database.",
                   "Details:", conditionMessage(e),
                   "\nPlease check your connection details:",
                   "\n- host:    ", host,
                   "\n- dbname:  ", dbname,
                   "\n- port:    ", port,
                   "\n- user:    ", user,
                   "\nTo update password, please use surveydown::sd_set_password().",
                   "\nIf you have verified all connection details are correct but still cannot access the database, consider setting the 'gssencmode' parameter to 'disable' in the sd_database() function."))
    })
}

#' Fetch data from a database table with automatic reactivity detection
#'
#' This function retrieves all data from a specified table in a database.
#' It automatically detects whether it's being used in a reactive context
#' (e.g., within a 'shiny' application) and behaves accordingly. In a reactive
#' context, it returns a reactive expression that automatically refreshes
#' the data at specified intervals.
#'
#' @param db A list containing database connection details created using
#'  `sd_database()`. Must have elements:
#'   \itemize{
#'     \item `db`: A `DBI` database connection object
#'     \item `table`: A string specifying the name of the table to query
#'   }
#' @param refresh_interval Numeric. The time interval (in seconds) between data
#'  refreshes when in a reactive context. Default is `NULL`, meaning the data
#'  will not refresh.
#'
#' @return In a non-reactive context, returns a data frame containing all rows
#' and columns from the specified table. In a reactive context, returns a
#' reactive expression that, when called, returns the most recent data from
#' the specified database table.
#'
#' @examples
#' # Non-reactive context example
#' \dontrun{
#'   library(surveydown)
#'
#'   # Assuming you have a database connection called db created using
#'   # sd_database(), you can fetch data with:
#'
#'   data <- sd_get_data(db)
#'   head(data)
#'
#'   # Reactive context example (inside a surveydown app)
#'
#'   server <- function(input, output, session) {
#'     data <- sd_get_data(db, refresh_interval = 10)
#'
#'     output$data_table <- renderTable({
#'       data()  # Note the parentheses to retrieve the reactive value
#'     })
#'   }
#' }
#' @export
sd_get_data <- function(db, refresh_interval = NULL) {
    if (is.null(db)) {
        warning("Database is not connected, db is NULL")
        return(NULL)
    }
    fetch_data <- function() {
        pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbReadTable(conn, db$table)
        })
    }
    if (!is.null(refresh_interval)) {
        if (is.null(shiny::getDefaultReactiveDomain())) {
            stop('If refresh_interval is set to a positive number, sd_get_data() must be called within a reactive context for the data to continously update in the server.')
        }
        if (!is.numeric(refresh_interval)) {
            stop('refresh_interval must be a positive number')
        }
        if (refresh_interval < 0) {
            stop('refresh_interval must be a positive number')
        }
        return(shiny::reactive({
            shiny::invalidateLater(refresh_interval * 1000)
            fetch_data()
        }))
    } else {
        # If not in a reactive context, just return the data once
        return(fetch_data())
    }
}

# Convert to SQL
r_to_sql_type <- function(r_type) {
    switch(toupper(r_type),
           CHARACTER = "TEXT",
           INTEGER = "TEXT",
           DOUBLE = "TEXT",
           LOGICAL = "TEXT",
           FACTOR = "TEXT",
           "TEXT")
}

create_table <- function(data_list, db, table) {
    # Create column definitions
    col_def <- sapply(names(data_list), function(col_name) {
        r_type <- typeof(data_list[[col_name]])
        sql_type <- r_to_sql_type(r_type)
        paste0('"', col_name, '" ', sql_type)
    })

    # Ensure session_id is the first column and set as PRIMARY KEY
    col_def <- c('"session_id" TEXT PRIMARY KEY', col_def[names(col_def) != "session_id"])

    # Join column definitions
    col_def_str <- paste(col_def, collapse = ", ")

    create_table_query <- paste0(
        'CREATE TABLE IF NOT EXISTS "', table, '" (', col_def_str, ")"
    )

    pool::poolWithTransaction(db, function(conn) {
        # Create the table
        DBI::dbExecute(conn, create_table_query)

        # Enable Row Level Security
        DBI::dbExecute(conn, paste0('ALTER TABLE "', table, '" ENABLE ROW LEVEL SECURITY;'))
    })

    message(paste("Table", table, "created in the database."))
}

# Solution found in this issue:
# https://github.com/r-dbi/DBI/issues/193
sqlInterpolateList <- function(conn, sql, vars=list(), list_vars=list()) {
    if (length(list_vars) > 0) {
        for (name in names(list_vars)) {
            sql <- sub(paste0("\\?", name), paste("?", name, "_list_var", 1:length(list_vars[[name]]), sep="", collapse=" , "), sql)
        }
        list_vars <- lapply(list_vars, function(sublist) {
            names(sublist) <- paste0("list_var", 1:length(sublist))
            sublist
        }) |> unlist()
        # unlist gives names as "outer.inner" but DBI doesn't like names with periods
        names(list_vars) <- sub("\\.", "_", names(list_vars))
        vars <- c(vars, list_vars)
    }
    DBI::sqlInterpolate(conn, sql, .dots=vars)
}

database_uploading <- function(data_list, db, table, changed_fields) {
    if(is.null(db)) return(warning("Databasing is not in use"))

    tryCatch({
        pool::poolWithTransaction(db, function(conn) {
            # Get the actual columns in the table
            existing_cols <- DBI::dbListFields(conn, table)

            # Check for new fields
            new_fields <- setdiff(names(data_list), existing_cols)
            if (length(new_fields) > 0) {
                for (field in new_fields) {
                    DBI::dbExecute(conn, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" TEXT', table, field))
                }
                existing_cols <- c(existing_cols, new_fields)
            }

            # Filter data_list to only include existing columns and changed fields
            data_list <- data_list[names(data_list) %in% c("session_id", intersect(changed_fields, existing_cols))]

            # If there's nothing to update (only session_id), return early
            if (length(data_list) <= 1) return()

            # Ensure session_id is the first column
            cols <- c("session_id", setdiff(names(data_list), "session_id"))
            data_list <- data_list[cols]

            # Prepare SQL components
            placeholders <- paste0("?", names(data_list))
            update_cols <- setdiff(cols, "session_id")
            update_set <- paste(sapply(update_cols, function(col) {
                sprintf('"%s" = EXCLUDED."%s"', col, col)
            }), collapse = ", ")

            # Build and execute query
            query_template <- sprintf(
                'INSERT INTO "%s" ("%s") VALUES (%s) ON CONFLICT (session_id) DO UPDATE SET %s',
                table,
                paste(cols, collapse = '", "'),
                paste(placeholders, collapse = ", "),
                update_set
            )

            query <- sqlInterpolateList(conn, query_template, list_vars = data_list)
            DBI::dbExecute(conn, query)
        })
    }, error = function(e) {
        warning("Error in database operation: ", e$message)
    })
}

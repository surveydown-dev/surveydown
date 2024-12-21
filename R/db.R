#' Create a database configuration file
#'
#' @param path Character string. Directory where .env should be created
#' @param overwrite Logical. Whether to overwrite existing .env
#'
#' @export
sd_create_db_config <- function(path = getwd(), overwrite = FALSE) {
    env_file <- file.path(path, ".env")
    if (file.exists(env_file) && !overwrite) {
        stop(".env already exists. Use overwrite = TRUE to replace it.")
    }

    # Interactive setup
    cat("\nDatabase Configuration Setup\n")
    cat("Press Enter to keep default value shown in brackets\n\n")

    host <- readline("Host [localhost]: ")
    if (host == "") host <- "localhost"

    dbname <- readline("Database name [postgres]: ")
    if (dbname == "") dbname <- "postgres"

    port <- readline("Port [5432]: ")
    if (port == "") port <- "5432"

    user <- readline("User: ")
    password <- readline("Password: ")

    table <- readline("Table name [responses]: ")
    if (table == "") table <- "responses"

    gssencmode <- readline("GSS encryption mode [prefer]: ")
    if (gssencmode == "") gssencmode <- "prefer"

    # Create template content with user input
    template <- paste(
        "# Database connection settings for surveydown",
        sprintf("SD_HOST=%s", host),
        sprintf("SD_DBNAME=%s", dbname),
        sprintf("SD_PORT=%s", port),
        sprintf("SD_USER=%s", user),
        sprintf("SD_TABLE=%s", table),
        sprintf("SD_PASSWORD=%s", password),
        sprintf("SD_GSSENCMODE=%s", gssencmode),
        sep = "\n"
    )

    # Write template to file
    writeLines(template, env_file)

    # Add to .gitignore
    gitignore_path <- file.path(path, ".gitignore")
    if (file.exists(gitignore_path)) {
        gitignore_content <- readLines(gitignore_path)
        if (!".env" %in% gitignore_content) {
            write("\n.env", gitignore_path, append = TRUE)
        }
    } else {
        write(".env", gitignore_path)
    }

    usethis::ui_done("Created .env file with your settings")
    usethis::ui_done("Added .env to .gitignore")

    # Show the contents
    cat("\nYour database configuration:\n")
    cat(readLines(env_file), sep = "\n")
    cat("\nTo modify these settings later, run sd_create_db_config() with overwrite = TRUE\n")
}

#' Show the database configuration settings
#'
#' This function displays the current database configuration settings from the .env file.
#' It includes a confirmation step before showing sensitive information like passwords.
#'
#' @param path Character string. Directory containing the .env file. Defaults to current directory.
#'
#' @return Invisibly returns NULL. Called for its side effects (printing to console).
#'
#' @export
sd_show_db_config <- function(path = getwd()) {
    env_file <- file.path(path, ".env")

    # Check if .env file exists
    if (!file.exists(env_file)) {
        usethis::ui_oops("No .env file found in the current directory.")
        usethis::ui_todo("Use sd_create_db_config() to create one.")
        return(invisible(NULL))
    }

    # Confirm before showing settings
    if (usethis::ui_yeah("Are you sure you want to display your database configuration in the console?")) {
        # Read and parse env file
        env_contents <- readLines(env_file)

        # Filter out comments and empty lines
        settings <- env_contents[grepl("^SD_", env_contents)]

        usethis::ui_info("Current database configuration:")
        usethis::ui_line()
        cat(settings, sep = "\n")
        usethis::ui_line()

        usethis::ui_info("To modify these settings, run sd_create_db_config() with overwrite = TRUE")
    } else {
        usethis::ui_info("Database configuration display cancelled.")
    }

    invisible(NULL)
}

#' Connect to database using configuration from .env
#'
#' @param table Character string. Optional. Override the table name from .env
#' @param env_file Character string. Path to the env file. Defaults to ".env"
#' @param ignore Logical. If TRUE, data will be saved to a local CSV file instead
#'   of the database. Defaults to FALSE.
#'
#' @return A list containing the database connection pool (`db`) and table name (`table`),
#'   or NULL if ignore is TRUE
#' @export
sd_connect <- function(table = NULL, env_file = ".env", ignore = FALSE) {
    if (ignore) {
        message("Database connection ignored. Saving data to local CSV file.\n")
        return(NULL)
    }

    # Load environment variables
    if (!file.exists(env_file)) {
        stop("No .env file found. Run sd_create_db_config() to create a template.")
    }
    dotenv::load_dot_env(env_file)

    # Get all required parameters
    params <- list(
        host = Sys.getenv("SURVEYDOWN_HOST"),
        dbname = Sys.getenv("SURVEYDOWN_DBNAME"),
        port = Sys.getenv("SURVEYDOWN_PORT"),
        user = Sys.getenv("SURVEYDOWN_USER"),
        password = Sys.getenv("SURVEYDOWN_PASSWORD"),
        gssencmode = Sys.getenv("SURVEYDOWN_GSSENCMODE", "prefer"),
        table = if (!is.null(table)) table else Sys.getenv("SURVEYDOWN_TABLE")
    )

    # Check for missing required parameters
    missing <- names(params)[!nchar(unlist(params))]
    if (length(missing) > 0) {
        stop(
            "Missing required environment variables: ",
            paste(missing, collapse = ", "), "\n",
            "Please check your .env file"
        )
    }

    # Create connection
    tryCatch({
        pool <- pool::dbPool(
            RPostgres::Postgres(),
            host = params$host,
            dbname = params$dbname,
            port = params$port,
            user = params$user,
            password = params$password,
            gssencmode = params$gssencmode
        )

        message("Successfully connected to the database.")
        return(list(db = pool, table = params$table))

    }, error = function(e) {
        stop(paste("Error: Failed to connect to the database.",
                   "Details:", conditionMessage(e),
                   "\nPlease check your connection details in .env"))
    })
}

#' #' Create a database configuration file
#' #'
#' #' @description
#' #' Creates a YAML file that stores database connection parameters.
#' #' The file will be used for establishing database connections.
#' #'
#' #' @param host Character string. The host address of the database.
#' #' @param dbname Character string. The name of the database.
#' #' @param port Character string or integer. The port number for the connection.
#' #' @param user Character string. The username for the database connection.
#' #' @param path Character string. Path where to create the configuration file.
#' #'   Defaults to the current working directory.
#' #'
#' #' @return Invisible NULL. Called for its side effects.
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   sd_create_db_config(
#' #'     host = "your-host.example.com",
#' #'     dbname = "your-database",
#' #'     port = "5432",
#' #'     user = "your-username"
#' #'   )
#' #' }
#' #' @export
#' sd_create_db_config <- function(host, dbname, port, user, path = getwd()) {
#'     if (missing(host) || missing(dbname) || missing(port) || missing(user)) {
#'         stop("All parameters (host, dbname, port, user) are required")
#'     }
#'
#'     # Create the configuration structure
#'     config <- list(
#'         connection = list(
#'             host = host,
#'             dbname = dbname,
#'             port = as.character(port),
#'             user = user
#'         )
#'     )
#'
#'     # Define the file path
#'     file_path <- file.path(path, "connectionParam.yml")
#'
#'     # Check if file already exists
#'     if (file.exists(file_path)) {
#'         stop("connectionParam.yml already exists. Remove it first if you want to create a new configuration.")
#'     }
#'
#'     # Write to YAML with a header comment
#'     header <- paste(
#'         "# Surveydown database connection parameters",
#'         "# Generated by sd_create_db_config()",
#'         "# Do not edit manually unless you know what you're doing",
#'         "",
#'         sep = "\n"
#'     )
#'
#'     yaml_content <- paste0(header, yaml::as.yaml(config))
#'     writeLines(yaml_content, con = file_path)
#'
#'     # Add to .gitignore
#'     gitignore_path <- file.path(path, ".gitignore")
#'     if (file.exists(gitignore_path)) {
#'         gitignore_content <- readLines(gitignore_path)
#'         if (!"connectionParam.yml" %in% gitignore_content) {
#'             write(c(gitignore_content, "connectionParam.yml"), gitignore_path)
#'         }
#'     } else {
#'         write("connectionParam.yml", gitignore_path)
#'     }
#'
#'     message(
#'         "Created database configuration file at: ", file_path,
#'         "\nFile has been added to .gitignore for security."
#'     )
#'     invisible(NULL)
#' }
#'
#' #' database connection function
#' #'
#' #' @description
#' #' reads configuration file and establishes database connection
#' #'
#' #' @param table character string. name of the table to interact with
#' #' @param ignore logical. if TRUE, saves data to local CSV file instead of database. defaults to FALSE
#' #' @param gssencmode character string. GSS encryption mode for database connection. defaults to "prefer"
#' #' @param min_size integer. minimum number of connections in pool. defaults to 1
#' #' @param max_size integer. maximum number of connections in pool. defaults to Inf
#' #'
#' #' @return list containing database connection pool (db) and table name, or NULL if in ignore mode
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   # create connection with default parameters
#' #'   db <- sd_connect(table = "responses")
#' #'
#' #'   # create connection with custom pool size
#' #'   db <- sd_connect(
#' #'     table = "responses",
#' #'     min_size = 2,
#' #'     max_size = 10
#' #'   )
#' #' }
#' #' @export
#' sd_connect <- function(
#'         table = NULL,
#'         ignore = FALSE,
#'         gssencmode = "prefer",
#'         min_size   = 1,
#'         max_size   = Inf
#'         )
#'     {
#'     if (ignore) {
#'         message("Database connection ignored. Saving data to local CSV file.\n")
#'         return(NULL)
#'     }
#'
#'     if (is.null(table)) {
#'         stop("Table name must be provided")
#'     }
#'
#'     # Check for config file
#'     config_path <- "connectionParam.yml"
#'     if (!file.exists(config_path)) {
#'         stop("No connectionParam.yml found. Create one using sd_create_db_config()")
#'     }
#'
#'     # Read configuration
#'     tryCatch({
#'         raw_yaml <- yaml::read_yaml(config_path)
#'         config <- raw_yaml$connection
#'
#'         if (is.null(config)) {
#'             stop("Invalid configuration file format. Missing 'connection' section.")
#'         }
#'
#'         # Get password from environment
#'         password <- Sys.getenv("SURVEYDOWN_PASSWORD")
#'         if (!nchar(password)) {
#'             stop("Please define your password using surveydown::sd_set_password()")
#'         }
#'
#'         # Create connection
#'         pool <- pool::dbPool(
#'             RPostgres::Postgres(),
#'             host = config$host,
#'             dbname = config$dbname,
#'             port = config$port,
#'             user = config$user,
#'             password = password,
#'             gssencmode = gssencmode,
#'             minSize = min_size,
#'             maxSize = max_size
#'         )
#'
#'         message("Successfully connected to the database.")
#'         return(list(db = pool, table = table))
#'     }, error = function(e) {
#'         stop("Failed to connect to database: ", e$message)
#'     })
#' }

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

    # v0.8.0
    .Deprecated("sd_connect")

    if (ignore) {
        message("Database connection ignored. Saving data to local CSV file.\n")
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
        # Check if table exists first
        table_exists <- pool::poolWithTransaction(db$db, function(conn) {
            DBI::dbExistsTable(conn, db$table)
        })

        if (!table_exists) {
            return(NULL)
        }

        # Only try to read if table exists
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

    message(paste0('Table "', table, '" created in the database.'))
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
    if(is.null(db)) {
        return(warning("Databasing is not in use"))
    }
    tryCatch({
        pool::poolWithTransaction(db, function(conn) {
            # Get the actual columns in the table
            existing_cols <- DBI::dbListFields(conn, table)

            # Check for new fields
            new_fields <- setdiff(names(data_list), existing_cols)
            if (length(new_fields) > 0) {
                # Add new fields to the table
                for (field in new_fields) {
                    DBI::dbExecute(conn, sprintf('ALTER TABLE "%s" ADD COLUMN "%s" TEXT', table, field))
                }
                # Update existing_cols
                existing_cols <- c(existing_cols, new_fields)
            }

            # Filter data_list to only include existing columns and changed fields
            data_list <- data_list[names(data_list) %in% c("session_id", intersect(changed_fields, existing_cols))]

            # If there's nothing to update (only session_id), return early
            if (length(data_list) <= 1) {
                return()
            }

            # Ensure session_id is the first column
            cols <- c("session_id", setdiff(names(data_list), "session_id"))
            data_list <- data_list[cols]

            # Prepare the placeholders
            placeholders <- paste0("?", names(data_list))

            # Prepare the update set
            update_cols <- setdiff(cols, "session_id")
            update_set <- paste(sapply(update_cols, function(col) {
                sprintf('"%s" = EXCLUDED."%s"', col, col)
            }), collapse = ", ")

            # Prepare the SQL query template
            query_template <- sprintf(
                'INSERT INTO "%s" ("%s") VALUES (%s) ON CONFLICT (session_id) DO UPDATE SET %s',
                table,
                paste(cols, collapse = '", "'),
                paste(placeholders, collapse = ", "),
                update_set
            )

            # Use sqlInterpolateList to safely insert values
            query <- sqlInterpolateList(
                conn,
                query_template,
                list_vars = data_list
            )

            # Execute the query
            DBI::dbExecute(conn, query)
        })
    }, error = function(e) {
        warning("Error in database operation: ", e$message)
        print(e)  # Print the full error for debugging
    })
}

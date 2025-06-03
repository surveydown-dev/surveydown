bot_checker <- function(db, ignore_mode, session_id) {

    #This function will check multiple different parameters as set by numerous studies
    #Here we will set a value between 0 - 3 where 0.5 represents the ideal human,



    # Get user data for this session
    if (ignore_mode) {
        df <- if (file.exists("preview_data.csv")) {
            utils::read.csv("preview_data.csv", stringsAsFactors = FALSE)
        } else {
            return()
        }
    } else {
        df <- sd_get_data(db)
    }

    user_data <- df[df$session_id == session_id, ]

    if (nrow(user_data) == 0) {
        warning("No data found for session_id: ", session_id)
        return()
    }

    current_bot_value <- as.numeric(user_data$is_bot)


    #------------------------- ALL CONDITIONS GO HERE -------------------------

    # If user is too fast, update is_bot to 2
    if (is_fast(user_data)) {
        # Create data_list for database update
        current_bot_value <- current_bot_value + 2
    }


    suspicious_time_instances <- start_time_checker(user_data, df)

    if(suspicious_time_instances$boolean) {
        current_bot_value <- current_bot_value + suspicious_time_instances$value
    }


    # Future: Add other checks here
    # is_straightlining(user_data)
    # multiple_ips(user_data)
    # Similar start times e.g. lets say time_start is +/- a few seconds of other table instances we update both values, the more instances the higher the penalty




    data_list <- list(
        session_id = session_id,
        is_bot = as.character(current_bot_value)
    )


    #------------------------- END OF BOT CONDITIONS -------------------------

    if(current_bot_value != as.numeric(user_data$is_bot)) {
        # Define which fields we're updating
        fields <- "is_bot"

        # Use appropriate update method based on mode
        if (ignore_mode) {
            if (file.access('.', 2) == 0) {
                tryCatch({
                    # Read existing data
                    existing_data <- if (file.exists("preview_data.csv")) {
                        utils::read.csv("preview_data.csv", stringsAsFactors = FALSE)
                    } else {
                        data.frame()
                    }
                    # Find if this session_id already exists
                    session_idx <- which(existing_data$session_id == data_list$session_id)
                    if (length(session_idx) > 0) {
                        # Update existing session data
                        existing_data[session_idx, "is_bot"] <- data_list[["is_bot"]]
                        updated_data <- existing_data
                    } else {
                        # This shouldn't happen since we're updating existing data
                        warning("Session not found in existing data for update")
                        return()
                    }
                    # Write updated data back to file
                    utils::write.csv(
                        updated_data,
                        "preview_data.csv",
                        row.names = FALSE,
                        na = ""
                    )
                }, error = function(e) {
                    warning("Unable to write to preview_data.csv: ", e$message)
                    message("Error details: ", e$message)
                })
            } else {
                message("Running in a non-writable environment.")
            }
        } else {
            # Database mode
            database_uploading(
                data_list = data_list,
                db = db$db,
                table = db$table,
                changed_fields = fields
            )
        }
    }
}

is_fast <- function(user_data) {
    # Filter to only question time columns
    cols_to_keep <- c("time_start", "time_end",
                      names(user_data)[grepl("time_q", names(user_data))])
    filtered_data <- user_data[, cols_to_keep]

    # Get only the time_q columns (exclude time_start and time_end)
    time_q_cols <- names(filtered_data)[grepl("^time_q", names(filtered_data))]

    # Initialize vector to store time differences
    question_times <- numeric(length(time_q_cols))
    names(question_times) <- time_q_cols

    # Start with time_start as the baseline
    previous_time <- filtered_data$time_start

    # Process each time_q column in order
    for (col in time_q_cols) {
        current_time <- filtered_data[[col]]

        if (!is.na(current_time) && current_time != "") {
            # Convert timestamps to POSIXct
            prev_parsed <- as.POSIXct(previous_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
            curr_parsed <- as.POSIXct(current_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

            # Calculate difference in seconds
            time_diff_seconds <- as.numeric(difftime(curr_parsed, prev_parsed, units = "secs"))

            # Store the time difference
            question_times[col] <- round(time_diff_seconds, 2)

            # Update previous_time for next calculation
            previous_time <- current_time

        } else {
            question_times[col] <- NA
        }
    }

    # Remove NA values
    valid_times <- question_times[!is.na(question_times)]

    # Define thresholds for "too fast"
    VERY_FAST_THRESHOLD <- 2
    FAST_THRESHOLD <- 4

    num_valid_questions <- length(valid_times)
    num_very_fast <- sum(valid_times < VERY_FAST_THRESHOLD)
    num_fast <- sum(valid_times < FAST_THRESHOLD)

    # Determine if user is answering too fast
    # Criteria: More than 50% of questions answered in under 5 seconds
    # OR more than 25% answered in under 1 second
    is_too_fast <- (num_fast / num_valid_questions >= 0.5) ||
        (num_very_fast / num_valid_questions >= 0.25)

    return(is_too_fast)
}


start_time_checker <-  function(user_data, df) {



    return()
}



bot_checker <- function(db, ignore_mode, session_id) {

    #This function will check multiple different parameters as set by numerous studies
    #Here we will set a value between 0 - 3 where 0.5 represents the ideal human, 1.5 represents a "bad human", and 2.5 represents a bot
    #Values way above 2.5 result in highly confident guesses that the user is a bot

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
    sessions_to_update <- list()

    #------------------------- ALL CONDITIONS GO HERE -------------------------

    # Check if user is too fast
    if (is_fast(user_data)) {
        current_bot_value <- current_bot_value + 1.5
        sessions_to_update[[session_id]] <- current_bot_value
    }

    suspicious_time_instances <- start_time_checker(user_data, df)
    if(suspicious_time_instances$boolean) {
        penalty <- suspicious_time_instances$value

        current_bot_value <- current_bot_value + penalty
        sessions_to_update[[session_id]] <- current_bot_value

        # Update ALL suspicious sessions found
        for (suspicious_session in suspicious_time_instances$session_ids) {
            suspicious_row <- df[df$session_id == suspicious_session, ]
            if (nrow(suspicious_row) > 0) {
                suspicious_bot_value <- as.numeric(suspicious_row$is_bot) + penalty
                sessions_to_update[[suspicious_session]] <- suspicious_bot_value
            }
        }
    }

    # Check for IP violations
    ip_violations <- ip_checker(user_data, df)
    if(ip_violations$boolean) {
        penalty <- ip_violations$value

        # Update current session
        current_bot_value <- current_bot_value + penalty
        sessions_to_update[[session_id]] <- current_bot_value

        # Update ALL sessions with same IP
        for (ip_session in ip_violations$session_ids) {
            # Get current bot value for this IP session
            ip_row <- df[df$session_id == ip_session, ]
            if (nrow(ip_row) > 0) {
                ip_bot_value <- as.numeric(ip_row$is_bot) + penalty
                sessions_to_update[[ip_session]] <- ip_bot_value
            }
        }
    }

    #------------------------- END OF BOT CONDITIONS -------------------------

    # Update all flagged sessions
    if (length(sessions_to_update) > 0) {
        for (update_session_id in names(sessions_to_update)) {
            new_bot_value <- sessions_to_update[[update_session_id]]

            data_list <- list(
                session_id = update_session_id,
                is_bot = as.character(new_bot_value)
            )

            # Update storage - Edit this section after dev
            if (ignore_mode) {
                update_local_csv_session(update_session_id, new_bot_value)
            } else {
                database_uploading(
                    data_list = data_list,
                    db = db$db,
                    table = db$table,
                    changed_fields = "is_bot"
                )
            }
        }
    }
}

#------------------------- ALL HELPER FUNCTIONS BELOW  -------------------------


#This will be removed after dev
update_local_csv_session <- function(session_id, new_bot_value) {
    if (file.access('.', 2) == 0) {
        tryCatch({
            existing_data <- utils::read.csv("preview_data.csv", stringsAsFactors = FALSE)
            session_idx <- which(existing_data$session_id == session_id)

            if (length(session_idx) > 0) {
                existing_data[session_idx, "is_bot"] <- as.character(new_bot_value)
                utils::write.csv(existing_data, "preview_data.csv", row.names = FALSE, na = "")
            }
        }, error = function(e) {
            warning("Unable to update local CSV for session ", session_id, ": ", e$message)
        })
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


start_time_checker <- function(user_data, df) {
    # Get the user's start time
    user_start_time <- as.POSIXct(user_data$time_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    user_session_id <- user_data$session_id

    # Initialize counter and list
    under_5_seconds_count <- 0
    suspicious_session_ids <- c()

    # Loop through all rows in df
    for (i in 1:nrow(df)) {
        row_session_id <- df$session_id[i]

        if (row_session_id == user_session_id) {
            next
        }

        row_start_time <- as.POSIXct(df$time_start[i], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

        time_diff <- abs(as.numeric(difftime(row_start_time, user_start_time, units = "secs")))

        # If difference is under 5 seconds, add to counter and list
        if (time_diff < 5) {
            under_5_seconds_count <- under_5_seconds_count + 1
            suspicious_session_ids <- c(suspicious_session_ids, row_session_id)
        }
    }


    is_suspicious <- under_5_seconds_count > 2

    # Determine penalty value based on severity
    penalty_value <- if (under_5_seconds_count >= 5) {
        1
    } else if (under_5_seconds_count > 2) {
        0.5
    } else {
        0
    }

    return(list(
        count = under_5_seconds_count,
        session_ids = suspicious_session_ids,
        boolean = is_suspicious,
        value = penalty_value
    ))
}

#------------------------- Research more on IPs before fully implementing this -------------------------

ip_checker <- function(user_data, df) {
    user_session_id <- user_data$session_id
    user_client_ip <- user_data$client_ip  # Store the user's IP

    if (is_institutional_ip(user_client_ip)) {
        return(list(count = 0, session_ids = c(), boolean = FALSE, value = 0))
    }

    similar_ip_count <- 0
    suspicious_session_ids <- c()

    for(i in 1:nrow(df)) {
        row_session_id <- df$session_id[i]

        if (row_session_id == user_session_id) {
            next
        }

        if(df$client_ip[i] == user_client_ip) {
            similar_ip_count <- similar_ip_count + 1
            suspicious_session_ids <- c(suspicious_session_ids, row_session_id)
        }
    }

    is_suspicious <- similar_ip_count > 2

    # Determine penalty value based on severity
    penalty_value <- if (similar_ip_count >= 5) {
        1
    } else if (similar_ip_count > 2) {
        0.5
    } else {
        0
    }

    return(list(
        count = similar_ip_count,
        session_ids = suspicious_session_ids,
        boolean = is_suspicious,
        value = penalty_value
    ))
}

is_institutional_ip <- function(ip) {
    # Common institutional/corporate IP patterns
    institutional_patterns <- c(
        "10\\.",        # Private Class A
        "172\\.(1[6-9]|2[0-9]|3[0-1])\\.",  # Private Class B
        "192\\.168\\.", # Private Class C
        "127\\.",
        "192\\.164\\." # GWU For some reason <- This may be a problem especially if other schools are similar this route may not be viable. (Studies also say the same)
    )
    any(sapply(institutional_patterns, function(p) grepl(p, ip)))
}






















#This function will check multiple different parameters as set by numerous studies
#Here we will set a value between 0 - 3 where 0.5 represents the ideal human, 1.5 represents a "bad human", and 2.5 represents a bot
#Values way above 2.5 result in highly confident guesses that the user is a bot





# THERE MAY BE A BUG IN THE WAY QUESTION TIMES ARE CALCULATED BE AWARE BOGDAN


bot_checker <- function(db, ignore_mode, session_id, question_labels = NULL) {
    if (ignore_mode) {
        df <- utils::read.csv("preview_data.csv", stringsAsFactors = FALSE)
    } else {
        tryCatch({
            conn <- pool::poolCheckout(db$db)
            df <- DBI::dbGetQuery(conn, sprintf('SELECT * FROM "%s"', db$table))
            pool::poolReturn(conn)
        }, error = function(e) {
            tryCatch(pool::poolReturn(conn), error = function(e2) {})
            return()
        })
    }

    user_data <- df[df$session_id == session_id, ]

    if (nrow(user_data) == 0) {
        warning("No data found for session_id: ", session_id)
        return()
    }

    current_bot_value <- user_data$survey_flags

    sessions_to_update <- list()

    # Check if user is too fast

    if(current_bot_value == "A" || is.na(current_bot_value)) { #Reset value to "" before checking anything else
        current_bot_value <- ""
        sessions_to_update[[session_id]] <- current_bot_value
    }

    if (is_fast(user_data, question_labels)) {
        current_bot_value <- paste(current_bot_value, "B", sep = "")
        sessions_to_update[[session_id]] <- current_bot_value
    }

    # Check if user start times are close to one another
    suspicious_timing <- start_time_checker(user_data, df)

    if (suspicious_timing$boolean) {
        current_bot_value <- paste(current_bot_value, "C", sep = "")
        sessions_to_update[[session_id]] <- current_bot_value

        # Update ALL suspicious sessions found
        for (suspicious_session in suspicious_timing$session_ids) {
            suspicious_row <- df[df$session_id == suspicious_session, ]
            if (nrow(suspicious_row) > 0) {
                suspicious_bot_value <- suspicious_row$is_bot
                if (is.na(suspicious_bot_value) || suspicious_bot_value == "" || suspicious_bot_value == "A") {
                    suspicious_bot_value <- ""
                }
                suspicious_bot_value <- paste(suspicious_bot_value, "C", sep = "")
                sessions_to_update[[suspicious_session]] <- suspicious_bot_value
            }
        }
    }

    if (is_straightlining(user_data)) {
        current_bot_value <- paste(current_bot_value, "D", sep = "")
        sessions_to_update[[session_id]] <- current_bot_value
    }

    # Check for IP violations
    #ip_violations <- ip_checker(user_data, df)
    #
    #if(ip_violations$boolean) {
    #    penalty <- ip_violations$value
    #
    #    # Update current session
    #    current_bot_value <- current_bot_value + penalty
    #    sessions_to_update[[session_id]] <- current_bot_value
    #
    #    # Update ALL sessions with same IP
    #    for (ip_session in ip_violations$session_ids) {
    #        # Get current bot value for this IP session
    #        ip_row <- df[df$session_id == ip_session, ]
    #        if (nrow(ip_row) > 0) {
    #            ip_bot_value <- as.numeric(ip_row$survey_flags) + penalty
    #            sessions_to_update[[ip_session]] <- ip_bot_value
    #        }
    #    }
    #}

    #------------------------- END OF BOT CONDITIONS -------------------------

    # Update all flagged sessions
    if (length(sessions_to_update) > 0) {
        for (update_session_id in names(sessions_to_update)) {
            new_bot_value <- sessions_to_update[[update_session_id]]

            data_list <- list(
                session_id = update_session_id,
                survey_flags = as.character(new_bot_value)
            )

            if (ignore_mode) {
                update_local_csv_session(update_session_id, new_bot_value)
            } else {
                tryCatch({
                    database_uploading(
                        data_list = data_list,
                        db = db$db,
                        table = db$table,
                        changed_fields = "survey_flags"
                    )
                }, error = function(e) {
                    warning("Bot checker database update failed: ", e$message)
                })
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
                existing_data[session_idx, "survey_flags"] <- as.character(new_bot_value)
                utils::write.csv(existing_data, "preview_data.csv", row.names = FALSE, na = "")
            }
        }, error = function(e) {
            warning("Unable to update local CSV for session ", session_id, ": ", e$message)
        })
    }
}

is_fast <- function(user_data, question_labels = NULL) {
    # Extract session_id from user_data
    session_id <- user_data$session_id

    # Debug ALL sessions for now
    cat("=== PROCESSING SESSION:", session_id, "===\n")

    all_time_cols <- names(user_data)[grepl("^time_", names(user_data))]
    events <- list()
    for (col in all_time_cols) {
        timestamp <- user_data[[col]]
        if (!is.na(timestamp) && timestamp != "" && !is.null(timestamp)) {
            events[[col]] <- list(
                time = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                type = col
            )
        }
    }
    # Add start time as the first event
    if (!is.na(user_data$time_start) && user_data$time_start != "") {
        events[["time_start"]] <- list(
            time = as.POSIXct(user_data$time_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
            type = "time_start"
        )
    }
    # Sort events by time
    events <- events[order(sapply(events, function(x) x$time))]

    # Show event sequence for ALL sessions
    cat("EVENT SEQUENCE:\n")
    for (i in 1:length(events)) {
        cat(i, ":", events[[i]]$type, "at", as.character(events[[i]]$time), "\n")
    }
    cat("\n")

    # Calculate time spent on each question
    question_times <- list()

    for (i in 2:length(events)) {
        current_event <- events[[i]]
        previous_event <- events[[i - 1]]

        if (grepl("^time_q_", current_event$type)) {
            question_id <- gsub("^time_q_", "", current_event$type)
            time_spent <- as.numeric(difftime(current_event$time, previous_event$time, units = "secs"))

            cat("Timing calc:", previous_event$type, "->", current_event$type, "\n")
            cat("  Time spent:", time_spent, "seconds\n")

            if (time_spent > 0) {
                question_times[[question_id]] <- time_spent
            }
        }
    }

    if (is.null(question_labels) || length(question_labels) == 0) {
        cat("No question labels provided - returning FALSE\n")
        return(FALSE)
    }

    cat("Question times calculated:", names(question_times), "\n")
    cat("Question labels available:", names(question_labels), "\n")

    num_fast_wpm <- 0
    num_very_fast_wmp <- 0
    valid_questions <- 0

    for (question_id in names(question_times)) {
        if (question_id %in% names(question_labels)) {
            question_text <- question_labels[[question_id]]

            if (is.null(question_text) || !is.character(question_text) ||
                length(question_text) == 0 || question_text == "") {
                next
            }

            if (length(question_text) > 1) {
                question_text <- question_text[1]
            }
            clean_text <- gsub("<[^>]*>", "", question_text)
            word_count <- length(strsplit(clean_text, "\\s+")[[1]])
            actual_time <- question_times[[question_id]]
            min_time_250wpm <- max(1, (word_count / 250) * 60)
            min_time_400wpm <- max(1, (word_count / 400) * 60)

            cat("Analyzing question:", question_id, "\n")
            cat("  Word count:", word_count, "\n")
            cat("  Actual time:", actual_time, "\n")
            cat("  Min 400WPM:", min_time_400wpm, "\n")
            cat("  Min 250WPM:", min_time_250wpm, "\n")

            if (actual_time < min_time_400wpm) {
                num_very_fast_wmp <- num_very_fast_wmp + 1
                cat("  FLAGGED: Very fast!\n")
            } else if (actual_time < min_time_250wpm) {
                num_fast_wpm <- num_fast_wpm + 0.5
                cat("  FLAGGED: Fast!\n")
            } else {
                cat("  OK: Normal speed\n")
            }
            valid_questions <- valid_questions + 1
        } else {
            cat("Question", question_id, "not found in labels\n")
        }
    }

    total_fast_score <- num_fast_wpm + num_very_fast_wmp
    fast_proportion <- total_fast_score / valid_questions

    cat("SUMMARY:\n")
    cat("  Valid questions:", valid_questions, "\n")
    cat("  Fast score:", total_fast_score, "\n")
    cat("  Fast proportion:", fast_proportion, "\n")
    cat("  Flagged as fast reader:", fast_proportion >= 0.5, "\n\n")

    return(fast_proportion >= 0.5)
}


start_time_checker <- function(user_data, df) {
    user_start_time <- as.POSIXct(user_data$time_start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    user_session_id <- user_data$session_id

    under_5_seconds_count <- 0
    suspicious_session_ids <- c()

    for (i in 1:nrow(df)) {
        row_session_id <- df$session_id[i]
        if (row_session_id == user_session_id) {
            next
        }

        row_start_time <- as.POSIXct(df$time_start[i], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        time_diff <- abs(as.numeric(difftime(row_start_time, user_start_time, units = "secs")))

        if (time_diff < 5) {
            under_5_seconds_count <- under_5_seconds_count + 1
            suspicious_session_ids <- c(suspicious_session_ids, row_session_id)
        }
    }

    return(list(
        boolean = under_5_seconds_count >= 5,
        session_ids = suspicious_session_ids
    ))
}


is_straightlining <- function(user_data) {

    rankings_cols <- names(user_data)[grepl("^rankings_", names(user_data)) & !grepl("^time_q_", names(user_data))]

    if (length(rankings_cols) == 0) {
        return(FALSE)
    }

    # Get all non-empty responses
    responses <- c()
    for (col in rankings_cols) {
        value <- user_data[[col]]
        if (!is.na(value) && value != "" && !is.null(value)) {
            responses <- c(responses, value)
        }
    }

    if (length(responses) < 2) {
        return(FALSE)
    }

    response_counts <- table(responses)
    most_common_count <- max(response_counts)
    total_responses <- length(responses)

    straightline_proportion <- most_common_count / total_responses

    return(straightline_proportion >= 0.75)
}

#------------------------- Research more on IPs before fully implementing this -------------------------

#ip_checker <- function(user_data, df) {
#    user_session_id <- user_data$session_id
#    user_client_ip <- user_data$client_ip  # Store the user's IP
#
#    if (is_institutional_ip(user_client_ip)) {
#        return(list(count = 0, session_ids = c(), boolean = FALSE, value = 0))
#    }
#
#    similar_ip_count <- 0
#    suspicious_session_ids <- c()
#
#    for(i in 1:nrow(df)) {
#        row_session_id <- df$session_id[i]
#
#        if (row_session_id == user_session_id) {
#            next
#        }
#
#        if(df$client_ip[i] == user_client_ip) {
#            similar_ip_count <- similar_ip_count + 1
#            suspicious_session_ids <- c(suspicious_session_ids, row_session_id)
#        }
#    }
#
#    is_suspicious <- similar_ip_count > 2
#
#    # Determine penalty value based on severity
#    penalty_value <- if (similar_ip_count > 5) {
#        1
#    } else if (similar_ip_count > 2) {
#        0.5
#    } else {
#        0
#    }
#
#    return(list(
#        count = similar_ip_count,
#        session_ids = suspicious_session_ids,
#        boolean = is_suspicious,
#        value = penalty_value
#    ))
#}
#
#is_institutional_ip <- function(ip) {
#    # Common institutional/corporate IP patterns
#    institutional_patterns <- c(
#        "10\\.",        # Private Class A
#        "172\\.(1[6-9]|2[0-9]|3[0-1])\\.",  # Private Class B
#        "192\\.168\\.", # Private Class C
#        "127\\.",
#        "192\\.164\\." # GWU For some reason <- This may be a problem especially if other schools are similar this route may not be viable. (Studies also say the same)
#    )
#    any(sapply(institutional_patterns, function(p) grepl(p, ip)))
#}






















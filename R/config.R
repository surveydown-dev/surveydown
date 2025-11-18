run_config <- function(
  required_questions,
  all_questions_required,
  start_page,
  skip_if,
  show_if,
  rate_survey,
  language
) {
  # Check for sd_close() in survey.qmd if rate_survey used
  if (rate_survey) {
    check_sd_close()
  }

  # Get paths to files and create '_survey' folder if necessary
  paths <- get_paths()

  # If changes detected, re-parse the '_survey/survey.html' file
  if (survey_files_need_updating(paths)) {
    message("Changes detected...re-parsing survey contents...")

    # Get the html content from the rendered survey.html file
    # Note: messages are now handled in create_settings_yaml()
    html_content <- rvest::read_html(paths$target_html)

    # Extract all divs with class "sd-page" and save to "_survey" folder
    pages <- extract_html_pages(
      paths,
      html_content,
      required_questions,
      all_questions_required,
      show_if
    )

    # Get question structure
    question_structure <- get_question_structure(paths, html_content)

    message(
      "Survey content saved to:\n",
      "  ",
      paths$target_html,
      "\n",
      "  ",
      paths$target_head,
      "\n",
      "  ",
      paths$target_pages,
      "\n",
      "  ",
      paths$target_questions,
      "\n",
      "  ",
      paths$target_settings,
      " (includes messages)"
    )
  } else {
    # If no changes, import from '_survey' folder
    message(
      'No changes detected. Importing contents from "_survey" folder.'
    )

    # Load pages object from _survey folder
    pages <- readRDS(paths$target_pages)

    # Load question structure from _survey folder
    question_structure <- load_question_structure_yaml(paths$target_questions)
  }

  # Get page and question IDs
  page_ids <- sapply(pages, function(p) p$id)
  question_ids <- names(question_structure)

  # Check for duplicate, overlapping, or pre-defined IDs
  check_ids(page_ids, question_ids)

  # Determine required questions, excluding matrix question IDs
  if (all_questions_required) {
    matrix_question_ids <- names(which(sapply(
      question_structure,
      `[[`,
      "is_matrix"
    )))
    question_required <- setdiff(question_ids, matrix_question_ids)
  } else {
    question_required <- required_questions
  }

  # Check that start_page (if used) points to an actual page
  if (!is.null(start_page) && !(start_page %in% page_ids)) {
    stop(
      "The specified start_page does not exist - check that you have not mis-spelled the id"
    )
  }

  # Set the start page
  if (is.null(start_page)) {
    start_page <- page_ids[1]
  }

  # Check skip_if and show_if inputs
  question_values <- unname(unlist(lapply(question_structure, `[[`, "options")))
  check_skip_show(
    question_ids,
    question_values,
    page_ids,
    skip_if,
    show_if
  )

  # Store all config settings
  config <- list(
    pages = pages,
    page_ids = page_ids,
    question_ids = question_ids,
    question_required = question_required,
    start_page = start_page,
    question_structure = question_structure
  )
  return(config)
}

check_sd_close <- function() {
  # Read the content of survey.qmd
  qmd_content <- readLines('survey.qmd', warn = FALSE)

  # Check for sd_close() call with any parameters
  sd_close_present <- any(grepl("sd_close\\s*\\(.*\\)", qmd_content))

  if (!sd_close_present) {
    warning(
      "\u274C No sd_close() function found in 'survey.qmd' file. You must use sd_close() to trigger the rating question response at the end of the survey. You can also remove this rating question by setting 'rate_survey = FALSE' in sd_server()."
    )
  }
}

get_paths <- function() {
  target_folder <- "_survey"
  if (!fs::dir_exists(target_folder)) {
    fs::dir_create(target_folder)
  }
  paths <- list(
    qmd = "survey.qmd",
    app = "app.R",
    root_html = "survey.html",
    msg = "messages.yml", # User-provided custom messages (root folder)
    target_html = file.path(target_folder, "survey.html"),
    target_pages = file.path(target_folder, "pages.rds"),
    target_head = file.path(target_folder, "head.rds"),
    target_questions = file.path(target_folder, "questions.yml"),
    target_settings = file.path(target_folder, "settings.yml") # Contains merged messages
  )
  return(paths)
}

survey_files_need_updating <- function(paths) {
  # Re-parse if any of the target files are missing
  targets <- c(
    paths$target_pages,
    paths$target_questions,
    paths$target_settings
  )
  if (any(!fs::file_exists(targets))) {
    return(TRUE)
  }

  # Re-parse if the target pages file is out of date with 'survey.qmd', 'app.R'
  time_qmd <- file.info(paths$qmd)$mtime
  time_app <- file.info(paths$app)$mtime
  time_pages <- file.info(paths$target_pages)$mtime

  if ((time_qmd > time_pages) || (time_app > time_pages)) {
    return(TRUE)
  }

  # Find all YAML files
  # find_all_yaml_files() defined in ui.R
  yaml_files <- find_all_yaml_files()

  # Check if any YAML file is newer than the parsed pages
  for (yaml_file in yaml_files) {
    if (fs::file_exists(yaml_file)) {
      time_yml <- file.info(yaml_file)$mtime
      if (time_yml > time_pages) {
        return(TRUE)
      }
    }
  }

  # Re-parse if the user provided a 'messages.yml' file which is out of date
  if (fs::file_exists(paths$msg)) {
    time_msg <- file.info(paths$msg)$mtime
    if (time_msg > time_pages) {
      return(TRUE)
    }
  }

  return(FALSE)
}

set_messages <- function(paths, language, metadata = NULL) {
  # Load default messages
  messages <- get_messages_default()

  # Check for valid language input (see https://shiny.posit.co/r/reference/shiny/1.7.0/dateinput)
  valid_languages <- get_valid_languages()

  # Fallback to English if not set or not supported (from shiny::dateInput())
  if (is.null(language) || !(language %in% valid_languages)) {
    message(
      "Invalid or unsupported language selected. Falling back to predefined English."
    )
    message(
      "Check https://shiny.posit.co/r/reference/shiny/1.7.0/dateinput for supported languages."
    )
    language <- "en"
  }

  # Include user provided messages if messages.yml file is in root
  if (fs::file_exists(paths$msg)) {
    # Read messages file and select messages for selected language
    tryCatch(
      {
        user_msg <- yaml::read_yaml(paths$msg)
        user_msg <- user_msg[unique(names(user_msg))]
        user_msg <- user_msg[names(user_msg) == language]

        # Check for valid set of messages for selected language
        if (length(user_msg) == 1) {
          message(
            "User provided messages for language '",
            language,
            "' from '",
            paths$msg,
            "' file loaded."
          )

          # Add missing texts from default messages (if not available, use English)
          if (language %in% names(messages)) {
            predef_msg <- messages[[language]]
          } else {
            predef_msg <- messages[["en"]]
          }
          user_msg[[1]] <- c(
            user_msg[[1]],
            predef_msg[!(names(predef_msg) %in% names(user_msg[[1]]))]
          )
        } else {
          user_msg <- NULL
        }
      },
      error = function(e) {
        message(
          "Error reading '",
          paths$msg,
          "' file. Please review and revise the file. Error details: ",
          e$message
        )
        user_msg <- NULL
      }
    )

    # Combine user provided messages with predefined messages
    messages <- c(user_msg, messages) # user provided take precedence
  }

  # Choose messages by chosen language
  if (!language %in% names(messages)) {
    # Fallback to English if no messages found for selected language
    sd_create_messages(language)
    messages <- messages["en"]
  } else {
    messages <- messages[names(messages) == language]
    messages <- messages[1]
  }

  # Restructure messages to use "system_message" as key instead of language code
  # Extract the message values (which are in a list under the language code)
  message_values <- messages[[1]]

  # Check for custom system-messages entries in YAML metadata (highest priority)
  if (!is.null(metadata)) {
    custom_messages <- get_yaml_value(metadata, "system-messages")
    if (!is.null(custom_messages)) {
      # Define valid kebab-case message keys
      valid_message_keys <- c(
        "cancel", "confirm-exit", "sure-exit", "submit-exit", "warning",
        "required", "rating-title", "rating-text", "rating-scale",
        "previous", "next", "exit", "close-tab", "choose-option",
        "click", "redirect", "seconds", "new-tab", "redirect-error"
      )

      # Merge custom messages with default messages (only valid kebab-case keys)
      # Custom entries override defaults
      valid_count <- 0
      for (key in names(custom_messages)) {
        if (key %in% valid_message_keys) {
          message_values[[key]] <- custom_messages[[key]]
          valid_count <- valid_count + 1
        } else {
          warning(
            "Invalid message key '", key, "' ignored. ",
            "Only kebab-case keys are supported. ",
            "Did you mean '", gsub("_", "-", key), "'?"
          )
        }
      }
      if (valid_count > 0) {
        message(
          "Custom system-messages entries found in YAML header. ",
          valid_count,
          " custom message(s) applied."
        )
      }
    }
  }

  # Create new structure with "system-messages" as the key (kebab-case)
  messages_restructured <- list(`system-messages` = message_values)

  # Return messages (will be written to settings.yml)
  return(list(
    language = language,
    messages = messages_restructured
  ))
}

# Helper function to clean up YAML apostrophes
# Converts: 'won''t' -> "won't" for better readability
clean_yaml_quotes <- function(yaml_string) {
  # Split into lines for easier processing
  lines <- strsplit(yaml_string, "\n")[[1]]

  # Process each line
  for (i in seq_along(lines)) {
    line <- lines[i]

    # Check if line contains single-quoted string with escaped apostrophes
    if (grepl("'[^']*''[^']*'", line)) {
      # Find all matches
      pattern <- "'([^']*(?:''[^']*)*)'"
      matches <- gregexpr(pattern, line, perl = TRUE)

      if (matches[[1]][1] != -1) {
        # Extract all matched strings
        match_text <- regmatches(line, matches)[[1]]

        # Process each match
        for (match in match_text) {
          # Only process if it contains ''
          if (grepl("''", match)) {
            # Remove outer quotes
            content <- substring(match, 2, nchar(match) - 1)
            # Replace '' with '
            content <- gsub("''", "'", content, fixed = TRUE)
            # Create double-quoted version
            replacement <- paste0('"', content, '"')
            # Replace in line
            line <- sub(match, replacement, line, fixed = TRUE)
          }
        }

        lines[i] <- line
      }
    }
  }

  # Rejoin lines
  return(paste(lines, collapse = "\n"))
}

# Helper function to create sectioned YAML output with organized comments
create_sectioned_yaml <- function(
  settings,
  general_params,
  theme_params,
  survey_params,
  messages = NULL
) {
  # Build YAML string with sections
  yaml_string <- ""

  # General Settings section (may be NULL if not using)
  if (!is.null(general_params) && length(general_params) > 0) {
    yaml_string <- paste0(yaml_string, "# General Settings (Quarto-defined)\n")
    general_settings <- settings[general_params]
    general_settings <- general_settings[!sapply(general_settings, is.null)]
    yaml_string <- paste0(yaml_string, yaml::as.yaml(general_settings))
  }

  # Theme Settings section - nested under theme-settings (kebab-case)
  if (!is.null(theme_params) && length(theme_params) > 0) {
    if (yaml_string != "") {
      yaml_string <- paste0(yaml_string, "\n")
    }
    theme_settings <- settings[theme_params]
    theme_settings <- theme_settings[!sapply(theme_settings, is.null)]
    # Nest under theme-settings key (kebab-case)
    theme_nested <- list(`theme-settings` = theme_settings)
    yaml_string <- paste0(yaml_string, yaml::as.yaml(theme_nested))
  }

  # Survey Settings section - nested under survey-settings (kebab-case)
  if (!is.null(survey_params) && length(survey_params) > 0) {
    if (yaml_string != "") {
      yaml_string <- paste0(yaml_string, "\n")
    }
    survey_settings <- settings[survey_params]
    survey_settings <- survey_settings[!sapply(survey_settings, is.null)]
    # Nest under survey-settings key (kebab-case)
    survey_nested <- list(`survey-settings` = survey_settings)
    yaml_string <- paste0(yaml_string, yaml::as.yaml(survey_nested))
  }

  # System Messages section
  if (!is.null(messages)) {
    if (yaml_string != "") {
      yaml_string <- paste0(yaml_string, "\n")
    }
    yaml_string <- paste0(yaml_string, yaml::as.yaml(messages))
  }

  # Clean up awkward YAML apostrophe escaping (won''t -> "won't")
  yaml_string <- clean_yaml_quotes(yaml_string)

  return(yaml_string)
}

create_settings_yaml <- function(paths, metadata) {
  # Extract server configuration from survey.qmd YAML metadata during UI rendering
  if (file.exists("survey.qmd")) {
    tryCatch(
      {
        # Detect sd_server() parameters from app.R FIRST
        # This ensures sd_server() overrides take precedence over YAML values
        sd_server_overrides <- detect_sd_server_params()

        # Get system language for messages
        # Priority: sd_server() > YAML header > default
        system_language <- get_system_language(metadata)
        if (is.null(system_language)) {
          system_language <- "en"
        }
        # Override with sd_server() if specified
        if (!is.null(sd_server_overrides$system_language)) {
          system_language <- sd_server_overrides$system_language
        }

        # Get messages for the selected language (pass metadata for custom overrides)
        msg_result <- set_messages(paths, system_language, metadata)

        # Define parameter categories (kebab-case only)
        # Note: format, echo, warning are now implicit via render_survey_qmd()
        theme_params <- c(
          "theme",
          "barposition",
          "barcolor",
          "footer-left",
          "footer-center",
          "footer-right"
        )
        survey_params <- c(
          "show-previous",
          "use-cookies",
          "auto-scroll",
          "rate-survey",
          "all-questions-required",
          "start-page",
          "system-language",
          "highlight-unanswered",
          "highlight-color",
          "capture-metadata",
          "required-questions"
        )

        # Combine all parameters
        all_params <- c(theme_params, survey_params)

        # Define defaults for all parameters (kebab-case keys)
        default_settings <- list(
          # Theme Settings
          theme = "default",
          barposition = "top",
          barcolor = NULL,
          `footer-left` = "",
          `footer-center` = "",
          `footer-right` = "",
          # Survey Settings
          `show-previous` = FALSE,
          `use-cookies` = TRUE,
          `auto-scroll` = FALSE,
          `rate-survey` = FALSE,
          `all-questions-required` = FALSE,
          `start-page` = NULL,
          `system-language` = "en",
          `highlight-unanswered` = TRUE,
          `highlight-color` = "gray",
          `capture-metadata` = TRUE,
          `required-questions` = NULL
        )

        # Extract YAML values using helper functions (priority: YAML > defaults)
        settings <- list()
        for (param in all_params) {
          # Use specific extraction functions for parameters that need special handling
          if (param == "theme") {
            value <- get_theme(metadata)
          } else if (param == "barposition") {
            value <- get_barposition(metadata)
          } else if (param == "barcolor") {
            value <- get_barcolor(metadata)
          } else if (param == "footer-left") {
            value <- get_footer_left(metadata)
          } else if (param == "footer-center") {
            value <- get_footer_center(metadata)
          } else if (param == "footer-right") {
            value <- get_footer_right(metadata)
          } else if (param == "use-cookies") {
            value <- get_use_cookies(metadata)
          } else if (param == "auto-scroll") {
            value <- get_auto_scroll(metadata)
          } else if (param == "rate-survey") {
            value <- get_rate_survey(metadata)
          } else if (param == "all-questions-required") {
            value <- get_all_questions_required(metadata)
          } else if (param == "start-page") {
            value <- get_start_page(metadata)
          } else if (param == "system-language") {
            value <- get_system_language(metadata)
          } else if (param == "highlight-unanswered") {
            value <- get_highlight_unanswered(metadata)
          } else if (param == "highlight-color") {
            value <- get_highlight_color(metadata)
          } else if (param == "capture-metadata") {
            value <- get_capture_metadata(metadata)
          } else if (param == "required-questions") {
            value <- get_required_questions(metadata)
          } else if (param == "show-previous") {
            value <- get_show_previous(metadata)
          } else {
            value <- NULL
          }

          if (!is.null(value)) {
            settings[[param]] <- value
          } else {
            # Use default value
            settings[[param]] <- default_settings[[param]]
          }
        }

        # Warn about invalid snake_case keys in YAML (theme-settings, survey-settings)
        yaml_data <- metadata$formats$html$metadata
        if (!is.null(yaml_data)) {
          # Check for snake_case keys in theme-settings
          if (!is.null(yaml_data$`theme-settings`)) {
            for (key in names(yaml_data$`theme-settings`)) {
              if (grepl("_", key)) {
                warning(
                  "Invalid key '", key, "' in theme-settings ignored. ",
                  "Only kebab-case keys are supported. ",
                  "Did you mean '", gsub("_", "-", key), "'?"
                )
              }
            }
          }
          # Check for snake_case keys in survey-settings
          if (!is.null(yaml_data$`survey-settings`)) {
            for (key in names(yaml_data$`survey-settings`)) {
              if (grepl("_", key)) {
                warning(
                  "Invalid key '", key, "' in survey-settings ignored. ",
                  "Only kebab-case keys are supported. ",
                  "Did you mean '", gsub("_", "-", key), "'?"
                )
              }
            }
          }
        }

        # Apply sd_server() parameters detected earlier
        # This prevents timing issues where settings.yml shows defaults before sd_server() updates it
        if (length(sd_server_overrides) > 0) {
          # Map snake_case sd_server() params to kebab-case settings keys
          param_to_kebab <- list(
            show_previous = "show-previous",
            use_cookies = "use-cookies",
            auto_scroll = "auto-scroll",
            rate_survey = "rate-survey",
            all_questions_required = "all-questions-required",
            start_page = "start-page",
            system_language = "system-language",
            highlight_unanswered = "highlight-unanswered",
            highlight_color = "highlight-color",
            capture_metadata = "capture-metadata",
            required_questions = "required-questions"
          )

          for (param in names(sd_server_overrides)) {
            kebab_key <- param_to_kebab[[param]]
            if (!is.null(kebab_key)) {
              settings[[kebab_key]] <- sd_server_overrides[[param]]
            }
          }
        }

        # Write settings file with sectioned YAML (includes messages)
        yaml_content <- create_sectioned_yaml(
          settings,
          NULL,
          theme_params,
          survey_params,
          msg_result$messages
        )
        comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
        comment_line2 <- "# All survey configuration settings with final resolved values\n\n"
        full_content <- paste0(comment_line1, comment_line2, yaml_content)
        writeLines(full_content, con = paths$target_settings)
      },
      error = function(e) {
        warning("Could not extract settings from survey.qmd: ", e$message)
        # Create settings file with defaults on error (kebab-case only)
        theme_params <- c(
          "theme",
          "barposition",
          "barcolor",
          "footer-left",
          "footer-center",
          "footer-right"
        )
        survey_params <- c(
          "show-previous",
          "use-cookies",
          "auto-scroll",
          "rate-survey",
          "all-questions-required",
          "start-page",
          "system-language",
          "highlight-unanswered",
          "highlight-color",
          "capture-metadata",
          "required-questions"
        )
        default_settings <- list(
          theme = "default",
          barposition = "top",
          barcolor = NULL,
          `footer-left` = "",
          `footer-center` = "",
          `footer-right` = "",
          `show-previous` = FALSE,
          `use-cookies` = TRUE,
          `auto-scroll` = FALSE,
          `rate-survey` = FALSE,
          `all-questions-required` = FALSE,
          `start-page` = NULL,
          `system-language` = "en",
          `highlight-unanswered` = TRUE,
          `highlight-color` = "gray",
          `capture-metadata` = TRUE,
          `required-questions` = NULL
        )
        # Get default English messages
        default_msg <- get_messages_default()
        yaml_content <- create_sectioned_yaml(
          default_settings,
          NULL,
          theme_params,
          survey_params,
          default_msg["en"]
        )
        comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
        comment_line2 <- "# All survey configuration settings with defaults (error reading YAML header)\n"
        comment_line3 <- "# Note: format, echo, warning are implicit in render_survey_qmd()\n\n"
        full_content <- paste0(
          comment_line1,
          comment_line2,
          comment_line3,
          yaml_content
        )
        writeLines(full_content, con = paths$target_settings)
      }
    )
  } else {
    # Create settings file with defaults if survey.qmd doesn't exist (kebab-case only)
    theme_params <- c(
      "theme",
      "barposition",
      "barcolor",
      "footer-left",
      "footer-center",
      "footer-right"
    )
    survey_params <- c(
      "show-previous",
      "use-cookies",
      "auto-scroll",
      "rate-survey",
      "all-questions-required",
      "start-page",
      "system-language",
      "highlight-unanswered",
      "highlight-color",
      "capture-metadata",
      "required-questions"
    )
    default_settings <- list(
      theme = "default",
      barposition = "top",
      barcolor = NULL,
      `footer-left` = "",
      `footer-center` = "",
      `footer-right` = "",
      `show-previous` = FALSE,
      `use-cookies` = TRUE,
      `auto-scroll` = FALSE,
      `rate-survey` = FALSE,
      `all-questions-required` = FALSE,
      `start-page` = NULL,
      `system-language` = "en",
      `highlight-unanswered` = TRUE,
      `highlight-color` = "gray",
      `capture-metadata` = TRUE,
      `required-questions` = NULL
    )
    # Get default English messages
    default_msg <- get_messages_default()
    yaml_content <- create_sectioned_yaml(
      default_settings,
      NULL,
      theme_params,
      survey_params,
      default_msg["en"]
    )
    comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
    comment_line2 <- "# All survey configuration settings with defaults (no survey.qmd file found)\n"
    comment_line3 <- "# Note: format, echo, warning are implicit in render_survey_qmd()\n\n"
    full_content <- paste0(
      comment_line1,
      comment_line2,
      comment_line3,
      yaml_content
    )
    writeLines(full_content, con = paths$target_settings)
  }
}

# Function to read settings from _survey/settings.yml file (kebab-case only)
read_settings_yaml <- function() {
  paths <- get_paths()

  # Define default settings to return if reading fails (kebab-case)
  defaults <- list(
    # Theme Settings
    theme = "default",
    barposition = "top",
    barcolor = NULL,
    `footer-left` = "",
    `footer-center` = "",
    `footer-right` = "",
    # Survey Settings
    `use-cookies` = TRUE,
    `auto-scroll` = FALSE,
    `rate-survey` = FALSE,
    `all-questions-required` = FALSE,
    `start-page` = NULL,
    `system-language` = "en",
    `highlight-unanswered` = TRUE,
    `highlight-color` = "gray",
    `capture-metadata` = TRUE,
    `required-questions` = NULL
  )

  # Try multiple possible locations for the settings file
  possible_paths <- c(
    paths$target_settings, # "_survey/settings.yml"
    file.path(getwd(), paths$target_settings), # Full path from current dir
    file.path(".", paths$target_settings), # Explicit relative path
    "settings.yml" # Fallback: just settings.yml in current dir
  )

  settings_raw <- NULL
  successful_path <- NULL

  # Try to read from each path directly (without file.exists check)
  for (path in possible_paths) {
    tryCatch(
      {
        settings_raw <- yaml::read_yaml(path)
        successful_path <- path
        break
      },
      error = function(e) {
        # Continue to next path
      }
    )
  }

  if (is.null(settings_raw)) {
    return(defaults)
  }

  # Flatten hierarchical structure: extract theme-settings and survey-settings (kebab-case)
  settings <- list()

  # Extract theme settings if nested (kebab-case section name)
  if (!is.null(settings_raw$`theme-settings`)) {
    for (key in names(settings_raw$`theme-settings`)) {
      settings[[key]] <- settings_raw$`theme-settings`[[key]]
    }
  }

  # Extract survey settings if nested (kebab-case section name)
  if (!is.null(settings_raw$`survey-settings`)) {
    for (key in names(settings_raw$`survey-settings`)) {
      settings[[key]] <- settings_raw$`survey-settings`[[key]]
    }
  }

  # Also check for flat structure (backward compatibility)
  # If theme-settings and survey-settings don't exist, use flat structure
  if (
    is.null(settings_raw$`theme-settings`) &&
      is.null(settings_raw$`survey-settings`)
  ) {
    settings <- settings_raw
  }

  # Process required-questions if it exists (convert list to character vector)
  if (!is.null(settings$`required-questions`)) {
    if (is.list(settings$`required-questions`)) {
      settings$`required-questions` <- unlist(settings$`required-questions`)
    }
  }

  return(settings)
}

# Function to update settings.yml with final resolved parameters from sd_server()
# This ONLY updates Survey Settings, preserving Theme Settings from the YAML header
update_settings_yaml <- function(resolved_params) {
  paths <- get_paths()

  # Define parameter categories (kebab-case only)
  # Note: format, echo, warning are now implicit in render_survey_qmd()
  theme_params <- c(
    "theme",
    "barposition",
    "barcolor",
    "footer-left",
    "footer-center",
    "footer-right"
  )
  survey_params <- c(
    "show-previous",
    "use-cookies",
    "auto-scroll",
    "rate-survey",
    "all-questions-required",
    "start-page",
    "system-language",
    "highlight-unanswered",
    "highlight-color",
    "capture-metadata",
    "required-questions"
  )

  # Read existing settings to preserve Theme Settings
  existing_settings <- read_settings_yaml()

  # Define defaults for survey parameters only (kebab-case)
  survey_defaults <- list(
    `show-previous` = FALSE,
    `use-cookies` = TRUE, # Note: this becomes TRUE when NULL is passed to sd_server
    `auto-scroll` = FALSE,
    `rate-survey` = FALSE,
    `all-questions-required` = FALSE,
    `start-page` = NULL,
    `system-language` = "en",
    `highlight-unanswered` = TRUE,
    `highlight-color` = "gray",
    `capture-metadata` = TRUE,
    `required-questions` = NULL
  )

  # Filter out language parameter to avoid breaking Quarto
  resolved_params$language <- NULL

  # Build final settings: preserve theme settings, update survey settings
  final_settings <- list()

  # Preserve theme settings from existing settings.yml (set by create_settings_yaml)
  for (param in theme_params) {
    if (!is.null(existing_settings[[param]])) {
      final_settings[[param]] <- existing_settings[[param]]
    }
  }

  # Update survey settings with resolved params
  # Map snake_case from resolved_params to kebab-case for storage
  param_mapping <- list(
    `show-previous` = "show_previous",
    `use-cookies` = "use_cookies",
    `auto-scroll` = "auto_scroll",
    `rate-survey` = "rate_survey",
    `all-questions-required` = "all_questions_required",
    `start-page` = "start_page",
    `system-language` = "system_language",
    `highlight-unanswered` = "highlight_unanswered",
    `highlight-color` = "highlight_color",
    `capture-metadata` = "capture_metadata",
    `required-questions` = "required_questions"
  )

  for (param in survey_params) {
    # Get the snake_case version of the parameter name for resolved_params lookup
    snake_param <- param_mapping[[param]]

    if (!is.null(snake_param) && !is.null(resolved_params[[snake_param]])) {
      final_settings[[param]] <- resolved_params[[snake_param]]
    } else {
      # Use default if not in resolved_params
      final_settings[[param]] <- survey_defaults[[param]]
    }
  }

  # Handle special case for use-cookies (NULL becomes TRUE)
  if (is.null(final_settings$`use-cookies`)) {
    final_settings$`use-cookies` <- TRUE
  }

  # Read existing messages from settings.yml to preserve them
  existing_msg <- NULL
  if (fs::file_exists(paths$target_settings)) {
    tryCatch(
      {
        full_settings <- yaml::read_yaml(paths$target_settings)
        # Look for the system-messages key (kebab-case) or old keys for backward compatibility
        if (!is.null(full_settings$`system-messages`)) {
          # New structure with system-messages (kebab-case)
          existing_msg <- list(`system-messages` = full_settings$`system-messages`)
        } else if (!is.null(full_settings$system_messages)) {
          # Old structure with system_messages (snake_case) - convert to kebab
          existing_msg <- list(`system-messages` = full_settings$system_messages)
        } else if (!is.null(full_settings$system_message)) {
          # Old structure with system_message (singular) - convert to kebab
          existing_msg <- list(`system-messages` = full_settings$system_message)
        } else {
          # Older structure - look for language keys for backward compatibility
          for (lang in c("en", "de", "es", "fr", "it", "zh-CN")) {
            if (!is.null(full_settings[[lang]])) {
              # Convert old structure to new structure
              existing_msg <- list(`system-messages` = full_settings[[lang]])
              break
            }
          }
        }
      },
      error = function(e) {
        # If reading fails, continue without messages
      }
    )
  }

  # If no existing messages found, use default English with system-messages key
  if (is.null(existing_msg)) {
    default_msg <- get_messages_default()["en"]
    existing_msg <- list(`system-messages` = default_msg[[1]])
  }

  # Create YAML content with sections (including messages)
  yaml_content <- create_sectioned_yaml(
    final_settings,
    NULL,
    theme_params,
    survey_params,
    existing_msg
  )
  comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
  comment_line2 <- "# All survey configuration settings with final resolved values\n\n"
  full_content <- paste0(
    comment_line1,
    comment_line2,
    yaml_content
  )

  # Write to file
  writeLines(full_content, con = paths$target_settings)
}

# Function to detect sd_server() parameter overrides in app.R
detect_sd_server_params <- function() {
  if (!file.exists("app.R")) {
    return(list())
  }

  tryCatch(
    {
      # Read app.R content as a single string to handle multiline sd_server() calls
      app_content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")

      # Extract the sd_server() function call content (including multiline)
      sd_server_pattern <- "sd_server\\s*\\(([^)]*(?:\\([^)]*\\)[^)]*)*)\\)"
      sd_server_matches <- regmatches(
        app_content,
        gregexpr(sd_server_pattern, app_content)
      )

      if (length(sd_server_matches[[1]]) == 0) {
        return(list())
      }

      # Get the parameters inside sd_server()
      sd_server_text <- sd_server_matches[[1]][1]

      # Extract parameters using regex
      overrides <- list()

      # Pattern to match parameter = value pairs
      param_patterns <- c(
        "show_previous\\s*=\\s*(TRUE|FALSE|T|F)",
        "use_cookies\\s*=\\s*(TRUE|FALSE|T|F)",
        "auto_scroll\\s*=\\s*(TRUE|FALSE|T|F)",
        "rate_survey\\s*=\\s*(TRUE|FALSE|T|F)",
        "all_questions_required\\s*=\\s*(TRUE|FALSE|T|F)",
        "start_page\\s*=\\s*[\"']([^\"']+)[\"']",
        "system_language\\s*=\\s*[\"']([^\"']+)[\"']",
        "highlight_unanswered\\s*=\\s*(TRUE|FALSE|T|F)",
        "highlight_color\\s*=\\s*[\"']([^\"']+)[\"']",
        "capture_metadata\\s*=\\s*(TRUE|FALSE|T|F)",
        "required_questions\\s*=\\s*c\\s*\\(([^)]+)\\)"
      )

      param_names <- c(
        "show_previous",
        "use_cookies",
        "auto_scroll",
        "rate_survey",
        "all_questions_required",
        "start_page",
        "system_language",
        "highlight_unanswered",
        "highlight_color",
        "capture_metadata",
        "required_questions"
      )

      for (i in seq_along(param_patterns)) {
        matches <- regmatches(
          sd_server_text,
          regexec(param_patterns[i], sd_server_text, ignore.case = TRUE)
        )
        if (length(matches[[1]]) > 1) {
          param_name <- param_names[i]
          value_str <- matches[[1]][2]

          # Convert to appropriate R type
          if (
            param_name %in%
              c(
                "show_previous",
                "use_cookies",
                "auto_scroll",
                "rate_survey",
                "all_questions_required",
                "highlight_unanswered",
                "capture_metadata"
              )
          ) {
            overrides[[param_name]] <- value_str %in% c("TRUE", "T")
          } else if (param_name %in% c("start_page", "system_language", "highlight_color")) {
            overrides[[param_name]] <- value_str
          } else if (param_name == "required_questions") {
            # Parse c("a", "b", "c") format
            items <- strsplit(value_str, ",")[[1]]
            items <- trimws(gsub("[\"']", "", items))
            overrides[[param_name]] <- items
          }
        }
      }

      return(overrides)
    },
    error = function(e) {
      # If parsing fails, return empty list
      return(list())
    }
  )
}

extract_html_pages <- function(
  paths,
  html_content,
  required_questions,
  all_questions_required,
  show_if
) {
  # Check for both sd-page and sd_page classes
  pages_dash <- html_content |> rvest::html_elements(".sd-page")
  pages_underscore <- html_content |> rvest::html_elements(".sd_page")

  # Check if mixing of classes exists
  if (length(pages_dash) > 0 && length(pages_underscore) > 0) {
    stop("Mixed use of '.sd-page' and '.sd_page'. Please use only one style.")
  }

  # Use whichever page style is found
  if (length(pages_dash) > 0) {
    pages_elements <- pages_dash
    class_used <- ".sd-page"
  } else if (length(pages_underscore) > 0) {
    pages_elements <- pages_underscore
    class_used <- ".sd_page"
  } else {
    stop(
      "No survey pages found. Add divs with either '.sd-page' or '.sd_page' class."
    )
  }

  # Get total number of pages to identify the last page
  total_pages <- length(pages_elements)

  pages <- lapply(seq_along(pages_elements), function(page_index) {
    x <- pages_elements[[page_index]]
    page_id <- rvest::html_attr(x, "id")
    question_containers <- rvest::html_elements(x, ".question-container")
    question_ids <- character(0)
    required_question_ids <- character(0)

    for (i in seq_along(question_containers)) {
      container <- question_containers[[i]]
      question_id <- rvest::html_attr(container, "data-question-id")
      question_ids <- c(question_ids, question_id)

      # Check if it's a matrix question
      is_matrix <- length(rvest::html_elements(container, ".matrix-question")) >
        0

      # Determine if the question is required
      is_required <- if (is_matrix) {
        all_questions_required || question_id %in% required_questions
      } else if (all_questions_required) {
        TRUE
      } else {
        question_id %in% required_questions
      }

      # Track required questions and display asterisk
      if (is_required) {
        if (is_matrix) {
          # Only show asterisks for subquestions, not the main matrix question
          sub_asterisks <- rvest::html_elements(
            container,
            ".matrix-question td .hidden-asterisk"
          )
          for (asterisk in sub_asterisks) {
            xml2::xml_attr(asterisk, "style") <- "display: inline;"
          }
        } else {
          asterisk <- rvest::html_element(container, ".hidden-asterisk")
          xml2::xml_attr(asterisk, "style") <- "display: inline;"
        }
        required_question_ids <- c(required_question_ids, question_id)
      }
      if (!is.null(show_if)) {
        if (question_id %in% show_if$targets) {
          current_style <- xml2::xml_attr(container, "style")
          new_style <- paste(current_style, "display: none;", sep = " ")
          xml2::xml_attr(container, "style") <- new_style
        }
      }
      question_containers[[i]] <- container
    }

    # Update the 'Next' button ID and extract the next_page_id
    next_button_id <- make_next_button_id(page_id)
    next_button <- rvest::html_element(x, "#page_id_next")
    if (is.na(next_button)) {
      # No next button on this page
      next_page_id <- NULL
    } else {
      xml2::xml_attr(next_button, "id") <- next_button_id
      next_page_id <- rvest::html_attr(
        xml2::xml_parent(next_button),
        "data-next-page"
      )
    }

    # Update the 'Previous' button ID (if present)
    prev_button_id <- make_prev_button_id(page_id)
    prev_button <- rvest::html_element(x, "#page_id_prev")
    has_prev_button <- !is.na(prev_button)

    if (has_prev_button) {
      xml2::xml_attr(prev_button, "id") <- prev_button_id
    } else {
      prev_button_id <- NULL
    }

    # AUTO-NAVIGATION: Check if we need to auto-insert sd_nav()
    # Only auto-insert if:
    # 1. This is NOT the last page
    # 2. No explicit sd_nav() or sd_close() button exists
    is_last_page <- (page_index == total_pages)

    # Check for sd_close() button (id="close-survey-button")
    has_close_button <- !is.na(rvest::html_element(x, "#close-survey-button"))

    # Check for sd_nav(show_buttons = FALSE) marker
    has_nav_marker <- !is.na(rvest::html_element(x, "#sd-nav-marker"))

    # Check if explicit navigation already exists
    has_explicit_nav <- !is.na(next_button) || has_close_button || has_nav_marker

    # Auto-insert navigation if conditions are met
    if (!is_last_page && !has_explicit_nav) {
      # Get settings including show_previous and translated messages
      settings_path <- file.path("_survey", "settings.yml")
      show_prev <- FALSE # Default
      messages <- list("previous" = "Previous", "next" = "Next") # Default

      if (file.exists(settings_path)) {
        tryCatch(
          {
            settings_yaml <- yaml::read_yaml(settings_path)

            # Get show-previous setting from survey-settings (kebab-case)
            if (!is.null(settings_yaml$`survey-settings`$`show-previous`)) {
              show_prev <- settings_yaml$`survey-settings`$`show-previous`
            }

            # Get translated messages
            if (!is.null(settings_yaml$system_messages)) {
              messages <- settings_yaml$system_messages
            }
          },
          error = function(e) {
            # Use defaults on error
          }
        )
      }

      # IMPORTANT: Never show Previous button on the first page or on pages with sd_close()
      is_first_page <- (page_index == 1)
      if (is_first_page || has_close_button) {
        show_prev <- FALSE
      }

      prev_label <- paste0("\u2190 ", messages[['previous']]) # ← Previous
      next_label <- paste0(messages[['next']], " \u2192") # Next →

      # Create auto-generated sd_nav() HTML
      # This matches the exact output from sd_nav() in R/ui.R
      # Include Previous button only if show_previous setting is TRUE AND not first page
      if (show_prev) {
        auto_nav_html <- sprintf(
          '
<div data-next-page="" style="margin-top: 1rem; margin-bottom: 0.5rem;" class="sd-nav-container">
  <button class="btn btn-default action-button sd-nav-button sd-nav-prev" id="page_id_prev" onclick="Shiny.setInputValue(&#39;prev_page&#39;, true, {priority: &#39;event&#39;});" style="float: left;" type="button">%s</button>
  <button class="btn btn-default action-button sd-enter-button sd-nav-button sd-nav-next" id="page_id_next" onclick="Shiny.setInputValue(&#39;next_page&#39;, this.parentElement.getAttribute(&#39;data-next-page&#39;));" style="float: right;" type="button">%s</button>
  <div style="clear: both;"></div>
</div>
        ',
          prev_label,
          next_label
        )
      } else {
        # Only Next button (centered when no Previous button)
        auto_nav_html <- sprintf(
          '
<div data-next-page="" style="margin-top: 1rem; margin-bottom: 0.5rem;" class="sd-nav-container">
  <button class="btn btn-default action-button sd-enter-button sd-nav-button sd-nav-next" id="page_id_next" onclick="Shiny.setInputValue(&#39;next_page&#39;, this.parentElement.getAttribute(&#39;data-next-page&#39;));" style="display: block; margin: auto;" type="button">%s</button>
  <div style="clear: both;"></div>
</div>
        ',
          next_label
        )
      }

      # Parse the HTML and append to page
      auto_nav_node <- xml2::read_html(auto_nav_html) |>
        rvest::html_element("body > div")

      xml2::xml_add_child(x, auto_nav_node)

      # Update the Next button ID (always present)
      next_button_id <- make_next_button_id(page_id)
      next_button <- rvest::html_element(x, "#page_id_next")
      xml2::xml_attr(next_button, "id") <- next_button_id

      # Update the Previous button ID (only if show_prev is TRUE)
      if (show_prev) {
        prev_button_id <- make_prev_button_id(page_id)
        prev_button <- rvest::html_element(x, "#page_id_prev")
        xml2::xml_attr(prev_button, "id") <- prev_button_id
        has_prev_button <- TRUE
      } else {
        prev_button_id <- NULL
        has_prev_button <- FALSE
      }

      next_page_id <- "" # Auto-nav always has empty data-next-page (sequential)
    }

    list(
      id = page_id,
      questions = question_ids,
      required_questions = required_question_ids,
      next_button_id = next_button_id,
      next_page_id = next_page_id,
      prev_button_id = prev_button_id,
      content = as.character(x)
    )
  })
  saveRDS(pages, paths$target_pages)
  return(pages)
}

# Get question structure ('smart': load YAML or extract from HTML and save to YAML)
get_question_structure <- function(paths, html_content) {
  question_structure <- extract_question_structure_html(html_content)
  write_question_structure_yaml(question_structure, paths$target_questions)
  return(question_structure)
}

# Extract question structure from HTML
extract_question_structure_html <- function(html_content) {
  question_nodes <- rvest::html_nodes(html_content, "[data-question-id]")
  question_structure <- list()

  # Loop through all question nodes and extract information
  for (question_node in question_nodes) {
    question_id <- rvest::html_attr(question_node, "data-question-id")
    type <- question_node |>
      rvest::html_nodes(glue::glue("#{question_id}")) |>
      rvest::html_attr("class")

    # Handle case where type is empty (e.g., for reactive questions)
    if (length(type) == 0 || is.na(type)) {
      type <- "unknown"
    }

    is_matrix <- length(rvest::html_nodes(question_node, ".matrix-question")) >
      0

    if (is_matrix) {
      type <- "matrix"
    }

    # Extract the question text (label)
    label <- question_node |>
      rvest::html_nodes("p") |>
      rvest::html_text(trim = TRUE)

    # Write main information to the question structure
    question_structure[[question_id]] <- list(
      type = type,
      is_matrix = is_matrix,
      label = label
    )

    # Extract options for the question ( mc, *_multiple, *_buttons, and select)
    if (length(type) > 0 && grepl("radio|checkbox|select|matrix", type)) {
      options <- question_node |>
        rvest::html_nodes(
          "input[type='radio'], input[type='checkbox'], option"
        ) |>
        rvest::html_attr("value")
      label_options <- question_node |>
        rvest::html_nodes("label>span, button, option") |>
        rvest::html_text(trim = TRUE)
      names(options) <- label_options

      # Handle empty names by setting them to their corresponding values
      empty_name_indices <- which(names(options) == "" | is.na(names(options)))
      if (length(empty_name_indices) > 0) {
        names(options)[empty_name_indices] <- options[empty_name_indices]
      }

      # Write options to the question structure
      question_structure[[question_id]]$options <- as.list(options)

      # Extract options for the question (slider)
    } else if (grepl("slider", type)) {
      # Check if this is a numeric slider
      is_numeric_slider <- length(rvest::html_nodes(
        question_node,
        ".js-range-slider:not(.sw-slider-text)"
      )) >
        0

      if (is_numeric_slider) {
        # Extract min, max, and step attributes from the numeric slider
        slider_element <- rvest::html_nodes(question_node, ".js-range-slider")

        min_value <- as.numeric(rvest::html_attr(slider_element, "data-min"))
        max_value <- as.numeric(rvest::html_attr(slider_element, "data-max"))
        step_value <- as.numeric(rvest::html_attr(slider_element, "data-step"))

        # Check if this is a range slider (has data-to attribute)
        is_range <- !is.na(rvest::html_attr(slider_element, "data-to"))

        # Get the from and to values for range sliders
        from_value <- as.numeric(rvest::html_attr(slider_element, "data-from"))
        to_value <- as.numeric(rvest::html_attr(slider_element, "data-to"))

        # Default to step=1 if missing or invalid
        if (is.na(step_value) || step_value <= 0) {
          step_value <- 1
        }

        # Generate sequence
        numeric_values <- seq(min_value, max_value, by = step_value)

        # Create a named list with actual numeric values
        # Start with empty list to ensure the types are preserved
        named_options <- list()
        for (val in numeric_values) {
          named_options[[as.character(val)]] <- val
        }

        # Note that this is a range slider in the structure if needed
        if (is_range) {
          question_structure[[question_id]]$is_range <- TRUE
          # Also store the default range values
          question_structure[[question_id]]$default <- c(from_value, to_value)
        } else if (!is.na(from_value)) {
          # For single sliders, store the default value
          question_structure[[question_id]]$default <- from_value
        }

        question_structure[[question_id]]$options <- named_options
      } else {
        # For regular text slider
        options_labels_raw <- question_node |>
          rvest::html_nodes(".js-range-slider") |>
          rvest::html_attr("data-swvalues")

        # Process labels
        options_labels <- gsub("\\[|\\]|\\\"", "", options_labels_raw) |>
          strsplit(",") |>
          unlist()

        # Convert labels to snake_case for values
        options_values <- sapply(options_labels, function(label) {
          # Convert to lowercase
          value <- tolower(label)
          # Replace spaces and special characters with underscore
          value <- gsub("[^a-z0-9]", "_", value)
          # Replace multiple underscores with a single one
          value <- gsub("_+", "_", value)
          # Remove leading and trailing underscores
          value <- gsub("^_|_$", "", value)
          return(value)
        })

        # Create named options list
        options <- options_values
        names(options) <- options_labels

        question_structure[[question_id]]$options <- as.list(options)
      }
    }

    # Extract the rows and options for the matrix main question
    if (is_matrix) {
      rows <- question_node |>
        rvest::html_nodes("div > div[id]") |>
        rvest::html_attr("id")

      # Remove the question ID prefix from the row names
      rows <- gsub(glue::glue("{question_id}_"), "", rows)
      label_rows <- question_node |>
        rvest::html_nodes("td:nth-child(1)") |>
        rvest::html_text(trim = TRUE)

      # remove first empty row label (option header)
      label_rows <- label_rows[label_rows != ""]
      names(rows) <- label_rows

      # Write rows to the question structure
      question_structure[[question_id]]$row <- as.list(rows)

      # Correct to unique options (first extraction multiplies by subquestions)
      options <- options[1:(length(options) / length(rows))]
      question_structure[[question_id]]$options <- as.list(options)
    }
  }
  return(question_structure)
}

# Write question structure to YAML
write_question_structure_yaml <- function(question_structure, file_yaml) {
  # Map question types to extracted html classes
  type_replacement <- c(
    'shiny-input-text form-control' = 'text',
    'shiny-input-textarea form-control' = 'textarea',
    'shiny-input-number form-control' = 'numeric',
    'form-group shiny-input-radiogroup shiny-input-container' = 'mc',
    'radio-group-buttons' = 'mc_buttons',
    'form-group shiny-input-checkboxgroup shiny-input-container' = 'mc_multiple',
    'checkbox-group-buttons' = 'mc_multiple_buttons',
    'shiny-input-select' = 'select',
    'js-range-slider sw-slider-text' = 'slider',
    'js-range-slider' = 'slider_numeric',
    'shiny-date-input form-group shiny-input-container' = 'date',
    'shiny-date-range-input form-group shiny-input-container' = 'daterange'
  )

  # Loop through question structure and clean/prepare questions
  question_structure <- lapply(question_structure, function(question) {
    # Rename type to function option names
    question$type <- type_replacement[names(type_replacement) == question$type]
    if (question$is_matrix) {
      question$type <- "matrix"
    }
    if (length(question$type) == 0) {
      question$type <- "unknown"
    }

    # Remove indicator if is matrix (type is correctly set)
    question$is_matrix <- NULL

    # Remove first option from select type question
    if (question$type == "select") {
      question$options <- question$options[-1]
    }

    # Special handling for numeric slider options to ensure they remain numeric
    if (question$type == "slider_numeric" && !is.null(question$options)) {
      # Include all options
      options_to_include <- seq_along(question$options)

      # Create a new options list with all elements
      if (length(options_to_include) > 0) {
        question$options <- question$options[options_to_include]
      }
    }

    return(question)
  })

  # Remove NULL elements (matrix subquestions)
  question_structure <- Filter(Negate(is.null), question_structure)

  # Write question to YAML (with comment in first lines)
  yaml_content <- yaml::as.yaml(question_structure)
  comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
  comment_line2 <- "# Question structure extracted from survey.html\n"
  full_content <- paste0(comment_line1, comment_line2, yaml_content)
  writeLines(full_content, con = file_yaml)
}

# Load question structure from YAML
load_question_structure_yaml <- function(file_yaml) {
  # Read question structure from YAML file
  question_structure <- yaml::read_yaml(file_yaml)

  # Add matrix question indicator to all questions as FALSE (correct later)
  question_structure <- lapply(question_structure, function(question) {
    question$is_matrix <- FALSE
    return(question)
  })

  # Get question types to create subquestions for matrix questions
  question_types <- sapply(question_structure, function(q) q$type)
  matrix_questions_ids <- names(question_types)[question_types == "matrix"]

  # Loop trough matrix questions and add subquestions
  for (matrix_question_id in matrix_questions_ids) {
    # Get matrix question and subquestion (rows option) from question list
    matrix_question <- question_structure[[matrix_question_id]]
    rows <- matrix_question$row

    # Loop over subquestions and add to question structure (with label and options)
    for (row_number in seq_along(rows)) {
      subquestion_id <- paste0(matrix_question_id, "_", rows[[row_number]])
      subquestion_structure <- list(
        type = "mc",
        label = names(rows)[row_number],
        options = matrix_question$options
      )
      question_structure[[subquestion_id]] <- subquestion_structure
    }

    # Add matrix question indicator
    question_structure[[matrix_question_id]]$is_matrix <- TRUE
  }

  # Negate NULL elements in is_matrix (matrix subquestions)
  for (question in names(question_structure)) {
    if (is.null(question_structure[[question]]$is_matrix)) {
      question_structure[[question]]$is_matrix <- FALSE
    }
  }
  return(question_structure)
}

get_output_ids <- function() {
  output <- shiny::getDefaultReactiveDomain()$output
  outs <- shiny::outputOptions(output)
  return(names(outs))
}

check_skip_show <- function(
  question_ids,
  question_values,
  page_ids,
  skip_if,
  show_if
) {
  if (!is.null(skip_if)) {
    invalid_skip_targets <- setdiff(skip_if$targets, page_ids)
    if (length(invalid_skip_targets) > 0) {
      stop(sprintf(
        "Invalid sd_skip_if targets: %s. These must be valid page IDs.",
        paste(invalid_skip_targets, collapse = ", ")
      ))
    }
  }

  if (!is.null(show_if)) {
    # Get any potential question_ids from the output
    # Now also include page_ids as valid targets
    invalid_show_targets <- setdiff(
      show_if$targets,
      c(question_ids, page_ids, get_output_ids())
    )
    if (length(invalid_show_targets) > 0) {
      stop(sprintf(
        "Invalid show_if targets: %s. These must be question IDs or page IDs defined in the survey.qmd file.",
        paste(invalid_show_targets, collapse = ", ")
      ))
    }
  }
  return(TRUE)
}

check_ids <- function(page_ids, question_ids) {
  # Check for duplicated IDs
  all_ids <- c(page_ids, question_ids)
  duplicate_ids <- all_ids[duplicated(all_ids)]
  if (length(duplicate_ids) > 0) {
    stop(
      "Duplicate IDs found: ",
      paste(duplicate_ids, collapse = ", "),
      ". All page IDs and question IDs must be unique."
    )
  }

  # Check for restricted IDs
  restricted_ids <- c(
    "session_id",
    "time_start",
    "time_end",
    "exit_survey_rating",
    "current_page",
    "browser",
    "ip_address"
  )
  used_restricted_ids <- intersect(restricted_ids, all_ids)
  if (length(used_restricted_ids) > 0) {
    stop(
      "Restricted IDs found: ",
      paste(used_restricted_ids, collapse = ", "),
      ". These IDs are reserved and should not be used for survey pages or questions."
    )
  }
}

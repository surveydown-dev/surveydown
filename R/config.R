run_config <- function(
  required_questions,
  all_questions_required,
  start_page,
  skip_forward,
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

    # Prepare translations (check for inputs)
    set_translations(paths, language)

    # Create settings YAML file from survey.qmd YAML metadata
    create_settings_yaml(paths)

    # Get the html content from the rendered survey.html file
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
      paths$target_transl,
      "\n",
      "  ",
      paths$target_settings
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

  # Check skip_forward and show_if inputs
  question_values <- unname(unlist(lapply(question_structure, `[[`, "options")))
  check_skip_show(
    question_ids,
    question_values,
    page_ids,
    skip_forward,
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
    transl = "translations.yml",
    target_transl = file.path(target_folder, "translations.yml"),
    target_html = file.path(target_folder, "survey.html"),
    target_pages = file.path(target_folder, "pages.rds"),
    target_head = file.path(target_folder, "head.rds"),
    target_questions = file.path(target_folder, "questions.yml"),
    target_settings = file.path(target_folder, "settings.yml")
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

  # Re-parse if the user provided a 'translations.yml' file which is out of date
  if (fs::file_exists(paths$transl)) {
    time_transl <- file.info(paths$transl)$mtime
    if (time_transl > time_pages) {
      return(TRUE)
    }
  }

  return(FALSE)
}

set_translations <- function(paths, language) {
  # Load default translations
  translations <- get_translations_default()

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

  # Include user provided translations if translations.yml file is in root
  if (fs::file_exists(paths$transl)) {
    # Read translations file and select translations for selected language
    tryCatch(
      {
        user_transl <- yaml::read_yaml(paths$transl)
        user_transl <- user_transl[unique(names(user_transl))]
        user_transl <- user_transl[names(user_transl) == language]

        # Check for valid set of translations for selected language
        if (length(user_transl) == 1) {
          message(
            "User provided translations for language '",
            language,
            "' from '",
            paths$transl,
            "' file loaded."
          )

          # Add missing texts from default translations (if not available, use English)
          if (language %in% names(translations)) {
            predef_transl <- translations[[language]]
          } else {
            predef_transl <- translations[["en"]]
          }
          user_transl[[1]] <- c(
            user_transl[[1]],
            predef_transl[!(names(predef_transl) %in% names(user_transl[[1]]))]
          )
        } else {
          user_transl <- NULL
        }
      },
      error = function(e) {
        message(
          "Error reading '",
          paths$transl,
          "' file. Please review and revise the file. Error details: ",
          e$message
        )
        user_transl <- NULL
      }
    )

    # Combine user provided translations with predefined translations
    translations <- c(user_transl, translations) # user provided take precedence
  }

  # Choose translations by chosen language
  if (!language %in% names(translations)) {
    # Fallback to English if no translations found for selected language
    sd_create_translations(language)
    translations <- translations["en"]
    names(translations) <- language
  } else {
    translations <- translations[names(translations) == language]
    translations <- translations[1]
  }

  # write translations file
  yaml::write_yaml(translations, paths$target_transl)
}

create_settings_yaml <- function(paths) {
  # Extract server configuration from survey.qmd YAML metadata during UI rendering
  if (file.exists("survey.qmd")) {
    tryCatch(
      {
        metadata <- quarto::quarto_inspect("survey.qmd")

        # Define all sd_server parameters that can be configured via YAML
        # Note: language is excluded to avoid breaking Quarto rendering
        server_params <- c(
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

        # Define defaults for all 10 parameters
        default_settings <- list(
          use_cookies = TRUE,
          auto_scroll = FALSE,
          rate_survey = FALSE,
          all_questions_required = FALSE,
          start_page = NULL,
          system_language = "en",
          highlight_unanswered = TRUE,
          highlight_color = "gray",
          capture_metadata = TRUE,
          required_questions = NULL
        )

        # Extract YAML values using helper functions (priority: YAML > defaults)
        settings <- list()
        for (param in server_params) {
          # Use specific extraction functions for parameters that need special handling
          if (param == "use_cookies") {
            value <- get_use_cookies(metadata)
          } else if (param == "auto_scroll") {
            value <- get_auto_scroll(metadata)
          } else if (param == "rate_survey") {
            value <- get_rate_survey(metadata)
          } else if (param == "all_questions_required") {
            value <- get_all_questions_required(metadata)
          } else if (param == "start_page") {
            value <- get_start_page(metadata)
          } else if (param == "system_language") {
            value <- get_system_language(metadata)
          } else if (param == "highlight_unanswered") {
            value <- get_highlight_unanswered(metadata)
          } else if (param == "highlight_color") {
            value <- get_highlight_color(metadata)
          } else if (param == "capture_metadata") {
            value <- get_capture_metadata(metadata)
          } else if (param == "required_questions") {
            value <- get_required_questions(metadata)
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

        # Remove NULL values to avoid YAML issues
        settings <- settings[!sapply(settings, is.null)]

        # Write settings file with YAML header values and defaults
        yaml_content <- yaml::as.yaml(settings)
        comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
        comment_line2 <- "# Server settings with YAML header values and defaults\n"
        full_content <- paste0(comment_line1, comment_line2, yaml_content)
        writeLines(full_content, con = paths$target_settings)
      },
      error = function(e) {
        warning("Could not extract settings from survey.qmd: ", e$message)
        # Create settings file with defaults on error
        default_settings <- list(
          use_cookies = TRUE,
          auto_scroll = FALSE,
          rate_survey = FALSE,
          all_questions_required = FALSE,
          highlight_unanswered = TRUE,
          highlight_color = "gray",
          capture_metadata = TRUE
        )
        yaml_content <- yaml::as.yaml(default_settings)
        comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
        comment_line2 <- "# Server settings with defaults (error reading YAML header)\n"
        full_content <- paste0(comment_line1, comment_line2, yaml_content)
        writeLines(full_content, con = paths$target_settings)
      }
    )
  } else {
    # Create settings file with defaults if survey.qmd doesn't exist
    default_settings <- list(
      use_cookies = TRUE,
      auto_scroll = FALSE,
      rate_survey = FALSE,
      all_questions_required = FALSE,
      highlight_unanswered = TRUE,
      highlight_color = "gray",
      capture_metadata = TRUE
    )
    yaml_content <- yaml::as.yaml(default_settings)
    comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
    comment_line2 <- "# Server settings with defaults (no survey.qmd file found)\n"
    full_content <- paste0(comment_line1, comment_line2, yaml_content)
    writeLines(full_content, con = paths$target_settings)
  }
}

# Function to read settings from _survey/settings.yml file
read_settings_yaml <- function() {
  paths <- get_paths()

  # Define default settings to return if reading fails
  defaults <- list(
    use_cookies = TRUE,
    auto_scroll = FALSE,
    rate_survey = FALSE,
    all_questions_required = FALSE,
    start_page = NULL,
    highlight_unanswered = TRUE,
    highlight_color = "gray",
    capture_metadata = TRUE,
    required_questions = NULL
  )

  # Try multiple possible locations for the settings file
  possible_paths <- c(
    paths$target_settings, # "_survey/settings.yml"
    file.path(getwd(), paths$target_settings), # Full path from current dir
    file.path(".", paths$target_settings), # Explicit relative path
    "settings.yml" # Fallback: just settings.yml in current dir
  )

  settings <- NULL
  successful_path <- NULL

  # Try to read from each path directly (without file.exists check)
  for (path in possible_paths) {
    tryCatch(
      {
        settings <- yaml::read_yaml(path)
        successful_path <- path
        break
      },
      error = function(e) {
        # Continue to next path
      }
    )
  }

  if (is.null(settings)) {
    return(defaults)
  }

  # Normalize dash-separated keys to underscore format for compatibility
  # Define the expected parameter names (underscore format)
  expected_params <- c(
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

  # Check for dash versions and convert to underscore format
  for (param in expected_params) {
    dash_param <- gsub("_", "-", param)
    # If dash version exists but underscore doesn't, copy it over
    if (!is.null(settings[[dash_param]]) && is.null(settings[[param]])) {
      settings[[param]] <- settings[[dash_param]]
      # Remove the dash version to avoid confusion
      settings[[dash_param]] <- NULL
    }
  }

  # Process required_questions if it exists (convert list to character vector)
  if (!is.null(settings$required_questions)) {
    if (is.list(settings$required_questions)) {
      settings$required_questions <- unlist(settings$required_questions)
    }
  }

  return(settings)
}

# Function to update settings.yml with final resolved parameters from sd_server()
update_settings_yaml <- function(resolved_params) {
  paths <- get_paths()

  # Define sd_server parameter defaults (from server.R)
  # Note: language is excluded to avoid breaking Quarto rendering
  default_params <- list(
    use_cookies = TRUE, # Note: this becomes TRUE when NULL is passed to sd_server
    auto_scroll = FALSE,
    rate_survey = FALSE,
    all_questions_required = FALSE,
    start_page = NULL,
    system_language = "en",
    highlight_unanswered = TRUE,
    highlight_color = "gray",
    capture_metadata = TRUE,
    required_questions = NULL
  )

  # Filter out language parameter to avoid breaking Quarto
  resolved_params$language <- NULL

  # Merge defaults with resolved params (resolved params take priority)
  final_settings <- default_params
  for (param_name in names(resolved_params)) {
    if (!is.null(resolved_params[[param_name]])) {
      final_settings[[param_name]] <- resolved_params[[param_name]]
    }
  }

  # Handle special case for use_cookies (NULL becomes TRUE)
  if (is.null(final_settings$use_cookies)) {
    final_settings$use_cookies <- TRUE
  }

  # Remove NULL values to avoid YAML issues
  final_settings <- final_settings[!sapply(final_settings, is.null)]

  # Create YAML content
  yaml_content <- yaml::as.yaml(final_settings)
  comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
  comment_line2 <- "# Server settings with final resolved parameters\n"
  comment_line3 <- "# (includes sd_server() parameters, YAML header, and defaults)\n"
  full_content <- paste0(
    comment_line1,
    comment_line2,
    comment_line3,
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
        "use_cookies\\s*=\\s*(TRUE|FALSE|T|F)",
        "auto_scroll\\s*=\\s*(TRUE|FALSE|T|F)",
        "rate_survey\\s*=\\s*(TRUE|FALSE|T|F)",
        "all_questions_required\\s*=\\s*(TRUE|FALSE|T|F)",
        "start_page\\s*=\\s*[\"']([^\"']+)[\"']",
        "highlight_unanswered\\s*=\\s*(TRUE|FALSE|T|F)",
        "highlight_color\\s*=\\s*[\"']([^\"']+)[\"']",
        "capture_metadata\\s*=\\s*(TRUE|FALSE|T|F)",
        "required_questions\\s*=\\s*c\\s*\\(([^)]+)\\)"
      )

      param_names <- c(
        "use_cookies",
        "auto_scroll",
        "rate_survey",
        "all_questions_required",
        "start_page",
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
                "use_cookies",
                "auto_scroll",
                "rate_survey",
                "all_questions_required",
                "highlight_unanswered",
                "capture_metadata"
              )
          ) {
            overrides[[param_name]] <- value_str %in% c("TRUE", "T")
          } else if (param_name %in% c("start_page", "highlight_color")) {
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

  pages <- lapply(pages_elements, function(x) {
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
    list(
      id = page_id,
      questions = question_ids,
      required_questions = required_question_ids,
      next_button_id = next_button_id,
      next_page_id = next_page_id,
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
    if (grepl("radio|checkbox|select|matrix", type)) {
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
  skip_forward,
  show_if
) {
  if (!is.null(skip_forward)) {
    invalid_skip_targets <- setdiff(skip_forward$targets, page_ids)
    if (length(invalid_skip_targets) > 0) {
      stop(sprintf(
        "Invalid sd_skip_forward targets: %s. These must be valid page IDs.",
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

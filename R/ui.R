#' Create the UI for a surveydown survey
#'
#' This function creates the user interface for a surveydown survey,
#' including necessary CSS and JavaScript files, and applies custom styling.
#' It retrieves theme and progress bar settings from the survey.qmd file.
#'
#' @return A 'shiny' UI object
#'
#' @details
#' The function reads the following settings from the survey.qmd YAML header:
#' \itemize{
#'   \item `theme`: The theme to be applied to the survey.
#'   \item `barcolor`: The color of the progress bar (should be a valid hex color).
#'   \item `barposition`: The position of the progress bar (`'top'`, `'bottom'`, or `'none'`).
#' }
#'
#' If `barcolor` is not specified or is `NULL`, the default theme color will be
#' used. If `barposition` is not specified, it defaults to 'top'.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_ui.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso `sd_server()` for creating the server-side logic of the survey
#'
#' @export
sd_ui <- function() {
  # Throw error if 'survey.qmd' or 'app.R' files are missing
  check_files_missing()

  # Get metadata from the 'survey.qmd' file
  metadata <- quarto::quarto_inspect("survey.qmd")

  theme <- get_theme(metadata)
  default_theme <- FALSE
  if (any(theme == "default")) {
    default_theme <- TRUE
  }
  barcolor <- get_barcolor(metadata)
  barposition <- get_barposition(metadata)
  footer <- get_footer(metadata)

  # Get paths to files and create '_survey' folder if necessary
  paths <- get_paths()

  # Render the 'survey.qmd' file if changes detected
  if (survey_needs_updating(paths)) {
    message("Changes detected...rendering survey files...")
    render_survey_qmd(paths, default_theme)

    # Move rendered file
    fs::file_move(paths$root_html, paths$target_html)

    # Extract head content and save
    html_content <- rvest::read_html(paths$target_html)
    head_content <- extract_head_content(html_content)
    saveRDS(head_content, paths$target_head)
  } else {
    # If no changes, just load head content from '_survey/head.rds'
    head_content <- readRDS(paths$target_head)
  }

  # Create settings YAML file from survey.qmd YAML metadata
  create_settings_yaml(paths, metadata)

  # Create the UI
  shiny::tagList(
    # Head content
    shiny::tags$head(
      # Survey head content (filtered)
      shiny::HTML(head_content)
    ),
    # Body content
    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::tags$script("var surveydownConfig = {};"),
      if (!is.null(barcolor)) {
        shiny::tags$style(htmltools::HTML(sprintf(
          "
            :root {
                --progress-color: %s;
            }
          ",
          barcolor
        )))
      },
      if (barposition != "none") {
        shiny::tags$div(
          id = "progressbar",
          class = barposition,
          shiny::tags$div(id = "progress")
        )
      },
      shiny::tags$div(
        class = "content",
        shiny::uiOutput("main")
      ),
      if (nchar(footer) > 0) {
        shiny::tags$div(
          class = "footer",
          shiny::HTML(footer)
        )
      }
    ) # fluidPage
  ) # shiny::tagList()
}

get_theme <- function(metadata) {
  x <- "survey.qmd"
  theme <- metadata$formats$html$metadata$theme
  if (is.null(theme)) {
    return("default")
  }
  return(theme)
}

get_barcolor <- function(metadata) {
  barcolor <- metadata$formats$html$metadata$barcolor
  if (!is.null(barcolor)) {
    if (!grepl("^#([0-9A-Fa-f]{3}){1,2}$", barcolor)) {
      stop("Invalid barcolor in YAML. Use a valid hex color.")
    }
  }
  return(barcolor)
}

get_barposition <- function(metadata) {
  barposition <- metadata$formats$html$metadata$barposition
  if (is.null(barposition)) {
    return("top")
  }
  return(barposition)
}

process_links <- function(text) {
  if (is.null(text)) {
    return("")
  }

  # Convert markdown links to HTML first
  text <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', text)

  # Then add target="_blank" to any HTML links that don't have it
  pattern <- '<a [^>]*?href="[^"]*?"[^>]*?>'
  matches <- gregexpr(pattern, text, perl = TRUE)
  if (length(matches[[1]]) > 0 && matches[[1]][1] != -1) {
    links <- regmatches(text, matches)[[1]]
    for (link in links) {
      if (!grepl('target=', link)) {
        new_link <- sub('>', ' target="_blank">', link)
        text <- sub(link, new_link, text, fixed = TRUE)
      }
    }
  }

  return(text)
}

get_footer <- function(metadata) {
  # Get the metadata safely
  meta <- metadata$formats$html$metadata
  if (is.null(meta)) {
    return("")
  }

  # Get footer-related fields with both underscore and dash support
  footer_left <- get_yaml_value(metadata, "footer_left")
  footer_right <- get_yaml_value(metadata, "footer_right")
  footer_center <- get_yaml_value(metadata, "footer_center")
  plain_footer <- meta$footer

  # If footer-center doesn't exist but plain footer does, use plain footer
  if (is.null(footer_center) && !is.null(plain_footer)) {
    footer_center <- plain_footer
  }

  # If all are NULL, return empty string
  if (is.null(footer_center) && is.null(footer_left) && is.null(footer_right)) {
    return("")
  }

  # Process each section if it exists
  footer_html <- c()

  if (!is.null(footer_left) && nchar(footer_left) > 0) {
    footer_html <- c(
      footer_html,
      sprintf('<div class="footer-left">%s</div>', process_links(footer_left))
    )
  }

  if (!is.null(footer_center) && nchar(footer_center) > 0) {
    footer_html <- c(
      footer_html,
      sprintf(
        '<div class="footer-center">%s</div>',
        process_links(footer_center)
      )
    )
  }

  if (!is.null(footer_right) && nchar(footer_right) > 0) {
    footer_html <- c(
      footer_html,
      sprintf('<div class="footer-right">%s</div>', process_links(footer_right))
    )
  }

  # Return the final HTML
  return(paste0(
    '<div class="footer-content">',
    paste(footer_html, collapse = ""),
    '</div>'
  ))
}

# Helper function to get YAML values supporting both underscore and dash formats
get_yaml_value <- function(metadata, key) {
  yaml_data <- metadata$formats$html$metadata
  # Try underscore version first
  if (!is.null(yaml_data[[key]])) {
    return(yaml_data[[key]])
  }
  # Try dash version
  dash_key <- gsub("_", "-", key)
  if (!is.null(yaml_data[[dash_key]])) {
    return(yaml_data[[dash_key]])
  }
  return(NULL)
}

get_use_cookies <- function(metadata) {
  use_cookies <- get_yaml_value(metadata, "use_cookies")
  if (is.null(use_cookies)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(use_cookies)) {
    return(use_cookies %in% c("TRUE", "True", "true", "yes", "Yes", "YES"))
  }
  return(as.logical(use_cookies))
}

get_auto_scroll <- function(metadata) {
  auto_scroll <- get_yaml_value(metadata, "auto_scroll")
  if (is.null(auto_scroll)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(auto_scroll)) {
    return(auto_scroll %in% c("TRUE", "True", "true", "yes", "Yes", "YES"))
  }
  return(as.logical(auto_scroll))
}

get_rate_survey <- function(metadata) {
  rate_survey <- get_yaml_value(metadata, "rate_survey")
  if (is.null(rate_survey)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(rate_survey)) {
    return(rate_survey %in% c("TRUE", "True", "true", "yes", "Yes", "YES"))
  }
  return(as.logical(rate_survey))
}

get_all_questions_required <- function(metadata) {
  all_questions_required <- get_yaml_value(metadata, "all_questions_required")
  if (is.null(all_questions_required)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(all_questions_required)) {
    return(
      all_questions_required %in% c("TRUE", "True", "true", "yes", "Yes", "YES")
    )
  }
  return(as.logical(all_questions_required))
}

get_start_page <- function(metadata) {
  start_page <- get_yaml_value(metadata, "start_page")
  if (is.null(start_page)) {
    return(NULL)
  }
  return(as.character(start_page))
}

get_system_language <- function(metadata) {
  system_language <- get_yaml_value(metadata, "system_language")
  if (is.null(system_language)) {
    return(NULL)
  }
  return(as.character(system_language))
}

get_language <- function(metadata) {
  language <- metadata$formats$html$metadata$language
  if (is.null(language)) {
    return(NULL)
  }
  return(as.character(language))
}

get_highlight_unanswered <- function(metadata) {
  highlight_unanswered <- get_yaml_value(metadata, "highlight_unanswered")
  if (is.null(highlight_unanswered)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(highlight_unanswered)) {
    return(
      highlight_unanswered %in% c("TRUE", "True", "true", "yes", "Yes", "YES")
    )
  }
  return(as.logical(highlight_unanswered))
}

get_highlight_color <- function(metadata) {
  highlight_color <- get_yaml_value(metadata, "highlight_color")
  if (is.null(highlight_color)) {
    return(NULL)
  }
  return(as.character(highlight_color))
}

get_capture_metadata <- function(metadata) {
  capture_metadata <- get_yaml_value(metadata, "capture_metadata")
  if (is.null(capture_metadata)) {
    return(NULL)
  }
  # Handle both TRUE/FALSE and yes/no formats
  if (is.character(capture_metadata)) {
    return(capture_metadata %in% c("TRUE", "True", "true", "yes", "Yes", "YES"))
  }
  return(as.logical(capture_metadata))
}

get_required_questions <- function(metadata) {
  required_questions <- get_yaml_value(metadata, "required_questions")
  if (is.null(required_questions)) {
    return(NULL)
  }
  # Handle both single string and list/vector of strings
  if (is.character(required_questions)) {
    return(required_questions)
  } else if (is.list(required_questions)) {
    # Convert list to character vector
    return(unlist(required_questions))
  } else {
    return(as.character(required_questions))
  }
}

find_all_yaml_files <- function() {
  # Find all yml files
  all_files <- list.files(
    path = ".",
    pattern = "\\.(yml|yaml)$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Exclude the _survey/ directory
  yaml_files <- all_files[!grepl("^\\./?\\_survey/", all_files)]

  return(unique(yaml_files))
}

survey_needs_updating <- function(paths) {
  # Re-render if any of the target files are missing
  targets <- c(paths$target_html, paths$target_head)
  if (any(!fs::file_exists(targets))) {
    return(TRUE)
  }

  # Re-render if '_survey/survey.html' is out of date with 'survey.qmd'
  time_qmd <- file.info(paths$qmd)$mtime
  time_html <- file.info(paths$target_html)$mtime

  if (time_qmd > time_html) {
    return(TRUE)
  }

  # Find all YAML files
  yaml_files <- find_all_yaml_files()

  # Check if any YAML file is newer than the rendered HTML
  for (yaml_file in yaml_files) {
    if (fs::file_exists(yaml_file)) {
      time_yml <- file.info(yaml_file)$mtime
      if (time_yml > time_html) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

render_survey_qmd <- function(paths, default_theme = TRUE) {
  # Copy lua filter to local folder
  lua_file <- 'surveydown.lua'
  fs::file_copy(
    system.file("lua/include-resources.lua", package = "surveydown"),
    lua_file,
    overwrite = TRUE
  )

  # Render the survey.qmd file
  quarto::quarto_render(
    input = paths$qmd,
    metadata = list(
      default_theme = default_theme
    ),
    pandoc_args = c(
      "--embed-resources",
      "--lua-filter=surveydown.lua"
    ),
    # Turn off quiet mode to capture output
    quiet = FALSE
  )

  # Delete lua file from root folder
  if (file.exists(lua_file)) {
    fs::file_delete(lua_file)
  }
}

extract_head_content <- function(html_content) {
  # Head content from the rendered 'survey.html' file
  head_content <- html_content |>
    rvest::html_element("head") |>
    rvest::html_children() |>
    sapply(as.character) |>
    paste(collapse = "\n")
  return(head_content)
}

#' Create a survey question
#'
#' This function creates various types of survey questions for use in a Surveydown survey.
#'
#' @param id A unique identifier for the question, which will be used as the
#' variable name in the resulting survey data.
#' @param type Specifies the type of question. Possible values are `"select"`,
#' `"mc"`, `"mc_multiple"`, `"mc_buttons"`, `"mc_multiple_buttons"`, `"text"`,
#' `"textarea"`, `"numeric"`, `"slider"`, `"slider_numeric"`, `"date"`,
#' `"daterange"`, and `"matrix"`. Defaults to `NULL`.
#' @param label Character string. The label for the UI element, which can be
#' formatted with markdown. Defaults to `NULL`
#' @param cols Integer. Number of columns for the `"textarea"` question type.
#' Defaults to `80`.
#' @param direction Character string. The direction for button groups
#' (`"horizontal"` or `"vertical"`). Defaults to `"horizontal"`.
#' @param status Character string. The status for button groups.
#' Defaults to `"default"`.
#' @param width Character string. The width of the UI element.
#' Defaults to `"100%"`.
#' @param height Character string. The height of the input for the
#' `"textarea"` question type. Defaults to `"100px"`.
#' @param selected Value. The selected value(s) for certain input elements.
#' @param label_select Character string. The label for the select input.
#' Defaults to `"Choose an option..."`.
#' @param grid Logical. Whether to show a grid for slider input.
#' Defaults to `TRUE`.
#' @param individual Logical. Whether buttons in a group should be individually
#'  styled. Defaults to `TRUE`.
#' @param justified Logical. Whether buttons in a group should fill the width
#' of the parent div. Defaults to `FALSE`.
#' @param force_edges Logical. Whether to force edges for slider input.
#' Defaults to `TRUE`.
#' @param option Named vector for the `"select"`, `"radio"`, `"checkbox"`,
#' and `"slider"` question types, or numeric vector for `"slider_numeric"`
#' question type.
#' @param placeholder Character string. Placeholder text for `"text"` and
#' `"textarea"` question types.
#' @param resize Character string. Resize option for textarea input.
#' Defaults to `NULL`.
#' @param row List. Used for `"matrix"` type questions. Contains the row labels
#' and their corresponding IDs.
#' @param default Numeric, length 1 (for a single sided slider), or 2 for a
#' two sided (range based) slider. Values to be used as the starting default
#' for the slider. Defaults to the median of values.
#' @param yml Character string. The name of the YAML file to load question configurations from.
#' Defaults to `"questions.yml"`. Custom YAML files can be specified, either in
#' the root directory or subdirectories (e.g., `"folder/custom.yml"`).
#' @param ... Additional arguments, often specific to different input types.
#' Examples include `pre`, `sep`, `step`, and `animate` for `"slider"` and
#' `"slider_numeric"` question types, etc.
#' @details
#' The function supports various question types:
#' - `"select"`: A dropdown selection
#' - `"mc"`: Multiple choice (single selection)
#' - `"mc_multiple"`: Multiple choice (multiple selections allowed)
#' - `"mc_buttons"`: Multiple choice with button-style options (single selection)
#' - `"mc_multiple_buttons"`: Multiple choice with button-style options (multiple selections allowed)
#' - `"text"`: Single-line text question
#' - `"textarea"`: Multi-line text question
#' - `"numeric"`: Numeric question
#' - `"slider"`: Slider question
#' - `"slider_numeric"`: Extended numeric slider question
#' - `"date"`: Date question
#' - `"daterange"`: Date range question
#' - `"matrix"`: Matrix-style question with rows and columns
#'
#' For `"matrix"` type questions, use the `row` parameter to define the rows of
#' the matrix. Each element in the `row` list should have a name (used as the
#' row ID) and a value (used as the row label).
#'
#' @return A 'shiny' UI element wrapped in a div with a data attribute for
#' question ID.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "basic_survey.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @export
sd_question <- function(
  id,
  type = NULL,
  label = NULL,
  option = NULL,
  cols = "80",
  direction = "horizontal",
  status = "default",
  width = "100%",
  height = NULL,
  selected = NULL,
  label_select = "Choose an option...",
  grid = TRUE,
  individual = TRUE,
  justified = FALSE,
  force_edges = TRUE,
  placeholder = NULL,
  resize = NULL,
  row = NULL,
  default = NULL,
  yml = "questions.yml",
  ...
) {
  # Define valid question types
  valid_types <- c(
    "select",
    "mc",
    "mc_multiple",
    "mc_buttons",
    "mc_multiple_buttons",
    "text",
    "textarea",
    "numeric",
    "slider",
    "slider_numeric",
    "date",
    "daterange",
    "matrix"
  )

  # Define types that require options
  types_requiring_options <- c(
    "select",
    "mc",
    "mc_multiple",
    "mc_buttons",
    "mc_multiple_buttons",
    "slider",
    "slider_numeric",
    "matrix"
  )

  # First check for missing arguments and try to load from local yml file
  missing_option <- is.null(option) &&
    !is.null(type) &&
    (type %in% types_requiring_options)
  if (is.null(type) || is.null(label) || missing_option) {
    # Check if the yml file exists first
    if (!file.exists(yml)) {
      # Throw error if the yml file doesn't exist, regardless of whether it's the default or custom
      stop("Specified yml file '", yml, "' not found for question ", id)
    }
    # Attempt to load existing yml file
    tryCatch(
      {
        root_questions <- yaml::read_yaml(yml)
        if (is.null(root_questions[[id]])) {
          stop("Question '", id, "' not found in yml file ", yml)
        } else {
          q_data <- root_questions[[id]]

          # Only override parameters that weren't provided
          if (is.null(type)) {
            type <- q_data$type
          }

          if (is.null(label)) {
            label <- q_data$label
          }

          # Handle different option formats based on question type
          if (is.null(option) && !is.null(q_data$options)) {
            if (is.list(q_data$options)) {
              # Convert list to named vector for option parameter
              option_names <- names(q_data$options)
              option_values <- unlist(q_data$options)
              option <- option_values
              names(option) <- option_names
            } else {
              option <- q_data$options
            }
          }

          # Load default value for numeric sliders
          if (
            is.null(default) &&
              type == "slider_numeric" &&
              !is.null(q_data$default)
          ) {
            default <- q_data$default
          }

          # Handle range slider flag
          if (
            type == "slider_numeric" &&
              !is.null(q_data$is_range) &&
              q_data$is_range
          ) {
            # If it's a range slider and we don't have a default value yet,
            # create a default range using min/max from options
            if (is.null(default) && !is.null(option)) {
              options_numeric <- as.numeric(option)
              min_val <- min(options_numeric)
              max_val <- max(options_numeric)
              # Default to 1/3 and 2/3 of the range
              range_width <- max_val - min_val
              default <- c(min_val + range_width / 3, max_val - range_width / 3)
            }
          }

          # Handle row for matrix questions
          if (is.null(row) && !is.null(q_data$row) && is.list(q_data$row)) {
            row_names <- names(q_data$row)
            row_values <- unlist(q_data$row)
            row <- row_values
            names(row) <- row_names
          }
        }
      },
      error = function(e) {
        stop("Error reading yml file '", yml, "': ", e$message)
      }
    )
  }

  # Check if provided type is valid
  if (is.null(type)) {
    stop(
      "Question type is required but missing. Please provide a type or ensure it exists in the questions.yml file."
    )
  }

  if (!type %in% valid_types) {
    stop(
      sprintf(
        "Invalid question type: '%s'. Valid types are: %s",
        type,
        paste(sort(valid_types), collapse = "', '")
      )
    )
  }

  output <- NULL

  # Load translations for selected label and date language option
  translations <- get_translations()
  language <- translations$language
  translations <- translations$translations

  # Check if question if answered
  js_interaction <- sprintf(
    "Shiny.setInputValue('%s_interacted', true, {priority: 'event'});",
    id
  )

  # Auto-save helper function for untouched questions
  create_autosave_js <- function(question_id, question_type, params = list()) {
    sprintf(
      "
      $(document).ready(function() {
        var questionId = '%s';
        var questionType = '%s';
        var hasInteracted = false;
        var params = %s;

        // Auto-save function for untouched questions
        function autoSaveQuestion() {
          // Check if user has already interacted with this question
          var interactedInput = Shiny.shinyapp.$inputValues[questionId + '_interacted'];
          if (interactedInput || hasInteracted) {
            // Question was already interacted with, don't auto-save
            return;
          }

          // Double-check by looking at actual DOM changes for sliders
          if (questionType === 'slider' || questionType === 'slider_numeric_single') {
            var currentElement = $('#' + questionId);
            var initialValue = currentElement.data('initial-value');

            // If we stored an initial value and current value differs, user interacted
            if (initialValue !== undefined) {
              var currentValue = currentElement.val();
              if (currentValue != initialValue) {
                hasInteracted = true;
                return; // Don't auto-save, user made changes
              }
            }
          }

          var valueToSave = null;

          // Handle different question types - use stored defaults, not current DOM values
          if (questionType === 'slider') {
            // Use the original default value from params, not current DOM value
            valueToSave = params.defaultValue;
          } else if (questionType === 'slider_numeric_single') {
            valueToSave = params.defaultValue;
          } else if (questionType === 'slider_numeric_range') {
            valueToSave = params.defaultValue.join(', ');
            Shiny.setInputValue(questionId + '_manual_range', valueToSave, {priority: 'event'});
            Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
            Shiny.setInputValue(questionId + '_autosave_timestamp', Date.now(), {priority: 'event'});
            clearQuestionHighlighting(questionId);
            return;
          } else if (questionType === 'date') {
            // For date inputs, try multiple ways to get the value
            var dateElement = $('#' + questionId);
            var inputElement = dateElement.find('input[type=\"text\"]');

            // Try different methods to get the date value
            valueToSave = inputElement.val() ||
                         dateElement.val() ||
                         dateElement.attr('data-date') ||
                         dateElement.find('input').val() || '';
          } else if (questionType === 'daterange') {
            // For date range inputs, get both start and end dates
            var container = $('#' + questionId);
            var startDate = container.find('input').eq(0).val() || '';
            var endDate = container.find('input').eq(1).val() || '';

            // Join with comma and space to match expected format: 2025-06-17, 2025-06-18
            if (startDate && endDate) {
              valueToSave = startDate + ', ' + endDate;
            } else if (startDate || endDate) {
              valueToSave = (startDate || '') + ', ' + (endDate || '');
            } else {
              valueToSave = '';
            }
          }

          // Mark as interacted and save value with timestamp trigger
          Shiny.setInputValue(questionId + '_interacted', true, {priority: 'event'});
          if (valueToSave !== null && valueToSave !== '') {
            Shiny.setInputValue(questionId, valueToSave, {priority: 'event'});
          }
          // Force timestamp update by sending a separate autosave timestamp signal
          Shiny.setInputValue(questionId + '_autosave_timestamp', Date.now(), {priority: 'event'});

          // Clear gray highlighting since this question is now interacted
          clearQuestionHighlighting(questionId);
        }

        // Function to clear highlighting for this specific question
        function clearQuestionHighlighting(questionId) {
          // Find question container using multiple strategies
          var questionContainer = $('[data-question-id=\\\"' + questionId + '\\\"]');
          if (questionContainer.length === 0) {
            questionContainer = $('#container-' + questionId);
          }
          if (questionContainer.length === 0) {
            var input = $('#' + questionId);
            if (input.length > 0) {
              questionContainer = input.closest('.question-container, .form-group, .shiny-input-container');
            }
          }

          if (questionContainer.length > 0) {
            // Remove all highlighting classes
            questionContainer.removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray required-question-highlight');
            // Also remove from form controls inside
            questionContainer.find('.form-control, input, select, textarea').removeClass('unanswered-question-highlight unanswered-question-highlight-orange unanswered-question-highlight-green unanswered-question-highlight-purple unanswered-question-highlight-gray required-question-highlight');
          }
        }

        // Mark as interacted when user actually interacts
        window['markInteracted_' + questionId] = function() {
          hasInteracted = true;
        };

        // Store initial values for sliders to detect changes and bind additional interaction events
        if (questionType === 'slider' || questionType === 'slider_numeric_single') {
          setTimeout(function() {
            var element = $('#' + questionId);
            if (element.length > 0) {
              element.data('initial-value', element.val());

              // Additional interaction tracking for edge cases
              element.on('input change slide slidechange', function() {
                hasInteracted = true;
                window['markInteracted_' + questionId] && window['markInteracted_' + questionId]();
              });
            }
          }, 100);
        } else if (questionType === 'slider_numeric_range') {
          // For range sliders, bind additional events to detect interactions
          setTimeout(function() {
            var element = $('#' + questionId);
            if (element.length > 0) {
              element.on('input change slide slidechange', function() {
                hasInteracted = true;
                window['markInteracted_' + questionId] && window['markInteracted_' + questionId]();
              });
            }
          }, 100);
        }

        // Listen for Next and Close button clicks
        $(document).on('click', '.sd-enter-button', function(e) {
          if ($(this).attr('onclick') && $(this).attr('onclick').includes('next_page')) {
            autoSaveQuestion();
          }
        });

        $(document).on('click', '#close-survey-button', function(e) {
          autoSaveQuestion();
        });
      });
    ",
      question_id,
      question_type,
      jsonlite::toJSON(params)
    )
  }

  # Create label with hidden asterisk
  label <- markdown_to_html(label)

  if (type == "select") {
    label_select <- translations[['choose_option']]

    # Add blank option for visible selected option
    option <- c("", option)
    names(option)[1] <- label_select

    output <- shiny::selectInput(
      inputId = id,
      label = label,
      choices = option,
      multiple = FALSE,
      selected = FALSE,
      ...
    )
  } else if (type == "mc") {
    output <- shiny::radioButtons(
      inputId = id,
      label = label,
      choices = option,
      selected = FALSE,
      ...
    )
  } else if (type == "mc_multiple") {
    output <- shiny::checkboxGroupInput(
      inputId = id,
      label = label,
      choices = option,
      selected = FALSE,
      ...
    )
  } else if (type == "mc_buttons") {
    output <- shinyWidgets::radioGroupButtons(
      inputId = id,
      label = label,
      choices = list_name_md_to_html(option),
      direction = direction,
      selected = character(0),
      ...
    )

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(sprintf(
        "
            $(document).on('click', '#%s .btn', function() {
                %s
                // Small delay to allow button state to update
                setTimeout(function() {
                    var selectedValue = '';
                    // Look for checked radio input within the container
                    var checkedInput = $('#%s input[type=\"radio\"]:checked');
                    if (checkedInput.length > 0) {
                        selectedValue = checkedInput.val();
                    }
                    Shiny.setInputValue('%s', selectedValue, {priority: 'event'});
                }, 50);
            });
        ",
        id,
        js_interaction,
        id,
        id
      )))
    )
  } else if (type == "mc_multiple_buttons") {
    output <- shinyWidgets::checkboxGroupButtons(
      inputId = id,
      label = label,
      choices = list_name_md_to_html(option),
      direction = direction,
      individual = individual,
      justified = FALSE,
      selected = character(0),
      ...
    )

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(sprintf(
        "
            $(document).on('click', '#%s .btn', function() {
                %s
                // Small delay to allow button state to update
                setTimeout(function() {
                    var selectedValues = [];
                    // Look for checked checkbox inputs within the container
                    $('#%s input[type=\"checkbox\"]:checked').each(function() {
                        selectedValues.push($(this).val());
                    });
                    Shiny.setInputValue('%s', selectedValues, {priority: 'event'});
                }, 50);
            });
        ",
        id,
        js_interaction,
        id,
        id
      )))
    )
  } else if (type == "text") {
    output <- shiny::textInput(
      inputId = id,
      label = label,
      placeholder = option,
      ...
    )
  } else if (type == "textarea") {
    output <- shiny::textAreaInput(
      inputId = id,
      label = label,
      height = "100px",
      cols = cols,
      value = NULL,
      rows = "6",
      placeholder = placeholder,
      resize = resize,
      ...
    )
  } else if (type == "numeric") {
    output <- shiny::textInput(
      inputId = id,
      label = label,
      value = "",
      ...
    )

    # Add interaction tracking, custom numeric validation, and native-style spinner
    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(sprintf(
        "
        $(document).ready(function() {
            $('#%s').on('focus input change', function() {
                Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
            });


            // Transform the input to look like a number input
            var inputElement = $('#%s');
            inputElement.attr('type', 'text'); // Keep as text for our validation
            inputElement.addClass('numeric-input-with-spinner');
            inputElement.wrap('<div class=\"numeric-input-container\"></div>');

            // Add native-style spinner
            inputElement.after(`
                <div class=\"native-spinner\">
                    <button type=\"button\" class=\"native-spinner-button spinner-up\" tabindex=\"-1\"></button>
                    <button type=\"button\" class=\"native-spinner-button spinner-down\" tabindex=\"-1\"></button>
                </div>
            `);

            var container = inputElement.parent();

            // Spinner functionality
            container.find('.spinner-up').on('mousedown', function(e) {
                e.preventDefault();
                var currentVal = parseFloat(inputElement.val()) || 0;
                var newVal = currentVal + 1;
                inputElement.val(newVal).trigger('input');
            });

            container.find('.spinner-down').on('mousedown', function(e) {
                e.preventDefault();
                var currentVal = parseFloat(inputElement.val()) || 0;
                var newVal = currentVal - 1;
                inputElement.val(newVal).trigger('input');
            });

            // Custom numeric validation
            $('#%s').on('input', function(e) {
                var val = $(this).val();
                var filtered = '';
                var hasDecimal = false;
                var hasSign = false;

                for (var i = 0; i < val.length; i++) {
                    var char = val[i];

                    // Allow +/- only at the beginning and only one
                    if ((char === '+' || char === '-') && i === 0 && !hasSign) {
                        filtered += char;
                        hasSign = true;
                    }
                    // Allow digits
                    else if (/[0-9]/.test(char)) {
                        filtered += char;
                    }
                    // Allow decimal point only once
                    else if (char === '.' && !hasDecimal) {
                        filtered += char;
                        hasDecimal = true;
                    }
                }

                if (val !== filtered) {
                    $(this).val(filtered);
                }
            });

            // Handle paste events
            $('#%s').on('paste', function(e) {
                setTimeout(function() {
                    $('#%s').trigger('input');
                }, 1);
            });
        });
    ",
        id,
        id,
        id,
        id,
        id,
        id
      )))
    )
  } else if (type == "slider") {
    # Extract display labels and values
    display_labels <- names(option)
    values <- unname(option)

    # Value to display mapping (for finding the display label from a selected value)
    value_to_label <- display_labels
    names(value_to_label) <- values

    # Create a choices vector that sliderTextInput will use
    slider_choices <- display_labels

    # Determine the selected display label based on the selected value
    selected_label <- NULL
    if (!is.null(selected) && selected != "") {
      selected_label <- value_to_label[selected]
    }

    # If no valid selection, default to first choice
    if (is.null(selected_label) || is.na(selected_label)) {
      selected_label <- slider_choices[1]
    }

    # Store the mapping for later use in JavaScript
    value_map <- option

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      session <- shiny::getDefaultReactiveDomain()
      session$userData[[paste0(id, "_values")]] <- value_map
    }

    # Create the slider with display labels
    output <- shinyWidgets::sliderTextInput(
      inputId = id,
      label = label,
      choices = slider_choices, # These are the display labels
      selected = selected_label, # Must be a display label, not a value
      force_edges = force_edges,
      grid = grid,
      ...
    )

    # Store the values in a data attribute for extraction
    values_json <- jsonlite::toJSON(values)

    # Add a data-values attribute to the input element for extraction
    js_add_values <- sprintf(
      '
        $(document).ready(function() {
          $("#%s input").attr("data-values", %s);
        });
      ',
      id,
      values_json
    )

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(js_add_values))
    )

    # JavaScript to map the display label back to the stored value and track interaction
    js_convert <- sprintf(
      "
      $(document).ready(function() {
        var valueMap = %s;

        $('#%s').on('focus mousedown change', function(e) {
          var currentLabel = $(this).val();

          // Track interaction on focus or mousedown (user initiated)
          if (e.type === 'focus' || e.type === 'mousedown') {
            window['markInteracted_%s']();
            Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
          }

          // Find the internal value that matches this display label
          Shiny.setInputValue('%s', valueMap[currentLabel]);
        });
      });
    ",
      jsonlite::toJSON(as.list(value_map)),
      id,
      id,
      id,
      id
    )

    # Add auto-save functionality
    # Convert selected_label to its corresponding value for auto-save
    default_value <- value_map[[selected_label]]
    autosave_js <- create_autosave_js(
      id,
      "slider",
      list(
        valueMap = as.list(value_map),
        defaultValue = default_value,
        defaultLabel = selected_label
      )
    )
    js_convert <- paste(js_convert, autosave_js, sep = "\n")

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(js_convert))
    )
  } else if (type == "slider_numeric") {
    # Handle numeric slider - supports BOTH single and range values
    slider_values <- option

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      session <- shiny::getDefaultReactiveDomain()
      session$userData[[paste0(id, "_values")]] <- slider_values
    }

    if (is.null(default)) {
      default <- stats::median(slider_values)
    }

    # Check if this is a range slider
    is_range <- is.numeric(default) && length(default) > 1

    # Create the slider
    output <- shiny::sliderInput(
      inputId = id,
      label = label,
      min = min(slider_values),
      max = max(slider_values),
      value = default, # Can be single value or vector of length 2 for range
      ...
    )

    # For range sliders, add a custom observer that manually creates a string value
    if (is_range) {
      # Add JavaScript to force a manual string representation of the range
      js_range_handler <- sprintf(
        "
      $(document).ready(function() {
        // Track interaction on mousedown/focus
        $('#%s').on('mousedown focus', function() {
          window['markInteracted_%s']();
          Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
        });

        // Handle value changes
        $('#%s').on('input change slide slidechange', function(event, ui) {
          // Force a string representation for range sliders
          if (ui && ui.values) {
            var rangeString = ui.values.join(', ');
            Shiny.setInputValue('%s_manual_range', rangeString);
          }
        });
      });
    ",
        id,
        id,
        id,
        id,
        id
      )

      # Add auto-save functionality
      autosave_js <- create_autosave_js(
        id,
        "slider_numeric_range",
        list(defaultValue = default)
      )
      js_range_handler <- paste(js_range_handler, autosave_js, sep = "\n")

      output <- shiny::tagAppendChild(
        output,
        shiny::tags$script(htmltools::HTML(js_range_handler))
      )

      # Add an observer in the server to capture this string value
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        session <- shiny::getDefaultReactiveDomain()

        # Debug observer to catch auto-save calls
        shiny::observe({
          debug_msg <- session$input$debug_range_autosave
          if (!is.null(debug_msg)) {
            cat("DEBUG Auto-save triggered from JS:", debug_msg, "\n")
          }
        })

        shiny::observe({
          # Get the range string from our custom input
          range_string <- session$input[[paste0(id, "_manual_range")]]
          interaction_flag <- session$input[[paste0(id, "_interacted")]]

          cat(
            "DEBUG Range Observer - ID:",
            id,
            "range_string:",
            range_string,
            "interaction_flag:",
            interaction_flag,
            "\n"
          )

          if (!is.null(range_string) && range_string != "") {
            # Store this directly using the main id
            sd_store_value(range_string, id)
            cat("DEBUG Range value stored for:", id, "\n")

            # Handle timestamp the same way as main observer
            if (!is.null(interaction_flag) && interaction_flag) {
              # Get access to all_data and changed_fields (same as main observer)
              all_data <- session$userData$all_data
              changed_fields <- session$userData$changed_fields

              cat(
                "DEBUG Checking userData - all_data exists:",
                !is.null(all_data),
                "changed_fields exists:",
                !is.null(changed_fields),
                "\n"
              )

              if (!is.null(all_data) && !is.null(changed_fields)) {
                timestamp <- get_utc_timestamp()
                ts_id <- paste0(id, "_timestamp")
                all_data[[ts_id]] <- timestamp
                changed_fields(c(changed_fields(), ts_id))
                cat(
                  "DEBUG Timestamp stored for:",
                  id,
                  "timestamp:",
                  timestamp,
                  "\n"
                )
              } else {
                cat("DEBUG Could not store timestamp - missing userData\n")
              }
            } else {
              cat("DEBUG No interaction flag, skipping timestamp\n")
            }
          } else {
            cat("DEBUG No range string to process\n")
          }
        })
      }
    } else {
      # For single sliders, just track interaction
      js_single_handler <- sprintf(
        "
      $(document).ready(function() {
        $('#%s').on('mousedown focus', function() {
          window['markInteracted_%s']();
          Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
        });
      });
    ",
        id,
        id,
        id
      )

      # Add auto-save functionality
      autosave_js <- create_autosave_js(
        id,
        "slider_numeric_single",
        list(defaultValue = default)
      )
      js_single_handler <- paste(js_single_handler, autosave_js, sep = "\n")

      output <- shiny::tagAppendChild(
        output,
        shiny::tags$script(htmltools::HTML(js_single_handler))
      )
    }
  } else if (type == "date") {
    output <- shiny::dateInput(
      inputId = id,
      label = label,
      value = NULL,
      min = NULL,
      max = NULL,
      format = "yyyy-mm-dd",
      startview = "month",
      weekstart = 0,
      language = language,
      autoclose = TRUE,
      datesdisabled = NULL,
      daysofweekdisabled = NULL,
      ...
    )

    # Add interaction tracking
    js_date_interaction <- sprintf(
      "setTimeout(function() {
              $('#%s').on('change', function() {
                  window['markInteracted_%s']();
                  Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
              });
           }, 1000);", # 1000 ms delay
      id,
      id,
      id
    )

    # Add auto-save functionality
    autosave_js <- create_autosave_js(id, "date", list())
    combined_js <- paste(js_date_interaction, autosave_js, sep = "\n")

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(combined_js))
    )
  } else if (type == "daterange") {
    output <- shiny::dateRangeInput(
      inputId = id,
      label = label,
      start = NULL,
      end = NULL,
      min = NULL,
      max = NULL,
      format = "yyyy-mm-dd",
      startview = "month",
      weekstart = 0,
      language = language,
      separator = "-",
      autoclose = TRUE,
      ...
    )

    # Add interaction tracking
    js_daterange_interaction <- sprintf(
      "setTimeout(function() {
              $('#%s').on('change', function() {
                  window['markInteracted_%s']();
                  Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
              });
           }, 1000);", # 1000 ms delay
      id,
      id,
      id
    )

    # Add auto-save functionality
    autosave_js <- create_autosave_js(id, "daterange", list())
    combined_js <- paste(js_daterange_interaction, autosave_js, sep = "\n")

    output <- shiny::tagAppendChild(
      output,
      shiny::tags$script(htmltools::HTML(combined_js))
    )
  } else if (type == "matrix") {
    header <- shiny::tags$tr(
      shiny::tags$th(""),
      lapply(names(option), function(opt) shiny::tags$th(opt))
    )
    rows <- lapply(row, function(q_id) {
      full_id <- paste(id, q_id, sep = "_")
      shiny::tags$tr(
        shiny::tags$td(names(row)[row == q_id]),
        shiny::tags$td(
          colspan = length(option),
          sd_question(
            type = "mc",
            id = full_id,
            label = "",
            option = option,
            direction = "horizontal",
            ...
          )
        )
      )
    })

    output <- shiny::div(
      class = "matrix-question-container",
      shiny::tags$label(class = "control-label", label),
      shiny::tags$table(
        class = "matrix-question",
        header,
        shiny::tags$tbody(rows)
      )
    )
  }

  # Create wrapper div
  output_div <- make_question_container(id, output, width)

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    # In a reactive context, directly add to output with renderUI
    # Use "_question" suffix to avoid input/output ID conflicts
    shiny::isolate({
      output_div <- shiny::tags$div(output)
      output <- shiny::getDefaultReactiveDomain()$output
      output[[paste0(id, "_question")]] <- shiny::renderUI({
        output_div
      })
    })
  } else {
    # If not in a reactive context, just return the element
    return(output_div)
  }
}

#' Create a Custom Question with a Shiny Widget
#'
#' @description
#' This function creates a custom survey question that incorporates any Shiny widget
#' and captures its interaction value. It allows for the integration of interactive
#' visualizations (e.g., maps, plots) or other custom Shiny outputs into a survey,
#' storing the result of user interaction as survey data.
#'
#' @param id Character string. A unique identifier for the question.
#' @param label Character string. The label text for the question, which can
#'   include HTML formatting.
#' @param output Shiny UI element. The output of a Shiny widget (e.g.,
#'   `leafletOutput()`, `plotlyOutput()`).
#' @param value Reactive expression that returns the value to be stored in the
#'   survey data when the user interacts with the widget.
#' @param height Character string. The height of the widget output. Defaults to
#'   "400px".
#'
#' @return None (called for side effects)
#'
#' @details
#' The function creates a custom question container that includes:
#' - A visible widget output that users can interact with
#' - A hidden text input that stores the value from the interaction
#' - Automatic tracking of user interaction for progress monitoring
#'
#' The value to be stored is controlled by the reactive expression provided to
#' the `value` parameter, which should update whenever the user interacts with
#' the widget in the desired way.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'   library(leaflet)
#'
#'   server <- function(input, output, session) {
#'     # Create map output
#'     output$usa_map <- renderLeaflet({
#'       leaflet() |>
#'         addTiles() |>
#'         setView(lng = -98.5795, lat = 39.8283, zoom = 4)
#'     })
#'
#'     # Reactive value for selected location
#'     selected_location <- reactiveVal(NULL)
#'
#'     # Click observer
#'     observeEvent(input$usa_map_click, {
#'       click <- input$usa_map_click
#'       if (!is.null(click)) {
#'         selected_location(
#'           sprintf("Lat: %0.2f, Lng: %0.2f", click$lat, click$lng)
#'         )
#'       }
#'     })
#'
#'     # Create the custom question
#'     sd_question_custom(
#'       id = "location",
#'       label = "Click on your location:",
#'       output = leafletOutput("usa_map", height = "400px"),
#'       value = selected_location
#'     )
#'
#'     sd_server()
#'   }
#'
#'   shinyApp(ui = sd_ui(), server = server)
#' }
#'
#' @seealso
#' [sd_question()] for standard question types
#'
#' @export
sd_question_custom <- function(
  id,
  label,
  output, # The UI component (e.g., leafletOutput, plotlyOutput)
  value, # Reactive expression that returns the value to store in the data
  height = "400px"
) {
  # Get the current shiny session
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    stop(
      "sd_question_widget must be called from within a Shiny reactive context"
    )
  }

  # Create the container div
  output_contents <- shiny::tagList(
    shiny::tags$label(class = "control-label", shiny::HTML(label)),
    shiny::div(
      style = "display: none;",
      shiny::textInput(id, label = NULL, value = "", width = "0px")
    ),
    output
  )
  output_div <- make_question_container(id, output_contents, "100%")

  # In a reactive context, directly add to output with renderUI
  # Use "_question" suffix to avoid input/output ID conflicts
  shiny::isolate({
    output_div <- shiny::tags$div(output_div)
    output <- shiny::getDefaultReactiveDomain()$output
    output[[paste0(id, "_question")]] <- shiny::renderUI({
      output_div
    })
  })

  # Observer to update the stored value when value changes
  shiny::observe({
    temp_value <- value()
    if (!is.null(temp_value)) {
      shiny::updateTextInput(session, id, value = as.character(temp_value))
    }
  })
}

# date_interaction function removed - now using unified auto-save helper

make_question_container <- function(id, object, width) {
  # Check if question if answered
  js_interaction <- sprintf(
    "Shiny.setInputValue('%s_interacted', true, {priority: 'event'});",
    id
  )
  return(shiny::tags$div(
    id = paste0("container-", id),
    `data-question-id` = id,
    class = "question-container",
    style = sprintf("width: %s;", width),
    oninput = js_interaction,
    onclick = js_interaction,
    object,
    shiny::tags$span(class = "hidden-asterisk", "*")
  ))
}

#' Create a 'Next' Button for Page Navigation
#'
#' This function creates a 'Next' button for navigating to the specified next page in a Surveydown survey.
#' The button can be activated by clicking or by pressing the Enter key when visible.
#'
#' @param next_page Character string. The ID of the next page to navigate to. This parameter is required.
#' @param label Character string. The label of the 'Next' button. Defaults to
#'   `NULL`, in which case the word `"Next"` will be used.
#'
#' @details The function generates a 'shiny' action button that, when clicked
#' or when the Enter key is pressed, sets the input value to the specified next
#' page ID, facilitating page navigation within the Shiny application. The
#' button is styled to appear centered on the page and includes a class for
#' Enter key functionality.
#'
#' @return A 'shiny' tagList containing the 'Next' button UI element.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_next.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @export
sd_next <- function(next_page = NULL, label = NULL) {
  # Get translations
  translations <- get_translations()$translations

  # If no label provided, use default
  if (is.null(label)) {
    label <- translations[['next']]
  }

  button_id <- "page_id_next" # Placeholder ID
  shiny::tagList(
    shiny::div(
      `data-next-page` = if (!is.null(next_page)) next_page else "",
      style = "margin-top: 1rem; margin-bottom: 0.5rem;",
      shiny::actionButton(
        inputId = button_id,
        label = label,
        class = "sd-enter-button",
        style = "display: block; margin: auto;",
        onclick = "Shiny.setInputValue('next_page', this.parentElement.getAttribute('data-next-page'));"
      )
    )
  )
}

# Generate Next Button ID
make_next_button_id <- function(page_id) {
  return(paste0(page_id, "_next"))
}

#' Create a 'Close' Button to Exit the Survey
#'
#' This function creates a 'Close' button that, when clicked, will trigger the exit process
#' for the survey. Depending on the server-side configuration, this may show a rating question
#' or a simple confirmation dialog before attempting to close the current browser tab or window.
#'
#' @param label Character string. The label of the 'Close' button. Defaults to
#'    `NULL`, in which case the word `"Exit Survey"` will be used.
#'
#' @return A 'shiny' tagList containing the 'Close' button UI element and
#' associated JavaScript for the exit process.
#'
#' @details
#' The function generates a 'shiny' action button that, when clicked, triggers
#' the 'show_exit_modal' event. The server-side logic (controlled by the
#' `rate_survey` parameter in `sd_server()`) determines whether to show a
#' rating question or a simple confirmation dialog.
#'
#' The function also includes a custom message handler for closing the window.
#' This is necessary because some browsers may not allow JavaScript to close
#' windows that were not opened by JavaScript. In such cases, the user will be
#' prompted to close the tab manually.
#'
#' @note The actual behavior of the exit process (whether to show a rating
#' question or not) is controlled by the `rate_survey` parameter in the
#' `sd_server()` function, not in this UI function.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_close.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @seealso \code{\link{sd_server}}
#'
#' @export
sd_close <- function(label = NULL) {
  # Get translations
  translations <- get_translations()$translations

  # If no label provided, use default
  if (is.null(label)) {
    label <- translations[['exit']]
  }

  button_id <- "close-survey-button"
  shiny::tagList(
    shiny::div(
      style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
      shiny::actionButton(
        inputId = button_id,
        label = label,
        class = "sd-enter-button",
        style = "display: block; margin: auto;",
        onclick = "Shiny.setInputValue('show_exit_modal', true, {priority: 'event'});"
      )
    ),
    shiny::tags$script(htmltools::HTML(
      "
      Shiny.addCustomMessageHandler('closeWindow', function(message) {
        window.close();
        if (!window.closed) {
          alert('Please close this tab manually to exit the survey.');
        }
      });
    "
    ))
  )
}

#' Create a Redirect Element for 'shiny' Applications
#'
#' This function creates a UI element that redirects the user to a specified
#' URL. It can be used in both reactive and non-reactive contexts within
#' 'shiny' applications.
#'
#' @param id A character string of a unique id to be used to identify the
#'   redirect button in the survey body. In reactive contexts, this becomes
#'   the output ID, while the actual button gets the ID `id + "_btn"` to
#'   avoid input/output conflicts.
#' @param url A character string specifying the URL to redirect to.
#' @param button A logical value indicating whether to create a button (`TRUE`)
#'   or a text element (`FALSE`) for the redirect. Default is `TRUE`.
#' @param label A character string for the button or text label. Defaults to
#'   `NULL`, in which case the words `"Click here"` will be used.
#' @param delay An optional numeric value specifying the delay in seconds before
#'   automatic redirection. If `NULL` (default), no automatic redirection
#'   occurs.
#' @param newtab A logical value indicating whether to open the URL in a new
#'   tab (`TRUE`) or in the current tab (`FALSE`). Default is `FALSE`.
#'
#' @return In a reactive context, creates an output with the specified ID that
#' can be displayed using `sd_output()`. The actual button element gets the
#' ID `id + "_btn"` to prevent input/output conflicts. In a non-reactive
#' context, returns the redirect element directly.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_redirect.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # Reactive expression that generates a url with an id variable
#'     # parsed from the url
#'     url_redirect <- reactive({
#'       params <- sd_get_url_pars()
#'       id <- params["id"]
#'       return(paste0("https://www.google.com?id=", id))
#'     })
#'
#'     # Create the redirect button
#'     sd_redirect(
#'       id = "redirect_url_pars",
#'       url = url_redirect(),
#'       button = TRUE,
#'       label = "Redirect"
#'     )
#'
#'     sd_skip_if(
#'       input$screening_question == "end_1" ~ "end_page_1",
#'       input$screening_question == "end_1" ~ "end_page_2",
#'     )
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#' @export
sd_redirect <- function(
  id,
  url,
  button = TRUE,
  label = "Click here",
  delay = NULL,
  newtab = FALSE
) {
  # Get translations
  translations <- get_translations()$translations

  # If no label provided, use default
  if (is.null(label)) {
    label <- translations[['click']]
  }

  if (!is.null(shiny::getDefaultReactiveDomain())) {
    # In a reactive context, directly add to output with renderUI
    shiny::isolate({
      output <- shiny::getDefaultReactiveDomain()$output
      output[[id]] <- shiny::renderUI({
        # Use a different ID for the actual input element to avoid conflicts
        button_id <- paste0(id, "_btn")
        create_redirect_element(button_id, url, button, label, delay, newtab)
      })
    })
  } else {
    # If not in a reactive context, just return the element
    return(create_redirect_element(id, url, button, label, delay, newtab))
  }
}

# Function to create the redirect element
create_redirect_element <- function(
  id,
  url,
  button,
  label,
  delay,
  newtab = FALSE
) {
  # Validate URL
  if (!grepl("^https?://", url)) {
    url <- paste0("https://", url)
  }

  # Create JavaScript for redirection
  redirect_js <- if (newtab) {
    paste0("window.open('", url, "', '_blank');")
  } else {
    paste0("window.location.href = '", url, "';")
  }

  # Create button or text element
  if (button) {
    element <- shiny::actionButton(
      inputId = id,
      label = label,
      onclick = redirect_js
    )
  } else {
    element <- shiny::span(label)
  }

  # Get translations
  translations <- get_translations()$translations
  text_redirect <- translations[["redirect"]]
  text_seconds <- translations[["seconds"]]
  text_newtab <- translations[["new_tab"]]
  text_error <- translations[["redirect_error"]]

  # Add automatic redirection if delay is specified
  if (!is.null(delay) && is.numeric(delay) && delay > 0) {
    countdown_id <- paste0("countdown_", id)
    element <- shiny::tagList(
      shiny::div(
        class = "sd-wrapper",
        shiny::div(
          id = id,
          class = "sd-container",
          element,
          shiny::p(
            style = "margin: 0.5rem 0 0 0;",
            text_redirect,
            " ",
            shiny::tags$strong(id = countdown_id, delay),
            " ",
            text_seconds,
            ".",
            if (newtab) {
              glue::glue(" ({text_newtab})")
            } else {
              NULL
            }
          )
        )
      ),
      shiny::tags$script(htmltools::HTML(sprintf(
        "startCountdown(%d, function() { %s }, '%s', '%s');",
        delay,
        redirect_js,
        countdown_id,
        id
      )))
    )
  } else if (!button) {
    # If no delay and no button, inform the user that no action is possible
    element <- shiny::div(
      class = "sd-wrapper",
      shiny::div(
        class = "sd-container",
        element,
        shiny::p(style = "margin: 0.5rem 0 0 0;", text_error)
      )
    )
  } else {
    # If it's a button without delay, just wrap it in the styled container
    element <- shiny::div(
      class = "sd-wrapper",
      shiny::div(
        class = "sd-container",
        element
      )
    )
  }

  return(element)
}

#' Get URL Parameters in a 'shiny' Application
#'
#' This function retrieves URL parameters from the current 'shiny' session.
#' It must be called from within a 'shiny' reactive context.
#'
#' @param ... Optional. Names of specific URL parameters to retrieve.
#'   If none are specified, all URL parameters are returned.
#'
#' @return A reactive expression that returns a list of URL parameters.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_redirect.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'
#'     # Reactive expression that generates a url with an id variable
#'     # parsed from the url
#'     url_redirect <- reactive({
#'       params <- sd_get_url_pars()
#'       id <- params["id"]
#'       return(paste0("https://www.google.com?id=", id))
#'     })
#'
#'     # Create the redirect button
#'     sd_redirect(
#'       id = "redirect_url_pars",
#'       url = url_redirect(),
#'       button = TRUE,
#'       label = "Redirect"
#'     )
#'
#'     sd_skip_if(
#'       input$screening_question == "end_1" ~ "end_page_1",
#'       input$screening_question == "end_1" ~ "end_page_2",
#'     )
#'
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @export
sd_get_url_pars <- function(...) {
  shiny::reactive({
    session <- shiny::getDefaultReactiveDomain()

    if (is.null(session)) {
      stop(
        "sd_get_url_pars() must be called from within a Shiny reactive context"
      )
    }

    full_url <- session$clientData$url_search
    parsed_query <- shiny::parseQueryString(full_url)

    requested_params <- list(...)

    if (length(requested_params) == 0) {
      return(parsed_query)
    }

    requested_params <- unlist(requested_params)
    filtered_query <- parsed_query[requested_params]
    filtered_query[!sapply(filtered_query, is.null)]
  })()
  # Extra parentheses is added so that the reactive expression is evaluated
  # when the function is called
}

#' Create a placeholder for a reactive survey question
#'
#' This function is depreciated - use `sd_output()` instead.
#'
#' @param id A unique identifier for the question.
#' @return A 'shiny' UI element that serves as a placeholder for the reactive
#' question.
#'
#' @export
sd_display_question <- function(id) {
  # v0.2.1
  .Deprecated("sd_output")
}

#' Display the value of a survey question
#'
#' This function is depreciated - use `sd_output()` instead.
#' @param id The ID of the question to display
#' @param display_type The type of display. Can be `"inline"` (default),
#'   `"text"`, `"verbatim"`, or `"ui"`.
#' @param wrapper A function to wrap the output
#' @param ... Additional arguments passed to the wrapper function
#'
#' @return A 'shiny' UI element displaying the question's value
#'
#' @export
sd_display_value <- function(id, display_type = "inline", wrapper = NULL, ...) {
  # v0.2.1
  .Deprecated("sd_output")
}

#' Output Function for Displaying reactive objects and values
#'
#' @param id Character string. A unique identifier for the output element.
#' @param type Character string. Specifies the type of output corresponding
#'   with the question `id`. Can be `"question"`, `"value"`, `"label_option"`,
#'    `"label_question"`, or `NULL.` If `"question"`, it will display a
#'    question defined in the server. If `"value"`, it will display the value
#'    of question `id` selected by the respondent. If `"label_option"`, it will
#'    display the label of the option for question `id` selected by the
#'    respondent. If `"label_question"`, it will display the `label` argument
#'    value for question `id`. Finally, if `NULL`, the function behaves like
#'    `shiny::uiOutput()`.
#' @param width Character string. The width of the UI element. Defaults to
#'   `"100%"`.
#' @param display Character string. Specifies the display type for `"value"`
#'    outputs. Can be `"text"`, `"verbatim"`, or `"ui"`. Only used when
#'   `type = "value"`.
#' @param inline Logical. Whether to render the output inline. Defaults to
#'   `TRUE`.
#' @param wrapper Function. A function to wrap the output. Only used when
#'   `type = "value"`.
#' @param ... Additional arguments passed to the underlying 'shiny' functions
#'   or the wrapper function.
#'
#' @return A 'shiny' UI element, the type of which depends on the input
#' parameters.
#'
#' @details
#' The function behaves differently based on the `type` parameter:
#' - If `type` is `NULL`, it acts like `shiny::uiOutput()`.
#' - If `type` is `"question"`, it creates a placeholder for a reactive survey question.
#' - If `type` is `"value"`, it creates an output to display the value of a survey question,
#'   with the display style determined by the `display` parameter.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Get path to example survey file
#'   survey_path <- system.file("examples", "sd_output.qmd",
#'                              package = "surveydown")
#'
#'   # Copy to a temporary directory
#'   temp_dir <- tempdir()
#'   file.copy(survey_path, file.path(temp_dir, "survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     sd_server()
#'   }
#'
#'   # Run the app
#'   shiny::shinyApp(ui = sd_ui(), server = server)
#'
#'   # Clean up
#'   setwd(orig_dir)
#' }
#'
#' @export
sd_output <- function(
  id,
  type = NULL,
  width = "100%",
  display = "text",
  inline = TRUE,
  wrapper = NULL,
  ...
) {
  # Use localStorage for reactive output restoration (simpler approach)
  js_localStorage_restore <- sprintf(
    "
    $(document).ready(function() {
      var id = '%s', key = 'surveydown_reactive_' + id;

      function save() {
        var content = $('#' + id).html();
        if (content && content.trim()) {
          try {
            localStorage.setItem(key, content);
          } catch(e) {
            console.warn('Could not save to localStorage:', e);
          }
        }
      }

      function restore() {
        try {
          var saved = localStorage.getItem(key);
          var el = $('#' + id);
          if (saved && el.length && !el.html().trim()) {
            el.html(saved);
          }
        } catch(e) {
          console.warn('Could not restore from localStorage:', e);
        }
      }

      setTimeout(restore, 100);
      new MutationObserver(function() { setTimeout(save, 200); })
        .observe(document.getElementById(id) || document.body, {childList: true, subtree: true});
      $(window).on('beforeunload', save);
    });
    ",
    id
  )

  if (is.null(type)) {
    output_element <- shiny::uiOutput(id, inline = inline, ...)
    return(shiny::tagList(
      output_element,
      shiny::tags$script(htmltools::HTML(js_localStorage_restore))
    ))
  }

  if (type == "question") {
    type_id <- paste0(id, "_", type)
    return(shiny::tagList(
      make_question_container(id, shiny::uiOutput(type_id), width),
      shiny::tags$script(htmltools::HTML(js_localStorage_restore))
    ))
  }

  if (type %in% c("value", "label_option", "label_question")) {
    type_id <- paste0(id, "_", type)

    if (!display %in% c("text", "verbatim", "ui")) {
      stop("Invalid display type. Choose 'text', 'verbatim', or 'ui'.")
    }

    output <- switch(
      display,
      "text" = shiny::textOutput(type_id, inline = inline),
      "verbatim" = shiny::verbatimTextOutput(type_id, inline = inline),
      "ui" = shiny::uiOutput(type_id, inline = inline),
      # Default to textOutput if display is not specified
      shiny::textOutput(type_id, inline = inline)
    )

    if (!is.null(wrapper)) {
      output <- wrapper(output, ...)
    }

    return(output)
  }

  stop("Invalid type. Choose 'question' or 'value'.")
}

#' Depreciated Survey Dashboard
#'
#' This dashboard was depreciated in version v0.13.0. Now the sdstudio package
#' fully includes the functionality that was previously included in this function.
#' @param gssencmode Character string. The GSS encryption mode for the database
#'   connection. Defaults to `"auto"`. Options are:
#'   - `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if GSSAPI negotiation fails
#'   - `"prefer"`: Uses GSSAPI encryption if available, plain connection otherwise
#'   - `"disable"`: Disables GSSAPI encryption entirely
#'   Set to `"disable"` if you're having connection issues on a secure connection like a VPN.
#'
#' @export
sd_dashboard <- function(gssencmode = "auto") {
  # v0.13.0
  .Deprecated(
    "This function was depreciated in v0.13.0; use the sdstudio package instead"
  )
}

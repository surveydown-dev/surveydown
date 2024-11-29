# Convert Markdown to HTML
markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(shiny::HTML(markdown::renderMarkdown(text = text)))
}

# Convert List Names from Markdown to HTML
list_name_md_to_html <- function(list) {
  list_names_md <- names(list)
  list_names_html <- lapply(list_names_md, function(name) {
    html_name <- markdown_to_html(name)
    plain_name <- gsub("<[/]?p>|\\n", "", html_name)
    return(plain_name)
  })
  names(list) <- unlist(list_names_html)
  return(list)
}

#' Display Package Information on Attach
#'
#' This function is called when the package is attached.
#' It displays version number, authors, and citation information.
#' It also adds some folders to the 'shiny' resource path with
#' shiny::addResourcePath()
#'
#' @param libname The library where the package is installed
#' @param pkgname The name of the package
#'
#' @noRd
.onAttach <- function(libname, pkgname) {

  # Add special folders to resource path
  folders <- c('_survey', 'images', 'css', 'js', 'www')
  for (folder in folders) { include_folder(folder) }

  # Print package data
  desc  <- utils::packageDescription(pkgname, libname)
  packageStartupMessage(
    "Version:  ", desc$Version, "\n",
    "Author:   ", "John Paul Helveston, Pingfan Hu, Bogdan Bunea (George Washington University)", "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/surveydown/issues/41.\n\n",
    "Please cite our package in your publications, see:\ncitation(\"surveydown\")"
  )
}

survey_file_exists <- function() {
  files <- basename(list.files(full.names = TRUE))
  if ("survey.qmd" %in% files) { return(TRUE) }
  return(FALSE)
}

is_self_contained <- function() {
  metadata <- quarto::quarto_inspect("survey.qmd")
  embedded <- metadata$formats$html$pandoc$`embed-resources`
  if (!is.null(embedded)) {
    if (embedded) {
      return(TRUE)
    }
  }
  self <- metadata$formats$html$pandoc$`self-contained`
  if (!is.null(self)) {
    if (self) {
      return(TRUE)
    }
  }
  return(FALSE)
}

include_folder <- function(folder, create = FALSE) {
  folder_exists <- dir.exists(folder)
  if (folder_exists) {
    shiny::addResourcePath(folder, folder)
  } else if (create) {
    dir.create(folder)
    shiny::addResourcePath(folder, folder)
  }
}

#' Include a folder to the 'shiny' resource path
#'
#' This function includes a specified folder to the 'shiny' resource path,
#' making it accessible for serving static files in a 'shiny' application.
#' It checks for pre-existing resource paths to avoid conflicts with
#' folders already included by the package.
#'
#' @param folder A character string specifying the name of the folder to
#'   include. This folder should exist in the root directory of your 'shiny'
#'   app.
#'
#' @return `NULL` invisibly. The function is called for its side effect of
#'   adding a resource path to 'shiny'.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   # Create an "images" folder
#'   dir.create("images")
#'
#'   # Include the folder in the shiny resource path
#'   sd_include_folder("images")
#' }
#'
#' @export
sd_include_folder <- function(folder) {
  # List of folders pre-included by the package
  pre_included_folders <- names(shiny::resourcePaths())

  if (folder %in% pre_included_folders) {
    message(paste("The folder", folder, "is already included by the package. No action needed."))
    return(invisible(NULL))
  }

  if (!dir.exists(folder)) {
    stop(paste("The folder", folder, "does not exist in the current directory."))
  }

  shiny::addResourcePath(folder, folder)
  message(paste("Successfully added", folder, "to Shiny's resource path."))

  invisible(NULL)
}

# Convert Vector to JSON Array
vector_to_json_array <- function(vec) {
  if (length(vec) == 0) return("[]")

  # Ensure all elements are properly quoted
  quoted_elements <- sapply(vec, function(x) {
    if (is.character(x)) {
      sprintf('"%s"', gsub('"', '\\"', x))  # Escape any quotes within strings
    } else {
      as.character(x)
    }
  })

  # Join elements and wrap in brackets
  sprintf("[%s]", paste(quoted_elements, collapse = ","))
}

# Dynamically load JS files
load_js_file <- function(name) {
  js_file_path <- system.file("js", name, package = "surveydown")
  js_code <- paste(readLines(js_file_path), collapse = "\n")
  shinyjs::runjs(js_code)
}

# Load CSS and JS files
load_resource <- function(..., package = "surveydown") {
  files <- c(...)
  lapply(files, function(file) {
    file_type <- tolower(tools::file_ext(file))
    if (!(file_type %in% c("css", "js"))) {
      stop(paste("Unsupported file type:", file_type, "for file:", file))
    }
    path <- system.file(paste0(file_type, "/", file), package = package)
    if (file.exists(path)) {
      if (file_type == "css") {
        shiny::includeCSS(path)
      } else {
        shiny::includeScript(path)
      }
    } else {
      warning(paste("File not found:", file, "in package:", package))
      NULL
    }
  })
}

tibble_to_list_of_lists <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop("Input must be a data frame or tibble")
  }

  if (!"condition" %in% names(tbl) || !"target" %in% names(tbl)) {
    stop("Input must have 'condition' and 'target' columns")
  }

  lapply(seq_len(nrow(tbl)), function(i) {
    list(
      condition = tbl$condition[[i]],
      target = tbl$target[i]
    )
  })
}

#' Create a new survey template
#'
#' This function creates a new survey template by copying files from the
#' package's template directory to a specified path. It handles file conflicts
#' and provides appropriate warnings and feedback.
#'
#' @param path A character string specifying the directory where the survey
#'   template should be created. Defaults to the current working directory.
#' @param structure A character string specifying the template structure to use.
#'   Must be either `"single"` or `"multi"`. Defaults to `"single"`.
#'
#' @return Invisible `NULL`. The function is called for its side effects of
#'   creating files and providing user feedback.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item If the specified path is the current working directory, it asks for user confirmation.
#'   \item Validates the specified structure (`"single"` or `"multi"`).
#'   \item Creates the target directory if it doesn't exist.
#'   \item Copies all files from the package's template directory (based on the specified structure) to the target path.
#'   \item Preserves the directory structure of the template.
#'   \item Skips existing files and provides warnings for each skipped file.
#'   \item Handles .Rproj files specially, skipping if any .Rproj file already exists in the target directory.
#'   \item Provides feedback on whether files were copied or if all files already existed.
#' }
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Create a single-page survey template
#'   sd_create_survey(structure = "single")
#'
#'   # Create a multi-page survey template
#'   sd_create_survey(structure = "multi")
#' }
sd_create_survey <- function(path = getwd(), structure = "single") {
  # Check if using current directory and confirm with user
  if (path == getwd() && !usethis::ui_yeah(paste("Use the current directory (", path, ") as the path?"))) {
    stop("Operation aborted by the user.")
  }

  # Validate the structure parameter
  if (!structure %in% c("single", "multi")) {
    stop("Invalid structure. Choose either 'single' or 'multi'.")
  }

  # Create the directory if it doesn't exist
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Get the path to the template folder and list files
  template_path <- system.file(file.path("templates", structure), package = "surveydown")
  if (!dir.exists(template_path)) {
    stop(paste("Template directory for", structure, "structure does not exist."))
  }
  template_files <- list.files(template_path, full.names = TRUE, recursive = TRUE)

  # Copy files, checking for conflicts
  files_copied <- sapply(template_files, function(file) {
    relative_path <- sub(template_path, "", file)
    target_file <- file.path(path, relative_path)

    # Ensure target directory exists
    dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)

    file_name <- basename(file)
    if (grepl("\\.Rproj$", file_name) && length(list.files(path, pattern = "\\.Rproj$"))) {
      warning("Skipping the .Rproj file since one already exists.", call. = FALSE, immediate. = TRUE)
      return(FALSE)
    } else if (file.exists(target_file)) {
      warning(paste("Skipping", file_name, "since it already exists."), call. = FALSE, immediate. = TRUE)
      return(FALSE)
    } else {
      file.copy(from = file, to = target_file, overwrite = FALSE)
      return(TRUE)
    }
  })

  # Provide feedback to the user
  if (any(files_copied)) {
    usethis::ui_done(paste(structure, "version of template created at", path))
  } else {
    usethis::ui_done("Since all files exist, no file was added.")
  }
}

#' Required Set Up Function
#'
#' This function is depreciated and no longer needed.
#'
#' @details The function configures the 'shiny' application to use Bootstrap 5
#'   for styling and enables 'shinyjs' for JavaScript functionalities within
#'   the application.
#'
#' @return This function does not return a value. It is called for its side
#' effects of setting up the 'shiny' application.
#'
#' @export
sd_setup <- function() {
  # v0.3.0
  .Deprecated("")
}

question_templates <- function(type = "mc") {
  templates <- list(
    mc = 'sd_question(
  type   = "mc",
  id     = "like_apple",
  label  = "Do you like apple?",
  option = c(
    "Yes" = "yes",
    "No"  = "no"
  )
)

',
  text = 'sd_question(
  type  = "text",
  id    = "apple_text",
  label = "Write a type of apple:"
)

',
textarea = 'sd_question(
  type  = "textarea",
  id    = "apple_textarea",
  label = "What do you like about apple?"
)

',
numeric = 'sd_question(
  type  = "numeric",
  id    = "apple_numeric",
  label = "How many apple(s) do you eat per day?"
)

',
mc_buttons = 'sd_question(
  type   = "mc_buttons",
  id     = "apple_mc_buttons",
  label  = "Which apple do you prefer most?",
  option = c(
    "Fuji"       = "fuji",
    "Gala"       = "gala",
    "Honeycrisp" = "honeycrisp"
  )
)

',
mc_multiple = 'sd_question(
  type  = "mc_multiple",
  id    = "apple_mc_multiple",
  label = "What are your favorite apple types (select all that apply)?",
  option = c(
    "Fuji"       = "fuji",
    "Gala"       = "gala",
    "Honeycrisp" = "honeycrisp"
  )
)

',
mc_multiple_buttons = 'sd_question(
  type  = "mc_multiple_buttons",
  id    = "apple_mc_multiple_buttons",
  label = "What are your favorite apple types (select all that apply)?",
  option = c(
    "Fuji"       = "fuji",
    "Gala"       = "gala",
    "Honeycrisp" = "honeycrisp"
  )
)

',
select = 'sd_question(
  type  = "select",
  id    = "apple_select",
  label = "Which apple do you prefer most?",
  option = c(
    "Fuji"       = "fuji",
    "Gala"       = "gala",
    "Honeycrisp" = "honeycrisp"
  )
)

',
slider = 'sd_question(
  type  = "slider",
  id    = "apple_slider",
  label = "To what extent do you like apple?",
  option = c(
    "Don\'t Like"    = "dont_like",
    "Somewhat Like" = "somewhat",
    "Neutral"       = "neutral",
    "Like"          = "like",
    "Strongly Like" = "strongly_like"
  )
)

',
date = 'sd_question(
  type  = "date",
  id    = "apple_date",
  label = "What is the last day you had apple?"
)

',
daterange = 'sd_question(
  type  = "daterange",
  id    = "vacation_daterange",
  label = "Please select the date range of your upcoming vacation."
)

'
  )

return(templates[[type]])
}

#' Add a Question Template to the Current Document
#'
#' This function inserts a template for a surveydown question at the current
#' cursor position in the active RStudio document. It supports various question
#' types and automatically removes the function call before inserting the
#' template if it exists in the document.
#'
#' @param type A character string specifying the type of question template to
#'   insert.
#'   Default is `"mc"` (multiple choice). Available options are:
#'   \itemize{
#'     \item `"mc"`: Multiple choice (single selection)
#'     \item `"select"`: Dropdown selection
#'     \item `"mc_multiple"`: Multiple choice (multiple selections)
#'     \item `"mc_buttons"`: Multiple choice with button layout (single selection)
#'     \item `"mc_multiple_buttons"`: Multiple choice with button layout (multiple selections)
#'     \item `"text"`: Short text input
#'     \item `"textarea"`: Long text input
#'     \item `"numeric"`: Numeric input
#'     \item `"slider"`: Slider input
#'     \item `"date"`: Date input
#'     \item `"daterange"`: Date range input
#'   }
#' @param chunk Logical. If `TRUE`, the code will be generated with the R code
#'   chunk wrapper. Defaults to `FALSE`.
#' @details
#' The function performs the following steps:
#' 1. Checks for and removes any existing `sd_add_question()` function call in the document.
#' 2. Inserts the appropriate question template at the current cursor position.
#'
#' @return This function does not return a value. It modifies the active
#' document as a side effect by inserting text and potentially removing a
#' function call.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Insert a default multiple choice question template
#'   sd_add_question()
#'
#'   # Insert a text input question template
#'   sd_add_question("text")
#'
#'   # Insert a slider question template
#'   sd_add_question("slider")
#' }
#'
#' @export
sd_add_question <- function(type = "mc", chunk = FALSE) {
  template <- question_templates(type)
  if (chunk) {
    template <- paste0("```{r}\n", template, "```\n")
  }

  # Get the current document context
  context <- rstudioapi::getActiveDocumentContext()
  # Get all lines of the document
  lines <- context$contents
  # Find the line containing the function call
  call_line <- which(grepl("sd_add_question\\(.*\\)", lines))

  if (length(call_line) > 0) {
    # Remove the line containing the function call
    rstudioapi::modifyRange(
      c(call_line, 1, call_line + 1, 1),
      ""
    )
    # Update the context after removal
    context <- rstudioapi::getActiveDocumentContext()
  }

  # Get the current cursor position
  cursor <- context$selection[[1]]$range$start
  # Insert the template
  rstudioapi::insertText(location = cursor, text = template)
}

#' Add a Page Template to the Current Document
#'
#' This function inserts a template for a surveydown page at the current cursor
#' position in the active RStudio document. It provides a basic structure for a
#' new page, including a title, content area, and a next button. If the
#' function call exists in the document, it will be removed before inserting
#' the template.
#'
#' @details
#' IMPORTANT: This function should be run outside any division or R code chunk
#' in your 'Quarto' document. Running it inside a division or code chunk may
#' result in an incorrect page structure.
#'
#' The function performs the following steps:
#' 1. Checks for and removes any existing `sd_add_page()` function call in the document.
#' 2. Inserts a template at the current cursor position.
#'
#' The template includes:
#' - A div with class `'sd-page'` and a placeholder page ID
#' - A placeholder for the page title
#' - A placeholder for page contents
#' - An R code chunk with a placeholder for questions and a next button
#'
#' @return This function does not return a value. It modifies the active
#' document as a side effect by inserting text and potentially removing a
#' function call.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Insert a new page template
#'   sd_add_page()
#' }
#'
#' @export
sd_add_page <- function() {
  # Display a pop-up notice
  message("Note: Run this function outside division or code chunk.")

  template <- '::: {#page_id .sd-page}

# Page Title

Page contents...

```{r}
# Insert your question here...

# Next button
sd_next()
```

:::

'
  # Get the current document context
  context <- rstudioapi::getActiveDocumentContext()
  # Get all lines of the document
  lines <- context$contents
  # Find the line containing the function call
  call_line <- which(grepl("sd_add_page\\(\\)", lines))

  if (length(call_line) > 0) {
    # Remove the line containing the function call
    rstudioapi::modifyRange(
      c(call_line, 1, call_line + 1, 1),
      ""
    )
    # Update the context after removal
    context <- rstudioapi::getActiveDocumentContext()
  }

  # Get the current cursor position
  cursor <- context$selection[[1]]$range$start
  # Insert the template
  rstudioapi::insertText(location = cursor, text = template)
}

#' Check Surveydown Version
#'
#' This function checks if the local surveydown package is up-to-date with
#' the latest online version. It compares the local version with the latest
#' version available on GitHub and provides information about whether an update
#' is needed.
#'
#' @return No return value, called for side effects (prints version information
#' and update status to the console).
#' @export
#'
#' @examples
#' surveydown::sd_version()
sd_version <- function() {
  # Get local version
  local_surveydown_version <- utils::packageVersion("surveydown")

  # Get latest online version
  latest_surveydown_version <- get_latest_version("https://raw.githubusercontent.com/surveydown-dev/surveydown/main/DESCRIPTION", "Version: ")

  # Display version information
  message("surveydown (local): ", local_surveydown_version)
  message("surveydown (latest): ",
          if(is.null(latest_surveydown_version)) "Unable to fetch" else latest_surveydown_version)

  # Check if update is needed
  if (is.null(latest_surveydown_version)) {
    message("\nUnable to determine if an update is available.")
    message("Please ensure you have an active internet connection and try again later.")
  } else {
    pkg_needs_update <- local_surveydown_version < latest_surveydown_version

    if (pkg_needs_update) {
      message("\nAn update is available. To update surveydown to the latest version, run: surveydown::sd_update()")
    } else {
      message("\nsurveydown is up to date.")
    }
  }
}

get_latest_version <- function(url, pattern) {
  tryCatch({
    content <- readLines(url)
    version_line <- grep(pattern, content, value = TRUE)
    if (length(version_line) > 0) {
      version <- sub(pattern, "", version_line[1])
      return(package_version(trimws(version)))
    } else {
      message("Version information not found in the file at ", url)
      return(NULL)
    }
  }, error = function(e) {
    message("Error occurred while fetching version from ", url, ": ", e$message)
    return(NULL)
  })
}

#' Create a translations template file
#'
#' This function creates a template translations.yml file in the project root directory
#' that users can customize to modify system messages.
#'
#' @param language Character string specifying the language to use. See
#'   https://shiny.posit.co/r/reference/shiny/1.7.0/dateinput for supported
#'   languages. Also, if `"en"`, `"de"`, `"es"`, `"fr"`, or `"it"` is chosen,
#'   default messages in those langauges will be used, otherwise the default
#'   English messages will be used. Defaults to `"en"`.
#' @param path Character string specifying the directory where the translations.yml
#'   file should be created. Defaults to the current working directory. The
#'   file should be placed in the root project folder of your surveydown survey.
#' @return Invisible `NULL`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Create English template
#'   sd_create_translations()
#'
#'   # Create German template
#'   sd_create_translations(language = "de")
#'
#'   # Create Japanese template
#'   # Will use English messages but Japanese date picker - user can modify
#'   # the messages as desired
#'   sd_create_translations(language = "ja")
#' }
sd_create_translations <- function(language = "en", path = getwd()) {
  # Define valid languages
  valid_languages <- get_valid_languages()

  # Check if language is valid
  if (is.null(language) || !(language %in% valid_languages)) {
    stop(
      "Invalid language selected. Check https://shiny.posit.co/r/reference/shiny/1.7.0/dateinput for supported languages."
    )
  }

  # Get default translations
  translations <- get_translations_default()

  # If language has default messages, use them; otherwise use English
  template <- list()
  if (language %in% names(translations)) {
    template[[language]] <- translations[[language]]
  } else {
    template[[language]] <- translations[["en"]]
    message(
      "No default messages available for '", language,
      "'. surveydown currently only provides default translations for the following languages: 'en', 'de', 'fr', 'it', 'es', and 'zh-CN'.\n\n",
      "Using English messages with ", language, " date picker.\n"
    )
  }

  # Create the file path
  file_path <- file.path(path, "translations.yml")

  # Check if file already exists
  if (file.exists(file_path)) {
    stop("translations.yml already exists in the specified path")
  }

  # Define template header
  header <- paste(
    "# Surveydown translations template",
    "# Edit the values below to customize system messages",
    "# Keep the structure and keys unchanged",
    "",
    sep = "\n"
  )

  # Write question to YAML (with comment in first lines)
  yaml_content <- paste0(header, yaml::as.yaml(template))
  writeLines(yaml_content, con = file_path)
  message(
    "Created translations template at: ", file_path,
    "\n\nModify it to provide custom messages in '", language, "'."
  )
  invisible(NULL)
}

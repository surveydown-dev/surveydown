# Convert Markdown to HTML
markdown_to_html <- function(text) {
  if (is.null(text)) {
    return(text)
  }
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
  # Set cli option to suppress theme warning
  options(cli.ignore_unknown_rstudio_theme = TRUE)

  # Add special folders to resource path
  folders <- c('_survey', 'images', 'css', 'js', 'www')
  for (folder in folders) {
    include_folder(folder)
  }

  # Print package data
  desc <- utils::packageDescription(pkgname, libname)
  packageStartupMessage(
    "Version:  ",
    desc$Version,
    "\n",
    "Author:   ",
    "John Paul Helveston, Pingfan Hu, Bogdan Bunea (George Washington University)",
    "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/surveydown/issues/41.\n\n",
    "Please cite our package in your publications, see:\ncitation(\"surveydown\")\n"
  )
}

survey_file_exists <- function() {
  files <- basename(list.files(full.names = TRUE))
  if ("survey.qmd" %in% files) {
    return(TRUE)
  }
  return(FALSE)
}

app_file_exists <- function() {
  files <- basename(list.files(full.names = TRUE))
  if ("app.R" %in% files) {
    return(TRUE)
  }
  return(FALSE)
}

check_files_missing <- function() {
  if (!survey_file_exists()) {
    stop(
      'Missing "survey.qmd" file - your survey file must be named "survey.qmd"'
    )
  }
  if (!app_file_exists()) {
    stop(
      'Missing "app.R" file - your app file must be named "app.R"'
    )
  }
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
    message(paste(
      "The folder",
      folder,
      "is already included by the package. No action needed."
    ))
    return(invisible(NULL))
  }

  if (!dir.exists(folder)) {
    stop(paste(
      "The folder",
      folder,
      "does not exist in the current directory."
    ))
  }

  shiny::addResourcePath(folder, folder)
  message(paste("Successfully added", folder, "to Shiny's resource path."))

  invisible(NULL)
}

# Convert Vector to JSON Array
vector_to_json_array <- function(vec) {
  if (length(vec) == 0) {
    return("[]")
  }

  # Ensure all elements are properly quoted
  quoted_elements <- sapply(vec, function(x) {
    if (is.character(x)) {
      sprintf('"%s"', gsub('"', '\\"', x)) # Escape any quotes within strings
    } else {
      as.character(x)
    }
  })

  # Join elements and wrap in brackets
  sprintf("[%s]", paste(quoted_elements, collapse = ","))
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
#' This function creates a new survey template by copying template files to the
#' specified directory. You can choose from various predefined templates,
#' including the default built-in template and specialized templates from the
#' surveydown-dev/templates repository.
#'
#' @param template A character string specifying the template to use.
#'   Default is "default" which uses the built-in package template.
#'   Other options include:
#'   \describe{
#'     \item{default}{The default built-in template}
#'     \item{conditional_showing}{Conditionally showing questions and pages}
#'     \item{conditional_skipping}{Conditionally skipping to forward pages}
#'     \item{conditional_stopping}{Conditionally stopping the navigation}
#'     \item{conjoint_buttons}{Conjoint analysis with button interface}
#'     \item{conjoint_tables}{Conjoint analysis with table interface}
#'     \item{custom_leaflet_map}{Survey with interactive Leaflet maps}
#'     \item{custom_plotly_chart}{Survey with Plotly visualizations}
#'     \item{external_redirect}{Template with external site redirects}
#'     \item{live_polling}{Live polling template for real-time surveys}
#'     \item{question_types}{Showcases all available question types}
#'     \item{questions_yml}{Survey with questions defined in a YAML file}
#'     \item{random_options}{Survey with randomized question options}
#'     \item{random_options_predefined}{Randomized options from predefined sets}
#'     \item{reactive_drilldown}{Dynamic questions with drill-down capability}
#'     \item{reactive_questions}{Survey with reactive questions}
#'   }
#' @param path A character string specifying the directory where the survey
#'   template should be created. Defaults to the current working directory.
#' @param ask Logical. If `TRUE` (default), prompts for user confirmation
#'   when creating the survey in the current directory. If `FALSE`, bypasses
#'   the confirmation prompt and proceeds without asking.
#'
#' @details
#' When creating a new survey template, this function will:
#' 1. Check if the specified template is valid
#' 2. Confirm the destination path with the user (if it's the current directory)
#' 3. Download template files from GitHub if a non-default template is specified
#' 4. Copy template files to the destination directory
#' 5. Skip .Rproj files if one already exists in the destination
#' 6. Prompt for confirmation before overwriting existing files
#'
#' External templates are downloaded from the surveydown-dev/templates GitHub repository.
#'
#' @return Invisible `NULL`. The function is called for its side effects.
#'
#' @examples
#' if (interactive()) {
#'   # Create a survey with the "question_types" template in the "my_survey" directory
#'   sd_create_survey(template = "question_types", path = "my_survey")
#'
#'   # Create a survey using the default template in the "my_survey" directory
#'   sd_create_survey(path = "my_survey")
#'
#'   # Create a survey with default template in current directory
#'   sd_create_survey("default")
#'
#'   # Create a survey without asking for confirmation
#'   sd_create_survey(template = "default", path = "my_survey", ask = FALSE)
#' }
#'
#' @export
sd_create_survey <- function(template = "default", path = getwd(), ask = TRUE) {
  # Available templates from surveydown-dev/templates
  available_templates <- c(
    "default",
    "conditional_showing",
    "conditional_skipping",
    "conditional_stopping",
    "conjoint_buttons",
    "conjoint_tables",
    "custom_leaflet_map",
    "custom_plotly_chart",
    "external_redirect",
    "live_polling",
    "question_types",
    "questions_yml",
    "random_options",
    "random_options_predefined",
    "reactive_drilldown",
    "reactive_questions"
  )

  # Check if template is valid
  if (!template %in% available_templates) {
    stop(
      "Invalid template. Available templates are: ",
      paste(available_templates, collapse = ", ")
    )
  }

  if (
    ask &&
      path == getwd() &&
      !yesno(paste0('Use the current directory "', path, '" as the path?'))
  ) {
    stop("Operation aborted by the user.")
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Determine where to get template files from
  if (template == "default") {
    # Use built-in template
    template_path <- system.file("template", package = "surveydown")
  } else {
    # Download from GitHub
    template_path <- download_template_from_github(template)
  }

  if (!dir.exists(template_path)) {
    stop("Template directory does not exist.")
  }

  template_files <- list.files(
    template_path,
    full.names = TRUE,
    recursive = TRUE
  )

  files_copied <- sapply(template_files, function(file) {
    relative_path <- sub(template_path, "", file)
    target_file <- file.path(path, relative_path)

    dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)

    file_name <- basename(file)

    # Special handling for .Rproj files - skip if one already exists
    if (
      grepl("\\.Rproj$", file_name) &&
        length(list.files(path, pattern = "\\.Rproj$"))
    ) {
      warning(
        "Skipping the .Rproj file since one already exists.",
        call. = FALSE,
        immediate. = TRUE
      )
      return(FALSE)
    } else if (file.exists(target_file)) {
      # For other files, prompt for confirmation if they already exist
      overwrite <- yesno(paste0(
        "File '",
        file_name,
        "' already exists. Overwrite it"
      ))
      if (overwrite) {
        file.copy(from = file, to = target_file, overwrite = TRUE)
        message(paste("Overwriting", file_name))
        return(TRUE)
      } else {
        message(paste("Skipping", file_name))
        return(FALSE)
      }
    } else {
      file.copy(from = file, to = target_file, overwrite = FALSE)
      return(TRUE)
    }
  })

  # Create success message that includes the template
  if (any(files_copied)) {
    if (template == "default") {
      cli::cli_alert_success(paste("Template created at", path))
    } else {
      cli::cli_alert_success(paste("Template of", template, "created at", path))
    }
  } else {
    cli::cli_alert_success("Since all files exist, no file was added.")
  }

  # Clean up temp directory if needed
  if (template != "default") {
    unlink(dirname(template_path), recursive = TRUE)
  }

  invisible(NULL)
}

# Helper function to download and extract a template from GitHub
download_template_from_github <- function(template) {
  # Create a temporary directory
  temp_dir <- tempfile("surveydown_template_")
  dir.create(temp_dir, recursive = TRUE)

  # Download the specific template directory from GitHub
  repo_url <- paste0(
    "https://github.com/surveydown-dev/templates/archive/refs/heads/main.zip"
  )

  temp_zip <- file.path(temp_dir, "template.zip")

  # Download the zip file
  utils::download.file(repo_url, temp_zip, quiet = TRUE, mode = "wb")

  # Extract the zip file
  utils::unzip(temp_zip, exdir = temp_dir)

  # Path to the extracted template folder
  template_path <- file.path(temp_dir, "templates-main", template)

  if (!dir.exists(template_path)) {
    stop("Template '", template, "' not found in the repository")
  }

  return(template_path)
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
  id     = "mc_id",
  label  = "mc_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b"
  )
)
',
    text = 'sd_question(
  type  = "text",
  id    = "text_id",
  label = "text_label"
)
',
    textarea = 'sd_question(
  type  = "textarea",
  id    = "textarea_id",
  label = "textarea_label"
)
',
    numeric = 'sd_question(
  type  = "numeric",
  id    = "numeric_id",
  label = "numeric_label"
)
',
    mc_buttons = 'sd_question(
  type   = "mc_buttons",
  id     = "mc_buttons_id",
  label  = "mc_buttons_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b"
  )
)
',
    mc_multiple = 'sd_question(
  type  = "mc_multiple",
  id    = "mc_multiple_id",
  label = "mc_multiple_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b"
  )
)
',
    mc_multiple_buttons = 'sd_question(
  type  = "mc_multiple_buttons",
  id    = "mc_multiple_buttons_id",
  label = "mc_multiple_buttons_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b"
  )
)
',
    select = 'sd_question(
  type  = "select",
  id    = "select_id",
  label = "select_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b"
  )
)
',
    slider = 'sd_question(
  type  = "slider",
  id    = "slider_id",
  label = "slider_label",
  option = c(
    "Option A" = "option_a",
    "Option B" = "option_b",
    "Option C" = "option_c"
  )
)
',
    slider_numeric = 'sd_question(
  type  = "slider_numeric",
  id    = "slider_numeric_id",
  label = "slider_numeric_label",
  option = seq(0, 10, 1)
)
',
    slider_numeric_2 = 'sd_question(
  type  = "slider_numeric",
  id    = "slider_numeric_id",
  label = "slider_numeric_label",
  option = seq(0, 10, 1),
  default = c(3, 5)
)
',
    date = 'sd_question(
  type  = "date",
  id    = "date_id",
  label = "date_label"
)
',
    daterange = 'sd_question(
  type  = "daterange",
  id    = "daterange_id",
  label = "daterange_label"
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
#' @param id A character string specifying the ID for the question. If not provided,
#'   a default ID based on the question type will be used. This ID should be unique
#'   within your survey.
#' @param label A character string specifying the label (question text) to display
#'   to respondents. If not provided, a default label placeholder will be used.
#' @param chunk Logical. If `TRUE`, the code will be generated with the R code
#'   chunk wrapper. Defaults to `FALSE`.
#' @details
#' The function performs the following steps:
#' 1. Checks for and removes any existing `sd_add_question()` function call in the document.
#' 2. Inserts the appropriate question template at the current cursor position.
#' 3. If an ID is provided, replaces the default ID in the template with the provided ID.
#' 4. If a label is provided, replaces the default label in the template with the provided label.
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
#'   # Insert a text input question with custom ID and label
#'   sd_add_question("text", id = "user_email", label = "What is your email address?")
#'
#'   # Insert a slider question template
#'   sd_add_question("slider", id = "satisfaction", label = "How satisfied were you with our service?")
#' }
#'
#' @export
sd_add_question <- function(
  type = "mc",
  id = NULL,
  label = NULL,
  chunk = FALSE
) {
  # Get the template
  template <- question_templates(type)

  # Replace the default ID with the provided ID if it's not NULL
  if (!is.null(id) && id != "") {
    # Replace the default ID in the template with the provided ID
    template <- gsub(paste0(type, "_id"), id, template)
  }

  # Replace the default label with the provided label if it's not NULL
  if (!is.null(label) && label != "") {
    # Replace the default label in the template with the provided label
    template <- gsub(paste0(type, "_label"), label, template)
  }

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

#' Show a Shiny gadget for selecting a question type
#'
#' This function displays a Shiny gadget that allows the user to select
#' a question type from a dropdown menu. Once submitted, it calls
#' sd_add_question() with the specified type.
#'
#' @param chunk Logical. If `TRUE`, the code will be generated with the R code
#'   chunk wrapper. Defaults to `FALSE`.
#'
#' @return The selected question type (invisibly).
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny dialogViewer runGadget selectInput actionButton observeEvent stopApp tags HTML
#' @export
#'
sd_question_gadget <- function(chunk = FALSE) {
  # Get all available question types (in alphabetical order)
  question_types <- c(
    "Date" = "date",
    "Date Range" = "daterange",
    "Multiple Choice" = "mc",
    "Multiple Choice (Multiple Selection)" = "mc_multiple",
    "Multiple Choice Buttons" = "mc_buttons",
    "Multiple Choice Buttons (Multiple Selection)" = "mc_multiple_buttons",
    "Numeric Input" = "numeric",
    "Select Dropdown" = "select",
    "Slider" = "slider",
    "Slider Numeric" = "slider_numeric",
    "Slider Numeric Range" = "slider_numeric_2",
    "Text Area" = "textarea",
    "Text Input" = "text"
  )

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        "
        $(document).ready(function() {
          // Add event listener for Enter key
          $(document).keypress(function(e) {
            if(e.which == 13) { // 13 is the Enter key code
              $('#submit').click();
              return false;
            }
          });
        });
      "
      ))
    ),
    miniUI::gadgetTitleBar("Add Survey Question"),
    miniUI::miniContentPanel(
      shiny::selectInput(
        "question_type",
        "Question Type:",
        choices = question_types,
        selected = "mc"
      ),
      shiny::textInput(
        "question_id",
        "Question ID:",
        value = "",
        placeholder = "Enter a unique question ID without spaces"
      ),
      shiny::textInput(
        "question_label",
        "Question Label:",
        value = "",
        placeholder = "Enter the question text to display to respondents"
      ),
      shiny::checkboxInput(
        "in_chunk",
        "Insert in R code chunk",
        value = FALSE
      ),
      shiny::actionButton("submit", "Create Question", class = "btn-primary")
    )
  )

  server <- function(input, output, session) {
    # When submit button is clicked
    shiny::observeEvent(input$submit, {
      # Get the selected question type, ID, and label
      q_type <- input$question_type
      q_id <- input$question_id
      q_label <- input$question_label
      use_chunk <- input$in_chunk

      # Validate the question ID (simple validation)
      if (q_id == "") {
        shiny::showNotification("Question ID cannot be empty", type = "error")
        return()
      }

      # Close the gadget and return the values
      shiny::stopApp(list(
        type = q_type,
        id = q_id,
        label = q_label,
        chunk = use_chunk
      ))
    })

    # Also handle the "Done" button in the title bar
    shiny::observeEvent(input$done, {
      shiny::stopApp(NULL) # Return NULL if canceled
    })
  }

  # Run the gadget with a dialog viewer
  result <- shiny::runGadget(
    ui,
    server,
    viewer = shiny::dialogViewer(
      "Add Survey Question",
      width = 400,
      height = 480
    )
  )

  # If a valid result was returned, call sd_add_question
  if (!is.null(result)) {
    sd_add_question(
      type = result$type,
      id = result$id,
      label = result$label,
      chunk = result$chunk
    )
  }

  # Return the question type invisibly
  invisible(if (!is.null(result)) result$type else NULL)
}

#' Add a Page Template to the Current Document
#'
#' This function inserts a template for a surveydown page at the current cursor
#' position in the active RStudio document. It provides a basic structure for a
#' new page, including a title, content area, and a next button. If the
#' function call exists in the document, it will be removed before inserting
#' the template.
#'
#' @param page_id A character string specifying the ID for the page.
#'   Defaults to "page_id".
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
#' - A div with class `'sd-page'` and the specified page ID
#' - A placeholder for the page title
#' - A placeholder for page contents
#' - An R code chunk with a placeholder for questions and a next button
#'
#' Special page_id values:
#' - When page_id is "end", a thank-you page template with `sd_close()` is inserted
#'
#' @return This function does not return a value. It modifies the active
#' document as a side effect by inserting text and potentially removing a
#' function call.
#'
#' @examples
#' if (interactive()) {
#'   library(surveydown)
#'
#'   # Insert a new page template with default ID
#'   sd_add_page()
#'
#'   # Insert a new page template with custom ID
#'   sd_add_page(page_id = "welcome")
#'
#'   # Insert an end/thank you page
#'   sd_add_page(page_id = "end")
#' }
#'
#' @export
sd_add_page <- function(page_id = "page_id") {
  # Different template for end page
  if (page_id == "end") {
    template <- '::: {.sd_page id=end}

## Thanks for taking our survey!

```{r}
# Close button
sd_close()
```

:::

'
  } else {
    template <- sprintf(
      '::: {.sd_page id=%s}

Add page contents...

```{r}
# Insert questions...

# Next button
sd_next()
```

:::

',
      page_id
    )
  }

  # Get the current document context
  context <- rstudioapi::getActiveDocumentContext()
  # Get all lines of the document
  lines <- context$contents
  # Find the line containing the function call
  call_pattern <- "sd_add_page\\(.*\\)"
  call_line <- which(grepl(call_pattern, lines))

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

#' Show a Shiny gadget for entering a page ID
#'
#' This function displays a Shiny gadget that allows the user to input
#' a page ID. Once submitted, it calls sd_add_page() with the specified ID.
#'
#' @return The entered page ID (invisibly).
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom shiny dialogViewer runGadget textInput actionButton observeEvent stopApp tags HTML
#' @export
#'
sd_page_gadget <- function() {
  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(
        "
        $(document).ready(function() {
          // Set focus to the page_id input when the gadget loads
          $('#page_id').focus();

          // Add event listener for Enter key
          $('#page_id').keypress(function(e) {
            if(e.which == 13) { // 13 is the Enter key code
              $('#submit').click();
              return false;
            }
          });
        });
      "
      ))
    ),
    miniUI::gadgetTitleBar("Add Survey Page"),
    miniUI::miniContentPanel(
      shiny::textInput(
        "page_id",
        "Page ID:",
        value = "",
        placeholder = "Enter a unique page ID without spaces"
      ),
      shiny::helpText("For ending page, use \"end\" as your page ID"),
      shiny::actionButton("submit", "Create Page", class = "btn-primary")
    )
  )

  server <- function(input, output, session) {
    # When submit button is clicked
    shiny::observeEvent(input$submit, {
      # Call the sd_add_page function with the provided page_id
      page_id <- input$page_id

      # Validate the page_id (simple validation for demo)
      if (page_id == "") {
        shiny::showNotification("Page ID cannot be empty", type = "error")
        return()
      }

      # Close the gadget
      shiny::stopApp(page_id)
    })

    # Also handle the "Done" button in the title bar
    shiny::observeEvent(input$done, {
      shiny::stopApp(NULL) # Return NULL if canceled
    })
  }

  # Run the gadget with a dialog viewer
  page_id <- shiny::runGadget(
    ui,
    server,
    viewer = shiny::dialogViewer("Add Survey Page", width = 400, height = 300)
  )

  # If a valid page_id was returned, insert the page
  if (!is.null(page_id)) {
    sd_add_page(page_id)
  }

  # Return the page_id invisibly
  invisible(page_id)
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
  latest_surveydown_version <- get_latest_version(
    "https://raw.githubusercontent.com/surveydown-dev/surveydown/main/DESCRIPTION",
    "Version: "
  )

  # Display version information
  message("surveydown (local): ", local_surveydown_version)
  message(
    "surveydown (latest): ",
    if (is.null(latest_surveydown_version)) {
      "Unable to fetch"
    } else {
      latest_surveydown_version
    }
  )

  # Check if update is needed
  if (is.null(latest_surveydown_version)) {
    message("\nUnable to determine if an update is available.")
    message(
      "Please ensure you have an active internet connection and try again later."
    )
  } else {
    pkg_needs_update <- local_surveydown_version < latest_surveydown_version

    if (pkg_needs_update) {
      message(
        "\nAn update is available. To update surveydown to the latest version, run: pak::pak('surveydown-dev/surveydown')"
      )
    } else {
      message("\nsurveydown is up to date.")
    }
  }
}

get_latest_version <- function(url, pattern) {
  tryCatch(
    {
      content <- readLines(url)
      version_line <- grep(pattern, content, value = TRUE)
      if (length(version_line) > 0) {
        version <- sub(pattern, "", version_line[1])
        return(package_version(trimws(version)))
      } else {
        message("Version information not found in the file at ", url)
        return(NULL)
      }
    },
    error = function(e) {
      message(
        "Error occurred while fetching version from ",
        url,
        ": ",
        e$message
      )
      return(NULL)
    }
  )
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
      "No default messages available for '",
      language,
      "'. surveydown currently only provides default translations for the following languages: 'en', 'de', 'fr', 'it', 'es', and 'zh-CN'.\n\n",
      "Using English messages with ",
      language,
      " date picker.\n"
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
    "Created translations template at: ",
    file_path,
    "\n\nModify it to provide custom messages in '",
    language,
    "'."
  )
  invisible(NULL)
}

# Helper function to check if error is GSSAPI-related
is_gssapi_error <- function(error_msg) {
  grepl("invalid response to GSSAPI negotiation", error_msg, ignore.case = TRUE)
}

# Helper function to try database connection with specific gssencmode
try_db_connection <- function(params, gss_mode) {
  # Build connection arguments
  conn_args <- list(
    drv = RPostgres::Postgres(),
    host = params$host,
    dbname = params$dbname,
    port = params$port,
    user = params$user,
    password = params$password
  )

  # Add gssencmode unless it's explicitly set to NULL
  if (!is.null(gss_mode)) {
    if (!gss_mode %in% c("auto", "prefer", "disable")) {
      cli::cli_alert_warning(
        "Invalid 'gssencmode' setting. Must be set to 'auto', 'prefer', 'disable', or NULL...setting to 'auto'"
      )
      conn_args$gssencmode <- "prefer" # Use prefer for auto mode
    } else if (gss_mode == "auto") {
      conn_args$gssencmode <- "prefer" # Auto mode starts with prefer
    } else {
      conn_args$gssencmode <- gss_mode
    }
  } else {
    cli::cli_alert_warning(
      "'gssencmode' is set to NULL, so the 'gssencmode' parameter will not be passed to the database connection."
    )
  }

  # Create pool with dynamic arguments
  do.call(pool::dbPool, conn_args)
}

# Get client IP address from various headers (handles proxies/load balancers)
get_client_ip <- function(request) {
  # List of headers to check in order of preference
  ip_headers <- c(
    "HTTP_X_FORWARDED_FOR",
    "HTTP_X_REAL_IP",
    "HTTP_CF_CONNECTING_IP", # Cloudflare
    "HTTP_X_CLUSTER_CLIENT_IP",
    "HTTP_X_FORWARDED",
    "HTTP_FORWARDED_FOR",
    "HTTP_FORWARDED",
    "REMOTE_ADDR"
  )

  for (header in ip_headers) {
    ip <- request[[header]]
    if (!is.null(ip) && ip != "") {
      # X-Forwarded-For can contain multiple IPs, take the first one
      if (header == "HTTP_X_FORWARDED_FOR") {
        ip <- trimws(strsplit(ip, ",")[[1]][1])
      }

      # Skip localhost/private IPs unless it's the only option
      if (
        !ip %in% c("127.0.0.1", "::1") &&
          !grepl("^10\\.|^172\\.(1[6-9]|2[0-9]|3[01])\\.|^192\\.168\\.", ip)
      ) {
        return(ip)
      }
    }
  }

  # Fallback to REMOTE_ADDR even if it's localhost
  return(request$REMOTE_ADDR)
}

# Parse user agent string to extract browser information
parse_user_agent <- function(user_agent) {
  if (is.null(user_agent) || user_agent == "") {
    return(list(browser = "Unknown", version = "Unknown", os = "Unknown"))
  }

  # Browser detection patterns (order matters - more specific first)
  browser_patterns <- list(
    "Electron" = "Electron/([0-9]+)",
    "Chrome Mobile" = "CriOS/([0-9]+)", # Chrome iOS app - check this first
    "Google App" = "GSA/([0-9]+).*(?!CriOS)", # Google Search App (but not if CriOS is present)
    "Firefox Mobile" = "FxiOS/([0-9]+)", # Firefox iOS app
    "Edge Mobile" = "EdgiOS/([0-9]+)", # Edge iOS app
    "Opera Mobile" = "OPiOS/([0-9]+)", # Opera iOS app
    "Edge" = "Edge?/([0-9]+)",
    "Chrome" = "Chrome/([0-9]+)",
    "Firefox" = "Firefox/([0-9]+)",
    "Safari" = "Version/([0-9]+).*Safari",
    "Opera" = "Opera/([0-9]+)|OPR/([0-9]+)",
    "Internet Explorer" = "MSIE ([0-9]+)|Trident.*rv:([0-9]+)"
  )

  # OS detection patterns (more comprehensive and matching uaparserjs format)
  os_patterns <- list(
    "Mac OS X" = "Mac OS X ([0-9._]+)",
    "iOS" = "iPhone OS ([0-9._]+)|iOS ([0-9._]+)",
    "Windows" = "Windows NT ([0-9.]+)",
    "Android" = "Android ([0-9.]+)",
    "Linux" = "Linux",
    "Ubuntu" = "Ubuntu"
  )

  # Extract browser and version
  browser <- "Unknown"
  version <- "Unknown"
  for (name in names(browser_patterns)) {
    pattern <- browser_patterns[[name]]
    match <- regexpr(pattern, user_agent, perl = TRUE)
    if (match > 0) {
      browser <- name
      version_match <- regmatches(user_agent, match)
      # Extract the first number found in the match
      version_num <- regmatches(version_match, regexpr("[0-9]+", version_match))
      if (length(version_num) > 0) {
        version <- version_num[1]
      }
      break
    }
  }

  # Extract OS
  os <- "Unknown"
  for (name in names(os_patterns)) {
    if (grepl(os_patterns[[name]], user_agent, ignore.case = TRUE)) {
      os <- name
      break
    }
  }

  return(list(browser = browser, version = version, os = os))
}

# Replaces usethis::ui_yeah, inspired by internal yesno function in devtools
yesno <- function(msg) {
  # Define fun options for yes/no
  yeses <- c(
    "Yes",
    "Definitely",
    "For sure",
    "Yup",
    "Yeah",
    "Of course",
    "Absolutely"
  )
  nos <- c("No way", "Not yet", "I forget", "No", "Nope", "Uhhhh... Maybe?")

  # Ensure message ends with question mark
  if (!grepl("\\?\\s*$", msg)) {
    msg <- paste0(msg, "?")
  }

  # Display the message
  cli::cli_inform(msg)

  # Create random options (1 yes, 2 no) and shuffle them
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  # Display menu and get response
  selection <- utils::menu(qs[rand])

  # If nothing was selected (0), return FALSE
  if (selection == 0) {
    return(FALSE)
  }

  # Find which index corresponds to the yes option
  yes_position <- which(rand == 1)

  # Return TRUE if the yes option was selected
  return(selection == yes_position)
}

#' Generate a Random Completion Code
#'
#' This function generates a random completion code with a specified number of
#' digits. The code is returned as a character string.
#'
#' @param digits An integer specifying the number of digits in the completion
#'   code. Must be a positive integer. Default is 6.
#'
#' @return A character string representing the random completion code.
#'
#' @examples
#' library(surveydown)
#'
#' sd_completion_code()  # generates a 6-digit code
#' sd_completion_code(digits = 8)  # generates an 8-digit code
#' sd_completion_code(digits = 4)  # generates a 4-digit code
#' sd_completion_code(digits = 10)  # generates a 10-digit code
#'
#' @export
sd_completion_code <- function(digits = 6) {
  if (!is.numeric(digits) || digits < 1 || digits != round(digits)) {
    stop("'digits' must be a positive integer")
  }

  # Generate random digits
  digits_vector <- sample(0:9, digits, replace = TRUE)

  # Ensure the first digit is not 0
  digits_vector[1] <- sample(1:9, 1)

  # Combine into a single string
  paste(digits_vector, collapse = "")
}

#' Store a value in the survey data
#'
#' This function allows storing additional values to be included in the survey
#' data, such as respondent IDs or other metadata. When a database connection
#' is provided, it implements session persistence - if a value already exists
#' for the current session, storage is skipped to maintain consistency across
#' page refreshes.
#'
#' @param value The value to be stored. This can be any R object that can be
#'   coerced to a character string.
#' @param id (Optional) Character string. The id (name) of the value in the
#'   data. If not provided, the name of the `value` variable will be used.
#' @param db (Optional) Database connection object created with sd_db_connect().
#'   If provided, enables session persistence. If not provided, will automatically
#'   look for a variable named 'db' in the calling environment, or fall back to
#'   the database connection from the session.
#' @param auto_assign Logical. If `TRUE` (default), automatically assigns the
#'   stored value back to the original variable in the calling environment.
#'   This eliminates the need for explicit assignment when session persistence
#'   is desired. If `FALSE`, the function only returns the value without
#'   modifying the original variable.
#'
#' @return The value that was stored (either the new value or existing value
#'   from database if session persistence applies). This allows the function
#'   to be used in variable assignments.
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
#'   file.copy(survey_path, file.path(temp_dir, "basic_survey.qmd"))
#'   orig_dir <- getwd()
#'   setwd(temp_dir)
#'
#'   # Define a minimal server
#'   server <- function(input, output, session) {
#'     # Set up database connection
#'     db <- sd_db_connect()
#'
#'     # Generate and store values with automatic assignment (default behavior)
#'     respondentID <- sample(1:1000, 1)
#'     sd_store_value(respondentID, "respID", db)  # respondentID automatically updated
#'
#'     completion_code <- sample(0:9, 6, replace = TRUE)
#'     sd_store_value(completion_code)  # completion_code automatically updated
#'
#'     # Traditional assignment approach (auto_assign = FALSE)
#'     some_value <- sd_store_value(42, "some_value", auto_assign = FALSE)
#'
#'     # The function ensures session persistence across page refreshes
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
sd_store_value <- function(value, id = NULL, db = NULL, auto_assign = TRUE) {
  if (is.null(id)) {
    id <- deparse(substitute(value))
  }

  shiny::isolate({
    session <- shiny::getDefaultReactiveDomain()
    if (is.null(session)) {
      stop(
        "sd_store_value must be called from within a Shiny reactive context"
      )
    }

    # If db parameter not provided, try to auto-detect from calling environment or session
    if (is.null(db)) {
      # First try to find 'db' variable in the calling environment
      calling_env <- parent.frame()
      if (exists("db", envir = calling_env)) {
        db <- get("db", envir = calling_env)
      } else {
        # Fall back to session userData (for backward compatibility)
        db <- session$userData$db
      }
    }

    # Check if value already exists (session persistence logic)
    # Works for both database and local CSV modes
    # But only if all_data is available - otherwise we need to defer this check
    existing_value <- NULL
    if (!is.null(session$userData$all_data)) {
      # Get session ID based on use_cookies setting
      search_session_id <- get_session_id(session, db)

      # Check if this value already exists for this session
      existing_data <- get_session_data(db, search_session_id)

      if (!is.null(existing_data) && nrow(existing_data) > 0) {
        if (
          id %in%
            names(existing_data) &&
            !is.na(existing_data[[id]]) &&
            existing_data[[id]] != ""
        ) {
          # Value already exists - use existing value for session persistence
          existing_value <- existing_data[[id]]
        }
      }
    }

    # Determine which value to use and store
    final_value <- if (!is.null(existing_value)) {
      # Use existing value for session persistence
      existing_value
    } else {
      # Use new value
      format_question_value(value)
    }

    # Initialize stored_values if it doesn't exist
    if (is.null(session$userData$stored_values)) {
      session$userData$stored_values <- list()
    }

    session$userData$stored_values[[id]] <- final_value

    # Make value accessible in the UI
    output <- shiny::getDefaultReactiveDomain()$output
    output[[paste0(id, "_value")]] <- shiny::renderText({
      final_value
    })

    # Get access to all_data and update it if available
    # This allows the stored value to be accessible through sd_output
    if (!is.null(session$userData$all_data)) {
      session$userData$all_data[[id]] <- final_value
      # Add to changed fields to trigger database update (only if using new value)
      if (
        is.null(existing_value) && !is.null(session$userData$changed_fields)
      ) {
        current_fields <- session$userData$changed_fields()
        session$userData$changed_fields(c(current_fields, id))
      }
    } else {
      # If all_data not available yet, store for deferred processing
      # This works for both database and local CSV modes
      search_session_id <- get_session_id(session, db)

      # Check for existing value in either database or local CSV
      existing_data <- get_session_data(db, search_session_id)

      should_store_new_value <- TRUE
      if (!is.null(existing_data) && nrow(existing_data) > 0) {
        if (
          id %in%
            names(existing_data) &&
            !is.na(existing_data[[id]]) &&
            existing_data[[id]] != ""
        ) {
          # Use existing value for persistence - IMPORTANT: Update final_value here!
          final_value <- existing_data[[id]]
          should_store_new_value <- FALSE # Don't overwrite existing value
        }
      }

      # Always store the final value for session initialization
      if (is.null(session$userData$deferred_values)) {
        session$userData$deferred_values <- list()
      }
      session$userData$deferred_values[[id]] <- final_value

      # Track which values should NOT be written to storage (for session persistence)
      if (!should_store_new_value) {
        if (is.null(session$userData$deferred_skip_db)) {
          session$userData$deferred_skip_db <- character(0)
        }
        session$userData$deferred_skip_db <- c(
          session$userData$deferred_skip_db,
          id
        )
      }
    }
  })

  # Auto-assign to parent environment if requested
  if (auto_assign) {
    # Get the variable name from the first argument
    var_name <- deparse(substitute(value))
    # Use <<- to assign to the parent environment
    assign(var_name, final_value, envir = parent.frame())
  }

  # Return the final value so it can be used in variable assignment
  return(final_value)
}

# Helper function to format a single question value
format_question_value <- function(val) {
  if (is.null(val) || identical(val, NA) || identical(val, "NA")) {
    return("")
  } else if (length(val) > 1) {
    return(paste(val, collapse = ", "))
  } else {
    return(as.character(val))
  }
}

# Helper function to determine session ID based on use_cookies setting
get_session_id <- function(session, db) {
  current_session_id <- session$token
  persistent_session_id <- shiny::isolate(session$input$stored_session_id)

  if (!is.null(db)) {
    # Database mode: always use persistent session ID if available
    search_session_id <- if (
      !is.null(persistent_session_id) && nchar(persistent_session_id) > 0
    ) {
      persistent_session_id
    } else {
      current_session_id
    }
  } else {
    # Local CSV mode: check use_cookies setting
    settings <- get_settings_yml()
    use_cookies_setting <- if (
      !is.null(settings) && !is.null(settings$use_cookies)
    ) {
      # Convert YAML boolean values to R logical
      if (is.character(settings$use_cookies)) {
        settings$use_cookies %in% c("yes", "true", "TRUE", "True")
      } else {
        as.logical(settings$use_cookies)
      }
    } else {
      TRUE # Default to TRUE if no setting found
    }

    # For local CSV mode, handle use_cookies setting changes
    if (
      use_cookies_setting &&
        !is.null(persistent_session_id) &&
        nchar(persistent_session_id) > 0
    ) {
      # use_cookies is TRUE and we have a persistent session ID
      search_session_id <- persistent_session_id
    } else {
      # use_cookies is FALSE OR no persistent session ID available
      # Always use current session ID to ensure fresh values
      search_session_id <- current_session_id
    }
  }

  return(search_session_id)
}

# Helper function to get settings.yml file
get_settings_yml <- function() {
  path <- file.path("_survey", "settings.yml")
  if (fs::file_exists(path)) {
    return(yaml::read_yaml("_survey/settings.yml"))
  }

  # Fallback: if settings.yml doesn't exist, read directly from survey.qmd YAML header
  if (file.exists("survey.qmd")) {
    tryCatch(
      {
        metadata <- quarto::quarto_inspect("survey.qmd")
        yaml_metadata <- metadata$formats$html$metadata

        # Extract all server configuration parameters if available
        settings <- list()
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

        for (param in server_params) {
          # Try underscore version first
          if (!is.null(yaml_metadata[[param]])) {
            settings[[param]] <- yaml_metadata[[param]]
          } else {
            # Try dash version
            dash_param <- gsub("_", "-", param)
            if (!is.null(yaml_metadata[[dash_param]])) {
              settings[[param]] <- yaml_metadata[[dash_param]]
            }
          }
        }

        if (length(settings) > 0) {
          return(settings)
        }
      },
      error = function(e) {
        # If quarto inspection fails, continue with NULL
      }
    )
  }

  return(NULL)
}

# Main function to get session data from any available source (database or local CSV)
get_session_data <- function(db, search_session_id) {
  if (!is.null(db)) {
    # Database mode
    return(get_db_data(db, search_session_id))
  } else {
    # Local CSV mode
    all_local_data <- get_local_data()
    if (!is.null(all_local_data)) {
      return(all_local_data[all_local_data$session_id == search_session_id, ])
    } else {
      return(NULL)
    }
  }
}

# Helper function to get local CSV data
get_local_data <- function() {
  if (file.exists("preview_data.csv")) {
    tryCatch(
      {
        return(utils::read.csv(
          "preview_data.csv",
          stringsAsFactors = FALSE
        ))
      },
      error = function(e) {
        warning("Error reading preview_data.csv: ", e$message)
        return(NULL)
      }
    )
  }
  return(NULL)
}

# Internal function to get data from database for a specific session only
get_db_data <- function(db, session_id) {
  if (is.null(db)) {
    return(NULL)
  }

  # Get the correct table name from db object
  table <- db$table
  if (is.null(table)) {
    warning("Table name not found in database object")
    return(NULL)
  }

  tryCatch(
    {
      # Check if table exists first
      table_exists <- pool::poolWithTransaction(db$db, function(conn) {
        DBI::dbExistsTable(conn, table)
      })

      if (!table_exists) {
        return(NULL)
      }

      # Use the same pooling pattern as sd_get_data()
      result <- pool::poolWithTransaction(db$db, function(conn) {
        query <- paste0(
          "SELECT * FROM ",
          table,
          " WHERE session_id = $1"
        )
        DBI::dbGetQuery(conn, query, params = list(session_id))
      })

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      return(result)
    },
    error = function(e) {
      warning("Failed to fetch session data: ", e$message)
      return(NULL)
    }
  )
}

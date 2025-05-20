#' Launch surveydown Studio
#'
#' This function serves two purposes:
#' 1. It generates a new survey project from a template
#' 2. It launches a Shiny app to preview and edit the survey
#'
#' @param path A character string specifying the directory where the survey
#'   template should be created. Defaults to the current working directory.
#' @param template A character string specifying the template to use.
#'   Default is "default" which uses the built-in package template.
#'   See `sd_create_survey()` for other available templates.
#'
#' @return No return value, called for its side effects of creating a survey
#'   project and launching a Shiny app.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Launch studio with default template
#'   sd_studio()
#'
#'   # Launch studio with a specific template and path
#'   sd_studio(path = "my_survey", template = "question_types")
#' }
sd_studio <- function(path = getwd(), template = "default") {
  # Check if the function was called with an explicit template parameter
  template_explicitly_provided <- !missing(template)
  
  # Check if survey.qmd and app.R files already exist
  survey_exists <- file.exists(file.path(path, "survey.qmd"))
  app_exists <- file.exists(file.path(path, "app.R"))
  
  # Create a new survey if template was explicitly provided or files don't exist
  if (template_explicitly_provided || !survey_exists || !app_exists) {
    sd_create_survey(path = path, template = template)
  }
  
  # Set working directory to the survey path
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(path)
  
  # Launch the Shiny app
  shiny::shinyApp(ui = studio_ui(), server = studio_server())
}

# UI ----

# Main UI framework
studio_ui <- function() {
  shiny::navbarPage(
    title = "surveydown Studio",
    id = "tabset",
    theme = bslib::bs_theme(version = 5),
    ui_construction_tab(),
    ui_preview_tab()
  )
}

# Construction tab UI
ui_construction_tab <- function() {
  shiny::tabPanel(
    "Construction",

    # Custom CSS and JavaScript
    shiny::tags$head(
      shiny::tags$style(HTML(get_studio_css())),
      shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.14.0/Sortable.min.js"),
      shiny::tags$script(HTML(get_studio_js()))
    ),

    shiny::fluidRow(
      # Left - Add Content Panel
      shiny::column(
        width = 3,
        style = "border-right: 1px solid #ddd;",
        
        # Add Page UI
        shiny::h5("Add Page", 
            style = "text-align: center; background-color: #ffe0b2; padding: 6px; margin-bottom: 10px; border-radius: 4px;"),
        shiny::wellPanel(
          style = "background-color: #fff9f0; border-color: #ffe0b2; padding: 0.5rem;",
          shiny::textInput("new_page_id", "Page ID:", placeholder = "Enter page ID (e.g., welcome, questions, end)"),
          shiny::actionButton("add_page_btn", "Add Page", class = "btn-success", style = "width: 100%;")
        ),
        
        shiny::tags$hr(style = "margin: 1rem 0;"),

        # Unified Add Content UI
        shiny::h5("Add Content", 
            style = "text-align: center; background-color: #ffe0b2; padding: 6px; margin-bottom: 10px; border-radius: 4px;"),
        shiny::wellPanel(
          style = "background-color: #fff3e0; border-color: #ffe0b2; padding: 0.5rem;",
          shiny::div(
            style = "overflow-y: auto; height: calc(100vh - 378px);",
            
            # Common inputs for both content types
            shiny::selectInput("page_for_content", "To Page:", choices = NULL),
            shiny::selectInput("content_type", "Content Type:", 
                      choices = c("Text" = "text", "Question" = "question")),
            
            # Conditional UI based on content type
            shiny::conditionalPanel(
              condition = "input.content_type == 'text'",
              shiny::textAreaInput("text_content", "Text:", rows = 3, 
                                placeholder = "Enter markdown text to add to the page")
            ),
            
            shiny::conditionalPanel(
              condition = "input.content_type == 'question'",
              shiny::selectInput("question_type", "Question Type:", 
                        choices = c(
                          "Multiple Choice" = "mc",
                          "Text Input" = "text",
                          "Textarea" = "textarea",
                          "Numeric Input" = "numeric",
                          "Multiple Choice Buttons" = "mc_buttons",
                          "Multiple Choice Multiple" = "mc_multiple",
                          "Multiple Choice Multiple Buttons" = "mc_multiple_buttons",
                          "Select Dropdown" = "select",
                          "Slider" = "slider",
                          "Slider Numeric" = "slider_numeric",
                          "Date" = "date",
                          "Date Range" = "daterange"
                        )),
              shiny::textInput("question_id", "Question ID:", placeholder = "Enter unique question ID"),
              shiny::textInput("question_label", "Question Label:", placeholder = "Enter question text")
            ),
            
            # Common add button
            shiny::actionButton("add_content_btn", "Add Content", class = "btn-primary", style = "width: 100%; margin-top: 10px;")
          )
        )
      ),
      
      # Middle - Structure Panel
      shiny::column(
        width = 4,
        style = "border-right: 1px solid #ddd;",
        shiny::div(
          shiny::h5("Structure", 
                    style = "text-align: center; background-color: #cce5ff; padding: 6px; margin-bottom: 10px; border-radius: 4px;"),
          shiny::wellPanel(
            style = "background-color: #f0f8ff; border-color: #cce5ff; padding: 0.5rem;",
            shiny::div(
              style = "overflow-y: auto; height: calc(100vh - 150px);",
              shiny::uiOutput("survey_structure")
            )
          )
        )
      ),
      
      # Right - Code Panel
      shiny::column(
        width = 5,
        style = "border-right: 1px solid #ddd;",
        shiny::div(
          style = "height: calc(100vh - 90px);",
          shiny::h5("Code", 
                    style = "text-align: center; background-color: #d4edda; padding: 6px; margin-bottom: 10px; border-radius: 4px;"),
          shiny::wellPanel(
            style = "background-color: #f0fff0; border-color: #d4edda; padding: 0.5rem;",
            shiny::tabsetPanel(
              id = "code_tabs",
              shiny::tabPanel(
                "survey.qmd",
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::uiOutput("survey_editor_ui")
                )
              ),
              shiny::tabPanel(
                "app.R",
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::uiOutput("app_editor_ui")
                )
              )
            )
          )
        )
      )
    )
  )
}

# Preview tab UI
ui_preview_tab <- function() {
  shiny::tabPanel(
    "Preview",
    shiny::div(
      style = "display: flex; flex-direction: column; align-items: center; height: calc(100vh - 89px);",
      
      # Preview iframe with reduced height to make room for button
      shiny::div(
        style = "width: 100%; height: calc(100vh - 125px); border: none;",
        shiny::uiOutput("preview_frame")
      ),
      
      # Centered button container
      shiny::div(
        style = "margin-top: 10px; text-align: center;",
        shiny::actionButton("refresh_preview_btn", "Refresh Preview", 
                           class = "btn-success btn-sm",
                           icon = shiny::icon("sync"))
      )
    )
  )
}

# Server ----

# Main server function
studio_server <- function() {
  function(input, output, session) {
    # Setup code editors
    output$survey_editor_ui <- shiny::renderUI({
      survey_content <- paste(readLines("survey.qmd", warn = FALSE), collapse = "\n")
      
      shinyAce::aceEditor(
        outputId = "survey_editor",
        value = survey_content,
        mode = "markdown",
        theme = "textmate",
        height = "calc(100vh - 203px)",
        fontSize = 14,
        wordWrap = TRUE
      )
    })

    output$app_editor_ui <- shiny::renderUI({
      app_content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
      
      shinyAce::aceEditor(
        outputId = "app_editor",
        value = app_content,
        mode = "r",
        theme = "chrome",
        height = "calc(100vh - 203px)",
        fontSize = 14,
        wordWrap = TRUE
      )
    })

    # Initialize structure and preview handlers
    survey_structure <- server_structure_handlers(input, output, session)
    preview_handlers <- server_preview_handlers(input, output, session)
    
    # Connect refresh button to preview function
    shiny::observeEvent(input$refresh_preview_btn, {
      preview_handlers$refresh_preview()
    })

    # Launch preview on startup
    shiny::observe({
      preview_handlers$refresh_preview()
    }, priority = 1000)
    
    # Handle R chunk separation for manual edits
    shiny::observeEvent(input$survey_editor, {
      shiny::invalidateLater(1000)
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Update page dropdown for content creation when pages change
    shiny::observe({
      page_ids <- survey_structure$get_page_ids()
      if (!is.null(page_ids) && length(page_ids) > 0) {
        shiny::updateSelectInput(session, "page_for_content", choices = page_ids)
      }
    })

    # Handle Add Page button
    shiny::observeEvent(input$add_page_btn, {
      page_id <- input$new_page_id
      if (is.null(page_id) || page_id == "") {
        shiny::showNotification("Please enter a page ID", type = "error")
        return()
      }
      
      # Get and prepare current editor content
      current_content <- input$survey_editor
      current_content <- r_chunk_separation(current_content)
      
      # Insert the new page
      updated_content <- insert_page_into_survey(page_id, current_content)
      
      # Update editor and clear input field
      shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
      shiny::updateTextInput(session, "new_page_id", value = "")
      shiny::showNotification(paste("Page", page_id, "added successfully!"), type = "message")
      
      # Refresh structure
      survey_structure$refresh()
    })
    
    # Handle Add Content button
    shiny::observeEvent(input$add_content_btn, {
      # Validate common inputs
      if (is.null(input$page_for_content) || input$page_for_content == "") {
        shiny::showNotification("Please select a page", type = "error")
        return()
      }
      
      # Get and prepare current editor content
      current_content <- input$survey_editor
      current_content <- r_chunk_separation(current_content)
      
      # Process based on content type
      if (input$content_type == "text") {
        # Validate text inputs
        if (is.null(input$text_content) || trimws(input$text_content) == "") {
          shiny::showNotification("Please enter some text content", type = "error")
          return()
        }
        
        # Insert the new text
        updated_content <- insert_text_into_survey(
          input$page_for_content,
          input$text_content,
          current_content
        )
        
        # Update editor if successful
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::updateTextAreaInput(session, "text_content", value = "")
          shiny::showNotification(paste("Text added to page", input$page_for_content), type = "message")
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to add text. Check page ID and try again.", type = "error")
        }
        
      } else if (input$content_type == "question") {
        # Validate question inputs
        if (is.null(input$question_id) || input$question_id == "") {
          shiny::showNotification("Please enter a question ID", type = "error")
          return()
        }
        
        # Insert the new question
        updated_content <- insert_question_into_survey(
          input$page_for_content,
          input$question_type,
          input$question_id,
          input$question_label,
          current_content
        )
        
        # Update editor if successful
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::updateTextInput(session, "question_id", value = "")
          shiny::updateTextInput(session, "question_label", value = "")
          shiny::showNotification(paste("Question", input$question_id, "added to page", input$page_for_content), type = "message")
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to add question. Check page ID and try again.", type = "error")
        }
      }
    })
    
    # Handle delete page button clicks
    shiny::observeEvent(input$delete_page_btn, {
      # The input$delete_page_btn will contain the page ID when a button is clicked
      page_id <- input$delete_page_btn
      
      if (!is.null(page_id) && page_id != "") {
        # Get and prepare current editor content
        current_content <- input$survey_editor
        current_content <- r_chunk_separation(current_content)
        
        # Delete the page
        updated_content <- delete_page_from_survey(page_id, current_content)
        
        # Update editor if successful
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          shiny::showNotification(paste("Page", page_id, "deleted successfully!"), type = "message")
          survey_structure$refresh()
        } else {
          shiny::showNotification(paste("Failed to delete page", page_id), type = "error")
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Handle page drag and drop reordering
    shiny::observeEvent(input$page_drag_completed, {
      # Prepare and separate content
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        Sys.sleep(0.1)
        current_content <- separated_content
      } else {
        current_content <- input$survey_editor
      }
      
      # Reorder pages if order array exists
      if (length(input$page_drag_completed$order) > 0) {
        updated_content <- reorder_pages(input$page_drag_completed$order, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to reorder pages", type = "error")
        }
      }
    }, ignoreInit = TRUE)

    # Handle content drag and drop reordering within a page
    shiny::observeEvent(input$content_drag_completed, {
      # Extract page ID and order
      page_id <- input$content_drag_completed$pageId
      flat_order <- input$content_drag_completed$order
      
      # Prepare and separate content
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        Sys.sleep(0.1)
        current_content <- separated_content
      } else {
        current_content <- input$survey_editor
      }
      
      # Process the flat order array into content list
      content_list <- process_content_order(flat_order)
      
      # Skip if no valid items
      if (length(content_list) == 0) {
        shiny::showNotification("No valid content items found", type = "error")
        return(NULL)
      }
      
      # Try to reorder content with error handling
      tryCatch({
        # Check for chunks that need separation
        check_and_separate_content(page_id, content_list, current_content, session)
        
        # Reorder the content
        updated_content <- reorder_page_content(page_id, content_list, current_content)
        
        if (!is.null(updated_content)) {
          # Ensure proper separation
          updated_content <- r_chunk_separation(updated_content)
          
          # Update editor
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          survey_structure$refresh()
          
          # Force a slight delay before allowing another drag
          shiny::invalidateLater(300)
        } else {
          shiny::showNotification(paste("Failed to reorder content in page", page_id), type = "error")
        }
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # Handle page reordering
    shiny::observeEvent(input$page_order, {
      if (length(input$page_order) > 0) {
        current_content <- input$survey_editor
        updated_content <- reorder_pages(input$page_order, current_content)
        
        if (!is.null(updated_content)) {
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to reorder pages", type = "error")
        }
      }
    }, ignoreInit = TRUE, priority = 100)
    
    # Clean up when session ends
    session$onSessionEnded(function() {
      process <- preview_handlers$preview_process()
      if (!is.null(process)) {
        try(tools::pskill(process), silent = TRUE)
      }
    })
  }
}

# Handler for survey structure management
server_structure_handlers <- function(input, output, session) {
  # Track structure changes
  structure_trigger <- shiny::reactiveVal(0)
  
  # Render the survey structure UI
  output$survey_structure <- shiny::renderUI({
    # Re-render on structure changes
    structure_trigger()
    
    # Parse survey structure
    survey_structure <- parse_survey_structure()
    
    # Check for errors
    if (!is.null(survey_structure$error)) {
      return(shiny::div(
        style = "color: red;",
        shiny::h4("Error"),
        shiny::p(survey_structure$error)
      ))
    }
    
    # Create the structure visualization
    render_survey_structure(survey_structure)
  })
  
  # Function to refresh the structure
  refresh_structure <- function() {
    structure_trigger(structure_trigger() + 1)
    shiny::invalidateLater(200)
  }
  
  # Function to get page IDs for dropdowns
  get_page_ids <- function() {
    survey_structure <- parse_survey_structure()
    if (!is.null(survey_structure$error)) {
      return(NULL)
    }
    return(survey_structure$page_ids)
  }
  
  # Refresh structure when survey.qmd is saved
  shiny::observeEvent(input$save_survey, {
    refresh_structure()
  }, ignoreInit = TRUE)
  
  # Monitor editor changes with debounce
  last_update_time <- shiny::reactiveVal(Sys.time())
  
  shiny::observeEvent(input$survey_editor, {
    current_time <- Sys.time()
    if (difftime(current_time, last_update_time(), units = "secs") > 1) {
      refresh_structure()
      last_update_time(current_time)
    } else {
      shiny::invalidateLater(1000)
    }
  }, ignoreInit = TRUE)
  
  # Return functions for external use
  list(
    refresh = refresh_structure,
    get_page_ids = get_page_ids
  )
}

# Handler for survey preview functionality
server_preview_handlers <- function(input, output, session) {
  # Store preview process
  preview_process <- shiny::reactiveVal(NULL)
  preview_port <- stats::runif(1, 3000, 8000) |> floor()
  
  # Launch preview function
  refresh_preview <- function() {
    # Get current process
    current_process <- NULL
    shiny::isolate({
      current_process <- preview_process()
    })
    
    # Stop existing process if it exists
    if (!is.null(current_process)) {
      try(tools::pskill(current_process), silent = TRUE)
      preview_process(NULL)
    }
    
    # Check if files exist
    if (!file.exists("survey.qmd") || !file.exists("app.R")) {
      shiny::showNotification("Error: survey.qmd or app.R file not found!", type = "error")
      return()
    }
    
    # Save current editor content to files before previewing
    if (exists("input") && !is.null(input$survey_editor)) {
      writeLines(input$survey_editor, "survey.qmd")
    }
    
    if (exists("input") && !is.null(input$app_editor)) {
      writeLines(input$app_editor, "app.R")
    }
    
    # Launch preview server
    new_process <- launch_preview_server(preview_port)
    
    # Store the process ID
    preview_process(new_process)
    
    # Display in iframe
    preview_url <- paste0("http://127.0.0.1:", preview_port)
    
    # Give the app a moment to start
    Sys.sleep(2)
    
    output$preview_frame <- shiny::renderUI({
      shiny::tags$iframe(
        src = preview_url,
        width = "100%",
        height = "100%",
        style = "border: 1px solid #ddd; border-radius: 5px; display: block;"
      )
    })
    shiny::showNotification("Survey refreshed!", type = "message")
  }
  
  # Return the refresh function and process for cleanup
  list(
    refresh_preview = refresh_preview,
    preview_process = preview_process
  )
}

# Content Editing Functions ----

# Insert a new page into the survey.qmd file
insert_page_into_survey <- function(page_id, editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the last page closing tag
  last_page_end <- max(which(grepl(":::", editor_content, fixed = TRUE)), 0)
  
  # If no page found, insert at the end of the file
  if (last_page_end == 0) {
    last_page_end <- length(editor_content)
  }
  
  # Generate the page template
  page_template <- generate_page_template(page_id)
  
  # Insert the page template after the last page or at the end
  result <- c(
    editor_content[1:last_page_end],
    page_template,
    if(last_page_end < length(editor_content)) editor_content[(last_page_end+1):length(editor_content)] else NULL
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Generate a page template based on page ID
generate_page_template <- function(page_id) {
  if (page_id == "end") {
    return(c(
      "::: {.sd_page id=end}",
      "",
      "## Thanks for taking our survey!",
      "",
      "```{r}",
      "# Close button",
      "sd_close()",
      "```",
      "",
      ":::",
      ""
    ))
  } else {
    return(c(
      paste0("::: {.sd_page id=", page_id, "}"),
      "",
      "```{r}",
      "sd_next()",
      "```",
      "",
      ":::",
      ""
    ))
  }
}

# Insert a question into a specific page
insert_question_into_survey <- function(page_id, question_type, question_id, question_label, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the end of the page
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Find the insertion point (before next button if exists)
  insertion_point <- find_insertion_point(editor_content, page_start_line, page_end_line)
  
  # Generate the question code
  question_code <- generate_question_code(question_type, question_id, question_label)
  
  # Create the question chunk
  question_chunk <- c(
    "```{r}",
    question_code,
    "```"
  )
  
  # Insert the question chunk
  result <- c(
    editor_content[1:(insertion_point-1)],
    question_chunk,
    editor_content[insertion_point:length(editor_content)]
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Insert text into a specific page
insert_text_into_survey <- function(page_id, text_content, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the end of the page
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Find the insertion point (before next button if exists)
  insertion_point <- find_insertion_point(editor_content, page_start_line, page_end_line)
  
  # Format the text (ensure it has proper line breaks)
  formatted_text <- strsplit(text_content, "\n")[[1]]
  
  # Insert the text
  result <- c(
    editor_content[1:(insertion_point-1)],
    formatted_text,
    "",  # Add an empty line after the text
    editor_content[insertion_point:length(editor_content)]
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Find the best insertion point for a new question
find_insertion_point <- function(editor_content, page_start_line, page_end_line) {
  # Find the next button chunk if it exists
  for (i in page_start_line:page_end_line) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      chunk_start <- i
      chunk_end <- NULL
      
      for (j in (i+1):page_end_line) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }
      
      if (!is.null(chunk_end)) {
        chunk_content <- editor_content[(chunk_start+1):(chunk_end-1)]
        if (any(grepl("sd_next\\(", chunk_content, perl = TRUE))) {
          return(chunk_start)
        }
      }
    }
  }
  
  # If no next button found, insert before the page end
  return(page_end_line)
}

# Reorder pages in the survey file
reorder_pages <- function(new_order, editor_content) {
  if (is.null(editor_content) || length(new_order) == 0) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find each page's start and end lines
  page_blocks <- list()
  
  for (page_id in new_order) {
    # Find the page start
    page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
    page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
    
    if (length(page_start_lines) == 0) {
      next  # Skip if page not found
    }
    
    # Use the first match
    page_start_line <- page_start_lines[1]
    page_end_line <- NULL
    
    # Find the page end
    for (i in page_start_line:length(editor_content)) {
      if (i > page_start_line && grepl("^:::$", editor_content[i])) {
        page_end_line <- i
        break
      }
    }
    
    if (!is.null(page_end_line)) {
      # Store the page block
      page_blocks[[page_id]] <- list(
        start = page_start_line,
        end = page_end_line,
        content = editor_content[page_start_line:page_end_line]
      )
    }
  }
  
  # Check if we found all pages
  if (length(page_blocks) != length(new_order)) {
    return(NULL)
  }
  
  # Find the position range for pages
  first_page_start <- min(sapply(page_blocks, function(block) block$start))
  last_page_end <- max(sapply(page_blocks, function(block) block$end))
  
  # Create the new content with reordered pages
  result <- c(
    editor_content[1:(first_page_start-1)],
    unlist(lapply(new_order, function(page_id) page_blocks[[page_id]]$content)),
    if(last_page_end < length(editor_content)) editor_content[(last_page_end+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Reorder content within a page
reorder_page_content <- function(page_id, new_content_order, editor_content) {
  if (is.null(editor_content) || is.null(page_id) || length(new_content_order) == 0) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the page end
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Get the page content
  original_page_content <- editor_content[page_start_line:page_end_line]
  
  # Find navigation chunks (containing sd_next, sd_prev, sd_close)
  navigation_chunks <- extract_navigation_chunks(original_page_content)
  
  # Get the current survey structure
  survey_structure <- parse_survey_structure()
  
  # Validate survey structure
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure)) ||
      !(page_id %in% names(survey_structure$pages))) {
    return(NULL)
  }
  
  # Get page items
  page_items <- survey_structure$pages[[page_id]]
  
  # Create new page content
  new_page_content <- c(
    editor_content[page_start_line],  # Page opening tag
    ""  # Add a blank line after the opening tag
  )
  
  # Add content in the new order
  for (item in new_content_order) {
    # Check if item has required fields and exists in page items
    if (!is.list(item) || !all(c("type", "id") %in% names(item)) ||
        !(item$id %in% names(page_items))) {
      next
    }
    
    current_item <- page_items[[item$id]]
    
    # Check if current item is valid
    if (!is.list(current_item) || !("is_question" %in% names(current_item))) {
      next
    }
    
    # Add the content based on item type
    if (current_item$is_question) {
      if ("raw" %in% names(current_item)) {
        r_chunk_lines <- strsplit(current_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, r_chunk_lines, "")
      }
    } else {
      if ("content" %in% names(current_item)) {
        text_lines <- strsplit(current_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
    }
  }
  
  # Add navigation chunks
  if (length(navigation_chunks) > 0) {
    for (chunk in navigation_chunks) {
      new_page_content <- c(new_page_content, chunk$lines, "")
    }
  }
  
  # Add page closing tag
  new_page_content <- c(new_page_content, ":::")
  
  # Combine with content before and after the page
  result <- c(
    editor_content[1:(page_start_line-1)],
    new_page_content,
    if (page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
  )
  
  return(paste(result, collapse = "\n"))
}

# Extract navigation chunks from page content
extract_navigation_chunks <- function(page_content) {
  navigation_chunks <- list()
  in_chunk <- FALSE
  chunk_start <- NULL
  
  for (i in seq_along(page_content)) {
    line <- page_content[i]
    
    if (grepl("^```\\{r\\}", line)) {
      in_chunk <- TRUE
      chunk_start <- i
    } else if (in_chunk && grepl("^```$", line)) {
      in_chunk <- FALSE
      # Check if this is a navigation chunk
      chunk_content <- page_content[(chunk_start+1):(i-1)]
      if (any(grepl("sd_next\\(|sd_prev\\(|sd_close\\(", chunk_content))) {
        navigation_chunks[[length(navigation_chunks) + 1]] <- list(
          start = chunk_start,
          end = i,
          lines = page_content[chunk_start:i]
        )
      }
    }
  }
  
  return(navigation_chunks)
}

# Separate multiple sd_* function calls into individual R chunks
r_chunk_separation <- function(editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  result <- character(0)
  i <- 1
  
  while (i <= length(editor_content)) {
    # Check if this is the start of an R chunk
    if (grepl("^```\\{r\\}", editor_content[i])) {
      # Find the end of this chunk
      chunk_start <- i
      chunk_end <- NULL
      
      for (j in (i+1):length(editor_content)) {
        if (grepl("^```$", editor_content[j])) {
          chunk_end <- j
          break
        }
      }
      
      # If we found a complete chunk
      if (!is.null(chunk_end)) {
        # Extract the chunk content
        chunk_content <- editor_content[(chunk_start+1):(chunk_end-1)]
        
        # Check for sd_ function calls
        sd_start_indices <- grep("\\bsd_[a-zA-Z0-9_]+\\s*\\(", chunk_content)
        
        # If multiple sd_ calls, split the chunk
        if (length(sd_start_indices) > 1) {
          # Process each sd_ call
          for (start_idx in sd_start_indices) {
            # Find the end of this call by tracking parentheses
            call_end <- find_function_call_end(chunk_content, start_idx)
            
            # Add this call as a separate chunk
            result <- c(
              result,
              "```{r}",
              chunk_content[start_idx:call_end],
              "```",
              ""  # Add an empty line between chunks
            )
          }
          
          # Skip past the original chunk
          i <- chunk_end + 1
        } else {
          # No multiple sd_ calls, keep the chunk as is
          result <- c(
            result,
            editor_content[chunk_start:chunk_end]
          )
          i <- chunk_end + 1
        }
      } else {
        # If no end of chunk found, add this line and continue
        result <- c(result, editor_content[i])
        i <- i + 1
      }
    } else {
      # Not an R chunk start, add this line and continue
      result <- c(result, editor_content[i])
      i <- i + 1
    }
  }
  
  # Remove trailing empty line if present
  if (length(result) > 0 && result[length(result)] == "") {
    result <- result[-length(result)]
  }
  
  return(paste(result, collapse = "\n"))
}

# Find the end of a function call by tracking parentheses
find_function_call_end <- function(chunk_content, start_idx) {
  call_end <- start_idx
  paren_count <- 0
  
  # Count initial opening parentheses in the first line
  opening <- gregexpr("\\(", chunk_content[start_idx])[[1]]
  if (opening[1] > 0) {
    paren_count <- length(opening)
  }
  
  # Count initial closing parentheses in the first line
  closing <- gregexpr("\\)", chunk_content[start_idx])[[1]]
  if (closing[1] > 0) {
    paren_count <- paren_count - length(closing)
  }
  
  # If parentheses not balanced in first line, find the end
  if (paren_count > 0) {
    for (k in (start_idx+1):length(chunk_content)) {
      # Update parenthesis count
      opening <- gregexpr("\\(", chunk_content[k])[[1]]
      if (opening[1] > 0) {
        paren_count <- paren_count + length(opening)
      }
      
      closing <- gregexpr("\\)", chunk_content[k])[[1]]
      if (closing[1] > 0) {
        paren_count <- paren_count - length(closing)
      }
      
      # If balanced, we found the end
      if (paren_count <= 0) {
        call_end <- k
        break
      }
    }
  }
  
  return(call_end)
}

# Generate question code based on type
generate_question_code <- function(type, id, label) {
  # Ensure we have valid inputs
  if (is.null(id) || id == "") id <- paste0(type, "_id")
  if (is.null(label) || label == "") label <- paste0(type, "_label")
  
  # Generate appropriate code based on question type
  if (type %in% c("mc", "mc_buttons", "mc_multiple", "mc_multiple_buttons", "select", "slider")) {
    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = c("),
      paste0("    \"Option A\" = \"option_a\","),
      paste0("    \"Option B\" = \"option_b\""),
      paste0("  )"),
      paste0(")")
    ))
  } else if (type == "slider_numeric") {
    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = seq(0, 10, 1)"),
      paste0(")")
    ))
  } else {
    # Simple questions (text, textarea, numeric, date, daterange)
    return(c(
      paste0("sd_question("),
      paste0("  type  = \"", type, "\","),
      paste0("  id    = \"", id, "\","),
      paste0("  label = \"", label, "\""),
      paste0(")")
    ))
  }
}

# Content Editing Helper Functions ----

# Parse survey structure from survey.qmd file
parse_survey_structure <- function() {
  # Read the survey content
  if (exists("input") && !is.null(input$survey_editor)) {
    survey_content <- input$survey_editor
    survey_content <- paste(survey_content, collapse = "\n")
  } else if (file.exists("survey.qmd")) {
    survey_content <- readLines("survey.qmd", warn = FALSE)
    survey_content <- paste(survey_content, collapse = "\n")
  } else {
    return(list(error = "survey.qmd file not found!"))
  }
  
  # Extract pages
  page_pattern_underscore <- ":::\\s*\\{\\s*\\.sd_page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  page_pattern_dash <- ":::\\s*\\{\\s*\\.sd-page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  
  # Check which pattern is used
  if (grepl(page_pattern_underscore, survey_content)) {
    page_pattern <- page_pattern_underscore
  } else if (grepl(page_pattern_dash, survey_content)) {
    page_pattern <- page_pattern_dash
  } else {
    return(list(error = "No pages found in survey.qmd!"))
  }
  
  # Find all pages
  page_matches <- gregexpr(page_pattern, survey_content, perl = TRUE)
  page_ids <- regmatches(survey_content, page_matches)
  
  if (length(page_ids) == 0 || length(page_ids[[1]]) == 0) {
    return(list(error = "No pages found in survey.qmd!"))
  }
  
  # Extract page IDs
  page_ids <- extract_page_ids(page_ids[[1]])
  
  # Split content by pages
  split_pattern <- ":::\\s*\\{\\s*\\.sd[_-]page"
  page_splits <- strsplit(survey_content, split_pattern, perl = TRUE)[[1]]
  
  if (length(page_splits) <= 1) {
    return(list(error = "Error parsing page content!"))
  }
  
  page_splits <- page_splits[-1]  # Remove content before first page
  
  # Process each page
  pages <- list()
  for (i in seq_along(page_ids)) {
    if (i <= length(page_splits)) {
      page_id <- page_ids[i]
      page_content <- extract_page_content(page_splits[i])
      pages[[page_id]] <- extract_page_items(page_content, page_id)
    }
  }
  
  return(list(pages = pages, page_ids = page_ids))
}

# Function to render the survey structure UI
render_survey_structure <- function(survey_structure) {
  shiny::div(
    id = "pages-container",
    lapply(survey_structure$page_ids, function(page_id) {
      page_items <- survey_structure$pages[[page_id]]
      
      # Sort items by position
      if (length(page_items) > 0) {
        positions <- sapply(page_items, function(item) item$position)
        sorted_items <- page_items[order(positions)]
      } else {
        sorted_items <- list()
      }
      
      shiny::div(
        class = "page-wrapper",
        `data-page-id` = page_id,
        
        # Page header with toggle and drag handle
        shiny::div(
          class = "page-header",
          shiny::div(
            class = "page-drag-handle drag-handle",
            shiny::icon("grip-lines")
          ),
          shiny::div(paste0("Page: ", page_id), style = "margin: 0; font-weight: bold;"),
          
          # Add this new section for the delete button
          shiny::div(
            class = "page-actions",
            style = "display: flex; gap: 5px;",
            # Delete page button
            shiny::actionButton(
              inputId = "delete_page_btn_ui",  # Use a common ID for all buttons
              label = NULL,
              icon = shiny::icon("trash-alt"),
              class = "btn-sm btn-outline-danger delete-page-btn",
              title = "Delete page",
              onclick = paste0("Shiny.setInputValue('delete_page_btn', '", page_id, "');"),
              `data-page-id` = page_id
            )
          ),
          
          shiny::div(
            class = "toggle-icon",
            shiny::icon("chevron-down")
          )
        ),
        
        # Content container
        shiny::div(
          class = "questions-container",
          id = paste0("page-", page_id, "-content"),
          `data-page-id` = page_id,
          style = "display: block;",
          
          # Add content items in their order
          if (length(sorted_items) > 0) {
            lapply(names(sorted_items), function(item_id) {
              render_content_item(sorted_items[[item_id]])
            })
          }
        )
      )
    })
  )
}

# Render a single content item (question or text)
render_content_item <- function(item) {
  if (item$is_question) {
    # Question item
    shiny::div(
      class = "question-item",
      `data-question-id` = item$id,
      `data-content-type` = "question",
      `data-position` = item$position,
      
      # Question drag handle
      shiny::div(
        class = "question-drag-handle drag-handle",
        shiny::icon("grip-lines")
      ),
      
      # Question content
      shiny::div(
        shiny::HTML(paste0("<strong>Question: ", item$id, "</strong>"))
      ),
      shiny::div(
        shiny::HTML(paste0("Type: ", item$type))
      ),
      shiny::div(
        shiny::HTML(paste0("Label: ", item$label))
      )
    )
  } else {
    # Text item
    shiny::div(
      class = "text-item",
      `data-text-id` = item$id,
      `data-content-type` = "text",
      `data-position` = item$position,
      
      # Text drag handle
      shiny::div(
        class = "text-drag-handle drag-handle",
        shiny::icon("grip-lines")
      ),
      
      # Text content preview
      shiny::div(
        shiny::HTML(paste0("<strong>Text:</strong> ", item$preview))
      )
    )
  }
}

# Extract page IDs from page matches
extract_page_ids <- function(page_matches) {
  extracted_ids <- vector("character", length(page_matches))
  
  for (i in seq_along(page_matches)) {
    id_match <- regexpr("id\\s*=\\s*([a-zA-Z0-9_]+)", page_matches[i], perl = TRUE)
    if (id_match > 0) {
      match_text <- regmatches(page_matches[i], list(id_match))[[1]]
      extracted_ids[i] <- gsub("id\\s*=\\s*", "", match_text)
    } else {
      extracted_ids[i] <- paste("page", i)
    }
  }
  
  return(extracted_ids)
}

# Extract cleaned page content
extract_page_content <- function(raw_page_content) {
  # Find the closing :::
  closing_idx <- regexpr(":::", raw_page_content, fixed = TRUE)
  if (closing_idx > 0) {
    raw_page_content <- substr(raw_page_content, 1, closing_idx - 1)
  }
  
  # Skip the id part
  id_end_idx <- regexpr("}", raw_page_content, fixed = TRUE)
  if (id_end_idx > 0) {
    return(substr(raw_page_content, id_end_idx + 1, nchar(raw_page_content)))
  }
  
  return(raw_page_content)
}

# Extract items (questions and text) from page content
extract_page_items <- function(page_content, page_id) {
  # Find all R code blocks
  r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
  r_blocks <- gregexpr(r_block_pattern, page_content, perl = TRUE)
  
  content_items <- list()
  item_index <- 1
  
  # If no R blocks, add entire content as text
  if (r_blocks[[1]][1] == -1) {
    if (nchar(trimws(page_content)) > 0) {
      content_items[[paste0("text_", page_id, "_1")]] <- create_text_item(
        paste0("text_", page_id, "_1"),
        trimws(page_content),
        1
      )
    }
    return(content_items)
  }
  
  # Get positions and lengths of R blocks
  r_positions <- r_blocks[[1]]
  r_lengths <- attr(r_blocks[[1]], "match.length")
  r_matches <- regmatches(page_content, r_blocks)[[1]]
  
  # Create a list to track all content segments
  segments <- list()
  
  # Add text before first R block if exists
  if (r_positions[1] > 1) {
    text_content <- substr(page_content, 1, r_positions[1] - 1)
    if (nchar(trimws(text_content)) > 0) {
      segments[[length(segments) + 1]] <- list(
        type = "text",
        start = 1,
        end = r_positions[1] - 1,
        content = trimws(text_content)
      )
    }
  }
  
  # Process each R block and text between blocks
  for (j in seq_along(r_positions)) {
    # Add the R block
    block_start <- r_positions[j]
    block_end <- block_start + r_lengths[j] - 1
    r_block <- r_matches[j]
    
    # Extract code content
    code_content <- gsub("```\\{r\\}", "", r_block)
    code_content <- gsub("```$", "", code_content)
    code_content <- trimws(code_content)
    
    # Check if it's a question
    is_question <- grepl("sd_question\\s*\\(", code_content, perl = TRUE)
    
    segments[[length(segments) + 1]] <- list(
      type = if(is_question) "question" else "r_block",
      start = block_start,
      end = block_end,
      content = r_block,
      code = code_content
    )
    
    # Add text after this block if not the last block
    if (j < length(r_positions)) {
      next_start <- r_positions[j + 1]
      text_start <- block_end + 1
      text_end <- next_start - 1
      
      if (text_end >= text_start) {
        text_content <- substr(page_content, text_start, text_end)
        if (nchar(trimws(text_content)) > 0) {
          segments[[length(segments) + 1]] <- list(
            type = "text",
            start = text_start,
            end = text_end,
            content = trimws(text_content)
          )
        }
      }
    } else if (block_end < nchar(page_content)) {
      # Add text after last block
      text_content <- substr(page_content, block_end + 1, nchar(page_content))
      if (nchar(trimws(text_content)) > 0) {
        segments[[length(segments) + 1]] <- list(
          type = "text",
          start = block_end + 1,
          end = nchar(page_content),
          content = trimws(text_content)
        )
      }
    }
  }
  
  # Process segments into content items
  text_counter <- 1
  for (segment in segments) {
    if (segment$type == "text") {
      text_id <- paste0("text_", page_id, "_", text_counter)
      text_counter <- text_counter + 1
      
      content_items[[text_id]] <- create_text_item(text_id, segment$content, item_index)
      item_index <- item_index + 1
    } else if (segment$type == "question") {
      # Extract question parameters
      question_params <- extract_question_params(segment$code)
      
      if (question_params$id != "unknown") {
        content_items[[question_params$id]] <- create_question_item(
          question_params$type,
          question_params$id,
          question_params$label,
          segment$content,
          item_index
        )
        item_index <- item_index + 1
      }
    }
  }
  
  return(content_items)
}

# Create a question item
create_question_item <- function(type, id, label, raw_content, position) {
  list(
    type = type,
    id = id,
    label = label,
    raw = raw_content,
    position = position,
    is_question = TRUE
  )
}

# Create a text item
create_text_item <- function(id, content, position) {
  # Generate preview (first 5 words)
  words <- strsplit(content, "\\s+")[[1]]
  preview <- paste(head(words, 5), collapse = " ")
  if (length(words) > 5) preview <- paste0(preview, "...")
  
  list(
    id = id,
    preview = preview,
    content = content,
    position = position,
    is_question = FALSE
  )
}

# Delete a page from the survey.qmd file
delete_page_from_survey <- function(page_id, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Ensure editor_content is in lines
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match
  page_start_line <- page_start_lines[1]
  
  # Find the end of the page
  page_end_line <- NULL
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  if (is.null(page_end_line)) {
    return(NULL)
  }
  
  # Remove the page
  result <- c(
    editor_content[1:(page_start_line-1)],
    if(page_end_line < length(editor_content)) editor_content[(page_end_line+1):length(editor_content)] else NULL
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Extract question parameters
extract_question_params <- function(code_text) {
  extract_param <- function(param_name, text) {
    # Try single quotes
    pattern <- paste0(param_name, "\\s*=\\s*'([^']*)'")
    match <- regexpr(pattern, text, perl = TRUE)
    
    if (match > 0 && !is.null(attr(match, "capture.start")) && 
        attr(match, "capture.start")[1] > 0) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }
    
    # Try double quotes
    pattern <- paste0(param_name, '\\s*=\\s*"([^"]*)"')
    match <- regexpr(pattern, text, perl = TRUE)
    
    if (match > 0 && !is.null(attr(match, "capture.start")) && 
        attr(match, "capture.start")[1] > 0) {
      start_pos <- attr(match, "capture.start")[1]
      length_val <- attr(match, "capture.length")[1]
      return(substr(text, start_pos, start_pos + length_val - 1))
    }
    
    return("unknown")
  }
  
  return(list(
    type = extract_param("type", code_text),
    id = extract_param("id", code_text),
    label = extract_param("label", code_text)
  ))
}

# Process content order array
process_content_order <- function(flat_order) {
  content_list <- list()
  
  if (is.vector(flat_order) || is.list(flat_order)) {
    # Ensure we have a character vector
    if (!is.character(flat_order)) {
      flat_order <- as.character(unlist(flat_order))
    }
    
    # Process pairs of type and id
    if (length(flat_order) >= 2) {
      for (i in seq(1, length(flat_order), by = 2)) {
        if (i + 1 <= length(flat_order)) {
          content_list[[length(content_list) + 1]] <- list(
            type = flat_order[i],
            id = flat_order[i + 1]
          )
        }
      }
    } else {
      shiny::showNotification("Order data too short. Needed pairs of type/id values.", type = "error")
      return(list())
    }
  } else {
    shiny::showNotification("Invalid content order format type", type = "error")
    return(list())
  }
  
  return(content_list)
}

# Check and separate content if necessary
check_and_separate_content <- function(page_id, content_list, current_content, session) {
  page_structure <- parse_survey_structure()
  
  if (!is.null(page_structure) && !is.null(page_structure$pages) && 
      page_id %in% names(page_structure$pages)) {
    
    # Check if any dragged item has multiple sd_* calls
    needs_separation <- FALSE
    for (item in content_list) {
      if (item$type == "question" && item$id %in% names(page_structure$pages[[page_id]])) {
        question_item <- page_structure$pages[[page_id]][[item$id]]
        if ("raw" %in% names(question_item)) {
          # Count sd_ function calls in this chunk
          raw_content <- question_item$raw
          sd_calls <- gregexpr("\\bsd_[a-zA-Z0-9_]+\\s*\\(", raw_content)
          if (sd_calls[[1]][1] > 0 && length(sd_calls[[1]]) > 1) {
            needs_separation <- TRUE
            break
          }
        }
      }
    }
    
    # If we need to separate content
    if (needs_separation) {
      current_content <- r_chunk_separation(current_content)
      shinyAce::updateAceEditor(session, "survey_editor", value = current_content)
      shiny::showNotification("Separated multiple functions in dragged content", 
                            type = "message", duration = 2)
      # Brief pause to let the editor update
      Sys.sleep(0.1)
    }
  }
}

# Launch preview server
launch_preview_server <- function(port) {
  # Create a temporary R script to run the app
  temp_script <- tempfile(fileext = ".R")
  writeLines(
    paste0(
      "library(shiny)\n",
      "port <- ", port, "\n",
      "setwd('", getwd(), "')\n",
      "source('app.R')\n",
      "options(shiny.port = port)\n",
      "options(shiny.host = '127.0.0.1')\n",
      "shiny::runApp(launch.browser = FALSE)\n"
    ), 
    temp_script
  )
  
  # Run the temp script in a separate R process
  r_path <- file.path(R.home("bin"), "R")
  system2(r_path, c("--vanilla", "-f", temp_script), wait = FALSE, stdout = NULL, stderr = NULL)
}

# CSS and JavaScript ----

# Function to get the custom CSS for the studio
get_studio_css <- function() {
  return("
    .shiny-ace.ace_editor {
      margin-bottom: 0.1rem !important;
    }
    
    /* Structure styling */
    .page-header {
      background-color: #cce5ff; 
      padding: 10px; 
      border-radius: 5px; 
      margin-bottom: 10px;
      cursor: pointer;
      display: flex;
      justify-content: space-between;
      align-items: center;
    }
    
    .page-header:hover {
      background-color: #ADD8FF;
    }
    
    .page-header .toggle-icon {
      margin-left: 10px;
    }
    
    .page-header .drag-handle {
      cursor: move;
      margin-right: 10px;
      color: #777;
    }
    
    .page-header .drag-handle:hover {
      color: #333;
    }
    
    .question-item {
      margin-left: 5px; 
      margin-bottom: 10px; 
      padding: 5px 5px 5px 25px; 
      border-left: 3px solid #5bc0de; 
      background-color: #f0f0f0;
      position: relative;
    }
    
    .question-item .drag-handle {
      position: absolute;
      left: 5px;
      cursor: move;
      color: #777;
    }
    
    .question-item .drag-handle:hover {
      color: #333;
    }
    
    /* Text section styling */
    .text-item {
      margin-left: 5px; 
      margin-bottom: 10px; 
      padding: 5px 5px 5px 25px; 
      border-left: 3px solid #28a745; 
      background-color: #e3e3e3;
      position: relative;
    }
    
    .text-item .drag-handle {
      position: absolute;
      left: 5px;
      cursor: move;
      color: #777;
    }
    
    .text-item .drag-handle:hover {
      color: #333;
    }
    
    /* Ghost class for sortable.js */
    .sortable-ghost {
      opacity: 0.4;
    }
    
    /* Drop placeholder */
    .sortable-placeholder {
      background-color: #f9f9f9;
      border: 1px dashed #ccc;
      margin: 5px 0;
    }
  ")
}

# Function to get the custom JavaScript for the studio
get_studio_js <- function() {
  return("
    $(document).ready(function() {
      // Initialize toggle functionality after DOM is ready
      function initToggle() {
        $('.page-header').off('click').on('click', function(e) {
          // Don't toggle if clicking on drag handle or delete button
          if (!$(e.target).hasClass('drag-handle') && 
              !$(e.target).closest('.drag-handle').length &&
              !$(e.target).hasClass('delete-page-btn') &&
              !$(e.target).closest('.delete-page-btn').length &&
              !$(e.target).closest('.page-actions').length) {
            var $questions = $(this).next('.questions-container');
            $questions.slideToggle();
            
            // Toggle icon
            var $icon = $(this).find('.toggle-icon i');
            if ($icon.hasClass('fa-chevron-down')) {
              $icon.removeClass('fa-chevron-down').addClass('fa-chevron-right');
            } else {
              $icon.removeClass('fa-chevron-right').addClass('fa-chevron-down');
            }
          }
        });
      }
      
      // Initialize drag and drop functionality
      function initSortable() {
        // Clean up and reattach all sortable instances
        
        // Pages sortable
        if (document.getElementById('pages-container')) {
          new Sortable(document.getElementById('pages-container'), {
            animation: 150,
            handle: '.page-drag-handle',
            ghostClass: 'sortable-ghost',
            filter: '.delete-page-btn, .page-actions', // Add this line to filter out these elements
            preventOnFilter: true, // Add this line to prevent default when clicking on filtered elements
            onEnd: function(evt) {
              // Gather the new page order
              var pageOrder = [];
              $('#pages-container > div.page-wrapper').each(function() {
                pageOrder.push($(this).attr('data-page-id'));
              });
              
              // Send both the separation trigger and page order to Shiny
              Shiny.setInputValue('page_drag_completed', {
                order: pageOrder,
                timestamp: new Date().getTime() // Add timestamp to ensure the event is always triggered
              });
            }
          });
        }
        
        // Content (questions and text) sortable - one for each page
        $('.questions-container').each(function() {
          var pageId = $(this).attr('data-page-id');
          new Sortable(this, {
            animation: 150,
            handle: '.drag-handle',
            ghostClass: 'sortable-ghost',
            onEnd: function(evt) {
              // Create an array of objects
              var contentOrder = [];
              
              // Select all direct children of the container
              $(this.el).children('div').each(function() {
                var $element = $(this);
                var type = $element.attr('data-content-type');
                
                // Get the ID based on the content type
                var id;
                if (type === 'question') {
                  id = $element.attr('data-question-id');
                } else if (type === 'text') {
                  id = $element.attr('data-text-id');
                }
                
                if (id && type) {
                  contentOrder.push({
                    type: type,
                    id: id
                  });
                }
              });
              
              // Only send if we have content
              if (contentOrder.length > 0) {
                // Convert to a simple array of strings
                var serializedOrder = [];
                for (var i = 0; i < contentOrder.length; i++) {
                  serializedOrder.push(contentOrder[i].type);
                  serializedOrder.push(contentOrder[i].id);
                }
                
                // Send event to Shiny with strict string format
                Shiny.setInputValue('content_drag_completed', {
                  pageId: pageId,
                  order: serializedOrder,
                  timestamp: new Date().getTime()
                }, {priority: 'event'});
              }
            }
          });
        });
      }

      // Ensure sortable is reinitialized whenever the DOM changes
      $(document).on('shiny:value', function(event) {
        if (event.name === 'survey_structure') {
          // Short delay to ensure DOM is updated
          setTimeout(function() {
            initToggle();
            initSortable();
          }, 100);
        }
      });
      
      // Watch for changes to the structure output
      var observer = new MutationObserver(function(mutations) {
        initToggle();
        initSortable();
      });
      
      // Start observing changes to the survey structure
      var target = document.getElementById('survey_structure');
      if (target) {
        observer.observe(target, { childList: true, subtree: true });
      }
    });
  ")
}

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
  # Create the survey from the template
  sd_create_survey(path = path, template = template)
  
  # Set working directory to the survey path
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(path)
  
  # Launch the Shiny app
  shiny::shinyApp(ui = studio_ui(), server = studio_server())
}

# UI - Framework
studio_ui <- function() {
  shiny::navbarPage(
    title = "surveydown Studio",
    id = "tabset",
    theme = bslib::bs_theme(version = 5),
    
    # Construction tab (merged Structure and Code)
    ui_construction_tab(),
    
    # Preview tab
    ui_preview_tab()
  )
}

# UI - Construction tab (merged Structure and Code)
ui_construction_tab <- function() {
  shiny::tabPanel(
    "Construction",
    # Custom CSS for Ace Editor and interactive structure
    shiny::tags$head(
      shiny::tags$style(HTML("
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
      ")),
      # Include Sortable.js library
      shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.14.0/Sortable.min.js"),
      # Custom JavaScript for toggle functionality
      shiny::tags$script(HTML("
        $(document).ready(function() {
          // Initialize toggle functionality after DOM is ready
          function initToggle() {
            $('.page-header').off('click').on('click', function(e) {
              // Don't toggle if clicking on drag handle
              if (!$(e.target).hasClass('drag-handle') && 
                  !$(e.target).closest('.drag-handle').length) {
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
      "))
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

        # Add Question UI
        shiny::h5("Add Question", 
            style = "text-align: center; background-color: #ffe0b2; padding: 6px; margin-bottom: 10px; border-radius: 4px;"),
        shiny::wellPanel(
          style = "background-color: #fff3e0; border-color: #ffe0b2; padding: 0.5rem;",
          shiny::div(
            style = "overflow-y: auto; height: calc(100vh - 378px);",
            shiny::selectInput("page_for_question", "To Page:", choices = NULL),
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
            shiny::textInput("question_label", "Question Label:", placeholder = "Enter question text"),
            shiny::actionButton("add_question_btn", "Add Question", class = "btn-primary", style = "width: 100%;")
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
                  shinyAce::aceEditor(
                    outputId = "survey_editor",
                    value = readLines("survey.qmd", warn = FALSE),
                    mode = "markdown",
                    theme = "textmate",
                    height = "calc(100vh - 203px)",
                    fontSize = 14,
                    wordWrap = TRUE
                  )
                )
              ),
              shiny::tabPanel(
                "app.R",
                shiny::div(
                  style = "margin-top: 10px",
                  shinyAce::aceEditor(
                    outputId = "app_editor",
                    value = readLines("app.R", warn = FALSE),
                    mode = "r",
                    theme = "chrome",
                    height = "calc(100vh - 203px)",
                    fontSize = 14,
                    wordWrap = TRUE
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# UI - Preview tab
ui_preview_tab <- function() {
  shiny::tabPanel(
    "Preview",
    shiny::div(
      shiny::div(
        style = "height: calc(100vh - 89px); border: none;",
        shiny::uiOutput("preview_frame")
      )
    )
  )
}

# Server - Framework
studio_server <- function() {
  function(input, output, session) {
    # Structure handlers (enhanced)
    survey_structure <- server_structure_handlers(input, output, session)
    
    # Preview handlers - initialize AFTER structure handlers
    preview_handlers <- server_preview_handlers(input, output, session)
    
    # Launch preview automatically on startup
    shiny::observe({
      preview_handlers$refresh_preview()
    }, priority = 1000) # High priority ensures this runs early

    # Update page dropdown for question creation when pages change
    shiny::observe({
      page_ids <- survey_structure$get_page_ids()
      if (!is.null(page_ids) && length(page_ids) > 0) {
        shiny::updateSelectInput(session, "page_for_question", choices = page_ids)
      }
    })
    
    # Handle Add Page button
    shiny::observeEvent(input$add_page_btn, {
      page_id <- input$new_page_id
      if (is.null(page_id) || page_id == "") {
        shiny::showNotification("Please enter a page ID", type = "error")
        return()
      }
      
      # Get current editor content
      current_content <- input$survey_editor
      
      # Apply R chunk separation first
      current_content <- r_chunk_separation(current_content)
      
      # Use the page insertion function
      updated_content <- insert_page_into_survey(page_id, current_content)
      
      # Update the editor with new content
      shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
      
      # Clear the page ID field
      shiny::updateTextInput(session, "new_page_id", value = "")
      
      # Show success message
      shiny::showNotification(paste("Page", page_id, "added successfully!"), type = "message")
      
      # Refresh structure
      survey_structure$refresh()
    })
    
    # Handle Add Question button
    shiny::observeEvent(input$add_question_btn, {
      # Validate inputs
      if (is.null(input$page_for_question) || input$page_for_question == "") {
        shiny::showNotification("Please select a page", type = "error")
        return()
      }
      
      if (is.null(input$question_id) || input$question_id == "") {
        shiny::showNotification("Please enter a question ID", type = "error")
        return()
      }
      
      # Get current editor content
      current_content <- input$survey_editor
      
      # Apply R chunk separation first
      current_content <- r_chunk_separation(current_content)
      
      # Add question to the selected page in the survey.qmd file
      updated_content <- insert_question_into_survey(
        input$page_for_question,
        input$question_type,
        input$question_id,
        input$question_label,
        current_content
      )
      
      # Check if the content was updated successfully
      if (!is.null(updated_content)) {
        # Update the editor with new content
        shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
        
        # Clear fields
        shiny::updateTextInput(session, "question_id", value = "")
        shiny::updateTextInput(session, "question_label", value = "")
        
        # Show success message
        shiny::showNotification(paste("Question", input$question_id, "added to page", input$page_for_question), type = "message")
        
        # Refresh structure
        survey_structure$refresh()
      } else {
        shiny::showNotification("Failed to add question. Check page ID and try again.", type = "error")
      }
    })
    
    # Combined handler for page drag and separation
    shiny::observeEvent(input$page_drag_completed, {
      # First, perform chunk separation
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      # Update the editor with separated content if changed
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        # Allow a very brief moment for the editor to update
        Sys.sleep(0.1)
        
        # Get the updated content
        current_content <- separated_content
        
        # Show notification for separation
        shiny::showNotification("R chunks with multiple functions have been separated", 
                              type = "message", duration = 2)
      } else {
        # Use the original content if no separation occurred
        current_content <- input$survey_editor
      }
      
      # Then, handle the page reordering
      if (length(input$page_drag_completed$order) > 0) {
        updated_content <- reorder_pages(input$page_drag_completed$order, current_content)
        
        if (!is.null(updated_content)) {
          # Update the editor with the new content
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          
          # Refresh structure
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to reorder pages", type = "error")
        }
      }
    }, ignoreInit = TRUE)

    # Combined handler for content drag and separation
    shiny::observeEvent(input$content_drag_completed, {
      # Extract page ID and order
      page_id <- input$content_drag_completed$pageId
      flat_order <- input$content_drag_completed$order
      
      # First, perform chunk separation
      current_content <- input$survey_editor
      separated_content <- r_chunk_separation(current_content)
      
      # Update the editor with separated content if changed
      if (!identical(current_content, separated_content)) {
        shinyAce::updateAceEditor(session, "survey_editor", value = separated_content)
        # Allow a very brief moment for the editor to update
        Sys.sleep(0.1)
        
        # Get the updated content
        current_content <- separated_content
        
        # Show notification for separation
        shiny::showNotification("R chunks with multiple functions have been separated", 
                              type = "message", duration = 2)
      } else {
        # Use the original content if no separation occurred
        current_content <- input$survey_editor
      }
      
      # Convert flat vector to list of items
      content_list <- list()
      
      # Better format validation and conversion
      if (is.vector(flat_order) || is.list(flat_order)) {
        # Ensure we have a character vector to work with
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
          return(NULL)
        }
      } else {
        shiny::showNotification("Invalid content order format type", type = "error")
        return(NULL)
      }
      
      # Skip if no valid items
      if (length(content_list) == 0) {
        shiny::showNotification("No valid content items found", type = "error")
        return(NULL)
      }
      
      # Try to reorder content with error handling
      tryCatch({
        updated_content <- reorder_page_content(page_id, content_list, current_content)
        
        if (!is.null(updated_content)) {
          # Update the editor with the new content
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          
          # Show success message
          shiny::showNotification(paste("Content in page", page_id, "reordered successfully!"), type = "message")
          
          # Refresh structure
          survey_structure$refresh()
          
          # Force a slight delay before allowing another drag
          shiny::invalidateLater(300)
        } else {
          shiny::showNotification(paste("Failed to reorder content in page", page_id), type = "error")
        }
      }, error = function(e) {
        # Show detailed error notification
        shiny::showNotification(paste("Error:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    # Handle page reordering
    shiny::observeEvent(input$page_order, {
      if (length(input$page_order) > 0) {
        # Get current editor content AFTER it's been updated by the chunk separation
        current_content <- input$survey_editor
        
        # Update the pages order in the file
        updated_content <- reorder_pages(input$page_order, current_content)
        
        if (!is.null(updated_content)) {
          # Update the editor with the new content
          shinyAce::updateAceEditor(session, "survey_editor", value = updated_content)
          
          # Refresh structure
          survey_structure$refresh()
        } else {
          shiny::showNotification("Failed to reorder pages", type = "error")
        }
      }
    }, ignoreInit = TRUE, priority = 100) # Lower priority than separation handlers
    
    # Clean up when session ends
    session$onSessionEnded(function() {
      # Safely access the preview process
      process <- preview_handlers$preview_process()
      if (!is.null(process)) {
        try(tools::pskill(process), silent = TRUE)
      }
    })
  }
}

# Server - File handlers
server_file_handlers <- function(input, output, session) {
  # Create reactive values to track the last saved content and save timing
  last_saved_survey <- shiny::reactiveVal("")
  last_saved_app <- shiny::reactiveVal("")
  last_survey_save_time <- shiny::reactiveVal(Sys.time())
  last_app_save_time <- shiny::reactiveVal(Sys.time())
  
  # Initialize with current file contents
  if (file.exists("survey.qmd")) {
    content <- paste(readLines("survey.qmd", warn = FALSE), collapse = "\n")
    last_saved_survey(content)
  }
  
  if (file.exists("app.R")) {
    content <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
    last_saved_app(content)
  }
  
  # Auto-save survey.qmd with debounce
  shiny::observe({
    # Only proceed if the editor content exists
    if (is.null(input$survey_editor)) return()
    
    # Get current time
    current_time <- Sys.time()
    
    # Get last save time
    last_save <- last_survey_save_time()
    
    # Check if content changed from last saved version
    if (input$survey_editor != last_saved_survey()) {
      # If sufficient time has passed since last save (3 seconds)
      if (difftime(current_time, last_save, units = "secs") > 3) {
        # Save the file
        tryCatch({
          writeLines(input$survey_editor, "survey.qmd")
          last_saved_survey(input$survey_editor)
          last_survey_save_time(current_time)
          shiny::showNotification("survey.qmd auto-saved", type = "message", duration = 1)
        }, error = function(e) {
          shiny::showNotification(paste("Error saving survey file:", e$message), type = "error")
        })
      } else {
        # Schedule check again after the remainder of the debounce period
        shiny::invalidateLater(1000)
      }
    }
  })
  
  # Auto-save app.R with debounce
  shiny::observe({
    # Only proceed if the editor content exists
    if (is.null(input$app_editor)) return()
    
    # Get current time
    current_time <- Sys.time()
    
    # Get last save time
    last_save <- last_app_save_time()
    
    # Check if content changed from last saved version
    if (input$app_editor != last_saved_app()) {
      # If sufficient time has passed since last save (3 seconds)
      if (difftime(current_time, last_save, units = "secs") > 3) {
        # Save the file
        tryCatch({
          writeLines(input$app_editor, "app.R")
          last_saved_app(input$app_editor)
          last_app_save_time(current_time)
          shiny::showNotification("app.R auto-saved", type = "message", duration = 1)
        }, error = function(e) {
          shiny::showNotification(paste("Error saving app file:", e$message), type = "error")
        })
      } else {
        # Schedule check again after the remainder of the debounce period
        shiny::invalidateLater(1000)
      }
    }
  })
}

# Server - Structure handlers
server_structure_handlers <- function(input, output, session) {
  # Create a reactive value to track survey structure changes
  structure_trigger <- shiny::reactiveVal(0)
  
  # Render the survey structure with toggle and drag functionality
  output$survey_structure <- shiny::renderUI({
    # Re-render when the structure_trigger changes
    structure_trigger()
    
    # Also re-render when the editor content changes
    if (exists("input") && !is.null(input$survey_editor)) {
      # This ensures we're always using the latest content
      # Even if not saved yet
    }
    
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
    
    # Create the structure visualization with toggle and drag features
    shiny::div(
      id = "pages-container",
      lapply(survey_structure$page_ids, function(page_id) {
        page_items <- survey_structure$pages[[page_id]]
        
        # Sort items by their position
        if (length(page_items) > 0) {
          # Get positions for sorting
          positions <- sapply(page_items, function(item) item$position)
          # Sort the page_items by position
          sorted_items <- page_items[order(positions)]
        } else {
          sorted_items <- list()
        }
        
        # Create each page with its content items
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
            shiny::div(
              class = "toggle-icon",
              shiny::icon("chevron-down")
            )
          ),
          
          # Content container - initially visible
          shiny::div(
            class = "questions-container",
            id = paste0("page-", page_id, "-content"),
            `data-page-id` = page_id,
            style = "display: block;",
            
            # Add content items in their original order
            if (length(sorted_items) > 0) {
              lapply(names(sorted_items), function(item_id) {
                item <- sorted_items[[item_id]]
                
                if (item$is_question) {
                  # It's a question
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
                  # It's a text section
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
              })
            }
          )
        )
      })
    )
  })
  
  # Function to refresh the structure
  refresh_structure <- function() {
    # Increment the trigger to force re-execution of renderUI
    structure_trigger(structure_trigger() + 1)
    
    # Add a slight delay to ensure UI updates completely
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
  
  # Automatically refresh structure when survey.qmd is saved
  shiny::observeEvent(input$save_survey, {
    refresh_structure()
  }, ignoreInit = TRUE)
  
  # Monitor editor changes with debounce
  last_update_time <- shiny::reactiveVal(Sys.time())
  
  shiny::observeEvent(input$survey_editor, {
    # Only update if at least 1 second has passed since last update
    current_time <- Sys.time()
    if (difftime(current_time, last_update_time(), units = "secs") > 1) {
      refresh_structure()
      last_update_time(current_time)
    } else {
      # Schedule an update after 1 second
      shiny::invalidateLater(1000)
    }
  }, ignoreInit = TRUE)
  
  # Return functions for external use
  list(
    refresh = refresh_structure,
    get_page_ids = get_page_ids
  )
}

# Parse survey structure from survey.qmd file
parse_survey_structure <- function() {
  # Read the survey.qmd file - use the current editor content if available
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
  # Try both patterns for page class (.sd_page and .sd-page)
  page_pattern_underscore <- ":::\\s*\\{\\s*\\.sd_page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  page_pattern_dash <- ":::\\s*\\{\\s*\\.sd-page\\s+id\\s*=\\s*([a-zA-Z0-9_]+)\\s*\\}"
  
  # Check which pattern is used in the document
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
  
  # Extract page IDs using safer extraction method
  page_ids <- page_ids[[1]]
  extracted_ids <- vector("character", length(page_ids))
  
  for (i in seq_along(page_ids)) {
    id_match <- regexpr("id\\s*=\\s*([a-zA-Z0-9_]+)", page_ids[i], perl = TRUE)
    if (id_match > 0) {
      match_text <- regmatches(page_ids[i], list(id_match))[[1]]
      extracted_ids[i] <- gsub("id\\s*=\\s*", "", match_text)
    } else {
      extracted_ids[i] <- paste("page", i)
    }
  }
  
  page_ids <- extracted_ids
  
  # Split content by pages
  split_pattern <- ":::\\s*\\{\\s*\\.sd[_-]page"
  page_splits <- strsplit(survey_content, split_pattern, perl = TRUE)[[1]]
  
  if (length(page_splits) <= 1) {
    return(list(error = "Error parsing page content!"))
  }
  
  page_splits <- page_splits[-1]  # Remove the content before the first page
  
  # Process each page to extract items in correct order
  pages <- list()
  for (i in seq_along(page_ids)) {
    page_id <- page_ids[i]
    
    # Use safer method to get page content
    if (i <= length(page_splits)) {
      raw_page_content <- page_splits[i]
      
      # Find the closing ::: for this page
      closing_idx <- regexpr(":::", raw_page_content, fixed = TRUE)
      if (closing_idx > 0) {
        raw_page_content <- substr(raw_page_content, 1, closing_idx - 1)
      }
      
      # Skip the id part at the beginning by finding the first closing brace
      id_end_idx <- regexpr("}", raw_page_content, fixed = TRUE)
      if (id_end_idx > 0) {
        page_content <- substr(raw_page_content, id_end_idx + 1, nchar(raw_page_content))
      } else {
        page_content <- raw_page_content
      }
      
      # Find all R code blocks with their exact positions
      r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
      r_blocks <- gregexpr(r_block_pattern, page_content, perl = TRUE)
      
      content_items <- list()
      item_index <- 1
      
      # If there are no R blocks, just add the entire content as a text section
      if (r_blocks[[1]][1] == -1) {
        if (nchar(trimws(page_content)) > 0) {
          text_id <- paste0("text_", page_id, "_1")
          
          # Get preview (first 5 words)
          words <- strsplit(trimws(page_content), "\\s+")[[1]]
          preview <- paste(head(words, 5), collapse = " ")
          if (length(words) > 5) preview <- paste0(preview, "...")
          
          content_items[[text_id]] <- list(
            id = text_id,
            preview = preview,
            content = trimws(page_content),
            position = 1,
            is_question = FALSE
          )
        }
      } else {
        # Get positions and lengths of all R blocks
        r_positions <- r_blocks[[1]]
        r_lengths <- attr(r_blocks[[1]], "match.length")
        
        # Get all R blocks
        r_matches <- regmatches(page_content, r_blocks)[[1]]
        
        # Create a list to track all content segments in order
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
            
            # Get preview
            words <- strsplit(segment$content, "\\s+")[[1]]
            preview <- paste(head(words, 5), collapse = " ")
            if (length(words) > 5) preview <- paste0(preview, "...")
            
            content_items[[text_id]] <- list(
              id = text_id,
              preview = preview,
              content = segment$content,
              position = item_index,
              is_question = FALSE
            )
            item_index <- item_index + 1
          } else if (segment$type == "question") {
            # Extract question parameters
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
            
            type <- extract_param("type", segment$code)
            id <- extract_param("id", segment$code)
            label <- extract_param("label", segment$code)
            
            if (id != "unknown") {
              content_items[[id]] <- list(
                type = type,
                id = id,
                label = label,
                raw = segment$content,
                position = item_index,
                is_question = TRUE
              )
              item_index <- item_index + 1
            }
          }
        }
      }
      
      # Store the processed content items
      pages[[page_id]] <- content_items
    }
  }
  
  return(list(pages = pages, page_ids = page_ids))
}

# Function to insert a new page into the survey.qmd file using the editor content
insert_page_into_survey <- function(page_id, editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  # Convert editor_content to a character vector if it's not already
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Look for the last page closing tag
  last_page_end <- max(which(grepl(":::", editor_content, fixed = TRUE)), 0)
  
  # If no page found, insert at the end of the file
  if (last_page_end == 0) {
    last_page_end <- length(editor_content)
  }
  
  # Generate the page template
  if (page_id == "end") {
    page_template <- c(
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
    )
  } else {
    page_template <- c(
      paste0("::: {.sd_page id=", page_id, "}"),
      "",
      "```{r}",
      "sd_next()",
      "```",
      "",
      ":::",
      ""
    )
  }
  
  # Insert the page template after the last page or at the end
  result <- c(
    editor_content[1:last_page_end],
    page_template,
    if(last_page_end < length(editor_content)) editor_content[(last_page_end+1):length(editor_content)] else NULL
  )
  
  # Return the updated content
  return(paste(result, collapse = "\n"))
}

# Function to insert a question into a specific page in the survey.qmd file
insert_question_into_survey <- function(page_id, question_type, question_id, question_label, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Convert editor_content to a character vector if it's not already
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find the page where we want to insert the question
  page_start_pattern <- paste0("::: \\{.sd[_-]page id=", page_id, "\\}")
  page_start_lines <- grep(page_start_pattern, editor_content, perl = TRUE)
  
  if (length(page_start_lines) == 0) {
    return(NULL)
  }
  
  # Use the first match if multiple pages with same ID
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
  
  # Find the next button chunk if it exists
  next_button_start <- NULL
  
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
          next_button_start <- chunk_start
          break
        }
      }
    }
  }
  
  # If no next button found, use a default insertion point before the page end
  insertion_point <- ifelse(!is.null(next_button_start), next_button_start, page_end_line)
  
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

# Function to reorder pages in the survey.qmd file
reorder_pages <- function(new_order, editor_content) {
  if (is.null(editor_content) || length(new_order) == 0) {
    return(NULL)
  }
  
  # Convert editor_content to a character vector if it's not already
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
    
    # Find the page end (the next ::: after the start)
    for (i in page_start_line:length(editor_content)) {
      if (i > page_start_line && grepl("^:::$", editor_content[i])) {
        page_end_line <- i
        break
      }
    }
    
    if (!is.null(page_end_line)) {
      # Store the page block (including the closing tag)
      page_blocks[[page_id]] <- list(
        start = page_start_line,
        end = page_end_line,
        content = editor_content[page_start_line:page_end_line]
      )
    }
  }
  
  # If we couldn't find all pages, return NULL
  if (length(page_blocks) != length(new_order)) {
    return(NULL)
  }
  
  # Find the position where pages start
  first_page_start <- min(sapply(page_blocks, function(block) block$start))
  
  # Find the position where pages end
  last_page_end <- max(sapply(page_blocks, function(block) block$end))
  
  # Create the new content with reordered pages
  new_content <- c(
    # Content before the first page
    editor_content[1:(first_page_start-1)]
  )
  
  # Add each page in the new order
  for (page_id in new_order) {
    new_content <- c(
      new_content,
      page_blocks[[page_id]]$content
    )
  }
  
  # Add content after the last page
  if (last_page_end < length(editor_content)) {
    new_content <- c(
      new_content,
      editor_content[(last_page_end+1):length(editor_content)]
    )
  }
  
  # Return the updated content
  return(paste(new_content, collapse = "\n"))
}

# Separate multiple sd_* function calls into individual R chunks
r_chunk_separation <- function(editor_content) {
  # Handle NULL input
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  # Ensure editor_content is a character vector split by lines
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
        # Extract the chunk content (without the ```{r} and ```)
        chunk_content <- editor_content[(chunk_start+1):(chunk_end-1)]
        
        # Check for any sd_ function calls
        sd_start_indices <- grep("\\bsd_[a-zA-Z0-9_]+\\s*\\(", chunk_content)
        
        # If multiple sd_ calls, split the chunk
        if (length(sd_start_indices) > 1) {
          # Process each sd_ call
          for (start_idx in sd_start_indices) {
            # Find the end of this call by tracking parentheses
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
            
            # Add this call as a separate chunk to the result
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
        # If we couldn't find the end of the chunk, just add this line and continue
        result <- c(result, editor_content[i])
        i <- i + 1
      }
    } else {
      # Not an R chunk start, just add this line and continue
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

# Function to split multiple questions in a chunk into individual chunks
split_questions_in_page <- function(page_id, editor_content) {
  if (is.null(editor_content) || is.null(page_id)) {
    return(NULL)
  }
  
  # Convert editor_content to a character vector if it's not already
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
  
  # Find the next button chunk
  next_button_start <- NULL
  next_button_end <- NULL
  
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
          next_button_start <- chunk_start
          next_button_end <- chunk_end
          break
        }
      }
    }
  }
  
  # Find all R code blocks with questions, excluding the next button chunk
  r_blocks <- list()
  current_block_start <- NULL
  
  for (i in page_start_line:(ifelse(!is.null(next_button_start), next_button_start - 1, page_end_line))) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      current_block_start <- i
    } else if (!is.null(current_block_start) && grepl("^```$", editor_content[i])) {
      # Check if this block contains questions
      block_content <- editor_content[(current_block_start+1):(i-1)]
      if (any(grepl("sd_question", block_content))) {
        r_blocks[[length(r_blocks) + 1]] <- list(
          start = current_block_start,
          end = i,
          content = block_content
        )
      }
      current_block_start <- NULL
    }
  }
  
  # If no blocks with questions found, return original content
  if (length(r_blocks) == 0) {
    return(editor_content)
  }
  
  # Process each block to split questions
  modified_content <- editor_content
  offset <- 0  # Track line number changes as we modify content
  
  for (block_idx in seq_along(r_blocks)) {
    block <- r_blocks[[block_idx]]
    adjusted_start <- block$start + offset
    adjusted_end <- block$end + offset
    
    # Find all question calls in this block
    q_calls <- gregexpr("sd_question\\s*\\(", paste(block$content, collapse = "\n"), perl = TRUE)
    if (q_calls[[1]][1] > 0) {
      # Count how many questions in this block
      num_questions <- length(q_calls[[1]])
      
      if (num_questions > 1) {
        # We need to split this block
        block_content <- modified_content[(adjusted_start+1):(adjusted_end-1)]
        
        # Extract individual questions with proper parenthesis matching
        q_blocks <- list()
        q_start_indices <- grep("sd_question\\s*\\(", block_content, perl = TRUE)
        
        for (i in seq_along(q_start_indices)) {
          q_start <- q_start_indices[i]
          q_end <- NULL
          
          # Find the end of this question (matching parentheses)
          open_count <- 0
          for (j in q_start:length(block_content)) {
            line <- block_content[j]
            open_count <- open_count + sum(gregexpr("\\(", line, fixed = TRUE)[[1]] > 0)
            close_count <- sum(gregexpr("\\)", line, fixed = TRUE)[[1]] > 0)
            open_count <- open_count - close_count
            
            if (open_count <= 0) {
              q_end <- j
              break
            }
          }
          
          if (!is.null(q_end)) {
            q_blocks[[length(q_blocks) + 1]] <- block_content[q_start:q_end]
          }
        }
        
        # Create new content with individual R chunks for each question
        new_content <- character(0)
        
        # Add content before the block
        new_content <- c(new_content, modified_content[1:(adjusted_start-1)])
        
        # Add each question in its own chunk
        for (q_block in q_blocks) {
          new_content <- c(
            new_content,
            "```{r}",
            q_block,
            "```"
          )
        }
        
        # Add content after the block
        new_content <- c(new_content, modified_content[(adjusted_end+1):length(modified_content)])
        
        # Update the modified content and offset
        modified_content <- new_content
        offset <- offset + (length(q_blocks) * 2) - (adjusted_end - adjusted_start + 1)
      }
    }
  }
  
  return(paste(modified_content, collapse = "\n"))
}

# Function to reorder content (questions and text) within a page
reorder_page_content <- function(page_id, new_content_order, editor_content) {
  if (is.null(editor_content) || is.null(page_id) || length(new_content_order) == 0) {
    return(NULL)
  }
  
  # First, ensure the editor_content is properly split by lines
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
  
  # Get the current survey structure
  survey_structure <- parse_survey_structure()
  
  # Check if survey structure is valid
  if (is.null(survey_structure) || !("pages" %in% names(survey_structure))) {
    return(NULL)
  }
  
  # Check if page exists in survey structure
  if (!page_id %in% names(survey_structure$pages)) {
    return(NULL)
  }
  
  # Get page content
  page_items <- survey_structure$pages[[page_id]]
  
  # Create new page content
  new_page_content <- c(
    editor_content[page_start_line],  # Page opening tag
    ""  # Add a blank line after the opening tag
  )
  
  # Add content in the new order
  for (i in seq_along(new_content_order)) {
    item <- new_content_order[[i]]
    
    # Check if item has required fields
    if (!is.list(item) || !all(c("type", "id") %in% names(item))) {
      next  # Skip this item
    }
    
    item_id <- item$id
    item_type <- item$type
    
    # Check if item exists in page items
    if (!item_id %in% names(page_items)) {
      next  # Skip this item
    }
    
    current_item <- page_items[[item_id]]
    
    # Check if current item is valid
    if (!is.list(current_item) || !("is_question" %in% names(current_item))) {
      next  # Skip this item
    }
    
    # Add the content based on item type
    if (current_item$is_question) {
      # Add question (R chunk)
      if ("raw" %in% names(current_item)) {
        r_chunk_lines <- strsplit(current_item$raw, "\n")[[1]]
        new_page_content <- c(new_page_content, r_chunk_lines, "")
      }
    } else {
      # Add text section
      if ("content" %in% names(current_item)) {
        text_lines <- strsplit(current_item$content, "\n")[[1]]
        new_page_content <- c(new_page_content, text_lines, "")
      }
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

# Helper function to generate question code
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

# Server - Preview handlers
server_preview_handlers <- function(input, output, session) {
  # Process to run the preview app
  preview_process <- shiny::reactiveVal(NULL)
  preview_port <- stats::runif(1, 3000, 8000) |> floor()
  
  # Launch preview function
  refresh_preview <- function() {
    # Get current process value inside a reactive context
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
    
    # Launch the app in a separate R process
    shiny::showNotification("Starting preview...", type = "message")
    
    # Create a temporary R script to run the app
    temp_script <- tempfile(fileext = ".R")
    writeLines(
      paste0(
        "library(shiny)\n",
        "port <- ", preview_port, "\n",
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
    new_process <- system2(r_path, c("--vanilla", "-f", temp_script), wait = FALSE, stdout = NULL, stderr = NULL)
    
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
    
    shiny::showNotification("Preview launched successfully!", type = "message")
  }
  
  # Return the refresh function and process for cleanup
  list(
    refresh_preview = refresh_preview,
    preview_process = preview_process
  )
}

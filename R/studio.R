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
    shiny::fluidRow(
      # Left side - Controls panel from Structure tab
      shiny::column(
        width = 3,
        style = "margin-top: 15px; border-right: 1px solid #ddd; height: calc(100vh - 150px);",
        shiny::h4("Add Content"),
        
        # Add Page UI
        shiny::wellPanel(
          shiny::h5("Add Page"),
          shiny::textInput("new_page_id", "Page ID:", placeholder = "Enter page ID (e.g., welcome, questions, end)"),
          shiny::actionButton("add_page_btn", "Add Page", class = "btn-success", style = "width: 100%;")
        ),
        
        # Add Question UI
        shiny::wellPanel(
          shiny::h5("Add Question"),
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
        ),
        
        shiny::hr(),
        shiny::actionButton("refresh_structure", "Refresh Structure", class = "btn-outline-secondary", style = "width: 100%;")
      ),
      
      # Middle - Structure display from Structure tab
      shiny::column(
        width = 4,
        shiny::div(
          style = "margin-top: 15px; border-right: 1px solid #ddd; height: calc(100vh - 150px); overflow-y: auto;",
          shiny::h4("Survey Structure"),
          shiny::div(
            style = "overflow-y: auto; max-height: calc(100vh - 200px);",
            shiny::uiOutput("survey_structure")
          )
        )
      ),
      
      # Right side - Code editing from Code tab
      shiny::column(
        width = 5,
        shiny::div(
          style = "margin-top: 15px; height: calc(100vh - 150px); overflow-y: auto;",
          shiny::h4("Code"),
          shiny::tabsetPanel(
            id = "code_tabs",
            shiny::tabPanel(
              "survey.qmd",
              shiny::div(
                style = "margin-top: 15px",
                shinyAce::aceEditor(
                  outputId = "survey_editor",
                  value = readLines("survey.qmd", warn = FALSE),
                  mode = "markdown",
                  theme = "github",
                  height = "calc(100vh - 280px)",
                  fontSize = 14
                ),
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::actionButton("save_survey", "Save survey.qmd", class = "btn-primary")
                )
              )
            ),
            shiny::tabPanel(
              "app.R",
              shiny::div(
                style = "margin-top: 15px",
                shinyAce::aceEditor(
                  outputId = "app_editor",
                  value = readLines("app.R", warn = FALSE),
                  mode = "r",
                  theme = "github",
                  height = "calc(100vh - 280px)",
                  fontSize = 14
                ),
                shiny::div(
                  style = "margin-top: 10px",
                  shiny::actionButton("save_app", "Save app.R", class = "btn-primary")
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
      style = "margin-top: 15px; height: calc(100vh - 100px);",
      shiny::div(
        style = "height: calc(100vh - 90px); border: none;",
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
  # Save survey.qmd file
  shiny::observeEvent(input$save_survey, {
    tryCatch({
      writeLines(input$survey_editor, "survey.qmd")
      shiny::showNotification("Survey file saved successfully!", type = "message")
    }, error = function(e) {
      shiny::showNotification(paste("Error saving survey file:", e$message), type = "error")
    })
  })
  
  # Save app.R file
  shiny::observeEvent(input$save_app, {
    tryCatch({
      writeLines(input$app_editor, "app.R")
      shiny::showNotification("App file saved successfully!", type = "message")
    }, error = function(e) {
      shiny::showNotification(paste("Error saving app file:", e$message), type = "error")
    })
  })
}

# Server - Structure handlers
server_structure_handlers <- function(input, output, session) {
  # Create a reactive value to track survey structure changes
  structure_trigger <- shiny::reactiveVal(0)
  
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
    # We need to use both patterns to be safe
    split_pattern <- ":::\\s*\\{\\s*\\.sd[_-]page"
    page_splits <- strsplit(survey_content, split_pattern, perl = TRUE)[[1]]
    
    if (length(page_splits) <= 1) {
      return(list(error = "Error parsing page content!"))
    }
    
    page_splits <- page_splits[-1]  # Remove the content before the first page
    
    # Find end markers for pages to know where each page ends
    end_markers <- gregexpr(":::", survey_content, fixed = TRUE)
    
    # Process each page to extract questions
    pages <- list()
    for (i in seq_along(page_ids)) {
      page_id <- page_ids[i]
      
      # Use safer method to get page content
      if (i <= length(page_splits)) {
        page_content <- page_splits[i]
        
        # Find the closing ::: for this page
        closing_idx <- regexpr(":::", page_content, fixed = TRUE)
        if (closing_idx > 0) {
          page_content <- substr(page_content, 1, closing_idx - 1)
        }
        
        # Extract R code blocks which might contain questions
        r_block_pattern <- "```\\{r\\}([\\s\\S]*?)```"
        r_blocks <- gregexpr(r_block_pattern, page_content, perl = TRUE)
        
        questions_list <- list()
        
        # If we found R code blocks
        if (r_blocks[[1]][1] > 0) {
          r_block_matches <- regmatches(page_content, r_blocks)
          
          if (length(r_block_matches) > 0 && length(r_block_matches[[1]]) > 0) {
            for (r_block in r_block_matches[[1]]) {
              # Extract the code content between ```{r} and ```
              code_content <- gsub("```\\{r\\}", "", r_block)
              code_content <- gsub("```$", "", code_content)
              
              # Find sd_question calls
              question_pattern <- "sd_question\\s*\\("
              q_positions <- gregexpr(question_pattern, code_content, perl = TRUE)
              
              # Only proceed if we found any questions
              if (q_positions[[1]][1] > 0) {
                q_starts <- q_positions[[1]]
                
                for (j in seq_along(q_starts)) {
                  # Find the closing parenthesis for this question
                  q_start <- q_starts[j]
                  
                  # Extract from q_start to the end of the code block
                  q_snippet <- substr(code_content, q_start, nchar(code_content))
                  
                  # Count opening and closing parentheses to find the matching closing parenthesis
                  open_count <- 0
                  close_count <- 0
                  end_pos <- 0
                  
                  for (k in 1:nchar(q_snippet)) {
                    char <- substr(q_snippet, k, k)
                    if (char == "(") open_count <- open_count + 1
                    if (char == ")") close_count <- close_count + 1
                    
                    if (open_count > 0 && open_count == close_count) {
                      end_pos <- k
                      break
                    }
                  }
                  
                  if (end_pos > 0) {
                    q_text <- substr(q_snippet, 1, end_pos)
                    
                    # Function to safely extract parameters
                    extract_param <- function(param_name, text) {
                      # Try with single quotes
                      pattern <- paste0(param_name, "\\s*=\\s*'([^']*)'")
                      match <- regexpr(pattern, text, perl = TRUE)
                      
                      if (match > 0 && attr(match, "capture.start")[1] > 0) {
                        return(substr(text, 
                                     attr(match, "capture.start")[1], 
                                     attr(match, "capture.start")[1] + attr(match, "capture.length")[1] - 1))
                      }
                      
                      # Try with double quotes
                      pattern <- paste0(param_name, '\\s*=\\s*"([^"]*)"')
                      match <- regexpr(pattern, text, perl = TRUE)
                      
                      if (match > 0 && attr(match, "capture.start")[1] > 0) {
                        return(substr(text, 
                                     attr(match, "capture.start")[1], 
                                     attr(match, "capture.start")[1] + attr(match, "capture.length")[1] - 1))
                      }
                      
                      return("unknown")
                    }
                    
                    # Extract parameters
                    type <- extract_param("type", q_text)
                    id <- extract_param("id", q_text)
                    label <- extract_param("label", q_text)
                    
                    # Extract options for multiple choice questions
                    options_list <- NULL
                    
                    # Check if it's a multiple choice question
                    if (grepl("'mc'", q_text, fixed = TRUE) || 
                        grepl('"mc"', q_text, fixed = TRUE) ||
                        grepl("'checkbox'", q_text, fixed = TRUE) || 
                        grepl('"checkbox"', q_text, fixed = TRUE)) {
                      
                      # Check if there's an option parameter
                      if (grepl("option\\s*=", q_text)) {
                        # Find the option block
                        option_start <- regexpr("option\\s*=\\s*c\\(", q_text, perl = TRUE)
                        
                        if (option_start > 0) {
                          # Extract everything from option = c( to the end
                          option_part <- substr(q_text, option_start, nchar(q_text))
                          
                          # Count parentheses to find the matching closing parenthesis
                          open_count <- 0
                          close_count <- 0
                          option_end <- 0
                          
                          for (k in 1:nchar(option_part)) {
                            char <- substr(option_part, k, k)
                            if (char == "(") open_count <- open_count + 1
                            if (char == ")") close_count <- close_count + 1
                            
                            if (open_count > 0 && open_count == close_count) {
                              option_end <- k
                              break
                            }
                          }
                          
                          if (option_end > 0) {
                            # Extract the options block
                            options_block <- substr(option_part, 1, option_end)
                            
                            # Extract individual options
                            # Look for patterns like 'Label' = 'value' or "Label" = "value"
                            opt_pattern <- "(['\"])([^'\"]+)\\1\\s*=\\s*(['\"])([^'\"]+)\\3"
                            opt_matches <- gregexpr(opt_pattern, options_block, perl = TRUE)
                            
                            if (opt_matches[[1]][1] > 0) {
                              options_list <- list()
                              
                              # Get all matches
                              opt_texts <- regmatches(options_block, opt_matches)[[1]]
                              
                              for (opt_text in opt_texts) {
                                # Extract label and value using a different approach
                                label_pattern <- "(['\"])([^'\"]+)\\1"
                                label_matches <- gregexpr(label_pattern, opt_text, perl = TRUE)
                                
                                if (label_matches[[1]][1] > 0 && length(attr(label_matches[[1]], "match.length")) >= 2) {
                                  # Extract the matched strings
                                  labels <- regmatches(opt_text, label_matches)[[1]]
                                  
                                  if (length(labels) >= 2) {
                                    opt_label <- gsub("^['\"]|['\"]$", "", labels[1])
                                    opt_value <- gsub("^['\"]|['\"]$", "", labels[2])
                                    
                                    options_list <- c(options_list, list(list(
                                      label = opt_label,
                                      value = opt_value
                                    )))
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                    
                    questions_list <- c(questions_list, list(list(
                      type = type,
                      id = id,
                      label = label,
                      options = options_list
                    )))
                  }
                }
              }
            }
          }
        }
        
        pages[[page_id]] <- questions_list
      }
    }
    
    return(list(pages = pages, page_ids = page_ids))
  }
  
  # Render the survey structure
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
    
    # Create the structure visualization
    pages_ui <- lapply(survey_structure$page_ids, function(page_id) {
      questions <- survey_structure$pages[[page_id]]
      questions_ui <- lapply(questions, function(q) {
        # Create the base question info UI
        question_ui <- list(
          shiny::div(
            style = "margin-left: 20px; margin-bottom: 15px; padding: 10px; border-left: 3px solid #5bc0de; background-color: #f8f9fa;",
            shiny::div(
              style = "font-weight: bold;",
              paste0("Type: ", q$type)
            ),
            shiny::div(
              paste0("ID: ", q$id)
            ),
            shiny::div(
              paste0("Label: ", q$label)
            )
          )
        )
        
        # Add options if they exist and it's a multiple choice question
        if (!is.null(q$options) && length(q$options) > 0) {
          options_ui <- shiny::div(
            style = "margin-top: 10px; margin-left: 20px; padding: 8px; background-color: #f0f0f0; border-radius: 4px;",
            shiny::div(
              style = "font-weight: bold; margin-bottom: 5px;",
              "Options:"
            ),
            lapply(q$options, function(opt) {
              shiny::div(
                style = "margin-left: 10px; margin-bottom: 3px;",
                paste0(opt$label, " = ", opt$value)
              )
            })
          )
          
          # Append options UI to question UI
          question_ui[[1]] <- shiny::tagAppendChild(question_ui[[1]], options_ui)
        }
        
        return(question_ui[[1]])
      })
      
      shiny::div(
        style = "margin-bottom: 30px;",
        shiny::div(
          style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          shiny::h4(paste0("Page: ", page_id)),
          shiny::div(
            style = "font-style: italic;",
            paste0("Questions: ", length(questions))
          )
        ),
        questions_ui
      )
    })
    
    shiny::div(
      style = "padding: 10px;",
      pages_ui
    )
  })
  
  # Function to refresh the structure
  refresh_structure <- function() {
    # Increment the trigger to force re-execution of renderUI
    structure_trigger(structure_trigger() + 1)
    if (exists("session") && !is.null(session)) {
      shiny::updateActionButton(session, "refresh_structure", label = "Structure Refreshed!")
    }
  }
  
  # Function to get page IDs for dropdowns
  get_page_ids <- function() {
    survey_structure <- parse_survey_structure()
    if (!is.null(survey_structure$error)) {
      return(NULL)
    }
    return(survey_structure$page_ids)
  }
  
  # Refresh structure when button is clicked
  shiny::observeEvent(input$refresh_structure, {
    refresh_structure()
  })
  
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

# Function to insert a new page into the survey.qmd file using the editor content
insert_page_into_survey <- function(page_id, editor_content) {
  if (is.null(editor_content)) {
    return(NULL)
  }
  
  # Convert editor_content to a character vector if it's not already
  if (is.character(editor_content) && length(editor_content) == 1) {
    editor_content <- strsplit(editor_content, "\n")[[1]]
  }
  
  # Find an appropriate place to insert the new page
  # Typically, we insert after the last page or at the end of the file
  
  # Look for the last page closing tag
  last_page_end <- max(which(grepl(":::", editor_content, fixed = TRUE)), 0)
  
  # If no page found, insert at the end of the file
  if (last_page_end == 0) {
    last_page_end <- length(editor_content)
  }
  
  # Generate the page template
  if (page_id == "end") {
    page_template <- c(
      "",
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
      "",
      paste0("::: {.sd_page id=", page_id, "}"),
      "",
      "Add page contents...",
      "",
      "```{r}",
      "# Insert questions...",
      "",
      "# Next button",
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
  
  # Use the first match if multiple pages with same ID (shouldn't happen but just in case)
  page_start_line <- page_start_lines[1]
  
  # Find the R code block within this page where we should insert the question
  r_block_start <- NULL
  r_block_end <- NULL
  page_end_line <- NULL
  
  for (i in page_start_line:length(editor_content)) {
    if (grepl("^```\\{r\\}", editor_content[i])) {
      r_block_start <- i
    } else if (!is.null(r_block_start) && grepl("^```$", editor_content[i])) {
      r_block_end <- i
      break
    } else if (grepl("^:::$", editor_content[i])) {
      page_end_line <- i
      break
    }
  }
  
  # If no R block found, need to create one or return false
  if (is.null(r_block_start)) {
    if (is.null(page_end_line)) {
      return(NULL)
    }
    
    # Create an R block before the page end
    r_block_template <- c(
      "```{r}",
      "# Insert questions",
      "",
      "# Next button",
      "sd_next()",
      "```"
    )
    
    # Insert the R block
    editor_content <- c(
      editor_content[1:(page_end_line-1)],
      r_block_template,
      editor_content[page_end_line:length(editor_content)]
    )
    
    # Update the positions
    r_block_start <- page_end_line
    r_block_end <- page_end_line + length(r_block_template) - 1
    page_end_line <- page_end_line + length(r_block_template)
  }
  
  # Generate the question template based on type
  question_code <- generate_question_code(question_type, question_id, question_label)
  
  # Find an appropriate position to insert the question
  # Typically, we insert before the next button
  next_button_line <- NULL
  
  for (i in r_block_start:r_block_end) {
    if (grepl("sd_next\\(\\)", editor_content[i], perl = TRUE)) {
      next_button_line <- i
      break
    }
  }
  
  # If no next button found, insert at the end of the R block
  if (is.null(next_button_line)) {
    next_button_line <- r_block_end
  }
  
  # Insert the question code before the next button or at the end of the R block
  insertion_point <- next_button_line - 1
  
  # Insert the question code
  result <- c(
    editor_content[1:insertion_point],
    question_code,
    editor_content[(insertion_point+1):length(editor_content)]
  )
  
  # Return the updated content
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
      paste0(")"),
      ""
    ))
  } else if (type == "slider_numeric") {
    return(c(
      paste0("sd_question("),
      paste0("  type   = \"", type, "\","),
      paste0("  id     = \"", id, "\","),
      paste0("  label  = \"", label, "\","),
      paste0("  option = seq(0, 10, 1)"),
      paste0(")"),
      ""
    ))
  } else {
    # Simple questions (text, textarea, numeric, date, daterange)
    return(c(
      paste0("sd_question("),
      paste0("  type  = \"", type, "\","),
      paste0("  id    = \"", id, "\","),
      paste0("  label = \"", label, "\""),
      paste0(")"),
      ""
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

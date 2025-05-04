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
    
    # Code tab
    ui_code_tab(),
    
    # Preview tab
    ui_preview_tab()
  )
}

# UI - Code tab
ui_code_tab <- function() {
  shiny::tabPanel(
    "Code",
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Survey Files"),
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
                height = "600px",
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
                height = "600px",
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
}

# UI - Preview tab
ui_preview_tab <- function() {
  shiny::tabPanel(
    "Preview",
    shiny::div(
      style = "margin-top: 15px",
      shiny::div(
        shiny::actionButton("refresh_preview", "Refresh Preview", class = "btn-primary"),
        shiny::div(
          style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
          shiny::uiOutput("preview_status")
        ),
        shiny::div(
          style = "margin-top: 10px; height: 600px; border: 1px solid #ddd; border-radius: 5px;",
          shiny::uiOutput("preview_frame")
        )
      )
    )
  )
}

# Server - Framework
studio_server <- function() {
  function(input, output, session) {
    # Reactive value to track preview status
    preview_status <- shiny::reactiveVal("Ready")
    
    # File handlers
    server_file_handlers(input, output, session, preview_status)
    
    # Preview handlers
    preview_handlers <- server_preview_handlers(input, output, session, preview_status)
    
    # Refresh preview when button is clicked
    shiny::observeEvent(input$refresh_preview, {
      preview_handlers$refresh_preview()
    })
    
    # Refresh preview when tab is selected
    shiny::observeEvent(input$tabset, {
      if (input$tabset == "Preview") {
        preview_handlers$refresh_preview()
      }
    })
    
    # Clean up when session ends
    session$onSessionEnded(function() {
      if (!is.null(preview_handlers$preview_process())) {
        try(tools::pskill(preview_handlers$preview_process()), silent = TRUE)
      }
    })
  }
}

# Server - File handlers
server_file_handlers <- function(input, output, session, preview_status) {
  # Save survey.qmd file
  shiny::observeEvent(input$save_survey, {
    tryCatch({
      writeLines(input$survey_editor, "survey.qmd")
      preview_status("Survey file saved successfully!")
    }, error = function(e) {
      preview_status(paste("Error saving survey file:", e$message))
    })
  })
  
  # Save app.R file
  shiny::observeEvent(input$save_app, {
    tryCatch({
      writeLines(input$app_editor, "app.R")
      preview_status("App file saved successfully!")
    }, error = function(e) {
      preview_status(paste("Error saving app file:", e$message))
    })
  })
}

# Server - Preview handlers
server_preview_handlers <- function(input, output, session, preview_status) {
  # Process to run the preview app
  preview_process <- shiny::reactiveVal(NULL)
  preview_port <- stats::runif(1, 3000, 8000) |> floor()
  
  # Preview status message
  output$preview_status <- shiny::renderUI({
    status <- preview_status()
    color <- if(grepl("Error", status)) "red" else "green"
    shiny::p(status, style = paste0("color: ", color, ";"))
  })
  
  # Launch preview function
  refresh_preview <- function() {
    # Stop existing process if it exists
    if (!is.null(preview_process())) {
      try(tools::pskill(preview_process()), silent = TRUE)
      preview_process(NULL)
    }
    
    # Check if files exist
    if (!file.exists("survey.qmd") || !file.exists("app.R")) {
      preview_status("Error: survey.qmd or app.R file not found!")
      return()
    }
    
    # Try to render the survey.qmd to HTML
    tryCatch({
      # Render the Quarto document if needed
      if (!file.exists("survey.html") || 
          file.info("survey.qmd")$mtime > file.info("survey.html")$mtime) {
        preview_status("Rendering survey.qmd...")
        quarto::quarto_render("survey.qmd", quiet = TRUE)
      }
      
      # Launch the app in a separate R process
      preview_status("Starting preview...")
      
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
      system2(r_path, c("--vanilla", "-f", temp_script), wait = FALSE, stdout = NULL, stderr = NULL)
      
      # Display in iframe
      preview_url <- paste0("http://127.0.0.1:", preview_port)
      
      # Give the app a moment to start
      Sys.sleep(2)
      
      output$preview_frame <- shiny::renderUI({
        shiny::tags$iframe(
          src = preview_url,
          width = "100%",
          height = "100%",
          style = "border: none;"
        )
      })
      
      preview_status("Preview launched successfully!")
    }, error = function(e) {
      preview_status(paste("Error launching preview:", e$message))
    })
  }
  
  # Return the refresh function and process for cleanup
  list(
    refresh_preview = refresh_preview,
    preview_process = preview_process
  )
}

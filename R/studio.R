#' Launch Surveydown Studio
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

#' Surveydown Studio UI
#'
#' This function creates the user interface for the Surveydown Studio Shiny app.
#'
#' @return A Shiny UI object.
#' @noRd
studio_ui <- function() {
  shiny::navbarPage(
    title = "Surveydown Studio",
    id = "tabset",
    theme = bslib::bs_theme(version = 5),
    
    # Code tab
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
    ),
    
    # Preview tab
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
  )
}

#' Surveydown Studio Server
#'
#' This function creates the server logic for the Surveydown Studio Shiny app.
#'
#' @return A Shiny server function.
#' @noRd
studio_server <- function() {
  function(input, output, session) {
    # Reactive value to track preview status
    preview_status <- shiny::reactiveVal("Ready")
    
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
    
    # Preview status message
    output$preview_status <- shiny::renderUI({
      status <- preview_status()
      color <- if(grepl("Error", status)) "red" else "green"
      shiny::p(status, style = paste0("color: ", color, ";"))
    })
    
    # Preview frame
    survey_app <- shiny::reactiveVal(NULL)
    
    # Launch preview when tab is selected or refresh button is clicked
    refresh_preview <- function() {
      # Stop existing app if it exists
      if (!is.null(survey_app())) {
        try(shiny::stopApp(survey_app()), silent = TRUE)
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
        
        # Source the app.R to get app
        preview_status("Starting preview...")
        app_env <- new.env()
        source("app.R", local = app_env)
        
        if (exists("ui", app_env) && exists("server", app_env)) {
          # Launch app in a separate process
          app <- shiny::shinyApp(ui = app_env$ui, server = app_env$server)
          
          # Store app reference for later stopping
          survey_app(app)
          
          # Display in iframe
          port <- floor(stats::runif(1, 3000, 8000))
          preview_url <- paste0("http://127.0.0.1:", port)
          
          output$preview_frame <- shiny::renderUI({
            shiny::tags$iframe(
              src = preview_url,
              width = "100%",
              height = "100%",
              style = "border: none;"
            )
          })
          
          # Start app in background
          shiny::runApp(app, launch.browser = FALSE, port = port)
          preview_status("Preview launched successfully!")
        } else {
          preview_status("Error: app.R doesn't contain proper ui and server objects!")
        }
      }, error = function(e) {
        preview_status(paste("Error launching preview:", e$message))
      })
    }
    
    # Refresh preview when button is clicked
    shiny::observeEvent(input$refresh_preview, {
      refresh_preview()
    })
    
    # Refresh preview when tab is selected
    shiny::observeEvent(input$tabset, {
      if (input$tabset == "Preview") {
        refresh_preview()
      }
    })
  }
}

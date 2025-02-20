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
    message("Changes detected...rendering 'survey.qmd' file...")
    render_survey_qmd(paths, default_theme)

    # Move rendered file
    fs::file_move(paths$root_html, paths$target_html)
    message("Survey saved to ", paths$target_html, "\n")

    # Extract head content and save
    html_content <- rvest::read_html(paths$target_html)
    head_content <- extract_head_content(html_content)
    saveRDS(head_content, paths$target_head)

  } else {
    # If no changes, just load head content from '_survey/head.rds'
    head_content <- readRDS(paths$target_head)
  }

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
        shiny::tags$style(htmltools::HTML(sprintf("
            :root {
                --progress-color: %s;
            }
          ", barcolor)))
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
  if (is.null(text)) return("")

  # Convert markdown links to HTML first
  text <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)",
               '<a href="\\2">\\1</a>',
               text)

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

  # Get footer-related fields
  footer_left <- meta$`footer-left`
  footer_right <- meta$`footer-right`
  footer_center <- meta$`footer-center`
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
    footer_html <- c(footer_html,
                     sprintf('<div class="footer-left">%s</div>',
                             process_links(footer_left)))
  }

  if (!is.null(footer_center) && nchar(footer_center) > 0) {
    footer_html <- c(footer_html,
                     sprintf('<div class="footer-center">%s</div>',
                             process_links(footer_center)))
  }

  if (!is.null(footer_right) && nchar(footer_right) > 0) {
    footer_html <- c(footer_html,
                     sprintf('<div class="footer-right">%s</div>',
                             process_links(footer_right)))
  }

  # Return the final HTML
  return(paste0('<div class="footer-content">',
                paste(footer_html, collapse = ""),
                '</div>'))
}

survey_needs_updating <- function(paths) {
  # Re-render if any of the target files are missing
  targets <- c(paths$target_html, paths$target_head)
  if (any(!fs::file_exists(targets))) { return(TRUE) }

  # Re-render if '_survey/survey.html' file is out of date with 'survey.qmd' file
  time_qmd <- file.info(paths$qmd)$mtime
  time_html <- file.info(paths$target_html)$mtime

  if (time_qmd > time_html) { return(TRUE) }

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
#' @param type Specifies the type of question. Possible values are "select", "mc",
#'   "mc_multiple", "mc_buttons", "mc_multiple_buttons", "text", "textarea",
#'   "numeric", "slider", 'slider_numeric', "date", "daterange", and "matrix".
#' @param id A unique identifier for the question, which will be used as the variable name in the resulting survey data.
#' @param label Character string. The label for the UI element, which can be formatted with markdown.
#' @param cols Integer. Number of columns for the textarea input. Defaults to 80.
#' @param direction Character string. The direction for button groups ("horizontal" or "vertical"). Defaults to "horizontal".
#' @param status Character string. The status for button groups. Defaults to "default".
#' @param width Character string. The width of the UI element. Defaults to "100%".
#' @param height Character string. The height of the textarea input. Defaults to "100px".
#' @param selected Value. The selected value(s) for certain input elements.
#' @param label_select Character string. The label for the select input. Defaults to "Choose an option...".
#' @param grid Logical. Whether to show a grid for slider input. Defaults to TRUE.
#' @param individual Logical. Whether buttons in a group should be individually styled. Defaults to TRUE.
#' @param justified Logical. Whether buttons in a group should fill the width of the parent div. Defaults to FALSE.
#' @param force_edges Logical. Whether to force edges for slider input. Defaults to TRUE.
#' @param option List, or numeric vector (for numeric sliders). Options for the select, radio, checkbox, and slider inputs.
#' @param placeholder Character string. Placeholder text for text and textarea inputs.
#' @param resize Character string. Resize option for textarea input. Defaults to NULL.
#' @param row List. Used for "matrix" type questions. Contains the row labels and their corresponding IDs.
#' @param default Numeric, length 1 (for a single sided slider), or 2 for a two sided (range based) slider. 
#' Values to be used as the starting default for the slider. Defaults to the median of values.  
#' @param ... Further arguments passed to ?shiny::sliderInput. 
#' @details
#' The function supports various question types:
#' - "select": A dropdown selection
#' - "mc": Multiple choice (single selection)
#' - "mc_multiple": Multiple choice (multiple selections allowed)
#' - "mc_buttons": Multiple choice with button-style options (single selection)
#' - "mc_multiple_buttons": Multiple choice with button-style options (multiple selections allowed)
#' - "text": Single-line text input
#' - "textarea": Multi-line text input
#' - "numeric": Numeric input
#' - "slider": Slider input
#' - "slider_numeric": Extended numeric slider types  
#' - "date": Date input
#' - "daterange": Date range input
#' - "matrix": Matrix-style question with rows and columns
#'
#' For "matrix" type questions, use the `row` parameter to define the rows of
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
    type,
    id,
    label,
    cols         = "80",
    direction    = "horizontal",
    status       = "default",
    width        = "100%",
    height       = NULL,
    selected     = NULL,
    label_select = "Choose an option...",
    grid         = TRUE,
    individual   = TRUE,
    justified    = FALSE,
    force_edges  = TRUE,
    option       = NULL,
    placeholder  = NULL,
    resize       = NULL,
    row          = NULL,
    default      = NULL,
    ...
    ) {

  # Define valid question types
  valid_types <- c(
    "select", "mc", "mc_multiple", "mc_buttons", "mc_multiple_buttons", 
    "text", "textarea", "numeric", "slider", "date", "daterange", "matrix",
    "slider_numeric"
  )

  # Check if provided type is valid
  if (!type %in% valid_types) {
    stop(
      sprintf(
        "Invalid question type: '%s'. Valid types are: %s",
        type, paste(sort(valid_types), collapse = "', '")
      )
    )
  }

  output <- NULL

  # Load translations for selected label and date language option
  translations <- get_translations()
  language <- translations$language
  translations <- translations$translations

  # Check if question if answered
  js_interaction <- sprintf("Shiny.setInputValue('%s_interacted', true, {priority: 'event'});", id)

  # Create label with hidden asterisk
  label <- markdown_to_html(label)

  if (type == "select") {
    label_select <- translations[['choose_option']]

    # Add blank option for visible selected option
    option <- c("", option)
    names(option)[1] <- label_select

    output <- shiny::selectInput(
      inputId  = id,
      label    = label,
      choices  = option,
      multiple = FALSE,
      selected = FALSE
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId  = id,
      label    = label,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId  = id,
      label    = label,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_buttons") {

    output <- shinyWidgets::radioGroupButtons(
      inputId   = id,
      label     = label,
      choices   = list_name_md_to_html(option),
      direction = direction,
      selected  = character(0)
    )

    output <- shiny::tagAppendChild(output, shiny::tags$script(htmltools::HTML(sprintf("\n            $(document).on('click', '#%s .btn', function() {\n                %s\n            });\n        ", id, js_interaction))))

  } else if (type == "mc_multiple_buttons") {

    output <- shinyWidgets::checkboxGroupButtons(
      inputId    = id,
      label      = label,
      choices    = list_name_md_to_html(option),
      direction  = direction,
      individual = individual,
      justified  = FALSE
    )

    output <- shiny::tagAppendChild(output, shiny::tags$script(htmltools::HTML(sprintf("\n            $(document).on('click', '#%s .btn', function() {\n                %s\n            });\n        ",  id, js_interaction))))
  } else if (type == "text") {

    output <- shiny::textInput(
      inputId     = id,
      label       = label,
      placeholder = option
    )

  } else if (type == "textarea") {

    output <- shiny::textAreaInput(
      inputId     = id,
      label       = label,
      height      = "100px",
      cols        = cols,
      value       = NULL,
      rows        = "6",
      placeholder = placeholder,
      resize      = resize
    )

  } else if (type == "numeric") {

    output <- shiny::numericInput(
      inputId = id,
      label   = label,
      value   = NULL
    )

  } else if (grepl('slider', type)) {

    if(type == 'slider'){slider_values <- make_slider_values(option)} else {
      slider_values <- option
    }  

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      session <- shiny::getDefaultReactiveDomain()
      session$userData[[paste0(id, "_values")]] <- slider_values
    }

    if(type == 'slider'){
      
      output <- shinyWidgets::sliderTextInput(
        inputId     = id,
        label       = label,
        choices     = names(slider_values),
        selected    = selected,
        force_edges = force_edges,
        grid        = grid
      )

      } else {
        if(is.null(default)){default <- median(slider_values)} 
        output <- shiny::sliderInput(
          inputId = id,
          label   = label,
          min     = min(slider_values),
          max     = max(slider_values),
          value   = default,
          ...
        )
        
    }

    js_convert <- sprintf("\n      $(document).on('change', '#%s', function() {\n        var valueMap = %s;\n        var currentValue = $(this).val();\n        Shiny.setInputValue('%s', valueMap[currentValue]);\n      });\n    ", id, jsonlite::toJSON(as.list(slider_values)), id)
    output <- shiny::tagAppendChild(output, shiny::tags$script(htmltools::HTML(js_convert)))

  } else if (type == "date") {

    output <- shiny::dateInput(
      inputId            = id,
      label              = label,
      value              = NULL,
      min                = NULL,
      max                = NULL,
      format             = "mm/dd/yyyy",
      startview          = "month",
      weekstart          = 0,
      language           = language,
      autoclose          = TRUE,
      datesdisabled      = NULL,
      daysofweekdisabled = NULL
    )

    output <- date_interaction(output, id)

  } else if (type == "daterange") {

    output <- shiny::dateRangeInput(
      inputId   = id,
      label     = label,
      start     = NULL,
      end       = NULL,
      min       = NULL,
      max       = NULL,
      format    = "mm/dd/yyyy",
      startview = "month",
      weekstart = 0,
      language  = language,
      separator = "-",
      autoclose = TRUE
    )

    output <- date_interaction(output, id)

  } else if (type == "matrix") {

   header <- shiny::tags$tr( {
      shiny::tags$th(""),
      lapply(names(option), function(opt) shiny::tags$th(opt)))
    rows <- lapply(row, function(q_id) {
      full_id <- paste(id, q_id, sep = "_")
      shiny::tags$tr(
        shiny::tags$td(names(row)[row == q_id]),
        shiny::tags$td(
          colspan = length(option),
          sd_question(
            type = "mc",
            id = full_id,
            label = NULL,
            option = option,
            direction = "horizontal"
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
    shiny::isolate({
      output_div <- shiny::tags$div(output)
      output <- shiny::getDefaultReactiveDomain()$output
      output[[id]] <- shiny::renderUI({ output_div })
    })
  } else {
    # If not in a reactive context, just return the element
    return(output_div)
  }
}

make_slider_values <- function(labels) {
  values <- tolower(gsub("[^[:alnum:]]", "_", labels))
  values <- gsub("_{2,}", "_", values)  # Replace multiple underscores with single
  values <- gsub("^_|_$", "", values)   # Remove leading/trailing underscores
  names(values) <- labels
  return(values)
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
    output,  # The UI component (e.g., leafletOutput, plotlyOutput)
    value,   # Reactive expression that returns the value to store in the data
    height = "400px"
) {
    # Get the current shiny session
    session <- shiny::getDefaultReactiveDomain()
    if (is.null(session)) {
        stop("sd_question_widget must be called from within a Shiny reactive context")
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
    shiny::isolate({
        output_div <- shiny::tags$div(output_div)
        output <- shiny::getDefaultReactiveDomain()$output
        output[[id]] <- shiny::renderUI({ output_div })
    })

    # Observer to update the stored value when value changes
    shiny::observe({
        temp_value <- value()
        if (!is.null(temp_value)) {
            shiny::updateTextInput(session, id, value = as.character(temp_value))
        }
    })
}

date_interaction <- function(output, id) {
  js_code <- sprintf(
    "setTimeout(function() {
            $('#%s').on('change', function() {
                Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
            });
         }, 1000);",  # 1000 ms delay
    id, id
  )
  shiny::tagAppendChild(output, shiny::tags$script(htmltools::HTML(js_code)))
}

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

  button_id <- "page_id_next"  # Placeholder ID
  shiny::tagList(
    shiny::div(
      `data-next-page` = if (!is.null(next_page)) next_page else "",
      style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
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
    shiny::tags$script(htmltools::HTML("
      Shiny.addCustomMessageHandler('closeWindow', function(message) {
        window.close();
        if (!window.closed) {
          alert('Please close this tab manually to exit the survey.');
        }
      });
    "))
  )
}

#' Create a Redirect Element for 'shiny' Applications
#'
#' This function creates a UI element that redirects the user to a specified
#' URL. It can be used in both reactive and non-reactive contexts within
#' 'shiny' applications.
#'
#' @param id A character string of a unique id to be used to identify the
#'   redirect button in the survey body.
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
#' @return In a reactive context, returns a function that when called, renders
#' the redirect element. In a non-reactive context, returns the redirect
#' element directly.
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
    label  = "Click here",
    delay  = NULL,
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
        create_redirect_element(id, url, button, label, delay, newtab)
      })
    })
  } else {
    # If not in a reactive context, just return the element
    return(create_redirect_element(id, url, button, label, delay, newtab))
  }
}

# Function to create the redirect element
create_redirect_element <- function(id, url, button, label, delay, newtab = FALSE) {
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
            text_redirect, " ",
            shiny::tags$strong(id = countdown_id, delay),
            " ", text_seconds, ".",
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
      stop("sd_get_url_pars() must be called from within a Shiny reactive context")
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
#' @param type Character string. Specifies the type of output. Can be
#'   `"question"`, `"value"`, or `NULL.` If `NULL`, the function behaves like
#'   `shiny::uiOutput()`.
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
  if (is.null(type)) {
    # If only id is provided, behave like shiny::uiOutput
    return(shiny::uiOutput(id, inline = inline, ...))
  }

  if (type == "question") {
    return(make_question_container(id, shiny::uiOutput(id), width))
  }

  if (type %in% c("value", "label_option", "label_question")) {
    type_id <- paste0(id, "_", type)

    if (!display %in% c("text", "verbatim", "ui")) {
      stop("Invalid display type. Choose 'text', 'verbatim', or 'ui'.")
    }

    output <- switch(display,
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

#' Create a survey question
#'
#' This function creates various types of survey questions for use in a Surveydown survey.
#'
#' @param type Specifies the type of question. Possible values are "select", "mc", "mc_multiple", "mc_buttons", "mc_multiple_buttons", "text", "textarea", "numeric", "slider", "date", and "daterange".
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
#' @param justified Logical. Whether buttons in a group should be justified. Defaults to FALSE.
#' @param force_edges Logical. Whether to force edges for slider input. Defaults to TRUE.
#' @param option List. Options for the select, radio, checkbox, and slider inputs.
#' @param placeholder Character string. Placeholder text for text and textarea inputs.
#' @param resize Character string. Resize option for textarea input. Defaults to NULL.
#'
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
#' - "date": Date input
#' - "daterange": Date range input
#'
#' @return A Shiny UI element wrapped in a div with a custom data attribute for question ID.
#'
#' @examples
#' sd_question("text", "name", "What is your name?")
#' sd_question("mc", "color", "What is your favorite color?", option = c("Red", "Blue", "Green"))
#'
#' @export
#' @importFrom shiny selectInput radioButtons checkboxGroupInput textInput textAreaInput numericInput dateInput dateRangeInput tags
#' @importFrom shinyWidgets radioGroupButtons checkboxGroupButtons sliderTextInput
sd_question <- function(
  type,
  id,
  label,
  cols         = "80",
  direction    = "horizontal",
  status       = "default",
  width        = "100%",
  height       = "100px",
  selected     = NULL,
  label_select = "Choose an option...",
  grid         = TRUE,
  individual   = TRUE,
  justified    = FALSE,
  force_edges  = TRUE,
  option       = NULL,
  placeholder  = NULL,
  resize       = NULL
) {

  output <- NULL

  # Always add red asterisk for required questions, but hide it initially
  label <- paste0(
    label,
    " <span class='required-asterisk' style='display:none; color: red; font-size: 1.5em; vertical-align: middle; position: relative; top: 0.1em;'>*</span>"
  )

  if (type ==  "select") {
    option <- c("", option)
    names(option)[1] <- label_select

    output <- shiny::selectInput(
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      multiple = FALSE,
      selected = FALSE
    )

  } else if (type == "mc") {

    output <- shiny::radioButtons(
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_multiple") {

    output <- shiny::checkboxGroupInput(
      inputId  = id,
      label    = markdown_to_html(label),
      width    = width,
      choices  = option,
      selected = FALSE
    )

  } else if (type == "mc_buttons") {

    output <- shinyWidgets::radioGroupButtons(
      inputId  = id,
      label    = markdown_to_html(label),
      choices  = list_name_md_to_html(option),
      selected = character(0),
      width    = width,
      status   = status
    )

  } else if (type == "mc_multiple_buttons") {

    output <- shinyWidgets::checkboxGroupButtons(
      inputId    = id,
      label      = markdown_to_html(label),
      choices    = list_name_md_to_html(option),
      direction  = direction,
      individual = individual,
      justified  = justified,
      width      = width
    )

  } else if (type == "text") {

    output <- shiny::textInput(
      inputId     = id,
      label       = markdown_to_html(label),
      width       = width,
      placeholder = option
    )

  } else if (type == "textarea") {

    output <- shiny::textAreaInput(
      inputId     = id,
      label       = markdown_to_html(label),
      width       = width,
      height      = height,
      cols        = cols,
      value       = NULL,
      rows        = "6",
      placeholder = placeholder,
      resize      = resize
    )

  } else if (type == "numeric") {

    output <- shiny::numericInput(
      inputId = id,
      label   = markdown_to_html(label),
      width   = width,
      value   = NULL
    )

  } else if (type == "slider") {

    output <- shinyWidgets::sliderTextInput(
      inputId      = id,
      label        = markdown_to_html(label),
      width        = width,
      choices      = option,
      selected     = selected,
      force_edges  = force_edges,
      grid         = grid,
      animate      = FALSE,
      hide_min_max = FALSE,
      from_fixed   = FALSE,
      to_fixed     = FALSE,
      from_min     = NULL,
      from_max     = NULL,
      to_min       = NULL,
      to_max       = NULL,
      pre          = NULL,
      post         = NULL,
      dragRange    = TRUE
    )

  } else if (type == "date") {

    output <- shiny::dateInput(
      inputId            = id,
      label              = markdown_to_html(label),
      width              = width,
      value              = NULL,
      min                = NULL,
      max                = NULL,
      format             = "mm/dd/yyyy",
      startview          = "month",
      weekstart          = 0,
      language           = "en",
      autoclose          = TRUE,
      datesdisabled      = NULL,
      daysofweekdisabled = NULL
    )

  } else if (type == "daterange") {

    output <- shiny::dateRangeInput(
      inputId   = id,
      label     = markdown_to_html(label),
      width     = width,
      start     = NULL,
      end       = NULL,
      min       = NULL,
      max       = NULL,
      format    = "mm/dd/yyyy",
      startview = "month",
      weekstart = 0,
      language  = "en",
      separator = "-",
      autoclose = TRUE
    )

  }

  # Wrap the output in a div with custom data attributes
  output_div <- shiny::tags$div(
      id = paste("container-", id),
      `data-question-id` = id,
      class = "question-container",
      output
  )

  return(output_div)
}

#' Create a reactive survey question
#'
#' This function creates various types of reactive survey questions for use in a Surveydown survey.
#' It wraps the sd_question function in a renderUI call, allowing for dynamic question generation.
#'
#' @param ... Other inputs to be passed to sd_question.
#'
#' @return A renderUI function that creates the survey question.
#'
#' @examples
#' sd_question_reactive(type = "text", id = "name", label = "What is your name?")
#'
#' @export
#' @importFrom shiny observe getDefaultReactiveDomain renderUI
sd_question_reactive <- function(...) {
  args <- list(...)
  id <- args$id

  shiny::observe({
    output <- shiny::getDefaultReactiveDomain()$output
    if (!is.null(output)) {
      output[[id]] <- shiny::renderUI({
        do.call(sd_question, args)
      })
    } else {
      warning("sd_question_reactive was not called within a Shiny reactive context")
    }
  })
}

#' Create a placeholder for a reactive survey question
#'
#' This function creates a placeholder div for a reactive survey question in a Surveydown survey.
#' It's used in conjunction with sd_question_reactive to allow for dynamic question rendering.
#'
#' @param id A unique identifier for the question.
#'
#' @return A Shiny UI element that serves as a placeholder for the reactive question.
#'
#' @examples
#' sd_reactive_output("name")
#'
#' @export
#' @importFrom shiny div uiOutput
sd_reactive_output <- function(id) {
  shiny::div(
    id = paste0("placeholder-", id),
    `data-question-id` = id,
    class = "question-container reactive-question-placeholder",
    shiny::uiOutput(id)
  )
}

#' Create a 'Next' Button for Page Navigation
#'
#' This function creates a 'Next' button for navigating to the specified next page in a Surveydown survey.
#'
#' @param next_page Character string. The ID of the next page to navigate to. This parameter is required.
#' @param label Character string. The label of the 'Next' button. Defaults to "Next".
#'
#' @details The function generates a Shiny action button that, when clicked, sets the input value
#'   to the specified next page ID, facilitating page navigation within the Shiny application.
#'   The button is styled to appear centered on the page.
#'
#' @return A Shiny action button UI element.
#'
#' @examples
#' sd_next("page2", "Continue to Next Section")
#'
#' @export
#' @importFrom shiny actionButton
sd_next <- function(next_page = NULL, label = "Next") {
  if (is.null(next_page)) {
    stop("You must specify the current_page for the 'Next' button.")
  }

  shiny::actionButton(
    inputId = make_next_button_id(next_page),
    label   = label,
    style   = "display: block; margin: auto;",
    onclick = sprintf("Shiny.setInputValue('next_page', '%s');", next_page)
  )
}

#' Generate Next Button ID
#'
#' This internal function generates a unique ID for the 'Next' button based on the next page ID.
#'
#' @param next_page Character string. The ID of the next page.
#'
#' @return A character string representing the button ID.
#'
#' @keywords internal
make_next_button_id <- function(next_page) {
  return(paste0("next-", next_page))
}

#' Create a survey question
#'
#' This function creates various types of survey questions.
#'
#' @param type Specifies the type of question. Possible values are "select", "mc", "mc_multiple", "mc_buttons", "mc_multiple_buttons", "text", "textarea", "numeric", "slider", "date", and "daterange". See "Details" section for more about each type.
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
#' @param required Logical. Whether the input is required. Defaults to FALSE.
#' @param resize Character string. Resize option for textarea input. Defaults to NULL.
#' @details
#' # Foo
#' @return A Shiny UI element wrapped in a div with a custom data attribute for question ID.
#'
#' @examples
#' # Foo
#' @export
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
  required     = FALSE,
  resize       = NULL
) {

  output <- NULL

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

  # Wrap the output in a div with a custom data attribute to facilitate
  # question_id scraping later
  output_div <- shiny::tags$div(
    id = paste("container-", id),
    `data-question-id` = id,   # Custom attribute to identify the question_id
    class = "question-container", # Additional CSS class for styling or scripts
    output
  )

  return(output_div)

}

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

make_next_button_id <- function(next_page) {
  return(paste0("next-", next_page))
}

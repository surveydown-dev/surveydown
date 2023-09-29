# Most of this code was modified from https://github.com/jdtrat/shinysurveys

#' Parse survey questions from a YAML file and return a data frame
#'
#' This function takes a YAML file containing survey questions and
#' returns a data frame with a row for each question and columns for
#' question attributes.
#'
#' @param name Input name
#' @param type Input name
#' @param required Is question required? Defaults to `FALSE`
#' @param label Label shown on question.
#' @param option Question options (e.g. for multiple choice questions)
#' @param dependence Defaults to `NULL`
#' @param dependence_value Defaults to `NULL`
#'
#' @return A shiny input.
#' @export
question <- function(
  name,
  type,
  required = FALSE,
  label    = "label",
  option   = NULL,
  dependence = NULL,
  dependence_value = NULL
) {

  if (type ==  "select") {

    output <-
      shiny::selectizeInput(
        inputId = name,
        label = label,
        choices = option,
        options = list(
          placeholder = '',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )

  } else if (type == "numeric") {

    output <-
      numberInput(
        inputId = name,
        label = addRequiredUI_internal(label, required),
        placeholder = option
      )

  } else if (type == "mc") {

    output <-
      shiny::radioButtons(
        inputId = name,
        label = addRequiredUI_internal(label, required),
        selected = base::character(0),
        choices = option
      )

  } else if (type == "text") {

    output <-
      shiny::textInput(
        inputId = name,
          label = addRequiredUI_internal(label, required),
          placeholder = option
    )

  } else if (inputType == "matrix") {

    output <-
      radioMatrixInput(
        inputId = name,
        responseItems = label,
        choices = option,
        selected = NULL,
        .required = required
      )

  }

  if (!is.null(dependence)) {
    output <- shiny::div(
      class = "questions dependence",
      id = paste0(name, "-question"),
      shiny::div(class = "question-input", output))
  } else {
    output <- shiny::div(
      class = "questions",
      id = paste0(name, "-question"),
      shiny::div(class = "question-input", output))
  }

  return(output)

}

# This function is for internal use. It will check if a question in the
# user-supplied questions dataframe is required. If so, it will add the label
# with an asterisk. If not, it will just return the label.
addRequiredUI_internal <- function(label, required) {

  if (required) {
    return(shiny::tagList(label, shiny::span("*", class = "required")))
  }
  return(label)
}

shinyInputLabel <- utils::getFromNamespace("shinyInputLabel", "shiny")

#' Create a numeric input
#'
#' Create an input control for entry of numeric values. This is identical to
#' [shiny::numericInput()] but is more flexible in **not** requiring an initial
#' value and in allowing placeholders.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value. NULL by default.
#' @param width The width of the input, e.g. `'400px'`, or `'100%'`; see
#'   [validateCssUnit()].
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control. Internet Explorer 8 and 9 do not support this
#'   option.
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param step Interval to use when stepping between min and max
#'
#' @return A numeric input control that can be added to a UI definition.
#'
#' @seealso [shiny::updateNumericInput()]
#'
#' @export
#'
numberInput <- function(
  inputId, label, value = NULL, min = NA, max = NA, step = NA,
  placeholder = NULL, width = NULL
) {

  inputTag <- shiny::tags$input(
    id = inputId, type = "number",
    class = "form-control",
    placeholder = placeholder
  )

  if (!is.na(min))
    inputTag$attribs$min <- min
  if (!is.na(max))
    inputTag$attribs$max <- max
  if (!is.na(step))
    inputTag$attribs$step <- step
  if (!is.null(value))
    inputTag$attribs$value <- value

  shiny::tagList(
    shiny::div(class = "surveyNumericInput form-group shiny-input-container",
               style = htmltools::css(width = shiny::validateCssUnit(width)),
               shinyInputLabel(inputId, label), inputTag)

    )
}

#' Create the actual radio button inputs
#'
#' @param inputId This is the ID for the question to which the radio button
#'   inputs correspond
#' @param choices The choices (values) for each radio button to indicate
#' @param selected A default selected value
#'
#' @return radio button input UI
#' @keywords internal
#'
radioMatrixButtons <- function (inputId, choices, selected = NULL) {

  options <- lapply(choices, FUN = function(value) {

    inputTag <-  shiny::tags$td(
      shiny::tags$input(type = "radio", name = inputId, value = value)
    )

    if (value %in% selected) {
      inputTag$attribs$checked <- "checked"
    }

    inputTag
  })
  options
}

#' Create radio input ID
#'
#' @param .responseItem
#'
#' @return The response item title in a form appropriate for HTML IDs (and tidy data)
#' @keywords internal
#'
create_radio_input_id <- function(.responseItem) {
  gsub(" ", "_", gsub("[\\'[:punct:]]", "", tolower(.responseItem), perl = FALSE))
}

#' Create the radio matrix input's header
#'
#' @param .choices Possible choices
#' @param .required Logical: TRUE/FALSE should a required asterisk be placed on the matrix question
#'
#' @return Header for the table (matrix input)
#' @keywords internal
#'
radioMatHeader <- function(.choices, .required) {

  if (.required) {
    required_placeholder <- shiny::tags$th(class = "required", "*", style = "font-size: 18px;")
  } else {
    required_placeholder <- shiny::tags$th()
  }

  shiny::tags$tr(
    required_placeholder,
    lapply(X = .choices, function(choice) {
      shiny::tags$th(choice)
    })
  )
}


#' Create the table body
#'
#' @param .responseItems Questions to be asked (row labels)
#' @param .choices Possible choices (values for radio buttons)
#' @param .selected Initial selected value
#'
#' @return UI for the matrix input (table) body
#' @keywords internal
#'
radioBody <- function(.responseItems, .choices, .selected = NULL) {

  shiny::tags$tbody(
    lapply(
      X = .responseItems, function(item, choice, select) {
        shiny::tags$tr(class = "radio-matrix-buttons",
                id = paste0("tr-", create_radio_input_id(item)),
                shiny::tags$td(class = "radio-matrix-buttons-label",
                id = paste0("td-", create_radio_input_id(item)),
                  item
                ),
                radioMatrixButtons(inputId = create_radio_input_id(item),
                                    choices = choice,
                                    selected = select)
        )
      },
      choice = .choices,
      select = .selected
    )
  )
}

#' Create a matrix of radio buttons.
#'
#' @param inputId The input id
#' @param responseItems The questions to be asked (row labels)
#' @param choices Possible choices (column labels)
#' @param selected Initial selected value
#' @param ... Additional arguments specific to {shinysurveys} required questions.
#'
#' @return A matrix of radio buttons that can be added to a UI definition. When
#'   run in a Shiny application, this will return \code{NULL} until all possible
#'   response items have been answered, at which time a data frame with the
#'   question_id, question_type, and response, the format used in
#'   \code{\link{getSurveyData}}.
#'
#' @export
#'
radioMatrixInput <- function(inputId, responseItems, choices, selected = NULL, ...) {
  shiny::tagList(
    htmltools::htmlDependency(
      name = "radioMatrixInput",
      version = utils::packageVersion("shinysurveys"),
      package = "shinysurveys",
      src = "radioMatrixInput",
      script = "js/radioMatrixInput.js",
      stylesheet = "css/radioMatrixInput.css"
    ),
    shiny::div(class = "radioMatrixInput",
               id = inputId,
               shiny::tags$table(
                 radioMatHeader(.choices = choices, ...),
                 radioBody(.responseItems = responseItems,
                           .choices = choices,
                           .selected = selected)
               )
    )
  )
}


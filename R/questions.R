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
  # For now just a placeholder - will eventually replace with actual
  # inputs based on args.
  shiny::selectInput(inputId = name, label = label, choices = option)
}

#' Generate the UI Code for demographic questions
#'
#' @param df One element (a dataframe) in the list of unique questions.
#'
#' @keywords internal
#' @return UI Code for a Shiny App.
#'
surveyOutput_individual <- function(df) {

  inputType <- base::unique(df$input_type)

  if (length(inputType) != 1) {
    if (!"instructions" %in% inputType) {
      stop("Please double check your data frame and ensure that the input type for all questions is supported.")
    } else if ("instructions" %in% inputType) {
      instructions <- df[which(df$input_type == "instructions"), "question", drop = FALSE]$question
      instructions <- shiny::tagList(
        shiny::div(class = "question-instructions",
                   instructions)
      )

      inputType <- inputType[which(inputType != "instructions")]
      df <- df[which(df$input_type != "instructions"),]
    }
  } else if (length(inputType == 1)) {
    instructions <- NULL
  }

  if (grepl("rank_{{", inputType, perl = T)) {
    stop('Ranking input types have been superseded by the "matrix" input type.')
  }

  survey_env$current_question <- df

  if (inputType ==  "select") {
    output <-
      shiny::selectizeInput(
        inputId = base::unique(df$input_id),
        label = addRequiredUI_internal(df),
        choices = df$option,
        options = list(
          placeholder = '',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
  } else if (inputType == "numeric") {

    output <-
      numberInput(
        inputId = base::unique(df$input_id),
        label = addRequiredUI_internal(df),
        placeholder = df$option
      )

  } else if (inputType == "mc") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$input_id),
        label = addRequiredUI_internal(df),
        selected = base::character(0),
        choices = df$option
      )
  } else if (inputType == "text") {

    output <-
      shiny::textInput(inputId = base::unique(df$input_id),
                       label = addRequiredUI_internal(df),
                       placeholder = df$option)

  } else if (inputType == "y/n") {

    output <-
      shiny::radioButtons(
        inputId = base::unique(df$input_id),
        label = addRequiredUI_internal(df),
        selected = base::character(0),
        choices = df$option
      )

  } else if (inputType == "matrix") {

    required_matrix <- ifelse(all(df$required), TRUE, FALSE)

    output <-
      radioMatrixInput(
        inputId = base::unique(df$input_id),
        responseItems = base::unique(df$question),
        choices = base::unique(df$option),
        selected = NULL,
        .required = required_matrix
      )

  } else if (inputType == "instructions") {

    output <- shiny::div(
      class = "instructions-only",
      shiny::markdown(df$question)
    )

  } else if (inputType %in% survey_env$input_type) {
    output <- eval(survey_env$input_extension[[inputType]])
  } else {
    stop(paste0("Input type '", inputType, "' from the supplied data frame of questions is not recognized by {shinysurveys}.
                Did you mean to register a custom input extension with `extendInputType()`?"))
  }

  if (!base::is.na(df$dependence[1])) {
    output <- shiny::div(class = "questions dependence",
                         id = paste0(df$input_id[1], "-question"),
                         shiny::div(class = "question-input",
                                    instructions,
                                    output))
  } else if (base::is.na(df$dependence[1])) {
    output <- shiny::div(class = "questions",
                         id = paste0(df$input_id[1], "-question"),
                         shiny::div(class = "question-input",
                                    instructions,
                                    output))
  }

  return(output)

}


#' Generate the UI Code for demographic questions
#'
#' Create the UI code for a Shiny app based on user-supplied questions.
#'
#' @param df A user supplied data frame in the format of teaching_r_questions.
#' @param survey_title (Optional) user supplied title for the survey
#' @param survey_description (Optional) user supplied description for the survey
#' @param theme A valid R color: predefined such as "red" or "blue"; hex colors
#'   such as #63B8FF (default). To customize the survey's appearance entirely, supply NULL.
#' @param ... Additional arguments to pass into \link[shiny]{actionButton} used to submit survey responses.
#'
#' @return UI Code for a Shiny App.
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   library(shiny)
#'   library(shinysurveys)
#'
#'   df <- data.frame(question = "What is your favorite food?",
#'                    option = "Your Answer",
#'                    input_type = "text",
#'                    input_id = "favorite_food",
#'                    dependence = NA,
#'                    dependence_value = NA,
#'                    required = F)
#'
#'   ui <- fluidPage(
#'     surveyOutput(df = df,
#'                  survey_title = "Hello, World!",
#'                  theme = "#63B8FF")
#'   )
#'
#'   server <- function(input, output, session) {
#'     renderSurvey()
#'
#'     observeEvent(input$submit, {
#'       showModal(modalDialog(
#'         title = "Congrats, you completed your first shinysurvey!",
#'         "You can customize what actions happen when a user finishes a survey using input$submit."
#'       ))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }


surveyOutput <- function(df, survey_title, survey_description, theme = "#63B8FF", ...) {

  survey_env$theme <- theme
  survey_env$question_df <- df
  survey_env$unique_questions <- listUniqueQuestions(df)
  if (!missing(survey_title)) {
    survey_env$title <- survey_title
  }
  if (!missing(survey_description)) {
    survey_env$description <- survey_description
  }

  if ("page" %in% names(df)) {
    main_ui <- multipaged_ui(df = df)
  } else if (!"page" %in% names(df)) {
    main_ui <- shiny::tagList(
      check_survey_metadata(survey_title = survey_title,
                            survey_description = survey_description),
      lapply(survey_env$unique_questions, surveyOutput_individual),
      shiny::div(class = "survey-buttons",
                 shiny::actionButton("submit",
                                     "Submit",
                                     ...)
      )
    )
  }

  if (!is.null(survey_env$theme)) {
    survey_style <- sass::sass(list(
      list(color = survey_env$theme),
      readLines(
        system.file("render_survey.scss",
                    package = "shinysurveys")
      )
    ))
  } else if (is.null(survey_env$theme)) {
    survey_style <- NULL
  }


  shiny::tagList(shiny::includeScript(system.file("shinysurveys-js.js",
                                                  package = "shinysurveys")),
                 shiny::includeScript(system.file("save_data.js",
                                                  package = "shinysurveys")),
                 shiny::tags$style(shiny::HTML(survey_style)),
                 shiny::div(class = "survey",
                            shiny::div(style = "display: none !important;",
                                       shiny::textInput(inputId = "userID",
                                                        label = "Enter your username.",
                                                        value = "NO_USER_ID")),
                            main_ui))

}









shinyInputLabel <- utils::getFromNamespace("shinyInputLabel", "shiny")

#' Create a numeric input
#'
#' Create an input control for entry of numeric values. This is identical to
#' [shiny::numericInput()] but is more flexible in **not** requiring an initial
#' value and in allowing placeholders.
#'
#'
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
#' @examples
#'
#' if (interactive()) {
#' library(shiny)
#' library(shinysurveys)
#'
#' ui <- fluidPage(
#'   numberInput("obs", "Observations:", placeholder = "How many do you see?", min = 1, max = 100),
#'   verbatimTextOutput("value")
#' )
#' server <- function(input, output) {
#'   output$value <- renderText({ input$obs })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @section Server value: A numeric vector of length 1.
#'
#' @export
#'
numberInput <- function(inputId, label, value = NULL, min = NA, max = NA, step = NA,
                        placeholder = NULL, width = NULL) {

  inputTag <- shiny::tags$input(id = inputId, type = "number",
                                class = "form-control",
                                placeholder = placeholder)

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

    inputTag <-  shiny::tags$td(shiny::tags$input(type = "radio", name = inputId, value = value))

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
#' @examples
#' # For use as a normal Shiny input:
#'
#' if (interactive()) {
#'
#'   library(shiny)
#'
#'   ui <- fluidPage(
#'     radioMatrixInput("matInput",
#'                      responseItems = c("Love sushi?", "Love chocolate?"),
#'                      choices = c("Disagree", "Neutral", "Agree"))
#'   )
#'
#'   server <- function(input, output, session) {
#'     observe({
#'       print(input$matInput)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' # For use in {shinysurveys}
#'
#' if (interactive()) {
#'
#'  df <- data.frame(
#'    question = c(rep("I love sushi.", 3), rep("I love chocolate.",3),
#'    "What's your favorite food?", rep("Goat cheese is the GOAT.", 5),
#'    rep("Yogurt and berries are a great snack.",5),
#'    rep("SunButter® is a fantastic alternative to peanut butter.", 5)),
#'    option = c(rep(c("Disagree", "Neutral", "Agree"), 2), "text",
#'    rep(c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"), 3)),
#'    input_type = c(rep("matrix", 6), "text", rep("matrix", 15)),
#'    # For matrix questions, the IDs should be the same for each question
#'    # but different for each matrix input unit
#'    input_id = c(rep("matId", 6), "favorite_food", rep("matId2", 15)),
#'    dependence = NA,
#'    dependence_value = NA,
#'    required = FALSE
#'  )
#'
#'  library(shiny)
#'
#'  ui <- fluidPage(
#'    surveyOutput(df)
#'  )
#'
#'  server <- function(input, output, session) {
#'    renderSurvey()
#'    observe({
#'      print(input$matId)
#'      print(input$favorite_food)
#'      print(input$matId2)
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#'
#' }
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


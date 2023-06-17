#' Parse survey questions from an RMarkdown file
#'
#' This function reads an RMarkdown file containing YAML metadata, searches for the `questions` field,
#' and if present, uses the `parse_survey_questions()` function to parse the questions in the specified .yml file.
#'
#' @param rmd_file The path to the RMarkdown file containing the survey YAML metadata.
#' @return A list of parsed survey questions if the `questions` field is found, otherwise NULL.
#' @importFrom yaml yaml.load
#' @export
parse_questions_yaml <- function(rmd_file) {
  # Check if the file exists
  if (!file.exists(rmd_file)) {
    stop("The specified RMarkdown file does not exist.")
  }

  # Use the provided helper function to split the YAML and body contents
  rmd_split <- split_yaml_body(rmd_file)

  # Check if the YAML section is empty
  if (length(rmd_split$yaml) == 0) {
    stop("No YAML metadata found in the specified RMarkdown file.")
  }

  # Remove the YAML start and end delimiters from the extracted content
  yaml_content <- rmd_split$yaml[-c(1, length(rmd_split$yaml))]

  # Parse the YAML metadata
  yaml_parsed <- yaml::yaml.load(paste(yaml_content, collapse = "\n"))

  # Check if the `questions` field is present in the YAML metadata
  if ("questions" %in% names(yaml_parsed)) {
    questions_file <- yaml_parsed$questions

    # Check if the questions file exists
    if (!file.exists(questions_file)) {
      stop("The specified questions file does not exist.")
    }

    # Parse the questions
    survey_questions <- parse_questions_yml_file(questions_file)
    return(survey_questions)
  } else {
    warning("The `questions` field was not found in the YAML metadata.")
    return(NULL)
  }
}


#' Parse survey questions from a YAML file and return a data frame
#'
#' This function takes a YAML file containing survey questions and
#' returns a data frame with a row for each question and columns for
#' question attributes.
#'
#' @param yml_file A string specifying the path to the YAML file containing
#'   survey questions.
#'
#' @return A data frame where each row corresponds to a question and
#'   columns contain question attributes.
#' @export
#' @examples
#' # Example usage:
#' survey_questions <- parse_survey_questions("path/to/your/yml_file.yml")
parse_questions_yml_file <- function(yml_file) {
  # Read the YAML file
  yml_content <- yaml::yaml.load_file(yml_file)

  # Convert the YAML content into a data frame
  questions_df <- tibble::enframe(
    yml_content,
    name = "input_id",
    value = "attributes"
  ) %>%
  dplyr::mutate(
    question = purrr::map_chr(attributes, "text"),
    option = purrr::map(attributes, "option", .default = NA_character_),
    page = purrr::map(attributes, "page", .default = NA_character_),
    input_type = purrr::map_chr(attributes, "type"),
    dependence = purrr::map_chr(attributes, "dependence", .default = NA_character_),
    dependence_value = purrr::map_chr(attributes, "dependence_value", .default = NA_character_),
    required = purrr::map_lgl(attributes, "required", .default = FALSE)
  ) %>%
  dplyr::select(
    question, option, page, input_type, input_id,
    dependence, dependence_value, required
  ) %>%
  # dplyr::mutate(option = purrr::map(option, as.character))

  # Extract the labels and values of the options
    dplyr::mutate(
      option = purrr::map(option, function(opt) unlist(opt)),
      option = purrr::map(option, function(opt) tibble::tibble(
        value = names(opt),
        option = as.character(unname(opt))
      ))
    ) %>%

  # Unnest the options column so that each item has its own row
  tidyr::unnest(option)

  return(questions_df)
}








#
# surveyOutput <- function(df, survey_title, survey_description, theme = "#63B8FF", ...) {
#
#   survey_env$theme <- theme
#   survey_env$question_df <- df
#   survey_env$unique_questions <- listUniqueQuestions(df)
#   if (!missing(survey_title)) {
#     survey_env$title <- survey_title
#   }
#   if (!missing(survey_description)) {
#     survey_env$description <- survey_description
#   }
#
#   if ("page" %in% names(df)) {
#     main_ui <- multipaged_ui(df = df)
#   } else if (!"page" %in% names(df)) {
#     main_ui <- shiny::tagList(
#       check_survey_metadata(survey_title = survey_title,
#                             survey_description = survey_description),
#       lapply(survey_env$unique_questions, surveyOutput_individual),
#       shiny::div(class = "survey-buttons",
#                  shiny::actionButton("submit",
#                                      "Submit",
#                                      ...)
#       )
#     )
#   }
#
#   if (!is.null(survey_env$theme)) {
#     survey_style <- sass::sass(list(
#       list(color = survey_env$theme),
#       readLines(
#         system.file("render_survey.scss",
#                     package = "shinysurveys")
#       )
#     ))
#   } else if (is.null(survey_env$theme)) {
#     survey_style <- NULL
#   }
#
#
#   shiny::tagList(shiny::includeScript(system.file("shinysurveys-js.js",
#                                                   package = "shinysurveys")),
#                  shiny::includeScript(system.file("save_data.js",
#                                                   package = "shinysurveys")),
#                  shiny::tags$style(shiny::HTML(survey_style)),
#                  shiny::div(class = "survey",
#                             shiny::div(style = "display: none !important;",
#                                        shiny::textInput(inputId = "userID",
#                                                         label = "Enter your username.",
#                                                         value = "NO_USER_ID")),
#                             main_ui))
#
# }
#

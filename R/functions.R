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
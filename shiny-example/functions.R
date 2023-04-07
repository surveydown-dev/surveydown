# Load required libraries
library(yaml)
library(dplyr)

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
parse_survey_questions <- function(yml_file) {
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
    input_type = purrr::map_chr(attributes, "type"),
    dependence = purrr::map_chr(attributes, "dependence", .default = NA_character_),
    dependence_value = purrr::map_chr(attributes, "dependence_value", .default = NA_character_),
    required = purrr::map_lgl(attributes, "required", .default = FALSE)
  ) %>%
  dplyr::select(
    question, option, input_type, input_id,
    dependence, dependence_value, required
  ) %>%
  # dplyr::mutate(option = purrr::map(option, as.character))

  # Extract the labels and values of the options
    dplyr::mutate(
      option = purrr::map(option, function(opt) unlist(opt)),
      option = purrr::map(option, function(opt) tibble(
        value = names(opt),
        option = as.character(unname(opt))
      ))
    ) %>%

  # Unnest the options column so that each item has its own row
  tidyr::unnest(option)

  return(questions_df)
}

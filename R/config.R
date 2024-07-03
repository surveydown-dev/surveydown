#' Configuration Function for surveydown Surveys
#'
#' This function sets up the configuration for a surveydown survey, including
#' page and question structures, conditional display settings, and navigation options.
#'
#' @param skip_if A list of conditions under which certain pages should be skipped. Defaults to NULL.
#' @param skip_if_custom A custom function to handle conditions under which certain pages should be skipped. Defaults to NULL.
#' @param show_if A list of conditions under which certain pages should be shown. Defaults to NULL.
#' @param show_if_custom A custom function to handle conditions under which certain pages should be shown. Defaults to NULL.
#' @param preview Logical. Whether the survey is in preview mode. Defaults to FALSE.
#' @param start_page Character string. The ID of the page to start on. Defaults to NULL.
#' @param show_all_pages Logical. Whether to show all pages initially. Defaults to FALSE.
#'
#' @details The function retrieves the survey metadata, checks the validity of the conditional
#'   display settings, and ensures that the specified start page (if any) exists. It then stores
#'   these settings in a configuration list.
#'
#' @return A list containing the configuration settings for the survey, including page and question
#'   structures, conditional display settings, and navigation options.
#'
#' @examples
#' \dontrun{
#'   config <- sd_config(
#'     skip_if = list(),
#'     skip_if_custom = NULL,
#'     show_if = list(),
#'     show_if_custom = NULL,
#'     preview = FALSE,
#'     start_page = "page1",
#'     show_all_pages = FALSE
#'   )
#' }
#'
#' @export
sd_config <- function(
    skip_if = NULL,
    skip_if_custom = NULL,
    show_if = NULL,
    show_if_custom = NULL,
    preview = FALSE,
    start_page = NULL,
    show_all_pages = FALSE
) {

  # Get survey metadata

  page_structure <- get_page_structure()
  question_structure <- get_question_structure()
  config <- list(
    page_structure = page_structure,
    question_structure = question_structure,
    page_ids       = names(page_structure),
    question_ids   = unname(unlist(page_structure))
  )

  # Check skip_if and show_if inputs

  check_skip_show(config, skip_if, skip_if_custom, show_if, show_if_custom)

  # Check that start_page (if used) points to an actual page

  if (!is.null(start_page)) {
    if (! start_page %in% config$page_ids) {
      stop(
        "The specified start_page does not exist - check that you have ",
        "not mis-spelled the id"
      )
    }
  }

  if (show_all_pages) {
    for (page in config$page_ids) {
      shinyjs::show(page)
    }
  }

  # Store remaining config settings

  config$skip_if <- skip_if
  config$skip_if_custom <- skip_if_custom
  config$show_if <- show_if
  config$show_if_custom <- show_if_custom
  config$preview <- preview
  config$start_page <- start_page
  config$show_all_pages <- show_all_pages

  return(config)
}

## Page structure ----

get_page_structure <- function() {

  # Get all page nodes
  page_nodes <- get_page_nodes()
  page_ids <- page_nodes |> rvest::html_attr("id")

  # Initialize a list to hold the results
  page_structure <- list()

  # Iterate over each page node to get the question_ids
  for (i in seq_along(page_nodes)) {
    page_id <- page_ids[i]
    page_node <- page_nodes[i]

    # Extract all question IDs within this page
    question_ids <- page_node |>
      rvest::html_nodes("[data-question-id]") |>
      rvest::html_attr("data-question-id")

    # Store the question IDs for this page
    page_structure[[page_id]] <- question_ids
  }

  return(page_structure)
}

get_page_nodes <- function() {

  # Get the list of .qmd files in the current working directory
  qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)

  # Check if there is exactly one .qmd file
  if (length(qmd_files) == 1) {
    qmd_file_name <- qmd_files[1]
    html_file_name <- sub("\\.qmd$", ".html", qmd_file_name)

    # Use the derived HTML file name to read the document with rvest
    pages <- rvest::read_html(html_file_name) |>
      rvest::html_nodes(".sd-page")
    return(pages)
  }

  stop("Error: {surveydown} requires that only one .qmd file in the directory.")

}

get_question_structure <- function() {

  # Should return a list where each item name is a question_id and
  # each item value is the set of question values (options), e.g.:
  # list(
  #   penguins = c('adelie', 'chinstrap', 'gentoo', 'other'),
  #   penguins_other = "",
  #   ...
  # )

  # This can be parsed out of the rendered html file

  question_nodes <- get_question_nodes()

}

get_question_nodes <- function() {

  # similar to get_page_nodes(), but the nodes should be the question_ids
  # also can update get_page_nodes() to find the html file directly rather than
  # the qmd file.

}

## Config checks ----

check_skip_show <- function(
    config, skip_if, skip_if_custom, show_if, show_if_custom
) {

  # Placeholder for now - need to check for:
  # - If the names of skip_if and show_if are "question_id", "question_value", and "target"
  # - That the "question_id" values in both show_if and skip_if are in config$question_ids
  # - That the "target" values in show_if are in config$question_ids
  # - That the "target" values in skip_if are in config$page_ids

  # Ensure skip_if and show_if are each a tibble or data frame
  if (!is.null(skip_if)) {
    if (!is.data.frame(skip_if)) {
      stop("skip_if must be a data frame or tibble.")
    }
  }
  if (!is.null(show_if)) {
    if (!is.data.frame(show_if)) {
      stop("show_if must be a data frame or tibble.")
    }
  }

  return(TRUE)
}

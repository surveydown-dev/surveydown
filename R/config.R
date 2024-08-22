#' Configuration Function for surveydown Surveys
#'
#' This function sets up the configuration for a surveydown survey, including
#' page and question structures, conditional display settings, and navigation options.
#'
#' @param skip_if A list of conditions under which certain pages should be skipped. Defaults to NULL.
#' @param skip_if_custom A custom function to handle conditions under which certain pages should be skipped. Defaults to NULL.
#' @param show_if A list of conditions under which certain pages should be shown. Defaults to NULL.
#' @param show_if_custom A custom function to handle conditions under which certain pages should be shown. Defaults to NULL.
#' @param required_questions Vector of character strings. The IDs of questions that must
#' be answered before the respondent can continue in the survey or survey can be
#' submitted. Defaults to NULL.
#' @param all_questions_required Logical. If TRUE, all questions in the survey will be required.
#' This overrides the `required_questions` parameter. Defaults to FALSE.
#' @param start_page Character string. The ID of the page to start on. Defaults to NULL.
#' @param show_all_pages Logical. Whether to show all pages initially. Defaults to FALSE.
#' @param admin_page Logical. Whether to include an admin page for viewing and downloading survey data. Defaults to FALSE.
#'
#' @details The function retrieves the survey metadata, checks for duplicate page and question IDs,
#'   validates the conditional display settings, and ensures that the specified start page (if any) exists.
#'   It then stores these settings in a configuration list. If `admin_page` is set to TRUE, an admin page
#'   will be included in the survey. This page allows viewing and downloading of survey data upon entering
#'   the correct survey password (set using `sd_set_password()`).
#'
#'   If `all_questions_required` is set to TRUE, it will override the `required_questions` parameter
#'   and set all questions in the survey as required.
#'
#' @return A list containing the configuration settings for the survey, including:
#'   \item{page_structure}{A list containing the structure of survey pages}
#'   \item{question_structure}{A list containing the structure of survey questions}
#'   \item{page_ids}{A vector of all page IDs}
#'   \item{question_ids}{A vector of all question IDs}
#'   \item{question_values}{A vector of all possible question values}
#'   \item{question_required}{A vector of IDs for required questions}
#'   \item{skip_if_custom}{Custom skip conditions}
#'   \item{skip_if}{Standard skip conditions}
#'   \item{show_if_custom}{Custom show conditions}
#'   \item{show_if}{Standard show conditions}
#'   \item{start_page}{The ID of the starting page}
#'   \item{show_all_pages}{Whether to show all pages initially}
#'   \item{admin_page}{Whether to include an admin page}
#'
#' @examples
#' \dontrun{
#' # These examples assume you have set up a survey with appropriate .qmd files
#'
#' # Basic configuration
#' config <- sd_config()
#'
#' # Configuration with custom settings
#' config <- sd_config(
#'   start_page = "intro",
#'   all_questions_required = TRUE,
#'   show_all_pages = FALSE,
#'   admin_page = TRUE
#' )
#' }
#' @export
sd_config <- function(
    skip_if            = NULL,
    skip_if_custom     = NULL,
    show_if            = NULL,
    show_if_custom     = NULL,
    required_questions = NULL,
    all_questions_required = FALSE,
    start_page         = NULL,
    show_all_pages     = FALSE,
    admin_page         = FALSE
) {

    # Get survey metadata
    page_structure     <- get_page_structure()
    question_structure <- get_question_structure()
    page_ids           <- attr(page_structure, "all_ids")
    question_ids       <- attr(question_structure, "all_ids")

    # Check for duplicate or overlapping IDs
    check_ids(page_ids, question_ids)

    question_values    <- unname(unlist(lapply(question_structure, `[[`, "options")))
    question_required  <- question_ids
    if (! all_questions_required) {
        question_required <- required_questions
    }

    # Check skip_if and show_if inputs
    check_skip_show(question_ids, question_values, page_ids, skip_if, show_if)

    # Check that start_page (if used) points to an actual page
    if (!is.null(start_page)) {
        if (! start_page %in% page_ids) {
            stop(
                "The specified start_page does not exist - check that you have ",
                "not mis-spelled the id"
            )
        }
    }

    # Convert show_if_custom and skip_if_custom to list of lists
    show_if_custom <- convert_to_list_of_lists(show_if_custom)
    skip_if_custom <- convert_to_list_of_lists(skip_if_custom)

    # Store all config settings
    config <- list(
        page_structure     = page_structure,
        question_structure = question_structure,
        page_ids           = page_ids,
        question_ids       = question_ids,
        question_values    = question_values,
        question_required  = question_required,
        skip_if_custom     = skip_if_custom,
        skip_if            = skip_if,
        show_if_custom     = show_if_custom,
        show_if            = show_if,
        start_page         = start_page,
        show_all_pages     = show_all_pages,
        admin_page         = admin_page
    )

    return(config)
}

# Get page structure from HTML
get_page_structure <- function() {

    # Get all page nodes
    page_nodes <- get_page_nodes()
    all_page_ids <- page_nodes |> rvest::html_attr("id")

    # Initialize a list to hold the results
    page_structure <- list()

    # Iterate over each page node to get the question_ids
    for (i in seq_along(page_nodes)) {
        page_id <- all_page_ids[i]
        page_node <- page_nodes[i]

        # Extract all question IDs within this page
        question_ids <- page_node |>
            rvest::html_nodes("[data-question-id]") |>
            rvest::html_attr("data-question-id")

        # Store the question IDs for this page
        page_structure[[length(page_structure) + 1]] <- list(
            id = page_id,
            questions = question_ids
        )
    }

    attr(page_structure, "all_ids") <- all_page_ids
    return(page_structure)
}

# Get page nodes from HTML
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

# Get question structure from HTML
get_question_structure <- function() {
    question_nodes <- get_question_nodes()

    # Initialize a list to hold the results
    question_structure <- list()
    all_question_ids <- character()

    # Iterate over each question node to get the question details
    for (question_node in question_nodes) {
        question_id <- rvest::html_attr(question_node, "data-question-id")

        # Add the question ID to our list of all IDs
        all_question_ids <- c(all_question_ids, question_id)

        # Extract the options for the question
        option_nodes <- question_node |>
            rvest::html_nodes("input[type='radio']")

        options <- sapply(option_nodes, function(opt) {
            rvest::html_attr(opt, "value")
        })

        # Store the options and required status for this question
        question_structure[[length(question_structure) + 1]] <- list(
            id = question_id,
            options = options
        )
    }

    attr(question_structure, "all_ids") <- all_question_ids
    return(question_structure)
}

# Get question nodes from HTML
get_question_nodes <- function() {

    # Get the list of .qmd files in the current working directory
    qmd_files <- list.files(pattern = "\\.qmd$", full.names = TRUE)

    # Check if there is exactly one .qmd file
    if (length(qmd_files) == 1) {
        qmd_file_name <- qmd_files[1]
        html_file_name <- sub("\\.qmd$", ".html", qmd_file_name)

        # Use the derived HTML file name to read the document with rvest
        questions <- rvest::read_html(html_file_name) |>
            rvest::html_nodes("[data-question-id]")

        return(questions)
    }

    stop("Error: {surveydown} requires that only one .qmd file in the directory.")
}

check_skip_show <- function(
    question_ids, question_values, page_ids, skip_if, show_if
) {
    required_names <- c("question_id", "question_value", "target")

    if (!is.null(skip_if)) {
        if (!is.data.frame(skip_if)) {
            stop("skip_if must be a data frame or tibble.")
        }
        if (!all(required_names %in% names(skip_if))) {
            stop("skip_if must contain the columns: question_id, question_value, and target.")
        }
        if (!all(skip_if$question_id %in% question_ids)) {
            stop("All question_id values in skip_if must be valid question IDs.")
        }
        if (!all(skip_if$target %in% page_ids)) {
            stop("All target values in skip_if must be valid page IDs.")
        }
        if (!all(skip_if$question_value %in% question_values)) {
            stop("All question_value values in skip_if must be valid question values.")
        }
    }

    if (!is.null(show_if)) {
        if (!is.data.frame(show_if)) {
            stop("show_if must be a data frame or tibble.")
        }
        if (!all(required_names %in% names(show_if))) {
            stop("show_if must contain the columns: question_id, question_value, and target.")
        }
        if (!all(show_if$question_id %in% question_ids)) {
            stop("All question_id values in show_if must be valid question IDs.")
        }
        if (!all(show_if$target %in% question_ids)) {
            stop("All target values in show_if must be valid question IDs.")
        }
        if (!all(show_if$question_value %in% question_values)) {
            stop("All question_value values in show_if must be valid question values.")
        }
    }

    return(TRUE)
}

convert_to_list_of_lists <- function(tbl) {
    if (is.data.frame(tbl)) {
        return(tibble_to_list_of_lists(tbl))
    } else {
        return(tbl)
    }
}

check_ids <- function(page_ids, question_ids) {
    # Check for duplicate page IDs
    duplicate_page_ids <- page_ids[duplicated(page_ids)]
    if (length(duplicate_page_ids) > 0) {
        stop("Duplicate page IDs found: ", paste(duplicate_page_ids, collapse = ", "))
    }

    # Check for duplicate question IDs
    duplicate_question_ids <- question_ids[duplicated(question_ids)]
    if (length(duplicate_question_ids) > 0) {
        stop("Duplicate question IDs found: ", paste(duplicate_question_ids, collapse = ", "))
    }
}

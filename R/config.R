#' Configuration Function for surveydown Surveys
#'
#' This function sets up the configuration for a surveydown survey, including
#' page and question structures, conditional display settings, and navigation options.
#' It also renders the Quarto document and extracts necessary information.
#'
#' @param survey Character string. The path to the survey .qmd file or .html file. If .qmd, the file will be rendered into a .html.
#' @param skip_if A list of conditions under which certain pages should be skipped. Defaults to NULL.
#' @param skip_if_custom A custom function to handle conditions under which certain pages should be skipped. Defaults to NULL.
#' @param show_if A list of conditions under which certain pages should be shown. Defaults to NULL.
#' @param show_if_custom A custom function to handle conditions under which certain pages should be shown. Defaults to NULL.
#' @param required_questions Vector of character strings. The IDs of questions that must be answered. Defaults to NULL.
#' @param all_questions_required Logical. If TRUE, all questions in the survey will be required. Defaults to FALSE.
#' @param start_page Character string. The ID of the page to start on. Defaults to NULL.
#' @param admin_page Logical. Whether to include an admin page for viewing and downloading survey data. Defaults to FALSE.
#'
#' @return A list containing the configuration settings for the survey and rendered HTML content.
#'
#' @export
sd_config <- function(
        survey = "survey.qmd",
        skip_if = NULL,
        skip_if_custom = NULL,
        show_if = NULL,
        show_if_custom = NULL,
        required_questions = NULL,
        all_questions_required = FALSE,
        start_page = NULL,
        admin_page = FALSE
) {

    # Get the html content from the qmd file (or html if pre-rendered)
    html_content <- get_html_content(survey)

    # Extract all divs with class "sd-page"
    pages <- extract_html_pages(
        html_content, required_questions, all_questions_required,
        show_if, show_if_custom
    )

    # Extract head content (for CSS and JS)
    head_content <- html_content |>
        rvest::html_element("head") |>
        rvest::html_children() |>
        sapply(as.character) |>
        paste(collapse = "\n")

    # Extract page and question structures
    question_structure <- get_question_structure(html_content)

    page_ids <- sapply(pages, function(p) p$id)
    question_ids <- names(question_structure)

    # Check for duplicate or overlapping IDs
    check_ids(page_ids, question_ids)

    question_values <- unname(unlist(lapply(question_structure, `[[`, "options")))
    question_required <- if (all_questions_required) question_ids else required_questions

    # Check skip_if and show_if inputs
    check_skip_show(question_ids, question_values, page_ids, skip_if, show_if)

    # Check that start_page (if used) points to an actual page
    if (!is.null(start_page) && !(start_page %in% page_ids)) {
        stop("The specified start_page does not exist - check that you have not mis-spelled the id")
    }

    # Convert show_if_custom and skip_if_custom to list of lists
    show_if_custom <- convert_to_list_of_lists(show_if_custom)
    skip_if_custom <- convert_to_list_of_lists(skip_if_custom)

    # Store all config settings
    config <- list(
        question_structure = question_structure,
        page_ids = page_ids,
        question_ids = question_ids,
        question_values = question_values,
        question_required = question_required,
        skip_if_custom = skip_if_custom,
        skip_if = skip_if,
        show_if_custom = show_if_custom,
        show_if = show_if,
        start_page = start_page,
        admin_page = admin_page,
        pages = pages,
        head_content = head_content
    )

    return(config)
}

get_html_content <- function(survey) {
    # Check if the file exists
    if (!file.exists(survey)) {
        stop("The specified survey file does not exist.")
    }

    # Get the file extension
    file_ext <- tools::file_ext(survey)

    # Process based on file type
    if (file_ext == "qmd") {
        temp_html <- quarto_render_temp(survey)
        html_content <- rvest::read_html(temp_html)
        unlink(temp_html)
    } else if (file_ext == "html") {
        html_content <- rvest::read_html(survey)
    } else {
        stop("Invalid file type. Please provide either a .qmd or .html file.")
    }
    return(html_content)
}

get_show_if_targets <- function(show_if, show_if_custom) {
    show_if_targets <- character(0)
    if (!is.null(show_if)) {
        show_if_targets <- unique(show_if$target)
    }

    show_if_custom_targets <- character(0)
    if (!is.null(show_if_custom)) {
        if (is.data.frame(show_if_custom)) {
            show_if_custom_targets <- unique(show_if_custom$target)
        } else if (is.list(show_if_custom)) {
            show_if_custom_targets <- unique(sapply(show_if_custom, function(x) x$target))
        }
    }

    return(unique(c(show_if_targets, show_if_custom_targets)))
}

extract_html_pages <- function(
    html_content, required_questions, all_questions_required,
    show_if, show_if_custom
) {
    all_hidden_targets <- get_show_if_targets(show_if, show_if_custom)
    pages <- html_content |>
        rvest::html_elements(".sd-page") |>
        lapply(function(x) {
            page_id <- rvest::html_attr(x, "id")
            question_containers <- rvest::html_elements(x, ".question-container")
            question_ids <- character(0)
            required_question_ids <- character(0)

            for (i in seq_along(question_containers)) {
                container <- question_containers[[i]]
                question_id <- rvest::html_attr(container, "data-question-id")
                question_ids <- c(question_ids, question_id)
                is_required <- all_questions_required | (question_id %in% required_questions)
                if (is_required) {
                    required_question_ids <- c(required_question_ids, question_id)
                    asterisk <- rvest::html_element(container, ".required-asterisk")
                    xml2::xml_attr(asterisk, "style") <- "display:inline; color: red; font-size: 1.5em; vertical-align: middle; position: relative; top: 0.1em;"
                }

                if (question_id %in% all_hidden_targets) {
                    current_style <- xml2::xml_attr(container, "style")
                    current_style <- if (is.na(current_style)) "" else current_style
                    new_style <- paste(current_style, "display: none;", sep = " ")
                    xml2::xml_attr(container, "style") <- new_style
                }

                question_containers[[i]] <- container
            }

            # Update the 'Next' button ID and extract the next_page_id
            next_button_id <- make_next_button_id(page_id)
            next_button <- rvest::html_element(x, "#page_id_next")
            if (is.na(next_button)) {
                # No next button on this page
                next_page_id <- NULL
            } else {
                xml2::xml_attr(next_button, "id") <- next_button_id
                next_page_id <- rvest::html_attr(
                    xml2::xml_parent(next_button), "data-next-page"
                )
            }

            list(
                id = page_id,
                questions = question_ids,
                required_questions = required_question_ids,
                next_button_id = next_button_id,
                next_page_id = next_page_id,
                content = as.character(x)
            )
        })
    return(pages)
}

# Get question structure from HTML
get_question_structure <- function(html_content) {

    question_nodes <- rvest::html_nodes(html_content, "[data-question-id]")

    # Initialize a named list to hold the results
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

        # Store the options for this question in a named list
        question_structure[[question_id]] <- list(
            id = question_id,
            options = options
        )
    }

    attr(question_structure, "all_ids") <- all_question_ids
    return(question_structure)
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

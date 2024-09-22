#' Configuration Function for surveydown Surveys
#'
#' This function sets up the configuration for a surveydown survey, including
#' page and question structures, conditional display settings, and navigation options.
#' It also renders the Quarto document and extracts necessary information.
#'
#' @param use_html Logical. By default, sd_config() will render the
#' `"survey.qmd"` file when loaded, which can be slow. Users can render it
#' first into a html file and set `use_html = TRUE` to use the pre-rendered
#' file, which is faster when the app loads. Defaults to `FALSE`.
#' @param required_questions Vector of character strings. The IDs of questions that must be answered. Defaults to NULL.
#' @param all_questions_required Logical. If TRUE, all questions in the survey will be required. Defaults to FALSE.
#' @param start_page Character string. The ID of the page to start on. Defaults to NULL.
#' @param admin_page Logical. Whether to include an admin page for viewing and downloading survey data. Defaults to FALSE.
#' @param skip_if A list of conditions & page targets created using the
#' `sd_skip_if()` function defining pages to skip to if a condition is `TRUE`.
#' Defaults to `NULL`.
#' @param show_if A list of conditions & question targets created using the
#' `sd_show_if()` function defining questions to show to if a condition is `TRUE`.
#' Defaults to `NULL`.
#'
#' @return A list containing the configuration settings for the survey and rendered HTML content.
#'
#' @export
sd_config <- function(
    use_html = FALSE,
    required_questions = NULL,
    all_questions_required = FALSE,
    start_page = NULL,
    admin_page = FALSE,
    skip_if = NULL,
    show_if = NULL
) {
    # Throw error if "survey.qmd" file missing
    check_survey_file_exists()

    survey_file <- "survey.qmd"
    if (use_html) { survey_file <- "survey.html" }

    # Get the html content from the qmd file (or html if pre-rendered)
    html_content <- get_html_content(survey_file)

    # Extract all divs with class "sd-page"
    pages <- extract_html_pages(
        html_content, required_questions, all_questions_required, show_if
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

    # Store all config settings
    config <- list(
        pages = pages,
        head_content = head_content,
        page_ids = page_ids,
        question_ids = question_ids,
        question_values = question_values,
        question_required = question_required,
        start_page = start_page,
        admin_page = admin_page,
        skip_if = skip_if,
        show_if = show_if
    )

    return(config)
}

get_html_content <- function(survey_file) {
    # Check if the file exists
    if (!file.exists(survey_file)) {
        stop("The specified survey file does not exist.")
    }

    # Get the file extension
    file_ext <- tools::file_ext(survey_file)

    # Process based on file type
    if (file_ext == "qmd") {
        temp_html <- quarto_render_temp(survey_file)
        html_content <- rvest::read_html(temp_html)
        unlink(temp_html)
    } else if (file_ext == "html") {
        html_content <- rvest::read_html(survey_file)
    } else {
        stop("Invalid file type. Please provide either a .qmd or .html file.")
    }
    return(html_content)
}

#' Define skip conditions for survey pages
#'
#' @description
#' This function is used to define conditions under which certain pages in the survey should be skipped.
#' It takes one or more formulas where the left-hand side is the condition and the right-hand side is the target page ID.
#'
#' @param ... One or more formulas defining skip conditions.
#'   The left-hand side of each formula should be a condition based on input values,
#'   and the right-hand side should be the ID of the page to skip to if the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the condition and the target page ID.
#'
#' @examples
#' sd_skip_if(
#'   input$age < 18 ~ "underage_page",
#'   input$country != "USA" ~ "international_page"
#' )
#'
#' @seealso \code{\link{sd_show_if}}, \code{\link{sd_config}}
#'
#' @export
sd_skip_if <- function(...) {
    return(parse_conditions(...))
}

#' Define show conditions for survey questions
#'
#' @description
#' This function is used to define conditions under which certain questions in the survey should be shown.
#' It takes one or more formulas where the left-hand side is the condition and the right-hand side is the target question ID.
#'
#' @param ... One or more formulas defining show conditions.
#'   The left-hand side of each formula should be a condition based on input values,
#'   and the right-hand side should be the ID of the question to show if the condition is met.
#'
#' @return A list of parsed conditions, where each element contains the condition and the target question ID.
#'
#' @examples
#' sd_show_if(
#'   input$has_pets == "yes" ~ "pet_details",
#'   input$employment == "employed" ~ "job_questions"
#' )
#'
#' @seealso \code{\link{sd_skip_if}}, \code{\link{sd_config}}
#'
#' @export
sd_show_if <- function(...) {
    return(parse_conditions(...))
}

parse_conditions <- function(...) {
    conditions <- list(...)
    lapply(conditions, function(cond) {
        if (!inherits(cond, "formula")) {
            stop("Each condition must be a formula (condition ~ target)")
        }
        list(
            condition = cond[[2]],  # Left-hand side of the formula
            target = eval(cond[[3]])  # Right-hand side of the formula
        )
    })
}

get_show_if_targets <- function(show_if) {
    if (is.null(show_if)) { return(character(0)) }
    return(get_unique_targets(show_if))
}

get_unique_targets <- function(a) {
    return(unique(sapply(a, function(x) x$target)))
}

extract_html_pages <- function(
    html_content, required_questions, all_questions_required, show_if
) {
    all_hidden_targets <- get_show_if_targets(show_if)
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
    if (!is.null(skip_if)) {
        skip_if_targets <- get_unique_targets(skip_if)
        invalid_skip_targets <- setdiff(skip_if_targets, page_ids)
        if (length(invalid_skip_targets) > 0) {
            stop(sprintf(
                "Invalid skip_if targets: %s. These must be valid page IDs.",
                paste(invalid_skip_targets, collapse = ", "))
            )
        }
    }

    if (!is.null(show_if)) {
        show_if_targets <- get_unique_targets(show_if)
        invalid_show_targets <- setdiff(show_if_targets, question_ids)
        if (length(invalid_show_targets) > 0) {
            stop(sprintf(
              "Invalid show_if targets: %s. These must be valid question IDs.",
              paste(invalid_show_targets, collapse = ", "))
            )
        }
    }

    return(TRUE)
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

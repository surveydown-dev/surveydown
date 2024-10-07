run_config <- function(
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

    # Always check for sd_close() in survey.qmd
    sd_close_present <- check_sd_close("survey.qmd")

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

    # Check for duplicate, overlapping, or pre-defined IDs
    check_ids(page_ids, question_ids)

    question_values <- unname(unlist(lapply(question_structure, `[[`, "options")))
    question_required <- if (all_questions_required) question_ids else required_questions

    # Check that start_page (if used) points to an actual page
    if (!is.null(start_page) && !(start_page %in% page_ids)) {
        stop("The specified start_page does not exist - check that you have not mis-spelled the id")
    }

    # Set the start page
    if (is.null(start_page)) {
        start_page <- page_ids[1]
    }

    # Check skip_if and show_if inputs
    check_skip_show(question_ids, question_values, page_ids, skip_if, show_if)

    # Store all config settings
    config <- list(
        pages = pages,
        head_content = head_content,
        page_ids = page_ids,
        question_ids = question_ids,
        question_values = question_values,
        question_required = question_required,
        start_page = start_page,
        admin_page = admin_page
    )

    return(config)
}

check_sd_close <- function(survey_file) {
    if (!file.exists(survey_file)) {
        stop(paste("The file", survey_file, "does not exist."))
    }

    # Read the content of survey.qmd
    qmd_content <- readLines(survey_file, warn = FALSE)

    # Check for sd_close() call
    sd_close_present <- any(grepl("sd_close\\(\\)", qmd_content))

    if (!sd_close_present) {
        message("\u274C No sd_close() call found in ", survey_file, ". This may cause issues with data submission.")
    }
    return(sd_close_present)
}

get_html_content <- function(survey_file) {
    if (survey_file == 'survey.qmd') { quarto::quarto_render(survey_file) }
    return(rvest::read_html('survey.html'))
}

extract_html_pages <- function(
        html_content, required_questions, all_questions_required, show_if
) {
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

                # Track required questions and display asterisk
                if (is_required) {
                    asterisk <- rvest::html_element(container, ".hidden-asterisk")
                    xml2::xml_attr(asterisk, "style") <- "display: inline;"
                    required_question_ids <- c(required_question_ids, question_id)
                }

                if (!is.null(show_if)) {
                    if (question_id %in% show_if$targets) {
                        current_style <- xml2::xml_attr(container, "style")
                        new_style <- paste(current_style, "display: none;", sep = " ")
                        xml2::xml_attr(container, "style") <- new_style
                    }
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

get_output_ids <- function() {
    output <- shiny::getDefaultReactiveDomain()$output
    outs <- outputOptions(output)
    return(names(outs))
}

check_skip_show <- function(
    question_ids, question_values, page_ids, skip_if, show_if
) {
    if (!is.null(skip_if)) {
        invalid_skip_targets <- setdiff(skip_if$targets, page_ids)
        if (length(invalid_skip_targets) > 0) {
            stop(sprintf(
                "Invalid skip_if targets: %s. These must be valid page IDs.",
                paste(invalid_skip_targets, collapse = ", "))
            )
        }
    }

    if (!is.null(show_if)) {
        # Get any potential question_ids from the output
        invalid_show_targets <- setdiff(
            show_if$targets,
            c(question_ids, get_output_ids())
        )
        if (length(invalid_show_targets) > 0) {
            stop(sprintf(
              "Invalid show_if targets: %s. These must be question IDs defined in the survey.qmd file.",
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

    # Check for restricted IDs
    restricted_ids <- c("session_id", "time_start", "time_end", "exit_survey_rating")
    used_restricted_ids <- intersect(restricted_ids, question_ids)
    if (length(used_restricted_ids) > 0) {
        stop("Restricted question IDs found: ", paste(used_restricted_ids, collapse = ", "),
             ". These IDs are reserved and should not be used for survey questions.")
    }
}

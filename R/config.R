run_config <- function(
    required_questions,
    all_questions_required,
    start_page,
    admin_page,
    skip_if,
    show_if,
    rate_survey
) {
    # Check for sd_close() in survey.qmd if rate_survey used
    if (rate_survey) { check_sd_close() }

    # Get paths to files and create '_survey' folder if necessary
    paths <- get_paths()

    # Check for changes in survey.qmd and app.R files
    files_need_updating <- check_files_need_updating(paths)

    if (files_need_updating) {
        message("Output files not up-to-date - rendering qmd file and extracting content.")

        # Render the qmd file into the "_survey" folder
        render_qmd(paths)

        # Get the html content from the rendered survey.html file
        html_content <- rvest::read_html(paths$target_html)

        # Extract head content (for CSS and JS) and save to "_survey" folder
        head_content <- extract_head_content(paths, html_content)

        # Extract all divs with class "sd-page" and save to "_survey" folder
        pages <- extract_html_pages(
            paths, html_content, required_questions, all_questions_required, show_if
        )

        # Get the question structure (If changes detected, extract from HTML, otherwise YAML)
        question_structure <- get_question_structure(paths, html_content)

        message(
          "Survey rendered and saved to '", paths$target_html,
          ". Extracted content saved to '", paths$target_pages, "', '",
          paths$target_head, "', and '", paths$target_questions, "' files."
        )

    } else {
        message(
          'Survey unchanged, loading from "_survey" folder.'
        )

        # Load head content from _survey folder
        head_content <- readRDS(paths$target_head)

        # Load pages object from _survey folder
        pages <- readRDS(paths$target_pages)

        # Load question structure from _survey folder
        question_structure <- load_question_structure_yaml(paths$target_questions)
    }

    # Get page and question IDs
    page_ids <- sapply(pages, function(p) p$id)
    question_ids <- names(question_structure)

    # Check for duplicate, overlapping, or pre-defined IDs
    check_ids(page_ids, question_ids)

    # Determine required questions, excluding matrix question IDs
    if (all_questions_required) {
        matrix_question_ids <- names(which(sapply(question_structure, `[[`, "is_matrix")))
        question_required <- setdiff(question_ids, matrix_question_ids)
    } else {
        question_required <- required_questions
    }

    # Check that start_page (if used) points to an actual page
    if (!is.null(start_page) && !(start_page %in% page_ids)) {
        stop("The specified start_page does not exist - check that you have not mis-spelled the id")
    }

    # Set the start page
    if (is.null(start_page)) {
        start_page <- page_ids[1]
    }

    # Check skip_if and show_if inputs
    question_values <- unname(unlist(lapply(question_structure, `[[`, "options")))

    check_skip_show(question_ids, question_values, page_ids, skip_if, show_if)

    # Store all config settings
    config <- list(
        pages = pages,
        head_content = head_content,
        page_ids = page_ids,
        question_ids = question_ids,
        question_required = question_required,
        start_page = start_page,
        admin_page = admin_page,
        question_structure = question_structure
    )

    return(config)
}

check_sd_close <- function() {
    # Read the content of survey.qmd
    qmd_content <- readLines('survey.qmd', warn = FALSE)

    # Check for sd_close() call with any parameters
    sd_close_present <- any(grepl("sd_close\\s*\\(.*\\)", qmd_content))

    if (!sd_close_present) {
        warning("\u274C No sd_close() function found in 'survey.qmd' file. You must use sd_close() to trigger the rating question response at the end of the survey. You can also remove this rating question by setting 'rate_survey = FALSE' in sd_server().")
    }
}

get_paths <- function() {
    target_folder <- "_survey"
    if (!fs::dir_exists(target_folder)) { fs::dir_create(target_folder)}
    paths <- list(
        qmd           = "survey.qmd",
        app           = "app.R",
        target_folder = target_folder,
        root_html     = "survey.html",
        target_html   = file.path(target_folder, "survey.html"),
        target_pages  = file.path(target_folder, "pages.rds"),
        target_head   = file.path(target_folder, "head.rds"),
        target_questions  = file.path(target_folder, "questions.yml")
    )
    return(paths)
}

check_files_need_updating <- function(paths) {
    # Re-render if any of the target files are missing
    targets <- c(
      paths$target_html, paths$target_pages,
      paths$target_head, paths$target_questions
    )
    if (any(!fs::file_exists(targets))) { return(TRUE) }

    # Re-render if the target pages file is out of date with 'survey.qmd' or 'app.R'
    time_qmd <- file.info(paths$qmd)$mtime
    time_app <- file.info(paths$app)$mtime
    time_pages <- file.info(paths$target_pages)$mtime
    return((time_qmd > time_pages) || (time_app > time_pages))
}

render_qmd <- function(paths) {
    tryCatch(
        {
            # Render the 'survey.qmd' file
            quarto::quarto_render(
                paths$qmd,
                pandoc_args = c("--embed-resources")
            )

            # Move rendered 'survey.html' into '_survey' folder
            fs::file_move(paths$root_html, paths$target_html)
        },
        error = function(e) {
            stop("Error rendering 'survey.qmd' file. Please review and revise the file. Error details: ", e$message)
        }
    )
}

extract_head_content <- function(paths, html_content) {
    head_content <- html_content |>
        rvest::html_element("head") |>
        rvest::html_children() |>
        sapply(as.character) |>
        paste(collapse = "\n")

    saveRDS(head_content, paths$target_head)

    return(head_content)
}

extract_html_pages <- function(
    paths, html_content, required_questions, all_questions_required, show_if
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

                # Check if it's a matrix question
                is_matrix <- length(rvest::html_elements(container, ".matrix-question")) > 0

                # Determine if the question is required
                is_required <- if (is_matrix) {
                    FALSE  # Matrix questions are not required by default
                } else if (all_questions_required) {
                    TRUE
                } else {
                    question_id %in% required_questions
                }

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

    saveRDS(pages, paths$target_pages)

    return(pages)
}

# Get question structure ('smart': load YAML or extract from HTML and save to YAML)
get_question_structure <- function(paths, html_content) {

    question_structure <- extract_question_structure_html(html_content)

    write_question_structure_yaml(question_structure, paths$target_questions)

    return(question_structure)
}

# Extract question structure from HTML
extract_question_structure_html <- function(html_content) {
    question_nodes <- rvest::html_nodes(html_content, "[data-question-id]")

    question_structure <- list()

    # Loop through all question nodes and extract information
    for (question_node in question_nodes) {
        question_id <- rvest::html_attr(question_node, "data-question-id")

        type <- question_node |>
            rvest::html_nodes(glue::glue("#{question_id}")) |>
            rvest::html_attr("class")

        is_matrix <- length(rvest::html_nodes(question_node, ".matrix-question")) > 0

        if (is_matrix) type <- "matrix"

        # Extract the question text (label)
        label <- question_node |>
            rvest::html_nodes("p") |>
            rvest::html_text(trim = TRUE)

        # Write main information to the question structure
        question_structure[[question_id]] <- list(
            type = type,
            is_matrix = is_matrix,
            label = label
          )

        # Extract options for the question ( mc, *_multiple, *_buttons, and select)
        if (grepl("radio|checkbox|select|matrix", type)) {

            options <- question_node |>
                rvest::html_nodes("input[type='radio'], input[type='checkbox'], option") |>
                rvest::html_attr("value")

            label_options <- question_node |>
                rvest::html_nodes("label>span, button, option") |>
                rvest::html_text(trim = TRUE)

            names(options) <- label_options

            # Write options to the question structure
            question_structure[[question_id]]$options <- as.list(options)

        # Extract options for the question (slider)
        } else if (grepl("slider", type)) {
            options_raw <- question_node |>
                rvest::html_nodes("input") |>
                rvest::html_attr("data-swvalues")

            options <- gsub("\\[|\\]|\\\"", "", options_raw) |>
                strsplit(",") |>
                unlist()

            # names(options) <- options # TODO no labels in html for slider

            question_structure[[question_id]]$options <- as.list(options)
        }

        # Extract the rows and options for the matrix main question
        if (is_matrix) {

            rows <- question_node  |>
                rvest::html_nodes("div > div[id]") |>
                rvest::html_attr("id")

            # Remove the question ID prefix from the row names
            rows <- gsub(glue::glue("{question_id}_"), "", rows)

            label_rows <- question_node |>
                rvest::html_nodes("td:nth-child(1)") |>
                rvest::html_text(trim = TRUE)

            # remove first empty row label (option header)
            label_rows <- label_rows[label_rows != ""]

            names(rows) <- label_rows

            # Write rows to the question structure
            question_structure[[question_id]]$row <- as.list(rows)

            # Correct to unique options (first extraction multiplies by subquestions)
            options <- options[1:(length(options) / length(rows))]
            question_structure[[question_id]]$options <- as.list(options)
        }
    }

    return(question_structure)
}

# Write question structure to YAML
write_question_structure_yaml <- function(question_structure, file_yaml) {

    # Map question types to extracted html classes
    type_replacement <- c(
        'shiny-input-text form-control' = 'text',
        'shiny-input-textarea form-control' = 'textarea',
        'shiny-input-number form-control' = 'numeric',
        'form-group shiny-input-radiogroup shiny-input-container' = 'mc',
        'radio-group-buttons' = 'mc_buttons',
        'form-group shiny-input-checkboxgroup shiny-input-container' = 'mc_multiple',
        'checkbox-group-buttons' = 'mc_multiple_buttons',
        'shiny-input-select' = 'select',
        'js-range-slider sw-slider-text' = 'slider',
        'shiny-date-input form-group shiny-input-container' = 'date',
        'shiny-date-range-input form-group shiny-input-container' = 'daterange'
    )

    # Loop through question structure and clean/prepare questions
    question_structure <- lapply(question_structure, function(question) {

        # Rename type to function option names
        question$type <- type_replacement[names(type_replacement) == question$type]
        if (question$is_matrix) { question$type <- "matrix" }
        if (length(question$type) == 0) question$type <- "unknown"

        # Remove indicator if is matrix (type is correctly set)
        question$is_matrix <- NULL

        # Mark matrix subquestion to remove from list (and end further processing)
        if (question$type == "mc" & length(question$label) == 0) {
            return(NULL)
        }

        # Remove first option from select type question
        if (question$type == "select") {
           question$options <- question$options[-1]
        }

        return(question)
    })

    # Remove NULL elements (matrix subquestions)
    question_structure <- Filter(Negate(is.null), question_structure)

    # Write question to YAML (with comment in first lines)
    yaml_content <- yaml::as.yaml(question_structure)

    comment_line1 <- "# ! JUST READ - don't change the content of this file\n"
    comment_line2 <- "# Question structure extracted from survey.html\n"

    full_content <- paste0(comment_line1, comment_line2, yaml_content)

    writeLines(full_content, con = file_yaml)
}

# Load question structure from YAML
load_question_structure_yaml <- function(file_yaml) {

    # Read question structure from YAML file
    question_structure <- yaml::read_yaml(file_yaml)

    # Add matrix question indicator to all questions as FALSE (correct later)
    question_structure <- lapply(question_structure, function(question) {
        question$is_matrix <- FALSE
        return(question)
    })

    # Get question types to create subquestions for matrix questions
    question_types <- sapply(question_structure, function(q) q$type)
    matrix_questions_ids <- names(question_types)[question_types == "matrix"]

    # Loop trough matrix questions and add subquestions
    for (matrix_question_id in matrix_questions_ids) {

        # Get matrix question and subquestion (rows option) from question list
        matrix_question <- question_structure[[matrix_question_id]]
        rows <- matrix_question$row

        # Loop over subquestions and add to question structure (with label and options)
        for (row_number in seq_along(rows)) {

            subquestion_id <- paste0(matrix_question_id, "_", rows[[row_number]])

            subquestion_structure <- list(
                type = "mc",
                label = names(rows)[row_number],
                options = matrix_question$options
            )

            question_structure[[subquestion_id]] <- subquestion_structure
        }

        # Add matrix question indicator
        question_structure[[matrix_question_id]]$is_matrix <- TRUE
    }

    return(question_structure)
}

get_output_ids <- function() {
    output <- shiny::getDefaultReactiveDomain()$output
    outs <- shiny::outputOptions(output)
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

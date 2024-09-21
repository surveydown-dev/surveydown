# Convert Markdown to HTML
markdown_to_html <- function(text) {
    if (is.null(text)) { return(text) }
    return(shiny::HTML(markdown::renderMarkdown(text = text)))
}

# Convert List Names from Markdown to HTML
list_name_md_to_html <- function(list) {
    list_names_md <- names(list)
    list_names_html <- lapply(list_names_md, function(name) {
        html_name <- markdown_to_html(name)
        plain_name <- gsub("<[/]?p>|\\n", "", html_name)
        return(plain_name)
    })
    names(list) <- unlist(list_names_html)
    return(list)
}

#' Display Package Information on Attach
#'
#' This function is called when the package is attached. It displays version number,
#' authors, and citation information.
#'
#' @param libname The library where the package is installed
#' @param pkgname The name of the package
#'
#' @noRd
.onAttach <- function(libname, pkgname) {

    # Add special folders to resource path
    folders <- c('images', 'css', 'js', 'www')
    for (folder in folders) { include_folder(folder) }

    # Add survey_files folder to resource path
    # (if survey.qmd exists and it's not self contained)
    folder <- get_survey_file_folder()
    if (!is.null(folder)) { include_folder(folder, create = TRUE) }

    # Print package data
    desc  <- utils::packageDescription(pkgname, libname)
    packageStartupMessage(
        "Version:  ", desc$Version, "\n",
        "Author:   ", "John Paul Helveston, Pingfan Hu, Bogdan Bunea (George Washington University)", "\n\n",
        "Consider submitting praise at\n",
        "https://github.com/jhelvy/surveydown/issues/41.\n\n",
        "Please cite our package in your publications, see:\ncitation(\"surveydown\")"
    )
}

get_survey_file_folder <- function() {
    if (!survey_file_exists()) { return(NULL) }
    if (!is_self_contained("survey.qmd")) {
        return("survey_files")
    }
    return(NULL)
}

survey_file_exists <- function() {
    files <- basename(list.files(full.names = TRUE))
    if ("survey.qmd" %in% files) { return(TRUE) }
    return(FALSE)
}

is_self_contained <- function(x) {
    result <- quarto::quarto_inspect(x)$formats$html$pandoc$`self-contained`
    if (is.null(result)) { return(FALSE) }
    return(result)
}

include_folder <- function(folder, create = FALSE) {
    folder_exists <- dir.exists(folder)
    if (folder_exists) {
        shiny::addResourcePath(folder, folder)
    } else if (create) {
        dir.create(folder)
        shiny::addResourcePath(folder, folder)
    }
}

check_survey_file_exists <- function() {
    if (!survey_file_exists()) {
        stop('Missing "survey.qmd" file. Your survey file must be named "survey.qmd"')
    }
}

#' Include a folder to Shiny's resource path
#'
#' This function includes a specified folder to Shiny's resource path,
#' making it accessible for serving static files in a Shiny application.
#' It checks for pre-existing resource paths to avoid conflicts with
#' folders already included by the package.
#'
#' @param folder A character string specifying the name of the folder to include.
#'   This folder should exist in the root directory of your Shiny app.
#'
#' @return `NULL` invisibly. The function is called for its side effect of
#'   adding a resource path to Shiny.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sd_include_folder("custom_images")
#' }
sd_include_folder <- function(folder) {
    # List of folders pre-included by the package
    pre_included_folders <- names(shiny::resourcePaths())

    if (folder %in% pre_included_folders) {
        message(paste("The folder", folder, "is already included by the package. No action needed."))
        return(invisible(NULL))
    }

    if (!dir.exists(folder)) {
        stop(paste("The folder", folder, "does not exist in the current directory."))
    }

    shiny::addResourcePath(folder, folder)
    message(paste("Successfully added", folder, "to Shiny's resource path."))

    invisible(NULL)
}

# Convert Vector to JSON Array
vector_to_json_array <- function(vec) {
    if (length(vec) == 0) return("[]")

    # Ensure all elements are properly quoted
    quoted_elements <- sapply(vec, function(x) {
        if (is.character(x)) {
            sprintf('"%s"', gsub('"', '\\"', x))  # Escape any quotes within strings
        } else {
            as.character(x)
        }
    })

    # Join elements and wrap in brackets
    sprintf("[%s]", paste(quoted_elements, collapse = ","))
}

# Load and Run JavaScript File
load_js_file <- function(name) {
    js_file_path <- system.file("js", name, package = "surveydown")
    js_code <- paste(readLines(js_file_path), collapse = "\n")
    shinyjs::runjs(js_code)
}

tibble_to_list_of_lists <- function(tbl) {
  if (!is.data.frame(tbl)) {
    stop("Input must be a data frame or tibble")
  }

  if (!"condition" %in% names(tbl) || !"target" %in% names(tbl)) {
    stop("Input must have 'condition' and 'target' columns")
  }

  lapply(seq_len(nrow(tbl)), function(i) {
    list(
      condition = tbl$condition[[i]],
      target = tbl$target[i]
    )
  })
}

# Function to render Quarto document to a temporary file
quarto_render_temp <- function(input) {

    # Create a temporary directory
    temp_dir <- tempdir()

    # Get the output file path in the original directory
    x <- quarto::quarto_inspect(input)
    output_format <- names(x$formats)
    original_output <- x$formats[[output_format]]$pandoc$`output-file`
    original_output_path <- file.path(dirname(input), original_output)

    # Render the Quarto document
    quarto::quarto_render(input)

    # Define the path for the temporary file
    temp_output_path <- file.path(temp_dir, "temp_output.html")

    # Copy the rendered file to the temporary location and delete the original
    file.copy(from = original_output_path, to = temp_output_path, overwrite = TRUE)
    file.remove(original_output_path)

    # Return the path to the temporary file
    return(temp_output_path)
}

#' Create a new survey template
#'
#' This function creates a new survey template by copying files from the package's
#' template directory to a specified path. It handles file conflicts and provides
#' appropriate warnings and feedback.
#'
#' @param path A character string specifying the directory where the survey template
#'   should be created. Defaults to the current working directory.
#' @param structure A character string specifying the template structure to use.
#'   Must be either "single" or "multi". Defaults to "single".
#'
#' @return Invisible NULL. The function is called for its side effects of creating
#'   files and providing user feedback.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item If the specified path is the current working directory, it asks for user confirmation.
#'   \item Validates the specified structure ("single" or "multi").
#'   \item Creates the target directory if it doesn't exist.
#'   \item Copies all files from the package's template directory (based on the specified structure) to the target path.
#'   \item Preserves the directory structure of the template.
#'   \item Skips existing files and provides warnings for each skipped file.
#'   \item Handles .Rproj files specially, skipping if any .Rproj file already exists in the target directory.
#'   \item Provides feedback on whether files were copied or if all files already existed.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a multi-page survey template in the current working directory
#' sd_create_survey()
#'
#' # Create a single-page survey template in a specific directory
#' sd_create_survey("path/to/my/survey", structure = "single")
#'
#' # Create a multi-page survey template in a specific directory
#' sd_create_survey("path/to/my/survey", structure = "multi")
#' }
sd_create_survey <- function(path = getwd(), structure = "single") {
    # Check if using current directory and confirm with user
    if (path == getwd() && !usethis::ui_yeah(paste("Use the current directory (", path, ") as the path?"))) {
        stop("Operation aborted by the user.")
    }

    # Validate the structure parameter
    if (!structure %in% c("single", "multi")) {
        stop("Invalid structure. Choose either 'single' or 'multi'.")
    }

    # Create the directory if it doesn't exist
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # Get the path to the template folder and list files
    template_path <- system.file(file.path("templates", structure), package = "surveydown")
    if (!dir.exists(template_path)) {
        stop(paste("Template directory for", structure, "structure does not exist."))
    }
    template_files <- list.files(template_path, full.names = TRUE, recursive = TRUE)

    # Copy files, checking for conflicts
    files_copied <- sapply(template_files, function(file) {
        relative_path <- sub(template_path, "", file)
        target_file <- file.path(path, relative_path)

        # Ensure target directory exists
        dir.create(dirname(target_file), recursive = TRUE, showWarnings = FALSE)

        file_name <- basename(file)
        if (grepl("\\.Rproj$", file_name) && length(list.files(path, pattern = "\\.Rproj$"))) {
            warning("Skipping the .Rproj file since one already exists.", call. = FALSE, immediate. = TRUE)
            return(FALSE)
        } else if (file.exists(target_file)) {
            warning(paste("Skipping", file_name, "since it already exists."), call. = FALSE, immediate. = TRUE)
            return(FALSE)
        } else {
            file.copy(from = file, to = target_file, overwrite = FALSE)
            return(TRUE)
        }
    })

    # Provide feedback to the user
    if (any(files_copied)) {
        usethis::ui_done(paste(structure, "version of template created at", path))
    } else {
        usethis::ui_done("Since all files exist, no file was added.")
    }
}

#' Deploy a Surveydown App
#'
#' This function is a wrapper for `rsconnect::deployApp()` specifically designed
#' for deploying Surveydown applications. It simplifies the deployment process
#' by allowing you to specify just the app name.
#'
#' @param name A character string specifying the name of the app. Default is "survey".
#'
#' @return This function doesn't return a value; it deploys the app to Shiny Server.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Deploy with default name "survey"
#' sd_deploy()
#'
#' # Deploy with a custom name
#' sd_deploy("my_custom_survey")
#' }
#'
#' @seealso \code{\link[rsconnect]{deployApp}}
#'
#' @importFrom rsconnect deployApp
sd_deploy <- function(name = "survey") {
    rsconnect::deployApp(appName = name)
}

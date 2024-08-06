#' Create a Survey Template
#'
#' This function creates a survey template in a specified directory. It can use
#' different templates, with `"simple"` being the default. The function prompts
#' the user to confirm the use of the current working directory if no path is
#' specified.
#'
#' @param path A character string specifying the directory in which to create
#' the survey template. Defaults to the current working directory.
#' @param template A character string specifying the survey template to use.
#' Defaults to `"simple"`.
#'
#' @return A message indicating the successful creation of the survey template.
#' @export
#'
#' @examples
#' \dontrun{
#' create_survey()
#' create_survey(path = "path/to/package", template = "simple")
#' }
create_survey <- function(path = getwd()) {
    # Use the usethis ui_ask_yes_no function for confirmation
    using_current_dir <- path == getwd()
    if (using_current_dir) {
        if (
            !usethis::ui_yeah(paste(
                "Do you want to use the current working directory (",
                path, ") as the path?"
            ))
        ) {
            stop("Operation aborted by the user.")
        }
    }

    # Define the URL for the GitHub repository
    repo_url <- "https://github.com/jhelvy/surveydown-ext/archive/refs/heads/main.zip"

    # Create a temporary file to store the downloaded zip
    temp_file <- tempfile(fileext = ".zip")

    # Download the zip file
    utils::download.file(repo_url, temp_file, mode = "wb")

    # Create a temporary directory to unzip the contents
    temp_dir <- tempfile()
    dir.create(temp_dir)

    # Unzip the file
    utils::unzip(temp_file, exdir = temp_dir)

    # Get the path of the unzipped "surveydown-ext-main" directory
    unzipped_dir <- file.path(temp_dir, "surveydown-ext-main")

    # Ensure the target path exists
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # Check if there's an .Rproj file in the current directory when using current directory
    if (using_current_dir) {
        existing_rproj <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)
        if (length(existing_rproj) > 0) {
            # Delete example.Rproj from the downloaded files
            example_rproj <- file.path(unzipped_dir, "example.Rproj")
            if (file.exists(example_rproj)) {
                file.remove(example_rproj)
            }
        }
    }

    # Delete existing surveydown folder if it exists
    target_surveydown_path <- file.path(path, "_extensions", "jhelvy", "surveydown")
    if (dir.exists(target_surveydown_path)) {
        unlink(target_surveydown_path, recursive = TRUE)
    }

    # Create _extensions/jhelvy folder if it doesn't exist
    target_jhelvy_path <- file.path(path, "_extensions", "jhelvy")
    dir.create(target_jhelvy_path, recursive = TRUE, showWarnings = FALSE)

    # Copy the surveydown folder from the downloaded files
    source_surveydown_path <- file.path(unzipped_dir, "_extensions", "jhelvy", "surveydown")
    file.copy(source_surveydown_path, target_jhelvy_path, recursive = TRUE)

    # Get list of other files and directories to move, excluding _extensions folder
    items_to_move <- list.files(unzipped_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    items_to_move <- items_to_move[!grepl("_extensions", items_to_move)]

    # Move other contents from unzipped_dir to the target path
    for (item in items_to_move) {
        if (dir.exists(item)) {
            # If it's a directory, use recursive copy
            file.copy(item, path, recursive = TRUE)
        } else {
            # If it's a file, use simple copy
            file.copy(item, file.path(path, basename(item)), overwrite = TRUE)
        }
    }

    # Clean up temporary files
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)

    usethis::ui_done(paste("Survey template created at", path))
}

#' Update Survey Extension
#'
#' This function updates or creates the _extensions/jhelvy/surveydown folder
#' with the latest contents from the surveydown-ext repository.
#'
#' @param path A character string specifying the directory in which to update
#' or create the extension. Defaults to the current working directory.
#'
#' @return A message indicating the successful update of the extension.
#' @export
#'
#' @examples
#' \dontrun{
#' update_extension()
#' update_extension(path = "path/to/project")
#' }
update_extension <- function(path = getwd()) {
    # Define the URL for the GitHub repository
    repo_url <- "https://github.com/jhelvy/surveydown-ext/archive/refs/heads/main.zip"

    # Create a temporary file to store the downloaded zip
    temp_file <- tempfile(fileext = ".zip")

    # Download the zip file
    utils::download.file(repo_url, temp_file, mode = "wb")

    # Create a temporary directory to unzip the contents
    temp_dir <- tempfile()
    dir.create(temp_dir)

    # Unzip the file
    utils::unzip(temp_file, exdir = temp_dir)

    # Get the path of the unzipped "surveydown-ext-main" directory
    unzipped_dir <- file.path(temp_dir, "surveydown-ext-main")

    # Define the source and target paths
    source_path <- file.path(unzipped_dir, "_extensions", "jhelvy", "surveydown")
    target_path <- file.path(path, "_extensions", "jhelvy", "surveydown")

    # Check if the target path exists
    if (dir.exists(target_path)) {
        # If it exists, delete all contents
        unlink(list.files(target_path, full.names = TRUE), recursive = TRUE)
    } else {
        # If it doesn't exist, create the directory
        dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
    }

    # Copy all contents from source_path to target_path
    file.copy(list.files(source_path, full.names = TRUE), target_path, recursive = TRUE)

    # Clean up temporary files
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)

    usethis::ui_done(paste("Survey extension updated at", target_path))
}

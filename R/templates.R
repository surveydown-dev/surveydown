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
    if (path == getwd()) {
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

    # Get list of files to move, excluding "." and ".."
    files_to_move <- list.files(unzipped_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)

    # Move all contents from unzipped_dir to the target path
    file.rename(files_to_move,
                file.path(path, basename(files_to_move)))

    # Clean up temporary files
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)

    usethis::ui_done(paste("Survey template created at", path))
}

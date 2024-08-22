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
#' Defaults to `"simple"`. Currently, only the "simple" template is available.
#'
#' @return Invisibly returns TRUE if the survey template was successfully created.
#' @export
#'
#' @details
#' This function downloads the latest version of the surveydown extension from GitHub,
#' and uses it to create a new survey project. It copies all necessary files and
#' directories to the specified path, excluding some files like README.md and .gitignore.
#'
#' @examples
#' \dontrun{
#' sd_create_survey()
#' sd_create_survey(path = "path/to/survey", template = "simple")
#' }
sd_create_survey <- function(path = getwd(), template = "simple") {
    using_current_dir <- path == getwd()
    if (using_current_dir && !usethis::ui_yeah(paste("Do you want to use the current working directory (", path, ") as the path?"))) {
        stop("Operation aborted by the user.")
    }

    temp_dir <- tempfile()
    dir.create(temp_dir)
    unzipped_dir <- download_extension(temp_dir)

    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    if (using_current_dir) {
        existing_rproj <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)
        if (length(existing_rproj) > 0) {
            example_rproj <- file.path(unzipped_dir, "example.Rproj")
            if (file.exists(example_rproj)) {
                file.remove(example_rproj)
            }
        }
    }

    target_surveydown_path <- file.path(path, "_extensions", "surveydown-dev", "surveydown")
    if (dir.exists(target_surveydown_path)) {
        unlink(target_surveydown_path, recursive = TRUE)
    }

    target_surveydown_dev_path <- file.path(path, "_extensions", "surveydown-dev")
    dir.create(target_surveydown_dev_path, recursive = TRUE, showWarnings = FALSE)

    source_surveydown_path <- file.path(unzipped_dir, "_extensions", "surveydown-dev", "surveydown")
    file.copy(source_surveydown_path, target_surveydown_dev_path, recursive = TRUE)

    items_to_move <- list.files(unzipped_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    items_to_move <- items_to_move[!grepl("_extensions", items_to_move)]

    # Exclude README.md and .gitignore
    exclude_files <- c("README.md", ".gitignore")
    items_to_move <- items_to_move[!basename(items_to_move) %in% exclude_files]

    for (item in items_to_move) {
        if (dir.exists(item)) {
            file.copy(item, path, recursive = TRUE)
        } else {
            file.copy(item, file.path(path, basename(item)), overwrite = TRUE)
        }
    }

    unlink(temp_dir, recursive = TRUE)

    usethis::ui_done(paste("Survey template created at", path))
}

#' Update Survey Extension
#'
#' This function updates or creates the _extensions/surveydown-dev/surveydown folder
#' with the latest contents from the surveydown-ext repository.
#'
#' @param path A character string specifying the directory in which to update
#' or create the extension. Defaults to the current working directory.
#'
#' @return Invisibly returns TRUE if the extension was successfully updated.
#' @export
#'
#' @details
#' This function downloads the latest version of the surveydown extension from GitHub,
#' and updates the local copy in the specified path. If the extension directory
#' doesn't exist, it will be created.
#'
#' @examples
#' \dontrun{
#' sd_update_extension()
#' sd_update_extension(path = "path/to/survey")
#' }
sd_update_extension <- function(path = getwd()) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    unzipped_dir <- download_extension(temp_dir)

    source_path <- file.path(unzipped_dir, "_extensions", "surveydown-dev", "surveydown")
    target_path <- file.path(path, "_extensions", "surveydown-dev", "surveydown")

    if (dir.exists(target_path)) {
        unlink(list.files(target_path, full.names = TRUE), recursive = TRUE)
    } else {
        dir.create(target_path, recursive = TRUE, showWarnings = FALSE)
    }

    file.copy(list.files(source_path, full.names = TRUE), target_path, recursive = TRUE)

    unlink(temp_dir, recursive = TRUE)

    usethis::ui_done(paste("Survey extension updated at", target_path))
}

# Download and Extract Survey Extension
download_extension <- function(temp_dir) {
    repo_url <- "https://github.com/surveydown-dev/surveydown-ext/archive/refs/heads/main.zip"
    temp_file <- tempfile(fileext = ".zip")

    utils::download.file(repo_url, temp_file, mode = "wb")
    utils::unzip(temp_file, exdir = temp_dir)

    unzipped_dir <- file.path(temp_dir, "surveydown-ext-main")
    unlink(temp_file)

    return(unzipped_dir)
}

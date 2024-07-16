#' Create a Survey Template
#'
#' This function creates a survey template in a specified directory. It can use
#' different templates, with `"simple"` being the default. The function prompts
#' the user to confirm the use of the current working directory if no path is
#' specified. It also opens the new project in RStudio.
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
create_survey <- function(path = getwd(), template = "simple") {

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

  # Ensure the path is valid and create the directory
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Define the source directory within surveydown package
  source_dir <- system.file(
    file.path("example", template),
    package = "surveydown"
  )

  # Check if the source directory exists
  if (source_dir == "") {
    stop("Source directory not found in the package for the specified template.")
  }

  # Copy files and directories recursively from the source to the target path
  file.copy(list.files(source_dir, full.names = TRUE), path, recursive = TRUE)

  usethis::ui_done(paste("Survey template created at", path))

  # Change to the new project if running in RStudio
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    proj_file <- file.path(path, "survey.Rproj")
    if (file.exists(proj_file)) {
      rstudioapi::openProject(proj_file)
    }
  }
}

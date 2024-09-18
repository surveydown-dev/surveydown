#' Update Surveydown Package
#'
#' This function checks and updates surveydown.
#' It ensures that the package is up-to-date.
#'
#' @param force Logical; if TRUE, forces an update regardless of current version.
#' Defaults to FALSE.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' sd_update()
#' sd_update(force = TRUE)
#' }
sd_update <- function(force = FALSE) {
    # Check surveydown version
    surveydown_version <- utils::packageVersion("surveydown")

    # Check latest version
    latest_version <- get_latest_version("https://raw.githubusercontent.com/surveydown-dev/surveydown/main/DESCRIPTION", "Version: ")

    if (is.null(latest_version)) {
        message("Unable to fetch the latest version. Please check your internet connection.")
        return(invisible())
    }

    if (force || surveydown_version < latest_version) {
        message("Updating surveydown and all dependencies...")
        remotes::install_github(
            "surveydown-dev/surveydown",
            force = TRUE,
            dependencies = TRUE,
            upgrade = "always"
        )
        message("Update complete.")
    } else {
        message("surveydown is up-to-date.")
    }
}

#' Check Surveydown Version
#'
#' This function checks if the local surveydown package is up-to-date with
#' the latest online version. It compares the local version with the latest
#' version available on GitHub and provides information about whether an update
#' is needed.
#'
#' @return No return value, called for side effects (prints version information
#' and update status to the console).
#' @export
#'
#' @examples
#' sd_version()
sd_version <- function() {
    # Get local version
    local_surveydown_version <- utils::packageVersion("surveydown")

    # Get latest online version
    latest_surveydown_version <- get_latest_version("https://raw.githubusercontent.com/surveydown-dev/surveydown/main/DESCRIPTION", "Version: ")

    # Display version information
    message("surveydown (local): ", local_surveydown_version)
    message("surveydown (latest): ",
            if(is.null(latest_surveydown_version)) "Unable to fetch" else latest_surveydown_version)

    # Check if update is needed
    if (is.null(latest_surveydown_version)) {
        message("\nUnable to determine if an update is available.")
        message("Please ensure you have an active internet connection and try again later.")
    } else {
        pkg_needs_update <- local_surveydown_version < latest_surveydown_version

        if (pkg_needs_update) {
            message("\nAn update is available. To update surveydown to the latest version, run: surveydown::sd_update()")
        } else {
            message("\nsurveydown is up to date.")
        }
    }
}

get_latest_version <- function(url, pattern) {
    tryCatch({
        content <- readLines(url)
        version_line <- grep(pattern, content, value = TRUE)
        if (length(version_line) > 0) {
            version <- sub(pattern, "", version_line[1])
            return(package_version(trimws(version)))
        } else {
            message("Version information not found in the file at ", url)
            return(NULL)
        }
    }, error = function(e) {
        message("Error occurred while fetching version from ", url, ": ", e$message)
        return(NULL)
    })
}

#' Update Surveydown Package and Extension
#'
#' This function checks and updates both the surveydown R package and its
#' associated Quarto extension. It ensures that both components are up-to-date
#' and their versions match.
#'
#' @param force Logical; if TRUE, forces an update regardless of current versions.
#' Defaults to FALSE.
#'
#' @return No return value, called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' sd_update_surveydown()
#' sd_update_surveydown(force = TRUE)
#' }
sd_update_surveydown <- function(force = FALSE) {
    # Check R package version
    pkg_version <- packageVersion("surveydown")

    # Check Quarto extension version
    ext_version <- get_extension_version()

    if (is.null(ext_version)) {
        message("Quarto extension not found. Installing both package and extension.")
        force <- TRUE
    } else if (pkg_version != ext_version) {
        message("Version mismatch detected. Updating both package and extension.")
        force <- TRUE
    }

    if (force) {
        message("Updating surveydown R package and all dependencies...")
        remotes::install_github(
            "jhelvy/surveydown",
            force = TRUE,
            dependencies = TRUE,
            upgrade = "always"
        )

        message("Updating surveydown Quarto extension...")
        surveydown::sd_update_extension()

        message("Update complete.")
    } else {
        message("Both R package and Quarto extension are up-to-date.")
    }
}

#' Check Surveydown Versions
#'
#' This function checks the versions of both the surveydown R package and its
#' associated Quarto extension, reporting any mismatches and suggesting an update
#' if necessary.
#'
#' @return No return value, called for side effects (prints version information).
#' @export
#'
#' @examples
#' sd_check_versions()
sd_check_versions <- function() {
    pkg_version <- packageVersion("surveydown")
    ext_version <- get_extension_version()

    message("Surveydown R package version: ", pkg_version)
    if (is.null(ext_version)) {
        message("Surveydown Quarto extension: Not found")
    } else {
        message("Surveydown Quarto extension version: ", ext_version)
    }

    if (is.null(ext_version) || pkg_version != ext_version) {
        message("To update both the package and extension to the latest version, run: sd_update_surveydown()")
    } else {
        message("Versions match and are up to date.")
    }
}

#' Get Surveydown Extension Version
#'
#' This function reads the version of the surveydown Quarto extension from its
#' _extension.yml file.
#'
#' @param path A character string specifying the directory to search for the
#' extension. Defaults to the current working directory.
#'
#' @return A character string representing the extension version, or NULL if
#' the extension is not found.
#' @keywords internal
get_extension_version <- function(path = getwd()) {
    ext_yaml <- file.path(path, "_extensions", "jhelvy", "surveydown", "_extension.yml")
    if (!file.exists(ext_yaml)) {
        return(NULL)
    }
    yaml_content <- yaml::read_yaml(ext_yaml)
    return(yaml_content$version)
}

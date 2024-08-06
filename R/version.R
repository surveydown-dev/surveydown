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
#' update_surveydown()
#' update_surveydown(force = TRUE)
#' }
update_surveydown <- function(force = FALSE) {
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
        message("Updating surveydown R package...")
        remotes::install_github("jhelvy/surveydown", force = TRUE)

        message("Updating surveydown Quarto extension...")
        surveydown::update_extension()

        message("Update complete.")
    } else {
        message("Both R package and Quarto extension are up-to-date.")
    }
}

#' Check Surveydown Versions
#'
#' This function checks the versions of both the surveydown R package and its
#' associated Quarto extension, reporting any mismatches.
#'
#' @return No return value, called for side effects (prints version information).
#' @export
#'
#' @examples
#' check_surveydown_versions()
check_surveydown_versions <- function() {
    pkg_version <- packageVersion("surveydown")
    ext_version <- get_extension_version()

    message("Surveydown R package version: ", pkg_version)
    if (is.null(ext_version)) {
        message("Surveydown Quarto extension: Not found")
    } else {
        message("Surveydown Quarto extension version: ", ext_version)
    }

    if (!is.null(ext_version) && pkg_version != ext_version) {
        warning("Version mismatch detected between R package and Quarto extension.")
    } else if (!is.null(ext_version)) {
        message("Versions match.")
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

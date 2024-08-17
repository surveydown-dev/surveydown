#' Load and Run JavaScript File
#'
#' This function loads a JavaScript file from the package's inst/js directory
#' and runs it using shinyjs::runjs().
#'
#' @param name Character string. The name of the JavaScript file to load.
#'
#' @return None (invisible NULL)
#'
#' @keywords internal
#' @importFrom shinyjs runjs
load_js_file <- function(name) {
    js_file_path <- system.file("js", name, package = "surveydown")
    js_code <- paste(readLines(js_file_path), collapse = "\n")
    shinyjs::runjs(js_code)
}

#' Hide All Survey Pages
#'
#' This function runs a JavaScript code snippet that hides all elements
#' with the class 'sd-page'.
#'
#' @return None (invisible NULL)
#'
#' @keywords internal
#' @importFrom shinyjs runjs
hide_all_pages <- function() {
    js_code <- "
    (function() {
        var pages = document.querySelectorAll('.sd-page');
        pages.forEach(function(page) {
            page.style.display = 'none';
        });
    })();
    "
    shinyjs::runjs(js_code)
}

#' Show First Survey Page
#'
#' This function runs a JavaScript code snippet that displays the first
#' element with the class 'sd-page'.
#'
#' @return None (invisible NULL)
#'
#' @keywords internal
#' @importFrom shinyjs runjs
show_first_page <- function() {
    js_code <- "
    (function() {
        var pages = document.querySelectorAll('.sd-page');
        if (pages.length > 0) {
            pages[0].style.display = 'block';
        }
    })();
    "
    shinyjs::runjs(js_code)
}

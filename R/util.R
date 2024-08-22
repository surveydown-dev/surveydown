#' Required Set Up Function
#'
#' This function is required for any surveydown survey. It sets up a Shiny application with Bootstrap 5 and initializes Shinyjs for JavaScript functionalities.
#'
#' @details The function configures the Shiny application to use Bootstrap 5 for styling and enables
#'   Shinyjs for JavaScript functionalities within the application.
#'
#' @return This function does not return a value. It is called for its side effects of setting up the Shiny application.
#'
#' @examples
#' \dontrun{
#'   ui <- fluidPage(
#'     sd_setup(),
#'     # Your UI elements here
#'   )
#'   server <- function(input, output, session) {
#'     # Your server logic here
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
sd_setup <- function() {
    shiny::shinyOptions(bootstrapTheme = bslib::bs_theme(version = 5L))
    shinyjs::useShinyjs(rmd = TRUE)
}

#' Convert Markdown to HTML
#'
#' This function converts markdown text to HTML.
#'
#' @param text A character string containing markdown text.
#'
#' @return An HTML string converted from the input markdown text.
#'
#' @keywords internal
markdown_to_html <- function(text) {
    if (is.null(text)) { return(text) }
    return(shiny::HTML(markdown::renderMarkdown(text = text)))
}

#' Convert List Names from Markdown to HTML
#'
#' This function converts the names of a list from markdown to HTML format.
#' It only works for mc_buttons and mc_multiple_buttons.
#'
#' @param list A list whose names are in markdown format.
#'
#' @return A list with names converted to HTML format.
#'
#' @keywords internal
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

#' Get Current UTC Timestamp
#'
#' This function returns the current timestamp in UTC format.
#'
#' @return A character string representing the current UTC timestamp.
#'
#' @keywords internal
get_utc_timestamp <- function() {
    return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

#' Initialize Timestamps for Pages and Questions
#'
#' This function initializes timestamps for pages and questions in a survey.
#'
#' @param page_ids A vector of page IDs.
#' @param question_ids A vector of question IDs.
#'
#' @return A list of initialized timestamps for pages and questions.
#'
#' @keywords internal
initialize_timestamps <- function(page_ids, question_ids) {
    timestamps <- list()

    timestamps[[make_ts_name("page", page_ids[1])]] <- get_utc_timestamp()
    for (i in 2:length(page_ids)) {
        timestamps[[make_ts_name("page", page_ids[i])]] <- NA
    }

    for (qid in question_ids) {
        timestamps[[make_ts_name("question", qid)]] <- NA
    }

    return(timestamps)
}

#' Make Timestamp Name
#'
#' This function creates a standardized name for timestamps based on the type and ID.
#'
#' @param type A character string, either "page" or "question".
#' @param id The ID of the page or question.
#'
#' @return A character string representing the timestamp name.
#'
#' @keywords internal
make_ts_name <- function(type, id) {
    if (type == "page") {
        return(paste0("time_p_", id))
    } else if (type == "question") {
        return(paste0("time_q_", id))
    }
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
    desc  <- utils::packageDescription(pkgname, libname)
    packageStartupMessage(
        "Version:  ", desc$Version, "\n",
        "Author:   ", "John Paul Helveston, Pingfan Hu, Bogdan Bunea (George Washington University)", "\n\n",
        "Consider submitting praise at\n",
        "https://github.com/jhelvy/surveydown/issues/41.\n\n",
        "Please cite our package in your publications, see:\ncitation(\"surveydown\")"
    )
}

#' Convert Vector to JSON Array
#'
#' This function converts a vector to a JSON array string. It's a custom alternative
#' to jsonlite::toJSON().
#'
#' @param vec A vector to be converted to a JSON array.
#'
#' @return A character string representing the JSON array.
#'
#' @keywords internal
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

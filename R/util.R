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

# Convert markdown to HTML
markdown_to_html <- function(text) {
  if (is.null(text)) { return(text) }
  return(shiny::HTML(markdown::renderMarkdown(text = text)))
}

# Convert list names from markdown to HTML
# Only works for mc_buttons and mc_multiple_buttons.
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

get_utc_timestamp <- function() {
  return(format(Sys.time(), tz = "UTC", usetz = TRUE))
}

initialize_timestamps <- function(page_ids, question_ids) {
  timestamps <- list()

  # Initialize timestamps for pages
  timestamps[[make_ts_name("page", page_ids[1])]] <- get_utc_timestamp()
  for (i in 2:length(page_ids)) {
    timestamps[[make_ts_name("page", page_ids[i])]] <- NA
  }

  # Initialize timestamps for questions
  for (qid in question_ids) {
    timestamps[[make_ts_name("question", qid)]] <- NA
  }

  return(timestamps)
}

make_ts_name <- function(type, id) {
  if (type == "page") {
    return(paste0("time_p_", id))
  } else if (type == "question") {
    return(paste0("time_q_", id))
  }
}

#' Display version number and date when the package is loaded.
#' @importFrom utils packageDescription
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

# Custom alternative to jsonlite::toJSON()
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

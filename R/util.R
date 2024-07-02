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
    "Author:   ", "John Paul Helveston (George Washington University)", "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/surveydown/issues/8.\n\n",
    "Please cite our package in your publications, see:\ncitation(\"surveydown\")"
  )
}









# Including this as an example of how to document a function



#' Predict probabilities and / or outcomes
#'
#' This function is a faster implementation of the "type 7" `quantile()`
#' algorithm and is modified from this gist:
#' https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
#' It returns sample quantiles corresponding to the given probabilities.
#' The smallest observation corresponds to a probability of 0 and the largest
#' to a probability of 1. For speed, output quantile names are removed as are
#' error handling such as checking if x are factors, or if probs lie outside
#' the `[0,1]` range.
#' @param x numeric vector whose sample quantiles are wanted. `NA` and `NaN`
#' values are not allowed in numeric vectors unless `na.rm` is `TRUE`.
#' @param probs numeric vector of probabilities with values in `[0,1]`.
#' (Values up to `2e-14` outside that range are accepted and moved to the
#' nearby endpoint.)
#' @param na.rm logical; if `TRUE`, any `NA` and `NaN`'s are removed from `x`
#' before the quantiles are computed.
#' @return A vector of length `length(probs)` is returned;
#' @export
#' @examples
#' library(logitr)
#'
fquantile <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  index <- 1 + (n - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]
  i  <- 1:length(probs)
  h  <- index - lo
  qs <- (1 - h) * qs + h * x[hi]
  return(qs)
}

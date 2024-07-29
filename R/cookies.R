#' Generate a unique session ID
#'
#' @return A character string representing a unique session ID
#' @export
generate_session_id <- function() {
    paste0("sid_", uuid::UUIDgenerate())
}

#' Set a cookie with the session ID
#'
#' @param session The Shiny session object
#' @param session_id The session ID to store
#' @export
set_session_cookie <- function(session, session_id) {
    shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(sprintf(
            "document.cookie = 'session_id=%s; path=/; max-age=2592000';",
            session_id
        ))
    )
}

#' Get the session ID from the cookie
#'
#' @param input The Shiny input object
#' @return The session ID if found, NULL otherwise
#' @export
get_session_cookie <- function(input) {
    if (!is.null(input$cookies) && "session_id" %in% names(input$cookies)) {
        return(input$cookies$session_id)
    }
    return(NULL)
}

#' Delete the session ID cookie
#'
#' @param session The Shiny session object
#' @export
delete_session_cookie <- function(session) {
    shiny::insertUI(
        selector = "head",
        where = "beforeEnd",
        ui = shiny::tags$script(
            "document.cookie = 'session_id=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';"
        )
    )
}

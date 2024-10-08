# Load resource file from the surveydown package (CSS or JS)
load_resource <- function(files, type = c("css", "js"), package = "surveydown") {
    type <- match.arg(type)
    sapply(files, function(file) {
        path <- system.file(paste0(type, "/", file), package = package)
        if (type == "css") {
            shiny::includeCSS(path)
        } else {
            shiny::includeScript(path)
        }
    }, simplify = FALSE, USE.NAMES = FALSE)
}

#' Create the UI for a surveydown survey
#'
#' This function creates the user interface for a surveydown survey,
#' including necessary CSS and JavaScript files, and applies custom styling.
#' It retrieves theme and progress bar settings from the survey.qmd file.
#'
#' @return A Shiny UI object
#' @export
#'
#' @details
#' The function reads the following settings from the survey.qmd YAML header:
#' \itemize{
#'   \item \code{theme}: The theme to be applied to the survey.
#'   \item \code{barcolor}: The color of the progress bar (should be a valid hex color).
#'   \item \code{barposition}: The position of the progress bar ('top', 'bottom', or 'none').
#' }
#'
#' If \code{barcolor} is not specified or is NULL, the default theme color will be used.
#' If \code{barposition} is not specified, it defaults to 'top'.
#'
#' @examples
#' \dontrun{
#' # In your app.R or ui.R file:
#' ui <- sd_ui()
#' }
sd_ui <- function() {
    # Throw error if "survey.qmd" file missing
    check_survey_file_exists()

    # Get the theme from the survey.qmd file
    metadata <- quarto::quarto_inspect("survey.qmd")
    theme <- get_theme(metadata)
    default_theme_css <- ""
    if (theme == "default") {
        default_theme_css <- "
        body, button, input, select, textarea {
            font-family: 'Raleway', sans-serif;
        }
        h1, h2, h3, h4, h5, h6 {
            font-family: 'Raleway', sans-serif;
            font-weight: 800;
        }
        "
    }

    # Get progress bar settings from the survey.qmd file
    barcolor <- get_barcolor(metadata)
    barposition <- get_barposition(metadata)

    shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::tags$head(
        shiny::tags$script(shiny::HTML(enter_key_js()))
      ),
      shiny::tags$style(HTML(default_theme_css)),
      load_resource("surveydown.css", type = "css"),
      load_resource("keep_alive.js", type = "js"),
      load_resource("auto_scroll.js", type = "js"),
      shiny::tags$script("var surveydownConfig = {};"),
      if (!is.null(barcolor)) {
        shiny::tags$style(HTML(sprintf("
                :root {
                    --progress-color: %s;
                }
            ", barcolor)))
      },
      if (barposition != "none") {
        shiny::tags$div(
          id = "progressbar",
          class = barposition,
          shiny::tags$div(id = "progress")
        )
      },
      shiny::tags$div(
        class = "content",
        shiny::uiOutput("main")
      )
    )
}

get_theme <- function(metadata) {
    x <- "survey.qmd"
    theme <- metadata$formats$html$metadata$theme
    if (is.null(theme)) {
        return("default")
    }
    return(theme)
}

get_barcolor <- function(metadata) {
    barcolor <- metadata$formats$html$metadata$barcolor
    if (!is.null(barcolor)) {
        if (!grepl("^#([0-9A-Fa-f]{3}){1,2}$", barcolor)) {
            stop("Invalid barcolor in YAML. Use a valid hex color.")
        }
    }
    return(barcolor)
}

get_barposition <- function(metadata) {
    barposition <- metadata$formats$html$metadata$barposition
    if (is.null(barposition)) {
        return("top")
    }
    return(barposition)
}

#' Create a survey question
#'
#' This function creates various types of survey questions for use in a Surveydown survey.
#'
#' @param type Specifies the type of question. Possible values are "select", "mc", "mc_multiple", "mc_buttons", "mc_multiple_buttons", "text", "textarea", "numeric", "slider", "date", "daterange", and "matrix".
#' @param id A unique identifier for the question, which will be used as the variable name in the resulting survey data.
#' @param label Character string. The label for the UI element, which can be formatted with markdown.
#' @param cols Integer. Number of columns for the textarea input. Defaults to 80.
#' @param direction Character string. The direction for button groups ("horizontal" or "vertical"). Defaults to "horizontal".
#' @param status Character string. The status for button groups. Defaults to "default".
#' @param width Character string. The width of the UI element. Defaults to "100%".
#' @param height Character string. The height of the textarea input. Defaults to "100px".
#' @param selected Value. The selected value(s) for certain input elements.
#' @param label_select Character string. The label for the select input. Defaults to "Choose an option...".
#' @param grid Logical. Whether to show a grid for slider input. Defaults to TRUE.
#' @param individual Logical. Whether buttons in a group should be individually styled. Defaults to TRUE.
#' @param justified Logical. Whether buttons in a group should fill the width of the parent div. Defaults to FALSE.
#' @param force_edges Logical. Whether to force edges for slider input. Defaults to TRUE.
#' @param option List. Options for the select, radio, checkbox, and slider inputs.
#' @param placeholder Character string. Placeholder text for text and textarea inputs.
#' @param resize Character string. Resize option for textarea input. Defaults to NULL.
#' @param row List. Used for "matrix" type questions. Contains the row labels and their corresponding IDs.
#'
#' @details
#' The function supports various question types:
#' - "select": A dropdown selection
#' - "mc": Multiple choice (single selection)
#' - "mc_multiple": Multiple choice (multiple selections allowed)
#' - "mc_buttons": Multiple choice with button-style options (single selection)
#' - "mc_multiple_buttons": Multiple choice with button-style options (multiple selections allowed)
#' - "text": Single-line text input
#' - "textarea": Multi-line text input
#' - "numeric": Numeric input
#' - "slider": Slider input
#' - "date": Date input
#' - "daterange": Date range input
#' - "matrix": Matrix-style question with rows and columns
#'
#' For "matrix" type questions, use the `row` parameter to define the rows of the matrix.
#' Each element in the `row` list should have a name (used as the row ID) and a value (used as the row label).
#'
#' @return A Shiny UI element wrapped in a div with a data attribute for question ID.
#'
#' @examples
#' sd_question("text", "name", "What is your name?")
#' sd_question("mc", "color", "What is your favorite color?", option = c("Red", "Blue", "Green"))
#'
#' # Example of a matrix question
#' sd_question("matrix", "satisfaction", "Rate your satisfaction with the following:",
#'             option = c("Unsatisfied" = 1,
#'                        "Neutral" = 2,
#'                        "Satisfied" = 3),
#'             row = list(service = "Customer Service",
#'                        quality = "Product Quality",
#'                        price = "Price"))
#'
#' @export
sd_question <- function(
  type,
  id,
  label,
  cols         = "80",
  direction    = "horizontal",
  status       = "default",
  width        = "100%",
  height       = "100px",
  selected     = NULL,
  label_select = "Choose an option...",
  grid         = TRUE,
  individual   = TRUE,
  justified    = FALSE,
  force_edges  = TRUE,
  option       = NULL,
  placeholder  = NULL,
  resize       = NULL,
  row          = NULL
) {

    output <- NULL

    # Check if question if answered
    js_interaction <- sprintf("Shiny.setInputValue('%s_interacted', true, {priority: 'event'});", id)

    # Create label with hidden asterisk
    label <- markdown_to_html(label)

    if (type ==  "select") {
        option <- c("", option)
        names(option)[1] <- label_select

        output <- shiny::selectInput(
            inputId  = id,
            label    = label,
            choices  = option,
            multiple = FALSE,
            selected = FALSE
        )

    } else if (type == "mc") {

        output <- shiny::radioButtons(
            inputId  = id,
            label    = label,
            choices  = option,
            selected = FALSE
        )

    } else if (type == "mc_multiple") {

        output <- shiny::checkboxGroupInput(
            inputId  = id,
            label    = label,
            choices  = option,
            selected = FALSE
        )

    } else if (type == "mc_buttons") {

        output <- shinyWidgets::radioGroupButtons(
            inputId   = id,
            label     = label,
            choices   = list_name_md_to_html(option),
            direction = direction,
            selected  = character(0)
        )

        output <- shiny::tagAppendChild(output, shiny::tags$script(shiny::HTML(sprintf("
            $(document).on('click', '#%s .btn', function() {
                %s
            });
        ", id, js_interaction))))

    } else if (type == "mc_multiple_buttons") {

        output <- shinyWidgets::checkboxGroupButtons(
            inputId    = id,
            label      = label,
            choices    = list_name_md_to_html(option),
            direction  = direction,
            individual = individual,
            justified  = FALSE
        )

        output <- shiny::tagAppendChild(output, shiny::tags$script(shiny::HTML(sprintf("
            $(document).on('click', '#%s .btn', function() {
                %s
            });
        ", id, js_interaction))))

    } else if (type == "text") {

        output <- shiny::textInput(
            inputId     = id,
            label       = label,
            placeholder = option
        )

    } else if (type == "textarea") {

        output <- shiny::textAreaInput(
            inputId     = id,
            label       = label,
            height      = height,
            cols        = cols,
            value       = NULL,
            rows        = "6",
            placeholder = placeholder,
            resize      = resize
        )

    } else if (type == "numeric") {

        output <- shiny::numericInput(
            inputId = id,
            label   = label,
            value   = NULL
        )

    } else if (type == "slider") {

        output <- shinyWidgets::sliderTextInput(
            inputId      = id,
            label        = label,
            choices      = option,
            selected     = selected,
            force_edges  = force_edges,
            grid         = grid,
            animate      = FALSE,
            hide_min_max = FALSE,
            from_fixed   = FALSE,
            to_fixed     = FALSE,
            from_min     = NULL,
            from_max     = NULL,
            to_min       = NULL,
            to_max       = NULL,
            pre          = NULL,
            post         = NULL,
            dragRange    = TRUE
        )

    } else if (type == "date") {

        output <- shiny::dateInput(
            inputId            = id,
            label              = label,
            value              = NULL,
            min                = NULL,
            max                = NULL,
            format             = "mm/dd/yyyy",
            startview          = "month",
            weekstart          = 0,
            language           = "en",
            autoclose          = TRUE,
            datesdisabled      = NULL,
            daysofweekdisabled = NULL
        )

        output <- date_interaction(output, id)

    } else if (type == "daterange") {

        output <- shiny::dateRangeInput(
            inputId   = id,
            label     = label,
            start     = NULL,
            end       = NULL,
            min       = NULL,
            max       = NULL,
            format    = "mm/dd/yyyy",
            startview = "month",
            weekstart = 0,
            language  = "en",
            separator = "-",
            autoclose = TRUE
        )

        output <- date_interaction(output, id)

    } else if (type == "matrix") {
      header <- shiny::tags$tr(
        shiny::tags$th(""),
        lapply(names(option), function(opt) shiny::tags$th(opt))
      )
      rows <- lapply(names(row), function(q_id) {
        full_id <- paste(id, q_id, sep = "_")
        shiny::tags$tr(
          shiny::tags$td(row[[q_id]]),
          shiny::tags$td(
            colspan = length(option),
            sd_question(
              type = "mc",
              id = full_id,
              label = NULL,
              option = option,
              direction = "horizontal"
            )
          )
        )
      })

      output <- shiny::div(
        class = "matrix-question-container",
        shiny::tags$label(class = "control-label", label),
        shiny::tags$table(
          class = "matrix-question",
          header,
          shiny::tags$tbody(rows)
        )
      )
    }

    # Create wrapper div
    output_div <- make_question_container(id, output, width)

    if (!is.null(shiny::getDefaultReactiveDomain())) {
      # In a reactive context, directly add to output with renderUI
      shiny::isolate({
        output_div <- shiny::tags$div(output)
        output <- shiny::getDefaultReactiveDomain()$output
        output[[id]] <- shiny::renderUI({ output_div })
      })
    } else {
      # If not in a reactive context, just return the element
      return(output_div)
    }
}

date_interaction <- function(output, id) {
    js_code <- sprintf(
        "setTimeout(function() {
            $('#%s').on('change', function() {
                Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
            });
         }, 1000);",  # 1000 ms delay
        id, id
    )
    shiny::tagAppendChild(output, shiny::tags$script(shiny::HTML(js_code)))
}

make_question_container <- function(id, object, width) {
    # Check if question if answered
    js_interaction <- sprintf(
        "Shiny.setInputValue('%s_interacted', true, {priority: 'event'});",
        id
    )
    return(shiny::tags$div(
        id = paste0("container-", id),
        `data-question-id` = id,
        class = "question-container",
        style = sprintf("width: %s;", width),
        oninput = js_interaction,
        object,
        shiny::tags$span(class = "hidden-asterisk", "*")
    ))
}

#' Create a 'Next' Button for Page Navigation
#'
#' This function creates a 'Next' button for navigating to the specified next page in a Surveydown survey.
#' The button can be activated by clicking or by pressing the Enter key when visible.
#'
#' @param next_page Character string. The ID of the next page to navigate to. This parameter is required.
#' @param label Character string. The label of the 'Next' button. Defaults to "Next".
#'
#' @details The function generates a Shiny action button that, when clicked or when the Enter key is pressed,
#'   sets the input value to the specified next page ID, facilitating page navigation within the Shiny application.
#'   The button is styled to appear centered on the page and includes a class for Enter key functionality.
#'
#' @return A Shiny tagList containing the 'Next' button UI element.
#'
#' @examples
#' sd_next("page2", "Continue to Next Section")
#'
#' @export
sd_next <- function(next_page = NULL, label = "Next") {
  button_id <- "page_id_next"  # Placeholder ID
  shiny::tagList(
    shiny::div(
      `data-next-page` = if (!is.null(next_page)) next_page else "",
      style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
      shiny::actionButton(
        inputId = button_id,
        label = label,
        class = "sd-enter-button",
        style = "display: block; margin: auto;",
        onclick = "Shiny.setInputValue('next_page', this.parentElement.getAttribute('data-next-page'));"
      )
    )
  )
}

# Generate Next Button ID
make_next_button_id <- function(page_id) {
    return(paste0(page_id, "_next"))
}

#' Create a 'Close' Button to Exit the Survey
#'
#' This function creates a 'Close' button that, when clicked, will trigger the exit process
#' for the survey. Depending on the server-side configuration, this may show a rating question
#' or a simple confirmation dialog before attempting to close the current browser tab or window.
#'
#' @param label Character string. The label of the 'Close' button. Defaults to "Exit Survey".
#'
#' @return A Shiny tagList containing the 'Close' button UI element and associated JavaScript for the exit process.
#'
#' @details
#' The function generates a Shiny action button that, when clicked, triggers the 'show_exit_modal' event.
#' The server-side logic (controlled by the `rate_survey` parameter in `sd_server()`) determines
#' whether to show a rating question or a simple confirmation dialog.
#'
#' The function also includes a custom message handler for closing the window. This is necessary
#' because some browsers may not allow JavaScript to close windows that were not opened by JavaScript.
#' In such cases, the user will be prompted to close the tab manually.
#'
#' @note The actual behavior of the exit process (whether to show a rating question or not)
#' is controlled by the `rate_survey` parameter in the `sd_server()` function, not in this UI function.
#'
#' @examples
#' \dontrun{
#' # In your UI code:
#' sd_close()
#'
#' # With a custom label:
#' sd_close("Finish and Exit")
#' }
#'
#' @seealso \code{\link{sd_server}}
#'
#' @export
sd_close <- function(label = "Exit Survey") {
  button_id <- "close-survey-button"
  shiny::tagList(
    shiny::div(
      style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
      shiny::actionButton(
        inputId = button_id,
        label = label,
        class = "sd-enter-button",
        style = "display: block; margin: auto;",
        onclick = "Shiny.setInputValue('show_exit_modal', true, {priority: 'event'});"
      )
    ),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('closeWindow', function(message) {
        window.close();
        if (!window.closed) {
          alert('Please close this tab manually to exit the survey.');
        }
      });
    "))
  )
}

#' Create a Redirect Element for Shiny Applications
#'
#' This function creates a UI element that redirects the user to a specified URL.
#' It can be used in both reactive and non-reactive contexts within Shiny applications.
#'
#' @param id A character string of a unique id to be used to identify the redirect button in the survey body.
#' @param url A character string specifying the URL to redirect to.
#' @param button A logical value indicating whether to create a button (TRUE) or
#'   a text element (FALSE) for the redirect. Default is TRUE.
#' @param label A character string for the button or text label. Default is "Click here".
#' @param delay An optional numeric value specifying the delay in seconds before
#'   automatic redirection. If NULL (default), no automatic redirection occurs.
#' @param newtab A logical value indicating whether to open the URL in a new tab (TRUE)
#'   or in the current tab (FALSE). Default is FALSE.
#'
#' @return In a reactive context, returns a function that when called, renders the
#'   redirect element. In a non-reactive context, returns the redirect element directly.
#'
#' @importFrom shiny renderUI tags HTML actionButton
#' @importFrom digest digest
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with a button
#' sd_redirect("my_button", "https://example.com")
#'
#' # Create a text link instead of a button
#' sd_redirect("my_link", "https://example.com", button = FALSE, label = "Visit Example")
#'
#' # Add a 5-second delay before redirection
#' sd_redirect("delayed_redirect", "https://example.com", delay = 5)
#'
#' # Open the link in a new tab
#' sd_redirect("new_tab_link", "https://example.com", newtab = TRUE)
#' }
sd_redirect <- function(
        id,
        url,
        button = TRUE,
        label  = "Click here",
        delay  = NULL,
        newtab = FALSE
) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
        # In a reactive context, directly add to output with renderUI
        shiny::isolate({
            output <- shiny::getDefaultReactiveDomain()$output
            output[[id]] <- shiny::renderUI({
                create_redirect_element(id, url, button, label, delay, newtab)
            })
        })
    } else {
        # If not in a reactive context, just return the element
        return(create_redirect_element(id, url, button, label, delay, newtab))
    }
}

# Function to create the redirect element
create_redirect_element <- function(id, url, button, label, delay, newtab = FALSE) {
    # Validate URL
    if (!grepl("^https?://", url)) {
        url <- paste0("https://", url)
    }

    # Create JavaScript for redirection
    redirect_js <- if (newtab) {
        paste0("window.open('", url, "', '_blank');")
    } else {
        paste0("window.location.href = '", url, "';")
    }

    # Styling for the container
    container_style <- "
        display: inline-block;
        text-align: center;
        border: 1px solid #ddd;
        border-radius: 5px;
        padding: 0.5rem 0.5rem;
        background-color: #f9f9f9;
        margin: 0.5rem 0.5rem;
        "

    # Wrapper for centering the container
    wrapper_style <- "
        text-align: center;
        margin: 0.5rem 0.5rem;
        "

    # Create button or text element
    if (button) {
        element <- shiny::actionButton(
            inputId = id,
            label = label,
            onclick = redirect_js
        )
    } else {
        element <- shiny::span(label)
    }

    # Add automatic redirection if delay is specified
    if (!is.null(delay) && is.numeric(delay) && delay > 0) {
        countdown_id <- paste0("countdown_", id)
        element <- shiny::tagList(
            shiny::div(
                style = wrapper_style,
                shiny::div(
                    id = id,
                    style = container_style,
                    element,
                    shiny::p(
                        style = "margin: 0.5rem 0 0 0;",
                        "Redirecting in ",
                        shiny::tags$strong(id = countdown_id, delay),
                        " seconds.",
                        if (newtab) " (Opens in a new tab)" else NULL
                    )
                )
            ),
            shiny::tags$script(shiny::HTML(countdown_js(delay, redirect_js, countdown_id, id)))
        )
    } else if (!button) {
        # If there's no delay and it's not a button, we need to inform the user that no action is possible
        element <- shiny::div(
            style = wrapper_style,
            shiny::div(
                style = container_style,
                element,
                shiny::p(style = "margin: 0.5rem 0 0 0;", "Error: This text won't trigger any redirection...")
            )
        )
    } else {
        # If it's a button without delay, just wrap it in the styled container
        element <- shiny::div(
            style = wrapper_style,
            shiny::div(
                style = container_style,
                element
            )
        )
    }

    return(element)
}

# Enter Key JS
enter_key_js <- function() {
  "
    $(document).on('shiny:sessioninitialized', function() {
        $(document).on('keydown', function(event) {
            if (event.key === 'Enter' && !event.repeat) {
                var $visibleButton = $('.sd-enter-button:visible').first();
                if ($visibleButton.length) {
                    $visibleButton.click();
                    event.preventDefault();
                }
            }
        });
    });
    "
}

# Countdown JS
countdown_js <- function(delay, redirect_js, countdown_id, unique_id) {
    sprintf(
        "
    $(document).ready(function() {
      var countdown = %d;
      var countdownTimer;

      function startCountdown() {
        countdownTimer = setInterval(function() {
          countdown--;
          if (countdown <= 0) {
            clearInterval(countdownTimer);
            %s
          } else {
            $('#%s').text(countdown);
          }
        }, 1000);
      }

      // Start countdown when this element becomes visible
      var observer = new IntersectionObserver(function(entries) {
        if(entries[0].isIntersecting === true) {
          startCountdown();
          observer.disconnect();
        }
      }, { threshold: [0] });

      observer.observe(document.getElementById('%s'));
    });
    ",
        delay,
        redirect_js,
        countdown_id,
        unique_id
    )
}

#' Get URL Parameters in a Shiny Application
#'
#' This function retrieves URL parameters from the current Shiny session.
#' It must be called from within a Shiny reactive context.
#'
#' @param ... Optional. Names of specific URL parameters to retrieve.
#'   If none are specified, all URL parameters are returned.
#'
#' @return A reactive expression that returns a list of URL parameters.
#'
#' @importFrom shiny reactive getDefaultReactiveDomain parseQueryString
#' @export
#'
#' @examples
#' # Examples here
sd_get_url_pars <- function(...) {
    shiny::reactive({
        session <- shiny::getDefaultReactiveDomain()

        if (is.null(session)) {
            stop("sd_get_url_pars() must be called from within a Shiny reactive context")
        }

        full_url <- session$clientData$url_search
        parsed_query <- shiny::parseQueryString(full_url)

        requested_params <- list(...)

        if (length(requested_params) == 0) {
            return(parsed_query)
        }

        requested_params <- unlist(requested_params)
        filtered_query <- parsed_query[requested_params]
        filtered_query[!sapply(filtered_query, is.null)]
    })()
    # Extra parentheses is added so that the reactive expression is evaluated
    # when the function is called

}

#' Create a placeholder for a reactive survey question
#'
#' This function is depreciated - use `sd_output()` instead.
#'
#' @param id A unique identifier for the question.
#' @return A Shiny UI element that serves as a placeholder for the reactive question.
#'
#' @examples
#' \dontrun{
#' # Deprecated:
#' sd_display_question("name")
#'
#' # Use instead:
#' sd_output("name", type = "question")
#' }
#'
#' @export
sd_display_question <- function(id) {
  # v0.2.1
  .Deprecated("sd_output")
}

#' Display the value of a survey question
#'
#' This function is depreciated - use `sd_output()` instead.
#' @param id The ID of the question to display
#' @param display_type The type of display. Can be "inline" (default), "text", "verbatim", or "ui".
#' @param wrapper A function to wrap the output
#' @param ... Additional arguments passed to the wrapper function
#'
#' @return A Shiny UI element displaying the question's value
#'
#' @examples
#' \dontrun{
#' # Deprecated:
#' sd_display_value("name")
#' sd_display_value("age", display_type = "text")
#' sd_display_value("email", display_type = "inline", wrapper = function(x) tags$strong(x))
#'
#' # Use instead:
#' sd_output("name", type = "value")
#' sd_output("age", type = "value", display = "text")
#' sd_output("email", type = "value", display = "inline", wrapper = function(x) tags$strong(x))
#' }
#'
#' @export
sd_display_value <- function(id, display_type = "inline", wrapper = NULL, ...) {
  # v0.2.1
  .Deprecated("sd_output")
}

#' Output Function for Displaying reactive objects and values
#'
#' @param id Character string. A unique identifier for the output element.
#' @param type Character string. Specifies the type of output. Can be "question", "value", or `NULL.`
#'   If `NULL`, the function behaves like `shiny::uiOutput()`.
#' @param width Character string. The width of the UI element. Defaults to "100%".
#' @param display Character string. Specifies the display type for "value" outputs.
#'   Can be "inline", "text", "verbatim", or "ui". Only used when `type = "value"`.
#' @param wrapper Function. A function to wrap the output. Only used when `type = "value"`.
#' @param ... Additional arguments passed to the underlying Shiny functions or the wrapper function.
#'
#' @return A Shiny UI element, the type of which depends on the input parameters.
#'
#' @details
#' The function behaves differently based on the `type` parameter:
#' - If `type` is `NULL`, it acts like `shiny::uiOutput()`.
#' - If `type` is `"question"`, it creates a placeholder for a reactive survey question.
#' - If `type` is `"value"`, it creates an output to display the value of a survey question,
#'   with the display style determined by the `display` parameter.
#'
#' @examples
#' \dontrun{
#' # Create a placeholder for a reactive question
#' sd_output('cbc1', type = 'question')
#'
#' # Display the value of a survey question inline
#' sd_output('cbc1', type = 'value', display = 'inline')
#'
#' # Use as a simple uiOutput
#' sd_output('redirect')
#'
#' # Use with a wrapper function
#' sd_output('age', type = 'value', display = 'text',
#'           wrapper = function(x) tags$strong(x))
#' }
#'
#' @export
sd_output <- function(
    id,
    type = NULL,
    width = "100%",
    display = "inline",
    wrapper = NULL,
    ...
) {
    if (is.null(type)) {
        # If only id is provided, behave like shiny::uiOutput
        return(shiny::uiOutput(id, ...))
    }

    if (type == "question") {
        return(make_question_container(id, shiny::uiOutput(id), width))
    }

    if (type == "value") {
        value_id <- paste0(id, "_value")

        if (!is.null(display) && !display %in% c("inline", "text", "verbatim", "ui")) {
            stop("Invalid display type. Choose 'inline', 'text', 'verbatim', or 'ui'.")
        }

        output <- switch(
            display,
            "inline" = shiny::textOutput(value_id, inline = TRUE),
            "text" = shiny::textOutput(value_id),
            "verbatim" = shiny::verbatimTextOutput(value_id),
            "ui" = shiny::uiOutput(value_id),
            shiny::uiOutput(value_id)  # Default to uiOutput if display is not specified
        )

        if (!is.null(wrapper)) {
            output <- wrapper(output, ...)
        }

        return(output)
    }

    stop("Invalid type. Choose 'question' or 'value'.")
}

#' Generate a Random Completion Code
#'
#' This function generates a random completion code with a specified number of digits.
#' The code is returned as a character string.
#'
#' @param digits An integer specifying the number of digits in the completion code.
#'   Must be a positive integer. Default is 6.
#'
#' @return A character string representing the random completion code.
#'
#' @examples
#' sd_completion_code()  # generates a 6-digit code
#' sd_completion_code(digits = 8)  # generates an 8-digit code
#' sd_completion_code(digits = 4)  # generates a 4-digit code
#' sd_completion_code(digits = 10)  # generates a 10-digit code
#'
#' @export
sd_completion_code <- function(digits = 6) {
    if (!is.numeric(digits) || digits < 1 || digits != round(digits)) {
        stop("'digits' must be a positive integer")
    }

    # Generate random digits
    digits_vector <- sample(0:9, digits, replace = TRUE)

    # Ensure the first digit is not 0
    digits_vector[1] <- sample(1:9, 1)

    # Combine into a single string
    paste(digits_vector, collapse = "")
}

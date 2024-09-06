#' Create a survey question
#'
#' This function creates various types of survey questions for use in a Surveydown survey.
#'
#' @param type Specifies the type of question. Possible values are "select", "mc", "mc_multiple", "mc_buttons", "mc_multiple_buttons", "text", "textarea", "numeric", "slider", "date", and "daterange".
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
#' @param justified Logical. Whether buttons in a group should be justified. Defaults to FALSE.
#' @param force_edges Logical. Whether to force edges for slider input. Defaults to TRUE.
#' @param option List. Options for the select, radio, checkbox, and slider inputs.
#' @param placeholder Character string. Placeholder text for text and textarea inputs.
#' @param resize Character string. Resize option for textarea input. Defaults to NULL.
#' @param reactive Logical. Whether the question should be reactive. Defaults to `FALSE`.
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
#'
#' @return A Shiny UI element wrapped in a div with a  data attribute for question ID.
#'
#' @examples
#' sd_question("text", "name", "What is your name?")
#' sd_question("mc", "color", "What is your favorite color?", option = c("Red", "Blue", "Green"))
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
        reactive     = FALSE
) {

    output <- NULL

    # Check if question if answered
    js_interaction <- sprintf("Shiny.setInputValue('%s_interacted', true, {priority: 'event'});", id)

    # Always add red asterisk for required questions, but hide it initially
    label <- paste0(
        label,
        " <span class='required-asterisk' style='display:none; color: red; font-size: 1.5em; vertical-align: middle; position: relative; top: 0.1em;'>*</span>"
    )

    if (type ==  "select") {
        option <- c("", option)
        names(option)[1] <- label_select

        output <- shiny::selectInput(
            inputId  = id,
            label    = markdown_to_html(label),
            width    = width,
            choices  = option,
            multiple = FALSE,
            selected = FALSE
        )

    } else if (type == "mc") {

        output <- shiny::radioButtons(
            inputId  = id,
            label    = markdown_to_html(label),
            width    = width,
            choices  = option,
            selected = FALSE
        )

    } else if (type == "mc_multiple") {

        output <- shiny::checkboxGroupInput(
            inputId  = id,
            label    = markdown_to_html(label),
            width    = width,
            choices  = option,
            selected = FALSE
        )

    } else if (type == "mc_buttons") {

        output <- shinyWidgets::radioGroupButtons(
            inputId  = id,
            label    = markdown_to_html(label),
            choices  = list_name_md_to_html(option),
            selected = character(0),
            width    = width,
            status   = status
        )

    } else if (type == "mc_multiple_buttons") {

        output <- shinyWidgets::checkboxGroupButtons(
            inputId    = id,
            label      = markdown_to_html(label),
            choices    = list_name_md_to_html(option),
            direction  = direction,
            individual = individual,
            justified  = justified,
            width      = width
        )

    } else if (type == "text") {

        output <- shiny::textInput(
            inputId     = id,
            label       = markdown_to_html(label),
            width       = width,
            placeholder = option
        )

    } else if (type == "textarea") {

        output <- shiny::textAreaInput(
            inputId     = id,
            label       = markdown_to_html(label),
            width       = width,
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
            label   = markdown_to_html(label),
            width   = width,
            value   = NULL
        )

    } else if (type == "slider") {

        output <- shinyWidgets::sliderTextInput(
            inputId      = id,
            label        = markdown_to_html(label),
            width        = width,
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
            label              = markdown_to_html(label),
            width              = width,
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
            label     = markdown_to_html(label),
            width     = width,
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

    }

    # Wrap the output in a div with custom data attributes
    output_div <- shiny::tags$div(
        id = paste("container-", id),
        `data-question-id` = id,
        class = "question-container",
        oninput = js_interaction,
        output
    )

    if (reactive) {

        shiny::isolate({
            output <- shiny::getDefaultReactiveDomain()$output
            if (!is.null(output)) {
                output[[id]] <- shiny::renderUI({
                    output_div
                })
            } else {
                stop("If reactive = TRUE, sd_question must be called within a Shiny reactive context")
            }
        })

    } else {
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

#' Create a placeholder for a reactive survey question
#'
#' This function creates a placeholder div for a reactive survey question in a Surveydown survey.
#' It's used in conjunction with sd_question_reactive to allow for dynamic question rendering.
#'
#' @param id A unique identifier for the question.
#'
#' @return A Shiny UI element that serves as a placeholder for the reactive question.
#'
#' @examples
#' sd_display_question("name")
#'
#' @export
sd_display_question <- function(id) {
    shiny::div(
        id = paste0("placeholder-", id),
        `data-question-id` = id,
        class = "question-container reactive-question-placeholder",
        shiny::uiOutput(id)
    )
}

#' Display the value of a survey question
#'
#' @param id The ID of the question to display
#' @param display_type The type of display. Can be "inline" (default), "text", "verbatim", or "ui".
#' @param wrapper A function to wrap the output
#' @param ... Additional arguments passed to the wrapper function
#'
#' @return A Shiny UI element displaying the question's value
#'
#' @examples
#' sd_display_value("name")
#' sd_display_value("age", display_type = "text")
#' \dontrun{
#'   sd_display_value("email", display_type = "inline", wrapper = function(x) tags$strong(x))
#' }
#'
#' @export
sd_display_value <- function(id, display_type = "inline", wrapper = NULL, ...) {
    value_id <- paste0(id, "_value")

    output <- switch(
        display_type,
        "inline" = shiny::textOutput(value_id, inline = TRUE),
        "text" = shiny::textOutput(value_id),
        "verbatim" = shiny::verbatimTextOutput(value_id),
        "ui" = shiny::uiOutput(value_id),
        stop("Invalid display_type. Choose 'inline', 'text', 'verbatim', or 'ui'.")
    )

    if (!is.null(wrapper)) {
        output <- wrapper(output, ...)
    }

    return(output)
}

#' Create a 'Next' Button for Page Navigation
#'
#' This function creates a 'Next' button for navigating to the specified next page in a Surveydown survey.
#' The button can be activated by clicking or by pressing the Enter key.
#'
#' @param next_page Character string. The ID of the next page to navigate to. This parameter is required.
#' @param label Character string. The label of the 'Next' button. Defaults to "Next".
#'
#' @details The function generates a Shiny action button that, when clicked or when the Enter key is pressed,
#'   sets the input value to the specified next page ID, facilitating page navigation within the Shiny application.
#'   The button is styled to appear centered on the page. The Enter key functionality is only active when the button is visible.
#'
#' @return A Shiny action button UI element with associated JavaScript for Enter key functionality.
#'
#' @examples
#' sd_next("page2", "Continue to Next Section")
#'
#' @export
sd_next <- function(next_page = NULL, label = "Next") {
    if (is.null(next_page)) {
        stop("You must specify the current_page for the 'Next' button.")
    }

    button_id <- make_next_button_id(next_page)

    shiny::tagList(
        shiny::actionButton(
            inputId = button_id,
            label = label,
            style = "display: block; margin: auto;",
            onclick = sprintf("Shiny.setInputValue('next_page', '%s');", next_page)
        ),
        shiny::tags$script(shiny::HTML(enter_key_js(button_id)))
    )
}

# Generate Next Button ID
make_next_button_id <- function(next_page) {
    return(paste0("next-", next_page))
}

#' Redirect button
#'
#' This function creates a UI element for redirecting to external links. It can provide a clickable button,
#' automatic redirection after a delay, or both. When a button is created, it can be activated by clicking
#' or by pressing the Enter key.
#'
#' @param url Character string. The target URL for redirection.
#' @param button Logical. If TRUE, creates a clickable button. If FALSE, creates non-clickable text. Defaults to TRUE.
#' @param label Character string. The text for the button or display. Defaults to "Click here".
#' @param delay Numeric. The delay in seconds before automatic redirection. If NULL, no automatic redirection occurs. Defaults to NULL.
#'
#' @return A Shiny UI element (button or text) with redirection functionality, horizontally centered and styled.
#'   If a button is created, it includes JavaScript for Enter key functionality.
#'
#' @details
#' When `button = TRUE`, the function creates a clickable button that can be activated by mouse click or by pressing the Enter key.
#' The Enter key functionality is only active when the button is visible on the page.
#' If a delay is specified, the function will initiate an automatic redirection after the specified number of seconds.
#'
#' @examples
#' sd_redirect(
#'   url = "https://www.google.com",
#'   button = TRUE,
#'   label = "Go to Google",
#'   delay = 5)
#' sd_redirect(
#'   url = "https://www.example.com",
#'   button = FALSE,
#'   label = "Redirecting to Example",
#'   delay = 10
#' )
#'
#' @importFrom digest digest
#' @export
sd_redirect <- function(url, button = TRUE, label = "Click here", delay = NULL) {
    # Validate URL
    if (!grepl("^https?://", url)) {
        url <- paste0("https://", url)
    }

    # Create JavaScript for redirection
    redirect_js <- sprintf("window.location.href = '%s';", url)

    # Create a unique ID for this instance of sd_redirect
    unique_id <- paste0("redirect_", digest::digest(paste(url, label, delay), algo = "md5"))

    # Styling for the container
    container_style <- "
    display: inline-block;
    text-align: center;
    border: 1px solid #ddd;
    border-radius: 5px;
    padding: 0.5rem 1rem;
    background-color: #f9f9f9;
    margin: 0.5rem 0;
  "

    # Wrapper for centering the container
    wrapper_style <- "
    text-align: center;
    margin: 1rem 0;
  "

    # Create button or text element
    if (button) {
        button_id <- paste0("button_", unique_id)
        element <- shiny::tagList(
            shiny::actionButton(
                inputId = button_id,
                label = label,
                onclick = redirect_js
            ),
            shiny::tags$script(shiny::HTML(enter_key_js(button_id)))
        )
    } else {
        element <- shiny::span(label)
    }

    # Add automatic redirection if delay is specified
    if (!is.null(delay) && is.numeric(delay) && delay > 0) {
        countdown_id <- paste0("countdown_", unique_id)

        element <- shiny::tagList(
            shiny::div(
                style = wrapper_style,
                shiny::div(
                    id = unique_id,
                    style = container_style,
                    element,
                    shiny::p(
                        style = "margin: 0.5rem 0 0 0;",
                        "Redirecting in ",
                        shiny::tags$strong(id = countdown_id, delay),
                        " seconds."
                    )
                )
            ),
            shiny::tags$script(shiny::HTML(countdown_js(delay, redirect_js, countdown_id, unique_id)))
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
enter_key_js <- function(button_id) {
    sprintf("
    $(document).ready(function() {
        var buttonId = '%s';
        $(document).on('keydown', function(event) {
            if (event.key === 'Enter' && !event.repeat) {
                var $button = $('#' + buttonId);
                if ($button.is(':visible')) {
                    $button.click();
                    event.preventDefault();
                }
            }
        });
    });
    ", button_id)
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

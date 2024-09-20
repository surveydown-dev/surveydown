# Load multiple resource files (CSS or JS)
load_resources <- function(files, type = c("css", "js"), package = "surveydown") {
    type <- match.arg(type)
    sapply(files, function(file) {
        path <- system.file(paste0(type, "/", file), package = package)
        if (type == "css") {
            shiny::includeCSS(path)
        } else {
            shiny::tags$script(src = path)
        }
    }, simplify = FALSE, USE.NAMES = FALSE)
}

#' Create the UI for a surveydown survey
#'
#' This function creates the user interface for a surveydown survey,
#' including necessary CSS and JavaScript files, and applies custom styling.
#'
#' @param barcolor Color of the progress bar. Can be a hex color or 'theme' to use the theme's primary color. Default is 'theme'.
#' @param barposition Position of the progress bar. Can be 'top', 'bottom', or 'none'. Default is 'top'.
#' @param theme The name of the Bootswatch theme to use. Default is NULL.
#' @param theme_color The color theme to use. If specified, overrides the color from the Bootswatch theme. Default is NULL.
#' @param theme_font The font theme to use. If specified, overrides the font from the Bootswatch theme. Default is NULL.
#' @param background Background color of the body. Default is '#f2f6f9'.
#' @param custom_css Path to a custom CSS file to override default styles. Default is NULL.
#'
#' @return A Shiny UI object
#' @export
#'
#' @examples
#' \dontrun{
#' sd_ui()
#' sd_ui(barcolor = "#FF0000", barposition = "bottom", theme = "flatly")
#' sd_ui(theme_color = "darkly", theme_font = "roboto", background = "#FFFFFF")
#' }
sd_ui <- function(
        barcolor    = 'theme',
        barposition = 'top',
        theme       = NULL,
        theme_color = NULL,
        theme_font  = NULL,
        background  = '#f2f6f9',
        custom_css  = NULL
) {
    # Helper function to convert to lowercase if not NULL
    to_lower <- function(x) if (!is.null(x)) tolower(x) else NULL

    # Convert inputs to lowercase
    theme       <- to_lower(theme)
    theme_color <- to_lower(theme_color)
    theme_font  <- to_lower(theme_font)

    # Determine color and font sources
    color_source <- ifelse(!is.null(theme_color), theme_color,
                           ifelse(!is.null(theme), theme, "cosmo"))
    font_source  <- ifelse(!is.null(theme_font), get_theme_font(theme_font),
                           ifelse(!is.null(theme), get_theme_font(theme), "Raleway"))
    font_source_with_fallback <- paste(font_source, "sans-serif", sep = ", ")

    # Determine progress bar color
    progress_color <- if (barcolor == 'theme' || !grepl("^#[0-9A-Fa-f]{6}$", barcolor)) {
        sprintf("var(--%s-color)", color_source)
    } else {
        barcolor
    }

    # Custom CSS rule
    custom_css_rule <- sprintf("
    :root {
      --theme-color: %s;
      --theme-font: %s;
      --body-background-color: %s;
    }
    body {
      font-family: var(--theme-font);
    }
  ", progress_color, font_source_with_fallback, background)

    shiny::fluidPage(
        # theme = bslib::bs_theme(version = 5, bootswatch = theme),
        shinyjs::useShinyjs(),
        # load_resources("surveydown.css", type = "css"),
        # load_resources("fonts.css", type = "css"),
        # shiny::tags$style(HTML(custom_css_rule)),
        if (!is.null(custom_css)) shiny::includeCSS(custom_css),
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

get_theme_font <- function(theme) {
    theme_font_mapping <- list(
        cerulean  = "Helvetica Neue, Helvetica, Arial",
        cosmo     = "Source Sans Pro",
        cyborg    = "Roboto",
        darkly    = "Lato",
        flatly    = "Lato",
        journal   = "News Cycle",
        litera    = "Roboto",
        lumen     = "Source Sans Pro",
        lux       = "Nunito Sans",
        materia   = "Roboto",
        minty     = "Montserrat",
        morph     = "Inter",
        pulse     = "Roboto",
        quartz    = "DM Sans",
        readable  = "Raleway",
        sandstone = "Roboto",
        simplex   = "Open Sans",
        sketchy   = "Neucha",
        slate     = "Roboto",
        spacelab  = "Open Sans",
        superhero = "Lato",
        united    = "Ubuntu",
        vapor     = "Open Sans",
        yeti      = "Open Sans"
    )

    if (is.null(theme) || !theme %in% names(theme_font_mapping)) {
        return("Raleway")
    } else {
        return(theme_font_mapping[[theme]])
    }
}

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
  resize       = NULL
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

        js <- sprintf("
      $('#%s .btn').on('click', function(e) {
        e.preventDefault();
        $('#%s .btn').removeClass('active');
        $(this).addClass('active');
        var $input = $(this).find('input[type=\"radio\"]');
        $input.prop('checked', true);
        $input.trigger('change');
        Shiny.setInputValue('%s_interacted', true, {priority: 'event'});
      });
      $('#%s .btn input:checked').closest('.btn').addClass('active');
    ", id, id, id, id)
        output <- shiny::tagAppendChild(output, shiny::tags$script(shiny::HTML(js)))

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

        js <- sprintf("
      $('#%s .btn').on('click', function() {
        $(this).toggleClass('active');
      });
    ", id)
        output <- shiny::tagAppendChild(output, shiny::tags$script(shiny::HTML(js)))

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
        id = paste0("container-", id),
        `data-question-id` = id,
        class = "question-container",
        oninput = js_interaction,
        output
    )

    if (!is.null(shiny::getDefaultReactiveDomain())) {
        # In a reactive context, directly add to output with renderUI
        shiny::isolate({
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
    button_id <- "page_id_next"  # Placeholder ID
    shiny::tagList(
        shiny::div(
            `data-next-page` = if (!is.null(next_page)) next_page else "",
            style = "margin-top: 0.5rem; margin-bottom: 0.5rem;",
            shiny::actionButton(
                inputId = button_id,
                label = label,
                style = "display: block; margin: auto;",
                onclick = "Shiny.setInputValue('next_page', this.parentElement.getAttribute('data-next-page'));"
            )
        ),
        shiny::tags$script(shiny::HTML(enter_key_js(button_id)))
    )
}

# Generate Next Button ID
make_next_button_id <- function(page_id) {
    return(paste0(page_id, "_next"))
}

#' Create a 'Close' Button to Exit the Survey
#'
#' This function creates a 'Close' button that, when clicked, will close the current browser tab or window.
#' The button can be activated by clicking or by pressing the Enter key.
#'
#' @param label Character string. The label of the 'Close' button. Defaults to "Exit Survey".
#'
#' @details The function generates a Shiny action button that, when clicked or when the Enter key is pressed,
#'   will attempt to close the current browser tab or window. Note that for security reasons,
#'   some browsers may not allow JavaScript to close windows that were not opened by JavaScript.
#'   In such cases, the button will prompt the user to close the tab manually.
#'
#' @return A Shiny action button UI element with associated JavaScript for closing the page and Enter key functionality.
#'
#' @examples
#' sd_close()
#' sd_close("Exit Survey")
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
                style = "display: block; margin: auto;",
                onclick = "window.close(); if (!window.closed) { alert('Please close this tab manually to exit the survey.'); }"
            )
        ),
        shiny::tags$script(shiny::HTML(enter_key_js(button_id)))
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
}

#' Create a placeholder for a reactive survey question
#'
#' This function creates a placeholder div for a reactive survey question in a Surveydown survey.
#' It's used in conjunction with sd_question to allow for dynamic question rendering.
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
sd_output <- function(id, type = NULL, display = "inline", wrapper = NULL, ...) {
    if (is.null(type)) {
        # If only id is provided, behave like shiny::uiOutput
        return(shiny::uiOutput(id, ...))
    }

    if (type == "question") {
        return(shiny::div(
            id = paste0("placeholder-", id),
            `data-question-id` = id,
            class = "question-container reactive-question-placeholder",
            shiny::uiOutput(id)
        ))
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

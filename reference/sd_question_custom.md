# Create a Custom Question with a Shiny Widget

This function creates a custom survey question that incorporates any
Shiny widget and captures its interaction value. It allows for the
integration of interactive visualizations (e.g., maps, plots) or other
custom Shiny outputs into a survey, storing the result of user
interaction as survey data.

## Usage

``` r
sd_question_custom(id, label, output, value, height = "400px")
```

## Arguments

- id:

  Character string. A unique identifier for the question.

- label:

  Character string. The label text for the question, which can include
  HTML formatting.

- output:

  Shiny UI element. The output of a Shiny widget (e.g.,
  [`leafletOutput()`](https://rstudio.github.io/leaflet/reference/map-shiny.html),
  `plotlyOutput()`).

- value:

  Reactive expression that returns the value to be stored in the survey
  data when the user interacts with the widget.

- height:

  Character string. The height of the widget output. Defaults to
  "400px".

## Value

None (called for side effects)

## Details

The function creates a custom question container that includes:

- A visible widget output that users can interact with

- A hidden text input that stores the value from the interaction

- Automatic tracking of user interaction for progress monitoring

The value to be stored is controlled by the reactive expression provided
to the `value` parameter, which should update whenever the user
interacts with the widget in the desired way.

## See also

[`sd_question()`](https://pkg.surveydown.org/reference/sd_question.md)
for standard question types

## Examples

``` r
if (interactive()) {
  library(surveydown)
  library(leaflet)

  server <- function(input, output, session) {
    # Create map output
    output$usa_map <- renderLeaflet({
      leaflet() |>
        addTiles() |>
        setView(lng = -98.5795, lat = 39.8283, zoom = 4)
    })

    # Reactive value for selected location
    selected_location <- reactiveVal(NULL)

    # Click observer
    observeEvent(input$usa_map_click, {
      click <- input$usa_map_click
      if (!is.null(click)) {
        selected_location(
          sprintf("Lat: %0.2f, Lng: %0.2f", click$lat, click$lng)
        )
      }
    })

    # Create the custom question
    sd_question_custom(
      id = "location",
      label = "Click on your location:",
      output = leafletOutput("usa_map", height = "400px"),
      value = selected_location
    )

    sd_server()
  }

  shinyApp(ui = sd_ui(), server = server)
}
```

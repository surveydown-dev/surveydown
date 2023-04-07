library(shiny)

# Define UI for each page
page1 <- fluidPage(
  titlePanel("Page 1"),
  sidebarLayout(
    sidebarPanel(
      h4("Sidebar panel for page 1")
    ),
    mainPanel(
      h2("Main panel for page 1")
    )
  )
)

page2 <- fluidPage(
  titlePanel("Page 2"),
  sidebarLayout(
    sidebarPanel(
      h4("Sidebar panel for page 2")
    ),
    mainPanel(
      h2("Main panel for page 2")
    )
  )
)

page3 <- fluidPage(
  titlePanel("Page 3"),
  sidebarLayout(
    sidebarPanel(
      h4("Sidebar panel for page 3")
    ),
    mainPanel(
      h2("Main panel for page 3")
    )
  )
)

# Define UI for the app
ui <- fluidPage(
  # Use a conditional panel to show only page 1 initially
  conditionalPanel(
    condition = "input.nextBtn == 0",
    page1
  ),
  # Use another conditional panel to show only page 2 after the "next" button is clicked
  conditionalPanel(
    condition = "input.nextBtn > 0 && input.nextBtn < 2",
    page2
  ),
  # Use another conditional panel to show only page 3 after the "next" button is clicked twice
  conditionalPanel(
    condition = "input.nextBtn > 1",
    page3
  ),
  # Define a "next" button that will show page 2 when clicked
  actionButton("nextBtn", "Next")
)


# Define server logic
server <- function(input, output) {
  # No server logic needed for this example
}

# Run the app
shinyApp(ui, server)

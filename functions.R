# The question function to render a Shiny input based on question type
question <- function(type, input_id, label, choices, ...) {
  switch(type,
         text = textInput(input_id, label, ...),
         select = selectInput(input_id, label, choices, ...),
         checkbox = checkboxGroupInput(input_id, label, ...),
         stop("Unknown type")
  )
}

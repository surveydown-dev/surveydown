library(shiny)

devtools::load_all()

render(file = file.path('inst', 'survey.Rmd'))

preview(ui = file.path('inst', 'survey.html'))

host(
  folder = 'survey',
  data_url = 'path_to_googlesheet',
  api_key = 'api_key'
)


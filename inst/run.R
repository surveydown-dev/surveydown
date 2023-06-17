devtools::load_all()

render(file = file.path('inst', 'survey.Rmd'))

surveydown::preview(file = 'survey.Rmd')

surveydown::host(
  folder = 'survey',
  data_url = 'path_to_googlesheet',
  api_key = 'api_key'
)

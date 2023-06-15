
surveydown::render_survey(file = 'survey.Rmd')

surveydown::host_survey(
  folder = 'survey',
  data_url = 'path_to_googlesheet',
  api_key = 'api_key'
)
- [x] Question dependency (showif)
- [x] Skip logic
- [] Set defaults for questions to not have any choices selected on launch.
- [] Required questions (require = TRUE): check that a response is present for any required questions before allowing next button (need to solve the default no selection first - the one above this one).
- [] Form validation: Make sure the user inputs the correct type depending on the question type.
- [] Admin page w/password (see https://github.com/daattali/shinyforms)
- [] Decide on db backend: googlesheets vs. supabase?
- [] Basic version working with a database
- [] User tracking via url parameters: https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html#user-tracking
- [] Add a getSurveyData() function so the survey designer can obtain the current survey results from inside the app: https://shinysurveys.jdtrat.com/articles/get-survey-data.html
- [] Ability to embed markdown inside choice options (like mc buttons in formr)
- [] Consider using formr forms (or other forms) instead of shiny widgets?
- Question types:
  - [x] Multiple choice (single)
  - [] Multiple choice (multiple selection)
  - [] Multiple choice (button)
  - [x] Select
  - [x] Text
  - [] Text area
  - [] Numeric
  - [] Matrix
  - [] Slider
  - [] Date


Resources / examples:

- [shinysurveys](https://github.com/jdtrat/shinysurveys)
- [shinyforms](https://github.com/daattali/shinyforms)
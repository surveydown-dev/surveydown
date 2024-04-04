This is a prototype of a way to create surveys using [Quarto shiny documents](https://quarto.org/docs/dashboards/interactivity/shiny-r.html).

Note that it is very much not production ready - it doesn't even save the survey data anywhere yet. For now it just writes to a local csv for purposes of testing and development.

If you want to try running the demo, download / clone / fork the main branch, open the surveydown.Rproj file to open RStudio, then open the survey.qmd file. You should just be able to click the "Run Document" button at the top. You need to have the {shiny} and {shinyjs} packages installed. If you want to run it using just the command line, clone the fences branch then follow instructions [here](https://quarto.org/docs/interactive/shiny/running.html).

It may also be worth reading my [blog post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/) on this idea from a while ago. The post is now outdated in terms of the UI I had in mind back then, but the motivation for developing something like this remains.

TODO List:

- [x] show_if (conditionally display question)
- [x] skip_if (conditionally skip to page)
- [x] Set defaults for questions to not have any choices selected on launch.
- [x] Ability to embed markdown inside choice options (like mc buttons in formr)
- [ ] Option for `preview = TRUE` in `sd_server()` (database is ignored).
- [ ] Option to start at a designated page, e.g. `start_at = 'skipif'`
- [ ] Include input checks for `skip_if` and `show_if` (`question_id` exists, and data frame names are correct)
- [ ] Required questions (require = TRUE): check that a response is present for any required questions before allowing next button.
- [ ] Form validation: Make sure the user inputs the correct type depending on the question type. (see https://shiny.posit.co/r/reference/shiny/0.14/validate.html)
- [ ] Admin page w/password (see https://github.com/daattali/shinyforms)
- [ ] Decide on database backend: googlesheets vs. supabase?
- [ ] Basic version working with a database
- [ ] User tracking via url parameters: https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html#user-tracking
- [ ] Add a getSurveyData() function so the survey designer can obtain the current survey results from inside the app: https://shinysurveys.jdtrat.com/articles/get-survey-data.html
- Question types:
  - [x] Multiple choice (single choice)
  - [x] Multiple choice (multiple choices)
  - [x] Select
  - [x] Text
  - [x] Numeric
  - [x] Multiple choice (button...like formr mc_button)
  - [ ] Text area
  - [ ] Matrix
  - [ ] Slider
  - [ ] Date

Resources / examples:

- [shinysurveys](https://github.com/jdtrat/shinysurveys)
- [shinyforms](https://github.com/daattali/shinyforms)

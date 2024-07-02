
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveydown <a href='https://jhelvy.github.io/surveydown/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

surveydown: Markdown-Based Surveys With [Quarto Shiny
Documents](https://quarto.org/docs/dashboards/interactivity/shiny-r.html).

This is still a very early-stage project and not quite production ready.
The general concept is to leverage Quarto shiny documents and the
{surveydown} package (along with a Quarto extension we’ve developed) to
create surveys using markdown and R code chunks. Surveys are defined as
Quarto documents that render to shiny apps. Survey data collected from
respondents is stored in a [supabase](https://supabase.com/) database.

It may also be worth reading my [blog
post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/) on this
idea to get a sense for what we’re trying to accomplish. The post is now
outdated in terms of the UI I had in mind back then, but the motivation
for developing something like this remains.

TODO List:

- [x] show_if (conditionally display question)
- [x] skip_if (conditionally skip to page)
- [x] Set defaults for questions to not have any choices selected on
  launch.
- [x] Ability to embed markdown inside choice options (like mc buttons
  in formr)
- [x] Option for `preview = TRUE` (database is ignored)
- [x] Export timestamps on each page in the data
- [x] Export timestamps on each question interaction (since this will
  increase the data size considerably, maybe add this as
  `question_times = FALSE` argument)
- [x] Option to start at a designated page, e.g. `start_page = 'skipif'`
- [x] A `show_all = TRUE` argument to show all the pages and hidden
  questions when launched (e.g. to be able to print out the entire
  survey text). Could also be called `print_mode = TRUE`.
- [x] Set up SCSS to be compatible with Quarto-supported bootstrap
  themes
- [ ] Include input checks for `skip_if` and `show_if` (`question_id`
  exists, and data frame names are correct)
- \[?\] Add a progress bar option in `sd_config()`, maybe
  `progress_bar = 'top'` or `progress_bar = 'bottom'` as a default, and
  `progress_bar = 'none'` to turn it off.
- [ ] Required questions (`required = TRUE`): post a popup if a question
  is required before allowing next button. Default should be
  `required = FALSE`.
- [ ] Form validation: Make sure the user inputs the correct type
  depending on the question type. (see
  <https://shiny.posit.co/r/reference/shiny/0.14/validate.html>)
- [ ] Admin page w/password (see
  <https://github.com/daattali/shinyforms>)
- [ ] Basic version working with a googlesheets database
- [ ] User tracking via url parameters:
  <https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html#user-tracking>
- [ ] Add a getSurveyData() function so the survey designer can obtain
  the current survey results from inside the app:
  <https://shinysurveys.jdtrat.com/articles/get-survey-data.html>
- Question types:
  - [x] Multiple choice (single choice)
  - [x] Multiple choice (multiple choices)
  - [x] Select
  - [x] Text
  - [x] Numeric
  - [x] Multiple choice (button…like formr mc_button)
  - [x] Text area
  - [x] Date
  - [ ] Matrix
  - [ ] Slider

Resources / examples:

- [shinysurveys](https://github.com/jdtrat/shinysurveys)
- [shinyforms](https://github.com/daattali/shinyforms)
- [Example](https://rtask.thinkr.fr/pimping-your-shiny-app-with-a-javascript-library-an-example-using-sweetalert2/)
  of adding JS modules to shiny app (pop up message) - could be useful
  for adding any number of JS-based features, like pop ups for required
  questions, JS-based progress bar, etc.

## [License Information](https://github.com/jhelvy/surveydown/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, please cite it! You can
get the citation by typing `citation("surveydown")` into R:

``` r
citation("surveydown")
```

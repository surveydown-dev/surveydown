
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveydown <a href='https://jhelvy.github.io/surveydown/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

### Markdown-Based Surveys With [Quarto Shiny Documents](https://quarto.org/docs/dashboards/interactivity/shiny-r.html).

> Note: This is still a very early-stage project and not quite
> production ready, though we’re happy if you’d like to try it out and
> give us feedback!

<br>
<center>
<img src='man/figures/technologies.png' align="center" style="max-height:200px;"/>
</center>

<br>

This project brings together three open source technologies
([Quarto](https://quarto.org/), [shiny](https://shiny.posit.co/), and
[supabase](https://supabase.com/)) to create dynamic, markdown-based
surveys. Here’s the basic concept:

1.  Design your survey as a [Quarto shiny
    document](https://quarto.org/docs/dashboards/interactivity/shiny-r.html)
    using markdown and R code.
2.  Render your doc into a [shiny](https://shiny.posit.co/) app that can
    be hosted online and sent to respondents.
3.  Store your survey responses in a [supabase](https://supabase.com/)
    database.

The {surveydown} R package works in tandem with our [surveydown Quarto
extension](https://github.com/jhelvy/surveydown-ext) to make everything
work. After installing the package, we recommend working from one of our
template example surveys to design your own.

**See the [documentation page](https://jhelvy.github.io/surveydown/) to
get started making your own surveydown survey!**

# Background & Motivation

`surveydown` was created as a markdown-based alternative to platforms
like Google Forms, Qualtrics, Survey Monkey, etc. Unlike every other
platform, it allows the user to define their entire survey content using
markdown and R code, making the survey itself fully reproducible and
easy to share and collaborate with others. The resulting shiny app for
each survey can be hosted on a number of platforms, like
[shinyapps.io](https://shinyapps.io/) or
[huggingface](https://huggingface.co/), and the survey data collected is
owned by the survey designer in their supabase account. Best of all,
everything is open source and free :)

If you’re curious where this whole idea came from, check out this [blog
post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/), which
outlines more on the general idea and the motivation for it. The post is
now outdated in terms of the overall design, but it provides something
of an origin story and some of the motivation for developing this
project.

# Quick Start

Install the {surveydown} **R package** (not available on CRAN yet):

``` r
# install.packages("remotes")
remotes::install_github("jhelvy/surveydown")
```

To start with an **example survey** (which includes the extension), run:

``` r
# To install the survey under your current path
surveydown::sd_create_survey()

# To designate a path inside your current path
surveydown::sd_create_survey("path/to/folder")
```

# TODO List

This is a running list of things we’re working on adding to the project:

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
- [x] Option to start at a designated page,
  e.g. `start_page = 'page_name'`
- [x] A `show_all = TRUE` argument to show all the pages and hidden
  questions when launched (e.g. to be able to print out the entire
  survey text). Could also be called `print_mode = TRUE`.
- [x] Set up SCSS to be compatible with Quarto-supported bootstrap
  themes
- [x] Add an optional progress bar
- [x] Include input checks for `skip_if` and `show_if` (`question_id`
  exists, and data frame names are correct)
- [x] Required questions (`required = TRUE`): post a popup if a question
  is required before allowing next button. Default should be
  `required = FALSE`.
- [ ] Form validation: Make sure the user inputs the correct type
  depending on the question type. (see
  <https://shiny.posit.co/r/reference/shiny/0.14/validate.html>)
- [ ] Admin page w/password to preview / download data (see
  <https://github.com/daattali/shinyforms>)
- [ ] User tracking via url parameters:
  <https://shinysurveys.jdtrat.com/articles/surveying-shinysurveys.html#user-tracking>
- [ ] Deal with cookies so users who close the browser can start back
  where they left off.
- [ ] Add a `sd_get_data()` function so the survey designer can obtain
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
  - \[?\] Slider

Resources / examples:

- [shinysurveys](https://github.com/jdtrat/shinysurveys)
- [shinyforms](https://github.com/daattali/shinyforms)
- [Example](https://rtask.thinkr.fr/pimping-your-shiny-app-with-a-javascript-library-an-example-using-sweetalert2/)
  of adding JS modules to shiny app (pop up message) - could be useful
  for adding any number of JS-based features, like pop ups for required
  questions, JS-based progress bar, etc.

# Documentation TODO

Vignettes for each of the following:

- [x] Quick overview / basic usage
- [x] List of question types and examples
- [x] Question formatting options
- [x] Flow control (conditional skipping and conditional display)
- [x] Setting up the database with supabase

## [License Information](https://github.com/jhelvy/surveydown/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, please cite it! You can
get the citation by typing `citation("surveydown")` into R:

``` r
citation("surveydown")
```

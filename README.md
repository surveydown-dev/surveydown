
<!-- README.md is generated from README.Rmd. Please edit this file -->

## surveydown

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/surveydown)](https://CRAN.R-project.org/package=surveydown)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/surveydown)](https://cran.r-project.org/package=surveydown)
<!-- badges: end -->

> Note: This site only documents the {surveydown} R package - visit our
> main site at [surveydown.org](https://surveydown.org) for more
> information!

## What is surveydown?

**surveydown** is a flexible, open-source platform for making surveys
with [R](https://www.r-project.org/), [Quarto](https://quarto.org/),
[Shiny](https://shiny.posit.co/), and [Supabase](https://supabase.com/).

<div align="center">

<img src='https://github.com/surveydown-dev/surveydown/blob/main/man/figures/technologies.png?raw=true' width="550px" alt="image showing the three technologies used in the surveydown platform along with their logos: quarto (for designing surveys), shiny (for rendering the survey), and supabase (for storing data)"/>

</div>

<br>

Here’s how it works:

1.  Design your survey as a [Quarto](https://quarto.org/) document using
    markdown and R code.
2.  Render your survey into a [shiny](https://shiny.posit.co/) app that
    can be hosted online and sent to respondents.
3.  Store survey responses in a PostgreSQL database - we recommend
    [Supabase](https://supabase.com/) as a free, secure, and easy to use
    option.

The {surveydown} R package provides functions to bring this all
together.

**See the [complete documentation](https://surveydown.org) to get
started making your own surveydown survey!**

## Why surveydown?

Most survey platforms (e.g., Google forms, Qualtrics, etc.) use graphic
interfaces or spreadsheets to define survey content, making version
control, collaboration, and reproducibility difficult or impossible. The
surveydown package was designed to address these problems. As an
open-source, markdown-based platform, all survey content is defined
using **plain text** (markdown and R code) in two files:

- `survey.qmd`: A Quarto document that contains the survey content
  (pages, questions, etc).
- `app.R`: An R script defining a shiny app that contains global
  settings (libraries, database configuration, etc.) and server
  configuration options (e.g., conditional skipping / display, etc.).

This approach makes your survey easy to reproduce, share, and version
control with common tools like Git. And since all survey data is stored
in a PostgreSQL database, you have total control over where your survey
data lives. We provide direct support for
[Supabase](https://supabase.com/) as a free, secure, and easy to use
option.

In case you’re interested in the background behind the project, this
[blog post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/)
provides something of an origin story. Note that the design discussed in
the post is now quite outdated with what ultimately became surveydown.

## Installation

### Install R & Quarto

You need both:

- Install [R](https://CRAN.R-project.org/)
- Install [Quarto](https://quarto.org/)

We also recommend working with an IDE that has good support for R,
Quarto, and Shiny.

[RStudio](https://posit.co/products/open-source/rstudio/) is great, and
we also like [VSCode](https://github.com/microsoft/vscode) and
[Positron](https://github.com/posit-dev/positron).

### Install the {surveydown} R package

You can install {surveydown} from CRAN in your R console:

``` r
install.packages("surveydown")
```

or you can install the development version from
[GitHub](https://github.com/surveydown-dev/surveydown):

``` r
# install.packages("pak")
pak::pak('surveydown-dev/surveydown')
```

Load the package with:

``` r
library(surveydown)
```

You can also check which version you have installed:

``` r
surveydown::sd_version()
```

## Roadmap

See our [todo](https://github.com/surveydown-dev/todo) repo for a
running list of things we’re working on / have already added to the
project.

## License

See the
[License](https://github.com/surveydown-dev/surveydown/blob/master/LICENSE.md).

## Citation

If you use this package for in a publication, please cite it! You can
get the citation by running `citation("surveydown")` in your R console:

``` r
citation("surveydown")
```

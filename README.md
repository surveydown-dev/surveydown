
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

**surveydown** is a flexible, open-source platform for making surveys
with [R](https://www.r-project.org/), [Quarto](https://quarto.org/),
[Shiny](https://shiny.posit.co/), and [Supabase](https://supabase.com/).

<br>

<div align="center">

<img src='man/figures/technologies.png' width="550px" alt="image showing the three technologies used in the surveydown platform along with their logos: quarto (for designing surveys), shiny (for rendering the survey), and supabase (for storing data)"/>

</div>

<br>

The basic concept is:

1.  Design your survey as a [Quarto](https://quarto.org/) document using
    markdown and R code.
2.  Convert your survey into a [Shiny](https://shiny.posit.co/) app that
    can be hosted online and sent to respondents.
3.  Store your survey responses in a [Supabase](https://supabase.com/)
    database (or any Postgres database).

The {surveydown} R package provides functions to bring this all
together.

**See the [documentation](https://surveydown.org) to get started making
your own surveydown survey!**

## Installation

### Install R & Quarto

- [R](https://CRAN.R-project.org/)
- [Quarto](https://quarto.org/)

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

## Background & Motivation

Most survey platforms (e.g., Google forms, Qualtrics, etc.) use graphic
interfaces to design surveys, making version control and collaboration
with others difficult. They’re also not easily reproducible, and many
require a paid subscription or license to use.

The surveydown package was designed to address these problems. As an
open-source, markdown-based platform, all survey content is defined with
plain text (markdown and R code) in a `survey.qmd` file and an `app.R`
file that renders your survey into a Shiny app that can be hosted
online. This makes your survey easy to reproduce, share, and version
control with common tools like Git. The researcher also has total
control over the data collected as it is stored in a PostgreSQL database
of their choosing (we recommend Supabase as a free and secure option).

If you’re curious where this whole idea came from, check out this [blog
post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/), which
outlines more on the general idea and the motivation for it. The post is
now outdated in terms of the overall package design, but it provides
something of an origin story and some of the motivation for developing
this project.

## Roadmap

See our [todo](https://github.com/surveydown-dev/todo) repo for a
running list of things we’re working on / have already added to the
project.

## [License Information](https://github.com/surveydown-dev/surveydown/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, please cite it! You can
get the citation by typing `citation("surveydown")` into R:

``` r
citation("surveydown")
```

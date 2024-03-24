This is a prototype of a way to create surveys using [Quarto shiny documents](https://quarto.org/docs/dashboards/interactivity/shiny-r.html).

Note that it is very much not production ready - it doesn't even save the survey data anywhere yet. For now it just writes to a local csv for purposes of testing and development.

If you want to try running the demo, download / clone / fork the main branch, open the surveydown.Rproj file to open RStudio, then open the survey.qmd file. You should just be able to click the "Run Document" button at the top. You need to have the {shiny} and {shinyjs} packages installed. If you want to run it using just the command line, clone the fences branch then follow instructions [here](https://quarto.org/docs/interactive/shiny/running.html).

It may also be worth reading my [blog post](https://www.jhelvy.com/blog/2023-04-06-markdown-surveys/) on this idea from a while ago. The post is now outdated in terms of the UI I had in mind back then, but the motivation for developing something like this remains.
# Include a folder to the 'shiny' resource path

This function includes a specified folder to the 'shiny' resource path,
making it accessible for serving static files in a 'shiny' application.
It checks for pre-existing resource paths to avoid conflicts with
folders already included by the package.

## Usage

``` r
sd_include_folder(folder)
```

## Arguments

- folder:

  A character string specifying the name of the folder to include. This
  folder should exist in the root directory of your 'shiny' app.

## Value

`NULL` invisibly. The function is called for its side effect of adding a
resource path to 'shiny'.

## Examples

``` r
if (interactive()) {
  library(shiny)

  # Create an "images" folder
  dir.create("images")

  # Include the folder in the shiny resource path
  sd_include_folder("images")
}
```

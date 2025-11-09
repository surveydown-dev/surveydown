# Create a translations template file

This function creates a template translations.yml file in the project
root directory that users can customize to modify system messages.

## Usage

``` r
sd_create_translations(language = "en", path = getwd())
```

## Arguments

- language:

  Character string specifying the language to use. See
  https://shiny.posit.co/r/reference/shiny/1.7.0/dateinput for supported
  languages. Also, if `"en"`, `"de"`, `"es"`, `"fr"`, or `"it"` is
  chosen, default messages in those langauges will be used, otherwise
  the default English messages will be used. Defaults to `"en"`.

- path:

  Character string specifying the directory where the translations.yml
  file should be created. Defaults to the current working directory. The
  file should be placed in the root project folder of your surveydown
  survey.

## Value

Invisible `NULL`.

## Examples

``` r
if (interactive()) {
  # Create English template
  sd_create_translations()

  # Create German template
  sd_create_translations(language = "de")

  # Create Japanese template
  # Will use English messages but Japanese date picker - user can modify
  # the messages as desired
  sd_create_translations(language = "ja")
}
```

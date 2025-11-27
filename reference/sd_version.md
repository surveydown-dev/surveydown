# Check Surveydown Version

This function checks if the local surveydown package is up-to-date with
the latest online version. It compares the local version with the latest
version available on GitHub and provides information about whether an
update is needed.

## Usage

``` r
sd_version()
```

## Value

No return value, called for side effects (prints version information and
update status to the console).

## Examples

``` r
surveydown::sd_version()
#> surveydown (local): 1.0.1
#> surveydown (latest): 1.0.1
#> 
#> surveydown is up to date.
```

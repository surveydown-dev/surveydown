# Depreciated Survey Dashboard

This dashboard was depreciated in version v0.13.0. Now the sdstudio
package fully includes the functionality that was previously included in
this function.

## Usage

``` r
sd_dashboard(gssencmode = "auto")
```

## Arguments

- gssencmode:

  Character string. The GSS encryption mode for the database connection.
  Defaults to `"auto"`. Options are:

  - `"auto"`: Tries `"prefer"` first, then falls back to `"disable"` if
    GSSAPI negotiation fails

  - `"prefer"`: Uses GSSAPI encryption if available, plain connection
    otherwise

  - `"disable"`: Disables GSSAPI encryption entirely Set to `"disable"`
    if you're having connection issues on a secure connection like a
    VPN.

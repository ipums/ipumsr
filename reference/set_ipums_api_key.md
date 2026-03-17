# Set your IPUMS API key

Set your IPUMS API key as the value associated with the `IPUMS_API_KEY`
environment variable.

The key can be stored for the duration of your session or for future
sessions. If saved for future sessions, it is added to the `.Renviron`
file in your home directory. If you choose to save your key to
`.Renviron`, this function will create a backup copy of the file before
modifying.

This function is modeled after the `census_api_key()` function from
[tidycensus](https://walker-data.com/tidycensus/).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
set_ipums_api_key(api_key, save = overwrite, overwrite = FALSE, unset = FALSE)
```

## Arguments

- api_key:

  API key associated with your user account.

- save:

  If `TRUE`, save the key for use in future sessions by adding it to the
  `.Renviron` file in your home directory. Defaults to `FALSE`, unless
  `overwrite = TRUE`.

- overwrite:

  If `TRUE`, overwrite any existing value of `IPUMS_API_KEY` in the
  `.Renviron` file with the provided `api_key`. Defaults to `FALSE`.

- unset:

  If `TRUE`, remove the existing value of `IPUMS_API_KEY` from the
  environment and the `.Renviron` file in your home directory.

## Value

The value of `api_key`, invisibly.

## See also

[`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/reference/set_ipums_default_collection.md)
to set a default collection.

## Examples

``` r
# Set for single session
set_ipums_api_key("your-api-key-here")
#> The environment variable IPUMS_API_KEY has been set. To save it for future sessions, set `save = TRUE`.

if (FALSE) { # \dontrun{
# Save key to .Renviron for future sessions
set_ipums_api_key("your-api-key-here", save = TRUE)

# Overwrite existing key in .Renviron
set_ipums_api_key("your-api-key-here", overwrite = TRUE)

# Remove existing key from .Renviron
set_ipums_api_key("your-api-key-here", unset = TRUE)
} # }
```

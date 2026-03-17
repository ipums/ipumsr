# Set your default IPUMS collection

Set the default IPUMS collection as the value associated with the
`IPUMS_DEFAULT_COLLECTION` environment variable. If this environment
variable exists, IPUMS API functions that require a collection
specification will use the value of `IPUMS_DEFAULT_COLLECTION`, unless
another collection is indicated.

The default collection can be stored for the duration of your session or
for future sessions. If saved for future sessions, it is added to the
`.Renviron` file in your home directory. If you choose to save your key
to `.Renviron`, this function will create a backup copy of the file
before modifying.

This function is modeled after the `census_api_key()` function from
[tidycensus](https://walker-data.com/tidycensus/).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
set_ipums_default_collection(
  collection = NULL,
  save = overwrite,
  overwrite = FALSE,
  unset = FALSE
)
```

## Arguments

- collection:

  Character string of the collection to set as your default collection.
  The collection must currently be supported by the IPUMS API.

  For a list of codes used to refer to each collection, see
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/reference/ipums_data_collections.md).

- save:

  If `TRUE`, save the default collection for use in future sessions by
  adding it to the `.Renviron` file in your home directory. Defaults to
  `FALSE`, unless `overwrite = TRUE`.

- overwrite:

  If `TRUE`, overwrite any existing value of `IPUMS_DEFAULT_COLLECTION`
  in the `.Renviron` file with the provided `collection`. Defaults to
  `FALSE`.

- unset:

  if `TRUE`, remove the existing value of `IPUMS_DEFAULT_COLLECTION`
  from the environment and the `.Renviron` file in your home directory.

## Value

The value of `collection`, invisibly.

## See also

[`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md)
to set an API key.

## Examples

``` r
set_ipums_default_collection("nhgis")
#> The environment variable IPUMS_DEFAULT_COLLECTION has been set. To save it for future sessions, set `save = TRUE`.

if (FALSE) { # \dontrun{
# Extract info will now be retrieved for the default collection:
get_last_extract_info()
get_extract_history()

is_extract_ready(1)
get_extract_info(1)

# Equivalent to:
get_extract_info("nhgis:1")
get_extract_info(c("nhgis", 1))

# Other collections can be specified explicitly
# Doing so does not alter the default collection
is_extract_ready("usa:2")
} # }

# Remove the variable from the environment and .Renviron, if saved
set_ipums_default_collection(unset = TRUE)
#> Warning: No .Renviron file to update.
#> Unsetting environment variable IPUMS_DEFAULT_COLLECTION.
```

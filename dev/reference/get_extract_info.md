# Retrieve the definition and latest status of an extract request

Retrieve the latest status of an extract request.

`get_last_extract_info()` is a convenience function to retrieve the most
recent extract for a given collection.

To browse definitions of your previously submitted extract requests, see
[`get_extract_history()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_history.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

## Usage

``` r
get_extract_info(extract, api_key = Sys.getenv("IPUMS_API_KEY"))

get_last_extract_info(collection = NULL, api_key = Sys.getenv("IPUMS_API_KEY"))
```

## Arguments

- extract:

  One of:

  - An
    [`ipums_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)
    object

  - The data collection and extract number formatted as a string of the
    form `"collection:number"` or as a vector of the form
    `c("collection", number)`

  - An extract number to be associated with your default IPUMS
    collection. See
    [`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_default_collection.md)

  For a list of codes used to refer to each collection, see
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_data_collections.md).

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_api_key.md).

- collection:

  Character string of the IPUMS collection for which to retrieve extract
  history. Defaults to the current default collection, if it exists. See
  [`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_default_collection.md).

  For a list of codes used to refer to each collection, see
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_data_collections.md).

## Value

An
[`ipums_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)
object.

## See also

[`get_extract_history()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_history.md)
to browse past extract definitions

[`wait_for_extract()`](https://tech.popdata.org/ipumsr/dev/reference/wait_for_extract.md)
to wait for an extract to finish processing.

[`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
to download an extract's data files.

[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
and
[`define_extract_from_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
to share an extract definition.

## Examples

``` r
my_extract <- define_extract_micro(
  collection = "usa",
  description = "2013-2014 ACS Data",
  samples = c("us2013a", "us2014a"),
  variables = c("SEX", "AGE", "YEAR")
)

if (FALSE) { # \dontrun{
submitted_extract <- submit_extract(my_extract)

# Get latest info for the request associated with a given `ipums_extract`
# object:
updated_extract <- get_extract_info(submitted_extract)

updated_extract$status

# Or specify the extract collection and number:
get_extract_info("usa:1")
get_extract_info(c("usa", 1))

# If you have a default collection, you can use the extract number alone:
set_ipums_default_collection("nhgis")
get_extract_info(1)

# To get the most recent extract (for instance, if you have forgotten its
# extract number), use `get_last_extract_info()`
get_last_extract_info("nhgis")
} # }
```

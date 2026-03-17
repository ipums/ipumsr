# Retrieve a catalog of available data sources for an IPUMS collection

Retrieve summary metadata containing API codes and descriptions for all
available data sources of a given type for an IPUMS data collection. See
the [IPUMS developer
documentation](https://developer.ipums.org/docs/v2/workflows/explore_metadata/)
for details about the metadata provided for individual data collections
and API endpoints. Use `catalog_types()` to determine available metadata
endpoints by collection.

To retrieve detailed metadata about a particular data source, use
[`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md).

Currently, comprehensive metadata is only available for IPUMS NHGIS and
IPUMS IHGIS, but a listing of samples is available for IPUMS microdata
collections.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
get_metadata_catalog(
  collection,
  metadata_type,
  delay = 0,
  api_key = Sys.getenv("IPUMS_API_KEY")
)

catalog_types(collection)
```

## Arguments

- collection:

  Character string indicating the IPUMS collection for which to retrieve
  metadata.

- metadata_type:

  The type of data source for which to retrieve summary metadata. Use
  `catalog_types()` for a list of accepted endpoints for a given
  collection.

- delay:

  Number of seconds to delay between successive API requests, if
  multiple requests are needed to retrieve all records.

  A delay is highly unlikely to be necessary and is intended only as a
  fallback in the event that you cannot retrieve all metadata records
  without exceeding the API rate limit.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the catalog of all data sources for the given `collection`
and `metadata_type`.

For `catalog_types()`, a character vector of valid catalog endpoints for
a given collection.

## See also

[`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
to obtain detailed metadata for a single data source.

[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to create an IPUMS aggregate data extract definition.

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
to create an IPUMS microdata extract definition.

## Examples

``` r
# List available metadata catalog endpoints:
catalog_types("nhgis")
#> [1] "datasets"           "data_tables"        "time_series_tables"
#> [4] "shapefiles"        

catalog_types("ihgis")
#> [1] "datasets"               "data_tables"            "tabulation_geographies"

if (FALSE) { # \dontrun{
# Get summary metadata for all available sources of a given data type
get_metadata_catalog("nhgis", "datasets")

get_metadata_catalog("ihgis", "tabulation_geographies")

# Filter to identify data sources of interest by their metadata values
all_tsts <- get_metadata_catalog("nhgis", "time_series_tables")

tsts <- all_tsts %>%
  filter(
    grepl("Children", description),
    grepl("Families", description),
    geographic_integration == "Standardized to 2010"
  )

tsts$name
} # }
```

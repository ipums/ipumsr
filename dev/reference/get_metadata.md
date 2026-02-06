# Retrieve detailed metadata about an IPUMS data source

Retrieve metadata containing API codes and descriptions for an IPUMS
data source. See the [IPUMS developer
documentation](https://developer.ipums.org/docs/v2/workflows/explore_metadata/)
for details about the metadata provided for individual data collections
and API endpoints.

To retrieve a summary of all available data sources of a particular
type, use
[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md).
This output can be used to identify the names of data sources for which
to request detailed metadata.

Currently, comprehensive metadata is only available for IPUMS NHGIS and
IPUMS IHGIS. See
[`get_sample_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_sample_info.md)
to list basic sample information for IPUMS microdata collections.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

## Usage

``` r
get_metadata(
  collection,
  dataset = NULL,
  data_table = NULL,
  time_series_table = NULL,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
```

## Arguments

- collection:

  Character string indicating the IPUMS collection for which to retrieve
  metadata.

- dataset:

  Name of an individual dataset from an IPUMS aggregate data collection
  for which to retrieve metadata.

- data_table:

  Name of an individual data table from an IPUMS aggregate data
  collection for which to retrieve metadata. If provided and
  `collection = "nhgis"`, an associated `dataset` must also be
  specified.

- time_series_table:

  If `collection = "nhgis"`, name of an individual time series table
  from IPUMS NHGIS for which to retrieve metadata.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_api_key.md).

## Value

A named list of metadata for the specified data source.

## See also

[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md)
to obtain a summary of available data sources for a given IPUMS data
collection.

[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md)
to create an IPUMS aggregate data extract definition.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Get detailed metadata for a single source with its associated argument:
cs5_meta <- get_metadata("nhgis", time_series_table = "CS5")
cs5_meta$geog_levels

# Use the available values when defining an NHGIS extract request
define_extract_agg(
  "nhgis",
  time_series_tables = tst_spec("CS5", geog_levels = "state")
)

# Detailed metadata is also provided for datasets and data tables
get_metadata("nhgis", dataset = "1990_STF1")
get_metadata("nhgis", data_table = "NP1", dataset = "1990_STF1")
get_metadata("ihgis", dataset = "KZ2009pop")

# Iterate over data sources to retrieve detailed metadata for several
# records. For instance, to get variable metadata for a set of data tables:
tables <- c("NP1", "NP2", "NP10")

var_meta <- purrr::map(
  tables,
  function(dt) {
    dt_meta <- get_metadata("nhgis", dataset = "1990_STF1", data_table = dt)

    # This ensures you avoid hitting rate limit for large numbers of tables
    Sys.sleep(1)

    dt_meta$variables
  }
)
} # }
```

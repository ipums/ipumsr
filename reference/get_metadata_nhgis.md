# List available data sources from IPUMS NHGIS

**\[deprecated\]**

This function has been deprecated because the IPUMS API now supports
metadata endpoints for multiple data collections. To obtain summary
metadata, please use
[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/reference/get_metadata_catalog.md).
To obtain detailed metadata, please use
[`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md)
and aggregate data extract definitions in
[`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/articles/ipums-api-agg.md).

## Usage

``` r
get_metadata_nhgis(
  type = NULL,
  dataset = NULL,
  data_table = NULL,
  time_series_table = NULL,
  delay = 0,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
```

## Arguments

- type:

  One of `"datasets"`, `"data_tables"`, `"time_series_tables"`, or
  `"shapefiles"` indicating the type of summary metadata to retrieve.
  Leave `NULL` if requesting metadata for a single `dataset`,
  `data_table`, or `time_series_table`.

- dataset:

  Name of an individual dataset for which to retrieve metadata.

- data_table:

  Name of an individual data table for which to retrieve metadata. If
  provided, an associated `dataset` must also be specified.

- time_series_table:

  Name of an individual time series table for which to retrieve
  metadata.

- delay:

  Number of seconds to delay between successive API requests, if
  multiple requests are needed to retrieve all records.

  A delay is highly unlikely to be necessary and is intended only as a
  fallback in the event that you cannot retrieve all metadata records
  without exceeding the API rate limit.

  Only used if `type` is provided.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

If `type` is provided, a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html) of
summary metadata for all data sources of the provided `type`. Otherwise,
a named list of metadata for the specified `dataset`, `data_table`, or
`time_series_table`.

## Metadata availability

The following sections summarize the metadata fields provided for each
data type. Summary metadata include a subset of the fields provided for
individual data sources.

### Datasets:

- **`name`:** The unique identifier for the dataset. This is the value
  that is used to refer to the dataset when interacting with the IPUMS
  API.

- **`group`:** The group of datasets to which the dataset belongs. For
  instance, 5 separate datasets are part of the
  `"2015 American Community Survey"` group.

- **`description`:** A short description of the dataset.

- **`sequence`:** Order in which the dataset will appear in the metadata
  API and extracts.

- **`has_multiple_data_types`:** Logical value indicating whether
  multiple data types exist for this dataset. For example, ACS datasets
  include both estimates and margins of error.

- **`data_tables`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names, codes, and descriptions for all data tables
  available for the dataset.

- **`geog_levels`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names, descriptions, and extent information for the
  geographic levels available for the dataset. The
  `has_geog_extent_selection` field contains logical values indicating
  whether extent selection is allowed for the associated geographic
  level. See `geographic_instances` below.

- **`breakdowns`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names, types, descriptions, and breakdown values for all
  breakdowns available for the dataset.

- **`years`:** A vector of years for which the dataset is available.
  This field is only present if a dataset is available for multiple
  years. Note that ACS datasets are not considered to be available for
  multiple years.

- **`geographic_instances`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names and descriptions for all valid geographic extents for
  the dataset. This field is only present if at least one of the
  dataset's `geog_levels` allows geographic extent selection.

### Data tables:

- **`name`:** The unique identifier for the data table within its
  dataset. This is the value that is used to refer to the data table
  when interacting with the IPUMS API.

- **`description`:** A short description of the data table.

- **`universe`:** The statistical population measured by this data table
  (e.g. persons, families, occupied housing units, etc.)

- **`nhgis_code`:** The code identifying the data table in the extract.
  Variables in the extract data will include column names prefixed with
  this code.

- **`sequence`:** Order in which the data table will appear in the
  metadata API and extracts.

- **`dataset_name`:** Name of the dataset to which this data table
  belongs.

- **`n_variables`:** Number of variables included in this data table.

- **`variables`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing variable descriptions and codes for the variables included
  in the data table

### Time series tables:

- **`name`:** The unique identifier for the time series table. This is
  the value that is used to refer to the time series table when
  interacting with the IPUMS API.

- **`description`:** A short description of the time series table.

- **`geographic_integration`:** The method by which the time series
  table aligns geographic units across time. `"Nominal"` integration
  indicates that geographic units are aligned by name (disregarding
  changes in unit boundaries). `"Standardized"` integration indicates
  that data from multiple time points are standardized to the indicated
  year's census units. For more information, click
  [here](https://www.nhgis.org/time-series-tables#geographic-integration).

- **`sequence`:** Order in which the time series table will appear in
  the metadata API and extracts.

- **`time_series`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names and descriptions for the individual time series
  available for the time series table.

- **`years`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing information on the available data years for the time series
  table.

- **`geog_levels`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names and descriptions for the geographic levels available
  for the time series table. The `has_geog_extent_selection` field
  contains logical values indicating whether extent selection is allowed
  for the associated geographic level.

- **`geographic_instances`:** A
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  containing names and descriptions for all valid geographic extents for
  the time series table. Includes all states or state equivalents that
  are valid for *any* year in the time series table. (Some instances may
  be valid for some but not all years.)

### Shapefiles:

- **`name`:** The unique identifier for the shapefile. This is the value
  that is used to refer to the shapefile when interacting with the IPUMS
  API.

- **`year`:** The survey year in which the shapefile's represented areas
  were used for tabulations, which may be different than the vintage of
  the represented areas. For more information, click
  [here](https://www.nhgis.org/gis-files#years).

- **`geographic_level`:** The geographic level of the shapefile.

- **`extent`:** The geographic extent covered by the shapefile.

- **`basis`:** The derivation source of the shapefile.

- **`sequence`:** Order in which the shapefile will appear in the
  metadata API and extracts.

## See also

[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to create an IPUMS aggregate data extract definition.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Get summary metadata for all available sources of a given data type
# Previously:
get_metadata_nhgis("datasets")

# Now:
get_metadata_catalog("nhgis", "datasets")

# Get detailed metadata for a single source with its associated argument
# Previously:
cs5_meta <- get_metadata_nhgis(time_series_table = "CS5")

# Now:
cs5_meta <- get_metadata("nhgis", time_series_table = "CS5")

cs5_meta$geog_levels

# Use the available values when defining an NHGIS extract request
define_extract_agg(
  "nhgis",
  time_series_tables = tst_spec("CS5", geog_levels = "state")
)
} # }
```

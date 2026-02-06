# Define an IPUMS NHGIS extract request

**\[deprecated\]**

Define the parameters of an IPUMS NHGIS extract request to be submitted
via the IPUMS API.

This function has been deprecated in favor of
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md),
which can be used to define extracts for both IPUMS aggregate data
collections (IPUMS NHGIS and IPUMS IHGIS). Please use that function
instead.

All NHGIS extract request parameters supported by
`define_extract_nhgis()` are supported by
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md)
and NHGIS extract definitions in
[`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-agg.md).

## Usage

``` r
define_extract_nhgis(
  description = "",
  datasets = NULL,
  time_series_tables = NULL,
  shapefiles = NULL,
  geographic_extents = NULL,
  breakdown_and_data_type_layout = NULL,
  tst_layout = NULL,
  data_format = NULL
)
```

## Arguments

- description:

  Description of the extract.

- datasets:

  List of dataset specifications for any
  [datasets](https://www.nhgis.org/overview-nhgis-datasets) to include
  in the extract request. Use
  [`ds_spec()`](https://tech.popdata.org/ipumsr/dev/reference/ds_spec.md)
  to create a `ds_spec` object containing a dataset specification. See
  examples.

- time_series_tables:

  List of time series table specifications for any [time series
  tables](https://www.nhgis.org/time-series-tables) to include in the
  extract request. Use
  [`tst_spec()`](https://tech.popdata.org/ipumsr/dev/reference/ds_spec.md)
  to create a `tst_spec` object containing a time series table
  specification. See examples.

- shapefiles:

  Names of any [shapefiles](https://www.nhgis.org/gis-files) to include
  in the extract request.

- geographic_extents:

  Vector of geographic extents to use for all of the `datasets` and
  `time_series_tables` in the extract definition (for instance, to
  obtain data within a specified state). By default, selects all
  available extents.

  Use
  [`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md)
  to identify the available extents for a given dataset or time series
  table, if any.

- breakdown_and_data_type_layout:

  The desired layout of any `datasets` that have multiple data types or
  breakdown values.

  - `"single_file"` (default) keeps all data types and breakdown values
    in one file

  - `"separate_files"` splits each data type or breakdown value into its
    own file

  Required if any `datasets` included in the extract definition consist
  of multiple data types (for instance, estimates and margins of error)
  or have multiple breakdown values specified. See
  [`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md)
  to determine whether a requested dataset has multiple data types.

- tst_layout:

  The desired layout of all `time_series_tables` included in the extract
  definition.

  - `"time_by_column_layout"` (wide format, default): rows correspond to
    geographic units, columns correspond to different times in the time
    series

  - `"time_by_row_layout"` (long format): rows correspond to a single
    geographic unit at a single point in time

  - `"time_by_file_layout"`: data for different times are provided in
    separate files

  Required when an extract definition includes any `time_series_tables`.

- data_format:

  The desired format of the extract data file.

  - `"csv_no_header"` (default) includes only a minimal header in the
    first row

  - `"csv_header"` includes a second, more descriptive header row.

  - `"fixed_width"` provides data in a fixed width format

  Note that by default,
  [`read_ipums_agg()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_agg.md)
  removes the additional header row in `"csv_header"` files.

  Required when an extract definition includes any `datasets` or
  `time_series_tables`.

## Value

An object of class
[`nhgis_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)
containing the extract definition.

## See also

[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md)
to find data to include in an extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md)
to submit an extract request for processing.

[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
and
[`define_extract_from_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
to share an extract definition.

## Examples

``` r
# Previously, you could create an NHGIS extract definition like so:
nhgis_extract <- define_extract_nhgis(
  description = "Example NHGIS extract",
  datasets = ds_spec(
    "1990_STF3",
    data_tables = "NP57",
    geog_levels = c("county", "tract")
  )
)
#> Warning: `define_extract_nhgis()` was deprecated in ipumsr 0.9.0.
#> ℹ Please use `define_extract_agg()` instead.

# Now, use the following:
nhgis_extract <- define_extract_agg(
  collection = "nhgis",
  description = "Example NHGIS extract",
  datasets = ds_spec(
    "1990_STF3",
    data_tables = "NP57",
    geog_levels = c("county", "tract")
  )
)
```

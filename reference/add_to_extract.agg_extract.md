# Add values to an existing IPUMS NHGIS extract definition

**\[experimental\]**

Add new values to an IPUMS aggregate data extract definition. All fields
are optional, and if omitted, will be unchanged. Supplying a value for
fields that take a single value, such as `description` and
`data_format`, will replace the existing value with the supplied value.

This function is marked as experimental because it is typically not the
best option for maintaining reproducible extract definitions and may be
retired in the future. For reproducibility, users should strive to build
extract definitions with
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md).

If you have a complicated extract definition to revise, but do not have
the original extract definition code that created it, we suggest that
you save the revised extract as a JSON file with
[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md).
This will create a stable version of the extract definition that can be
used in the future as needed.

To remove existing values from an IPUMS NHGIS extract definition, use
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.agg_extract.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
# S3 method for class 'agg_extract'
add_to_extract(
  extract,
  description = NULL,
  datasets = NULL,
  time_series_tables = NULL,
  geographic_extents = NULL,
  shapefiles = NULL,
  breakdown_and_data_type_layout = NULL,
  tst_layout = NULL,
  data_format = NULL,
  ...
)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- description:

  Description of the extract.

- datasets:

  List of `ds_spec` objects created by
  [`ds_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md)
  containing the specifications for the
  [datasets](https://www.nhgis.org/overview-nhgis-datasets) to include
  in the extract request. See examples.

  If a dataset already exists in the extract, its new specifications
  will be added to those that already exist for that dataset.

- time_series_tables:

  For NHGIS extracts, list of `tst_spec` objects created by
  [`tst_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md)
  containing the specifications for the [time series
  tables](https://www.nhgis.org/time-series-tables) to include in the
  extract request.

  If a time series table already exists in the extract, its new
  specifications will be added to those that already exist for that time
  series table.

- geographic_extents:

  For NHGIS extracts, vector of geographic extents to use for all of the
  `datasets` and `time_series_tables` in the extract definition (for
  instance, to obtain data within a specified state). By default,
  selects all available extents.

  Use
  [`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
  to identify the available extents for a given dataset or time series
  table, if any.

- shapefiles:

  For NHGIS extracts, names of any
  [shapefiles](https://www.nhgis.org/gis-files) to include in the
  extract request.

- breakdown_and_data_type_layout:

  For NHGIS extracts, the desired layout of any `datasets` that have
  multiple data types or breakdown values.

  - `"single_file"` (default) keeps all data types and breakdown values
    in one file

  - `"separate_files"` splits each data type or breakdown value into its
    own file

  Required if any `datasets` included in the extract definition consist
  of multiple data types (for instance, estimates and margins of error)
  or have multiple breakdown values specified. See
  [`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
  to determine whether a requested dataset has multiple data types.

- tst_layout:

  For NHGIS extracts, the desired layout of all `time_series_tables`
  included in the extract definition.

  - `"time_by_column_layout"` (wide format, default): rows correspond to
    geographic units, columns correspond to different times in the time
    series

  - `"time_by_row_layout"` (long format): rows correspond to a single
    geographic unit at a single point in time

  - `"time_by_file_layout"`: data for different times are provided in
    separate files

  Required when an extract definition includes any `time_series_tables`.

- data_format:

  For NHGIS extracts, the desired format of the extract data file.

  - `"csv_no_header"` (default) includes only a minimal header in the
    first row

  - `"csv_header"` includes a second, more descriptive header row.

  - `"fixed_width"` provides data in a fixed width format

  Note that by default,
  [`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
  removes the additional header row in `"csv_header"` files.

  Required when an extract definition includes any `datasets` or
  `time_series_tables`.

- ...:

  Ignored

## Value

A modified `agg_extract` object

## Details

For extract fields that take a single value,
[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.md)
will replace the existing value with the new value provided for that
field. It is not necessary to first remove this value using
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.md).

If the supplied extract definition comes from a previously submitted
extract request, this function will reset the definition to an
unsubmitted state.

## See also

[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.agg_extract.md)
to remove values from an extract definition.

[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to create a new extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request.

[`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
to download extract data files.

## Examples

``` r
extract <- define_extract_agg(
  "nhgis",
  datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "county")
)

# Add a new dataset or time series table to the extract
add_to_extract(
  extract,
  datasets = ds_spec("1990_STF2a", "NPA1", "county")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county
#> 
#> Dataset: 1990_STF2a
#>   Tables: NPA1
#>   Geog Levels: county

add_to_extract(
  extract,
  time_series_tables = tst_spec("A00", "state")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county
#> 
#> Time Series Table: A00
#>   Geog Levels: state

# If a dataset/time series table name already exists in the definition
# its specification will be modified by adding the new specifications to
# the existing ones
add_to_extract(
  extract,
  datasets = ds_spec("1990_STF1", "NP4", "nation")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2, NP4
#>   Geog Levels: county, nation

# You can add new datasets and modify existing ones simultaneously by
# providing a list of `ds_spec` objects
add_to_extract(
  extract,
  datasets = list(
    ds_spec("1990_STF1", "NP4", "nation"),
    ds_spec("1990_STF2a", "NPA1", "county")
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2, NP4
#>   Geog Levels: county, nation
#> 
#> Dataset: 1990_STF2a
#>   Tables: NPA1
#>   Geog Levels: county

# Values that can only take a single value are replaced
add_to_extract(extract, data_format = "fixed_width")$data_format
#> [1] "fixed_width"
```

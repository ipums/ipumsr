# Define an extract request for an IPUMS aggregate data collection

Define the parameters of an IPUMS aggregate data extract request to be
submitted via the IPUMS API.

The IPUMS API currently supports the following aggregate data
collections:

- [IPUMS NHGIS](https://www.nhgis.org/)

- [IPUMS IHGIS](https://ihgis.ipums.org/)

Note that not all extract request parameters and options apply to all
collections. For a summary of supported features by collection, see the
details below and the [IPUMS API
documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/).

Use
[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/reference/get_metadata_catalog.md)
and
[`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
to browse and identify data sources for use in an extract definition.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md)
and aggregate data extract definitions in
[`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/articles/ipums-api-agg.md).

## Usage

``` r
define_extract_agg(
  collection,
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

- collection:

  Code for the IPUMS collection represented by this extract request.
  Currently, `"nhgis"` and `"ihgis"` are supported.

- description:

  Description of the extract.

- datasets:

  List of dataset specifications for any datasets to include in the
  extract request. Use
  [`ds_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md) to
  create a `ds_spec` object containing a dataset specification. See
  examples.

- time_series_tables:

  For NHGIS extracts, list of time series table specifications for any
  [time series tables](https://www.nhgis.org/time-series-tables) to
  include in the extract request. Use
  [`tst_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md)
  to create a `tst_spec` object containing a time series table
  specification. See examples.

- shapefiles:

  For NHGIS extracts, names of any
  [shapefiles](https://www.nhgis.org/gis-files) to include in the
  extract request.

- geographic_extents:

  For NHGIS extracts, vector of geographic extents to use for all of the
  `datasets` and `time_series_tables` in the extract definition (for
  instance, to obtain data within a specified state). By default,
  selects all available extents.

  Use
  [`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
  to identify the available extents for a given dataset or time series
  table, if any.

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

## Value

An object of class
[`agg_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
containing the extract definition.

## Details

### IPUMS NHGIS

An NHGIS extract definition (`collection = "nhgis"`) must include at
least one dataset, time series table, or shapefile specification.

Create a dataset specification with
[`ds_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md).
Each dataset must be associated with a selection of `data_tables` and
`geog_levels`. Some datasets also support the selection of `years` and
`breakdown_values`.

Create an NHGIS time series table specification with
[`tst_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md).
Each time series table must be associated with a selection of
`geog_levels` and may optionally be associated with a selection of
`years`.

### IPUMS IHGIS

An IHGIS extract definition (`collection = "ihgis"`) must include a
dataset specification. IHGIS does not support time series table or
shapefile specifications.

Create a dataset specification with
[`ds_spec()`](https://tech.popdata.org/ipumsr/reference/ds_spec.md).
Each dataset must be associated with a selection of `data_tables` and
`tabulation_geographies`.

See examples or
[`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/articles/ipums-api-agg.md)
for more details about specifying datasets and time series tables in an
aggregate data extract definition.

## See also

[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/reference/get_metadata_catalog.md)
and
[`get_metadata()`](https://tech.popdata.org/ipumsr/reference/get_metadata.md)
to find data to include in an extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request for processing.

[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md)
and
[`define_extract_from_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md)
to share an extract definition.

## Examples

``` r
# Extract definition for tables from an NHGIS dataset
# Use `ds_spec()` to create an NHGIS dataset specification
nhgis_extract <- define_extract_agg(
  "nhgis",
  description = "Example NHGIS extract",
  datasets = ds_spec(
    "1990_STF3",
    data_tables = "NP57",
    geog_levels = c("county", "tract")
  )
)

nhgis_extract
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example NHGIS extract
#> 
#> Dataset: 1990_STF3
#>   Tables: NP57
#>   Geog Levels: county, tract

# Extract definition for tables from an IHGIS dataset
define_extract_agg(
  "ihgis",
  description = "Example IHGIS extract",
  datasets = ds_spec(
    "KZ2009pop",
    data_tables = c("KZ2009pop.AAA", "KZ2009pop.AAB"),
    tabulation_geographies = c("KZ2009pop.g0", "KZ2009pop.g1")
  )
)
#> Unsubmitted IPUMS IHGIS extract 
#> Description: Example IHGIS extract
#> 
#> Dataset: KZ2009pop
#>   Tables: KZ2009pop.AAA, KZ2009pop.AAB
#>   Tabulation Geogs: KZ2009pop.g0, KZ2009pop.g1

# Use `tst_spec()` to create an NHGIS time series table specification
define_extract_agg(
  "nhgis",
  description = "Example NHGIS extract",
  time_series_tables = tst_spec("CL8", geog_levels = "county"),
  tst_layout = "time_by_row_layout"
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example NHGIS extract
#> 
#> Time Series Table: CL8
#>   Geog Levels: county

# To request multiple datasets, provide a list of `ds_spec` objects
define_extract_agg(
  "nhgis",
  description = "Extract definition with multiple datasets",
  datasets = list(
    ds_spec("2014_2018_ACS5a", "B01001", c("state", "county")),
    ds_spec("2015_2019_ACS5a", "B01001", c("state", "county"))
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Extract definition with multiple datasets
#> 
#> Dataset: 2014_2018_ACS5a
#>   Tables: B01001
#>   Geog Levels: state, county
#> 
#> Dataset: 2015_2019_ACS5a
#>   Tables: B01001
#>   Geog Levels: state, county

# If you need to specify the same table or geographic level for
# many datasets, you may want to make a set of datasets before defining
# your extract request:
dataset_names <- c("2014_2018_ACS5a", "2015_2019_ACS5a")

dataset_spec <- purrr::map(
  dataset_names,
  ~ ds_spec(
    .x,
    data_tables = "B01001",
    geog_levels = c("state", "county")
  )
)

define_extract_agg(
  "nhgis",
  description = "Extract definition with multiple datasets",
  datasets = dataset_spec
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Extract definition with multiple datasets
#> 
#> Dataset: 2014_2018_ACS5a
#>   Tables: B01001
#>   Geog Levels: state, county
#> 
#> Dataset: 2015_2019_ACS5a
#>   Tables: B01001
#>   Geog Levels: state, county

# You can request datasets, time series tables, and shapefiles in the same
# definition:
define_extract_agg(
  "nhgis",
  description = "Extract with datasets and time series tables",
  datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "county"),
  time_series_tables = tst_spec("CL6", "state"),
  shapefiles = "us_county_1990_tl2008"
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Extract with datasets and time series tables
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county
#> 
#> Time Series Table: CL6
#>   Geog Levels: state
#> 
#> Shapefiles: us_county_1990_tl2008

# Geographic extents are applied to all datasets/time series tables in the
# definition
define_extract_agg(
  "nhgis",
  description = "Extent selection",
  datasets = list(
    ds_spec("2018_2022_ACS5a", "B01001", "blck_grp"),
    ds_spec("2017_2021_ACS5a", "B01001", "blck_grp")
  ),
  geographic_extents = c("010", "050")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Extent selection
#> 
#> Dataset: 2018_2022_ACS5a
#>   Tables: B01001
#>   Geog Levels: blck_grp
#> 
#> Dataset: 2017_2021_ACS5a
#>   Tables: B01001
#>   Geog Levels: blck_grp
#> 
#> Geographic extents: 010, 050

# Extract specifications can be indexed by name
names(nhgis_extract$datasets)
#> [1] "1990_STF3"

nhgis_extract$datasets[["1990_STF3"]]
#> $name
#> [1] "1990_STF3"
#> 
#> $data_tables
#> [1] "NP57"
#> 
#> $geog_levels
#> [1] "county" "tract" 
#> 
#> attr(,"class")
#> [1] "ds_spec"    "ipums_spec" "list"      

if (FALSE) { # \dontrun{
# Use the extract definition to submit an extract request to the API
submit_extract(nhgis_extract)
} # }
```

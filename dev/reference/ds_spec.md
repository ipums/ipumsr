# Create dataset and time series table specifications for IPUMS aggregate data extract definitions

Provide specifications for individual datasets and time series tables
when defining an IPUMS aggregate data extract request. This includes
extract requests for IPUMS NHGIS and IPUMS IHGIS.

Use
[`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md)
to identify available values for dataset and time series table
specification parameters.

Learn more about aggregate data extract definitions in
[`vignette("ipums-api-agg")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-agg.md).

## Usage

``` r
ds_spec(
  name,
  data_tables = NULL,
  geog_levels = NULL,
  years = NULL,
  breakdown_values = NULL,
  tabulation_geographies = NULL
)

tst_spec(name, geog_levels = NULL, years = NULL)
```

## Arguments

- name:

  Name of the dataset or (for IPUMS NHGIS) time series table.

- data_tables:

  Vector of summary tables to retrieve for the given dataset.

- geog_levels:

  Geographic levels (e.g. `"county"` or `"state"`) at which to obtain
  data for the given dataset or time series table.

  Only applicable for IPUMS NHGIS extract definitions.

- years:

  Years for which to obtain the data for the given dataset or time
  series table.

  For time series tables, all years are selected by default. For
  datasets, use `"*"` to select all available years. Use
  [`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md)
  to determine if a dataset allows year selection.

  Only applicable for IPUMS NHGIS extract definitions.

- breakdown_values:

  [Breakdown
  values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
  to apply to the given dataset.

  Only applicable for IPUMS NHGIS extract definitions.

- tabulation_geographies:

  Tabulation geographies to apply to the given dataset. These represent
  the level of geographic aggregation for the requested data.

  Only applicable for IPUMS IHGIS extract definitions.

## Value

A `ds_spec` or `tst_spec` object.

## Details

For IPUMS NHGIS extract definitions, `data_tables` and `geog_levels` are
required for all dataset specifications, and `geog_levels` are required
for all time series table specifications.

For IPUMS IHGIS extract definitions, `data_tables` and
`tabulation_geographies` are required for all dataset specifications.

However, it is possible to make a temporary specification for an
incomplete dataset or time series table by omitting required values.
This supports the syntax used when modifying an existing extract (see
[`add_to_extract()`](https://tech.popdata.org/ipumsr/dev/reference/add_to_extract.agg_extract.md)
or
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/dev/reference/remove_from_extract.agg_extract.md)).

## Examples

``` r
dataset <- ds_spec(
  "2013_2017_ACS5a",
  data_tables = c("B00001", "B01002"),
  geog_levels = "state"
)

tst <- tst_spec(
  "CW5",
  geog_levels = c("county", "tract"),
  years = "1990"
)

# Use variable specifications in an extract definition:
define_extract_agg(
  "nhgis",
  description = "Example extract",
  datasets = dataset,
  time_series_tables = tst
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example extract
#> 
#> Dataset: 2013_2017_ACS5a
#>   Tables: B00001, B01002
#>   Geog Levels: state
#> 
#> Time Series Table: CW5
#>   Geog Levels: county, tract
#>   Years: 1990

# IHGIS datasets need a `tabulation_geographies` specification:
define_extract_agg(
  "ihgis",
  description = "Example extract",
  datasets = ds_spec(
    "AL2001pop",
    data_tables = "AL2001pop.ADF",
    tabulation_geographies = c("AL2001pop.g0", "AL2001pop.g1")
  )
)
#> Unsubmitted IPUMS IHGIS extract 
#> Description: Example extract
#> 
#> Dataset: AL2001pop
#>   Tables: AL2001pop.ADF
#>   Tabulation Geogs: AL2001pop.g0, AL2001pop.g1
```

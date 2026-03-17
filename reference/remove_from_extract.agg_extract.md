# Remove values from an existing NHGIS extract definition

**\[experimental\]**

Remove existing values from an IPUMS aggregate data extract definition.
All fields are optional, and if omitted, will be unchanged.

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

To add new values to an IPUMS NHGIS extract definition, use
[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.agg_extract.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
# S3 method for class 'agg_extract'
remove_from_extract(
  extract,
  datasets = NULL,
  time_series_tables = NULL,
  geographic_extents = NULL,
  shapefiles = NULL,
  ...
)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- datasets:

  Dataset specifications to remove from the extract definition. All
  `data_tables`, `geog_levels`, `years`, and `breakdown_values`
  associated with the specified `datasets` will also be removed.

- time_series_tables:

  Names of the time series tables to remove from the extract definition.
  All `geog_levels` and `years` associated with the specified
  `time_series_tables` will also be removed.

- geographic_extents:

  Geographic extents to remove from the extract definition.

- shapefiles:

  Shapefiles to remove from the extract definition.

- ...:

  Ignored

## Value

A modified `agg_extract` object

## Details

Any extract fields that are rendered irrelevant after modifying the
extract will be automatically removed. (For instance, if all
`time_series_tables` are removed from an extract, `tst_layout` will also
be removed.) Thus, it is not necessary to explicitly remove these
values.

If the supplied extract definition comes from a previously submitted
extract request, this function will reset the definition to an
unsubmitted state.

## See also

[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.agg_extract.md)
to add values to an extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request.

[`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
to download extract data files.

[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to create a new extract definition.

## Examples

``` r
extract <- define_extract_agg(
  "nhgis",
  datasets = ds_spec(
    "1990_STF1",
    data_tables = c("NP1", "NP2", "NP3"),
    geog_levels = "county"
  ),
  time_series_tables = list(
    tst_spec("CW3", c("state", "county")),
    tst_spec("CW5", c("state", "county"))
  )
)

# Providing names of datasets or time series tables will remove them and
# all of their associated specifications from the extract:
remove_from_extract(
  extract,
  time_series_tables = c("CW3", "CW5")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2, NP3
#>   Geog Levels: county

# To remove detailed specifications from a dataset or time series table,
# use `ds_spec()` or `tst_spec()`. The named dataset or time series table
# will be retained in the extract, but modified by removing the indicated
# specifications:
remove_from_extract(
  extract,
  datasets = ds_spec("1990_STF1", data_tables = c("NP2", "NP3"))
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1
#>   Geog Levels: county
#> 
#> Time Series Table: CW3
#>   Geog Levels: state, county
#> 
#> Time Series Table: CW5
#>   Geog Levels: state, county

# To make multiple modifications, use a list of `ds_spec()` or `tst_spec()`
# objects:
remove_from_extract(
  extract,
  time_series_tables = list(
    tst_spec("CW3", geog_levels = "county"),
    tst_spec("CW5", geog_levels = "state")
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2, NP3
#>   Geog Levels: county
#> 
#> Time Series Table: CW3
#>   Geog Levels: state
#> 
#> Time Series Table: CW5
#>   Geog Levels: county
```

# Add values to an existing IPUMS extract definition

**\[experimental\]**

Add or replace values in an existing `ipums_extract` object. This
function is an S3 generic whose behavior will depend on the subclass
(i.e. collection) of the extract being modified.

- To add to an **IPUMS microdata** extract definition, click
  [here](https://tech.popdata.org/ipumsr/reference/add_to_extract_micro.md).
  This includes:

  - IPUMS USA

  - IPUMS CPS

  - IPUMS International

  - IPUMS Time Use (ATUS, AHTUS, MTUS)

  - IPUMS Health Surveys (NHIS, MEPS)

- To add to an **IPUMS aggregate data** extract definition, click
  [here](https://tech.popdata.org/ipumsr/reference/add_to_extract.agg_extract.md).
  This includes:

  - IPUMS NHGIS

  - IPUMS IHGIS

This function is marked as experimental because it is typically not the
best option for maintaining reproducible extract definitions and may be
retired in the future. For reproducibility, users should strive to build
extract definitions with
[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
or
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md).

If you have a complicated extract definition to revise, but do not have
the original extract definition code that created it, we suggest that
you save the revised extract as a JSON file with
[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md).
This will create a stable version of the extract definition that can be
used in the future as needed.

To remove existing values from an extract definition, use
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
add_to_extract(extract, ...)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- ...:

  Additional arguments specifying the extract fields and values to add
  to the extract definition.

  All arguments available in
  [`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
  (for microdata extract requests) or
  [`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
  (for aggregate data extract requests) can be passed to
  `add_to_extract()`.

## Value

An object of the same class as `extract` containing the modified extract
definition

## See also

[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.md)
to remove values from an extract definition.

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
or
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to define an extract request manually.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request for processing.

## Examples

``` r
# Microdata extracts
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "2013 ACS Data",
  samples = "us2013a",
  variables = c("SEX", "AGE", "YEAR")
)

# Add new samples and variables
add_to_extract(
  usa_extract,
  samples = c("us2014a", "us2015a"),
  variables = var_spec("MARST", data_quality_flags = TRUE)
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (3 total) us2013a, us2014a, us2015a
#> Variables: (4 total) SEX, AGE, YEAR, MARST

# Update existing variables
add_to_extract(
  usa_extract,
  variables = var_spec("SEX", case_selections = "1")
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (1 total) us2013a
#> Variables: (3 total) SEX, AGE, YEAR

# Modify/add multiple variables
add_to_extract(
  usa_extract,
  variables = list(
    var_spec("SEX", case_selections = "1"),
    var_spec("RELATE")
  )
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (1 total) us2013a
#> Variables: (4 total) SEX, AGE, YEAR, RELATE

# NHGIS extracts
nhgis_extract <- define_extract_agg(
  "nhgis",
  datasets = ds_spec(
    "1990_STF1",
    data_tables = c("NP1", "NP2"),
    geog_levels = "county"
  )
)

# Add a new dataset or time series table
add_to_extract(
  nhgis_extract,
  datasets = ds_spec(
    "1980_STF1",
    data_tables = "NT1A",
    geog_levels = c("county", "state")
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county
#> 
#> Dataset: 1980_STF1
#>   Tables: NT1A
#>   Geog Levels: county, state

# Update existing datasets/time series tables
add_to_extract(
  nhgis_extract,
  datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "state")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county, state

# Modify/add multiple datasets or time series tables
add_to_extract(
  nhgis_extract,
  time_series_tables = list(
    tst_spec("CW3", geog_levels = "state"),
    tst_spec("CW4", geog_levels = "state")
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP1, NP2
#>   Geog Levels: county
#> 
#> Time Series Table: CW3
#>   Geog Levels: state
#> 
#> Time Series Table: CW4
#>   Geog Levels: state

# Values that can only take a single value are replaced
add_to_extract(nhgis_extract, data_format = "fixed_width")$data_format
#> [1] "fixed_width"
```

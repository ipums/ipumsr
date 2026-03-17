# Remove values from an existing IPUMS extract definition

**\[experimental\]**

Remove values for specific fields in an existing `ipums_extract` object.
This function is an S3 generic whose behavior will depend on the
subclass (i.e. collection) of the extract being modified.

- To remove from an **IPUMS Microdata** extract definition, click
  [here](https://tech.popdata.org/ipumsr/reference/remove_from_extract.micro_extract.md).
  This includes:

  - IPUMS USA

  - IPUMS CPS

  - IPUMS International

  - IPUMS Time Use (ATUS, AHTUS, MTUS)

  - IPUMS Health Surveys (NHIS, MEPS)

- To remove from an **IPUMS aggregate data** extract definition, click
  [here](https://tech.popdata.org/ipumsr/reference/remove_from_extract.agg_extract.md).
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

To add new values to an extract, see
[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
remove_from_extract(extract, ...)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- ...:

  Additional arguments specifying the extract fields and values to
  remove from the extract definition.

## Value

An object of the same class as `extract` containing the modified extract
definition

## See also

[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.md)
to add values to an extract definition.

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
or
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to define an extract request manually

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request for processing.

## Examples

``` r
# Microdata extracts
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "USA example",
  samples = c("us2013a", "us2014a"),
  variables = list(
    var_spec("AGE"),
    var_spec("SEX", case_selections = "2"),
    var_spec("YEAR")
  )
)

# Remove variables from an extract definition
remove_from_extract(
  usa_extract,
  samples = "us2014a",
  variables = c("AGE", "SEX")
)
#> Unsubmitted IPUMS USA extract 
#> Description: USA example
#> 
#> Samples: (1 total) us2013a
#> Variables: (1 total) YEAR

# Remove detailed specifications for an existing variable
remove_from_extract(
  usa_extract,
  variables = var_spec("SEX", case_selections = "2")
)
#> Unsubmitted IPUMS USA extract 
#> Description: USA example
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (3 total) AGE, SEX, YEAR

# NHGIS extracts
nhgis_extract <- define_extract_agg(
  "nhgis",
  datasets = ds_spec(
    "1990_STF1",
    data_tables = c("NP1", "NP2", "NP3"),
    geog_levels = "county"
  ),
  time_series_tables = tst_spec("A00", geog_levels = "county")
)

# Remove an existing dataset or time series table
remove_from_extract(nhgis_extract, datasets = "1990_STF1")
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Time Series Table: A00
#>   Geog Levels: county

# Remove detailed specifications from an existing dataset or
# time series table
remove_from_extract(
  nhgis_extract,
  datasets = ds_spec("1990_STF1", data_tables = "NP1")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: 
#> 
#> Dataset: 1990_STF1
#>   Tables: NP2, NP3
#>   Geog Levels: county
#> 
#> Time Series Table: A00
#>   Geog Levels: county
```

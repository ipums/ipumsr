# Remove values from an existing extract definition for an IPUMS microdata project

**\[experimental\]**

Remove existing values from an IPUMS microdata extract definition. All
fields are optional, and if omitted, will be unchanged.

This function is marked as experimental because it is typically not the
best option for maintaining reproducible extract definitions and may be
retired in the future. For reproducibility, users should strive to build
extract definitions with
[`define_extract_micro()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_micro.md).

If you have a complicated extract definition to revise, but do not have
the original extract definition code that created it, we suggest that
you save the revised extract as a JSON file with
[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md).
This will create a stable version of the extract definition that can be
used in the future as needed.

To add new values to an IPUMS microdata extract definition, see
[`add_to_extract()`](https://tech.popdata.org/ipumsr/dev/reference/add_to_extract_micro.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

## Usage

``` r
# S3 method for class 'micro_extract'
remove_from_extract(
  extract,
  samples = NULL,
  variables = NULL,
  time_use_variables = NULL,
  sample_members = NULL,
  ...
)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)
  object.

- samples:

  Character vector of sample names to remove from the extract
  definition.

- variables:

  Names of the variables to remove from the extract definition. All
  variable-specific fields for the indicated variables will also be
  removed. For removing values from variable-specific fields while
  retaining the variable, see examples.

- time_use_variables:

  Names of the time use variables to remove from the extract definition.
  All time use variable-specific fields for the indicated time use
  variables will also be removed. For removing time use
  variable-specific fields while retaining the time use variable, see
  examples.

- sample_members:

  Sample members to remove from the extract definition.

- ...:

  Ignored

## Value

A modified `micro_extract` object

## Details

If the supplied extract definition comes from a previously submitted
extract request, this function will reset the definition to an
unsubmitted state.

## See also

[`add_to_extract()`](https://tech.popdata.org/ipumsr/dev/reference/add_to_extract_micro.md)
to add values to an extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md)
to submit an extract request.

[`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
to download extract data files.

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_micro.md)
to create a new extract definition from scratch.

## Examples

``` r
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "USA example",
  samples = c("us2013a", "us2014a"),
  variables = list(
    var_spec("AGE", data_quality_flags = TRUE),
    var_spec("SEX", case_selections = "1"),
    "RACE"
  )
)

# Providing names of samples or variables will remove them and
# all of their associated specifications from the extract:
remove_from_extract(
  usa_extract,
  samples = "us2014a",
  variables = c("AGE", "RACE")
)
#> Unsubmitted IPUMS USA extract 
#> Description: USA example
#> 
#> Samples: (1 total) us2013a
#> Variables: (1 total) SEX

# To remove detailed specifications from a variable or time use variable,
# indicate the specifications to remove within `var_spec()` or
# `tu_var_spec()`. The named variable will be retained in the extract, but
# modified by removing the indicated specifications.
remove_from_extract(
  usa_extract,
  variables = var_spec("SEX", case_selections = "1")
)
#> Unsubmitted IPUMS USA extract 
#> Description: USA example
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (3 total) AGE, SEX, RACE

# To make multiple modifications, use a list of `var_spec()` objects.
remove_from_extract(
  usa_extract,
  variables = list(
    var_spec("SEX", case_selections = "1"),
    var_spec("AGE")
  )
)
#> Unsubmitted IPUMS USA extract 
#> Description: USA example
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (2 total) SEX, RACE
```

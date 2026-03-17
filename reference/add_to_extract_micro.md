# Add values to an existing extract definition for an IPUMS microdata collection

**\[experimental\]**

Add new values or replace existing values in an IPUMS microdata extract
definition. All fields are optional, and if omitted, will be unchanged.
Supplying a value for fields that take a single value, such as
`description` and `data_format`, will replace the existing value with
the supplied value.

This function is marked as experimental because it is typically not the
best option for maintaining reproducible extract definitions and may be
retired in the future. For reproducibility, users should strive to build
extract definitions with
[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md).

If you have a complicated extract definition to revise, but do not have
the original extract definition code that created it, we suggest that
you save the revised extract as a JSON file with
[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md).
This will create a stable version of the extract definition that can be
used in the future as needed.

To remove existing values from an IPUMS microdata extract definition,
use
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.micro_extract.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
# S3 method for class 'micro_extract'
add_to_extract(
  extract,
  description = NULL,
  samples = NULL,
  variables = NULL,
  time_use_variables = NULL,
  sample_members = NULL,
  data_format = NULL,
  data_structure = NULL,
  rectangular_on = NULL,
  case_select_who = NULL,
  data_quality_flags = NULL,
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

- samples:

  Vector of samples to include in the extract request. Use
  [`get_sample_info()`](https://tech.popdata.org/ipumsr/reference/get_sample_info.md)
  to identify sample IDs for a given collection.

- variables:

  Character vector of variable names or a list of `var_spec` objects
  created by
  [`var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md)
  containing specifications for all variables to include in the extract.

  If a variable already exists in the extract, its specifications will
  be added to those that already exist for that variable.

- time_use_variables:

  Vector of names of IPUMS-defined time use variables or a list of
  specifications for user-defined time use variables to include in the
  extract request. Use
  [`tu_var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md)
  to create a `tu_var_spec` object containing a time use variable
  specification.

- sample_members:

  Indication of whether to include additional sample members in the
  extract request. If provided, must be one of
  `"include_non_respondents"`, `"include_household_members"`, or both.

  Sample member selection is only available for the IPUMS ATUS
  collection (`"atus"`).

- data_format:

  Format for the output extract data file. Either `"fixed_width"` or
  `"csv"`.

  Note that while `"stata"`, `"spss"`, or `"sas9"` are also accepted,
  these file formats are not supported by ipumsr data-reading functions.

- data_structure:

  Data structure for the output extract data.

  - `"rectangular"` provides data in which every row has the same record
    type (determined by `"rectangular_on"`), with variables from other
    record types written onto associated records of the chosen type
    (e.g. household variables written onto person records).

  - `"hierarchical"` provides data that include rows of differing record
    types, with records ordered according to their hierarchical
    structure (e.g. each person record is followed by the activity
    records for that person).

  - `"household_only"` provides household records only. This data
    structure is only available for the IPUMS USA collection (`"usa"`).

- rectangular_on:

  If `data_structure` is `"rectangular"`, records on which to
  rectangularize. One of `"P"` (person), `"A"` (activity), `"I"`
  (injury) or `"R"` (round).

  Defaults to `"P"` if `data_structure` is `"rectangular"` and `NULL`
  otherwise.

- case_select_who:

  Indication of how to interpret any case selections included for
  variables in the extract definition.

  - `"individuals"` includes records for all individuals who match the
    specified case selections.

  - `"households"` includes records for all members of each household
    that contains an individual who matches the specified case
    selections.

  Defaults to `"individuals"`. Use
  [`var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md)
  to add case selections for specific variables.

- data_quality_flags:

  Set to `TRUE` to include data quality flags for all applicable
  variables in the extract definition. This will override the
  `data_quality_flags` specification for individual variables in the
  definition.

  Use
  [`var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md)
  to add data quality flags for specific variables.

- ...:

  Ignored

## Value

A modified `micro_extract` object

## Details

If the supplied extract definition comes from a previously submitted
extract request, this function will reset the definition to an
unsubmitted state.

To modify variable-specific parameters for variables that already exist
in the extract, create a new variable specification with
[`var_spec()`](https://tech.popdata.org/ipumsr/reference/var_spec.md).

## See also

[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.micro_extract.md)
to remove values from an extract definition.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request.

[`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
to download extract data files.

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
to create a new extract definition from scratch

## Examples

``` r
extract <- define_extract_micro(
  collection = "usa",
  description = "2013 ACS Data",
  samples = "us2013a",
  variables = c("SEX", "AGE", "YEAR")
)

# Add a single sample
add_to_extract(extract, samples = "us2014a")
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (3 total) SEX, AGE, YEAR

# Add samples and variables
extract2 <- add_to_extract(
  extract,
  samples = "us2014a",
  variables = c("MARST", "BIRTHYR")
)

# Modify specifications for variables in the extract by using `var_spec()`
# with the existing variable name:
add_to_extract(
  extract,
  samples = "us2014a",
  variables = var_spec("SEX", case_selections = "2")
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (3 total) SEX, AGE, YEAR

# You can make multiple modifications or additions by providing a list
# of `var_spec()` objects:
add_to_extract(
  extract,
  samples = "us2014a",
  variables = list(
    var_spec("RACE", attached_characteristics = "mother"),
    var_spec("SEX", case_selections = "2"),
    var_spec("RELATE")
  )
)
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS Data
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (5 total) SEX, AGE, YEAR, RACE, RELATE

# Values that only take a single value are replaced
add_to_extract(extract, description = "New description")$description
#> [1] "New description"
```

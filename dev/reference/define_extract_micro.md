# Define an extract request for an IPUMS microdata collection

Define the parameters of an IPUMS microdata extract request to be
submitted via the IPUMS API.

The IPUMS API currently supports the following microdata collections:

- [IPUMS USA](https://usa.ipums.org/)

- [IPUMS CPS](https://cps.ipums.org/)

- [IPUMS International](https://international.ipums.org/)

- IPUMS Time Use ([ATUS](https://www.atusdata.org/atus/),
  [AHTUS](https://www.ahtusdata.org/ahtus/),
  [MTUS](https://www.mtusdata.org/mtus/))

- IPUMS Health Surveys ([NHIS](https://nhis.ipums.org/),
  [MEPS](https://meps.ipums.org/))

Note that not all extract request parameters and options apply to all
collections. For a summary of supported features by collection, see the
[IPUMS API
documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/microdata/).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md)
and microdata extract definitions in
[`vignette("ipums-api-micro")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-micro.md).

## Usage

``` r
define_extract_micro(
  collection,
  description,
  samples,
  variables = NULL,
  time_use_variables = NULL,
  sample_members = NULL,
  data_format = "fixed_width",
  data_structure = "rectangular",
  rectangular_on = NULL,
  case_select_who = "individuals",
  data_quality_flags = NULL
)
```

## Arguments

- collection:

  Code for the IPUMS collection represented by this extract request. See
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_data_collections.md)
  for supported microdata collection codes.

- description:

  Description of the extract.

- samples:

  Vector of samples to include in the extract request. Use
  [`get_sample_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_sample_info.md)
  to identify sample IDs for a given collection.

- variables:

  Vector of variable names or a list of detailed variable specifications
  to include in the extract request. Use
  [`var_spec()`](https://tech.popdata.org/ipumsr/dev/reference/var_spec.md)
  to create a `var_spec` object containing a detailed variable
  specification. See examples.

- time_use_variables:

  Vector of names of IPUMS-defined time use variables or a list of
  specifications for user-defined time use variables to include in the
  extract request. Use
  [`tu_var_spec()`](https://tech.popdata.org/ipumsr/dev/reference/var_spec.md)
  to create a `tu_var_spec` object containing a time use variable
  specification. See examples.

  Time use variables are only available for IPUMS Time Use collections
  (`"atus"`, `"ahtus"`, and `"mtus"`).

- sample_members:

  Indication of whether to include additional sample members in the
  extract request. If provided, must be one of
  `"include_non_respondents"`, `"include_household_members"`, or both.

  Sample member selection is only available for the IPUMS ATUS
  collection (`"atus"`).

- data_format:

  Format for the output extract data file. Either `"fixed_width"` or
  `"csv"`.

  Note that while `"stata"`, `"spss"`, and `"sas9"` are also accepted,
  these file formats are not supported by ipumsr data-reading functions.

  Defaults to `"fixed_width"`.

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

  Defaults to `"rectangular"`.

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
  [`var_spec()`](https://tech.popdata.org/ipumsr/dev/reference/var_spec.md)
  to add case selections for specific variables.

- data_quality_flags:

  Set to `TRUE` to include data quality flags for all applicable
  variables in the extract definition. This will override the
  `data_quality_flags` specification for individual variables in the
  definition.

  Use
  [`var_spec()`](https://tech.popdata.org/ipumsr/dev/reference/var_spec.md)
  to add data quality flags for specific variables.

## Value

An object of class
[`micro_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)
containing the extract definition.

## See also

[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md)
to submit an extract request for processing.

[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
and
[`define_extract_from_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
to share an extract definition.

## Examples

``` r
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "2013-2014 ACS Data",
  samples = c("us2013a", "us2014a"),
  variables = c("SEX", "AGE", "YEAR")
)

usa_extract
#> Unsubmitted IPUMS USA extract 
#> Description: 2013-2014 ACS Data
#> 
#> Samples: (2 total) us2013a, us2014a
#> Variables: (3 total) SEX, AGE, YEAR

# Use `var_spec()` to created detailed variable specifications:
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "Example USA extract definition",
  samples = c("us2013a", "us2014a"),
  variables = var_spec(
    "SEX",
    case_selections = "2",
    attached_characteristics = c("mother", "father")
  )
)

# For multiple variables, provide a list of `var_spec` objects and/or
# variable names.
cps_extract <- define_extract_micro(
  collection = "cps",
  description = "Example CPS extract definition",
  samples = c("cps2020_02s", "cps2020_03s"),
  variables = list(
    var_spec("AGE", data_quality_flags = TRUE),
    var_spec("SEX", case_selections = "2"),
    "RACE"
  )
)

cps_extract
#> Unsubmitted IPUMS CPS extract 
#> Description: Example CPS extract definition
#> 
#> Samples: (2 total) cps2020_02s, cps2020_03s
#> Variables: (3 total) AGE, SEX, RACE

# To recycle specifications to many variables, it may be useful to
# create variables prior to defining the extract:
var_names <- c("AGE", "SEX")

my_vars <- purrr::map(
  var_names,
  ~ var_spec(.x, attached_characteristics = "mother")
)

ipumsi_extract <- define_extract_micro(
  collection = "ipumsi",
  description = "Extract definition with predefined variables",
  samples = c("br2010a", "cl2017a"),
  variables = my_vars
)

# Extract specifications can be indexed by name
names(ipumsi_extract$samples)
#> [1] "br2010a" "cl2017a"

names(ipumsi_extract$variables)
#> [1] "AGE" "SEX"

ipumsi_extract$variables$AGE
#> $name
#> [1] "AGE"
#> 
#> $attached_characteristics
#> [1] "mother"
#> 
#> attr(,"class")
#> [1] "var_spec"   "ipums_spec" "list"      

# IPUMS Time Use collections allow selection of IPUMS-defined and
# user-defined time use variables:
define_extract_micro(
  collection = "atus",
  description = "ATUS extract with time use variables",
  samples = "at2007",
  time_use_variables = list(
    "ACT_PCARE",
    tu_var_spec(
      "MYTIMEUSEVAR",
      owner = "example@example.com"
    )
  )
)
#> Unsubmitted IPUMS ATUS extract 
#> Description: ATUS extract with time use variables
#> 
#> Samples: (1 total) at2007
#> Time Use Variables: (2 total) ACT_PCARE, MYTIMEUSEVAR

if (FALSE) { # \dontrun{
# Use the extract definition to submit an extract request to the API
submit_extract(usa_extract)
} # }
```

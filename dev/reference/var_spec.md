# Create variable and sample specifications for IPUMS microdata extract requests

Provide specifications for individual variables and time use variables
when defining an IPUMS microdata extract request.

Currently, no additional specifications are available for IPUMS samples.

Note that not all variable-level options are available across all IPUMS
data collections. For a summary of supported features by collection, see
the [IPUMS API
documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/microdata/).

Learn more about microdata extract definitions in
[`vignette("ipums-api-micro")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-micro.md).

## Usage

``` r
var_spec(
  name,
  case_selections = NULL,
  case_selection_type = NULL,
  attached_characteristics = NULL,
  data_quality_flags = NULL,
  adjust_monetary_values = NULL,
  preselected = NULL
)

tu_var_spec(name, owner = NULL)

samp_spec(name)
```

## Arguments

- name:

  Name of the sample, variable, or time use variable.

- case_selections:

  A character vector of values of the given variable that should be used
  to select cases. Values should be specified exactly as they appear in
  the "CODES" tab for the given variable in the web-based extract
  builder, including zero-padding (e.g. see the "CODES" tab for IPUMS
  CPS variable
  [EDUC](https://cps.ipums.org/cps-action/variables/EDUC#codes_section)).

- case_selection_type:

  One of `"general"` or `"detailed"` indicating whether the values in
  `case_selections` should be matched against the general or detailed
  codes for the given variable. Only some variables have detailed codes.
  See IPUMS USA variable
  [RACE](https://usa.ipums.org/usa-action/variables/RACE#codes_section)
  for an example of a variable with general and detailed codes.

  Defaults to `"general"` if any `case_selections` are specified.

- attached_characteristics:

  Whose characteristics should be attached, if any? Accepted values are
  `"mother"`, `"father"`, `"spouse"`, `"head"`, or a combination.
  Specifying attached characteristics will add variables to your extract
  that contain the values for the given variable for the specified
  household members. For example, variable "AGE_MOM" will be added if
  `"mother"` is specified for the variable `"AGE"`.

  For data collections with information on same-sex couples, specifying
  `"mother"` or `"father"` will attach the characteristics of both
  mothers or both fathers for children with same-sex parents, by adding
  variables with names of the form "AGE_MOM" and "AGE_MOM2".

- data_quality_flags:

  Logical indicating whether to include data quality flags for the given
  variable. By default, data quality flags are not included.

- adjust_monetary_values:

  Logical indicating whether to include the variable's
  inflation-adjusted equivalent, if available.

- preselected:

  Logical indicating whether the variable is preselected. This is not
  needed for external use.

- owner:

  For user-defined time use variables, the email of the user account
  associated with the time use variable. Currently, only the email of
  the user submitting the extract request is supported.

## Value

A `var_spec`, `tu_var_spec`, or `samp_spec` object.

## Examples

``` r
var1 <- var_spec(
  "SCHOOL",
  case_selections = c("1", "2"),
  data_quality_flags = TRUE
)

var2 <- var_spec(
  "RACE",
  case_selections = c("140", "150"),
  case_selection_type = "detailed",
  attached_characteristics = c("mother", "spouse")
)

# Use variable specifications in a microdata extract definition:
extract <- define_extract_micro(
  collection = "usa",
  description = "Example extract",
  samples = "us2017b",
  variables = list(var1, var2)
)

extract$variables$SCHOOL
#> $name
#> [1] "SCHOOL"
#> 
#> $case_selections
#> [1] "1" "2"
#> 
#> $data_quality_flags
#> [1] TRUE
#> 
#> $case_selection_type
#> [1] "general"
#> 
#> attr(,"class")
#> [1] "var_spec"   "ipums_spec" "list"      

extract$variables$RACE
#> $name
#> [1] "RACE"
#> 
#> $case_selections
#> [1] "140" "150"
#> 
#> $attached_characteristics
#> [1] "mother" "spouse"
#> 
#> $case_selection_type
#> [1] "detailed"
#> 
#> attr(,"class")
#> [1] "var_spec"   "ipums_spec" "list"      

# For IPUMS Time Use collections, use `tu_var_spec()` to include user-defined
# time use variables
my_time_use_variable <- tu_var_spec(
  "MYTIMEUSEVAR",
  owner = "example@example.com"
)

# IPUMS-defined time use variables can be included either as `tu_var_spec`
# objects or with just the variable name:
define_extract_micro(
  collection = "atus",
  description = "Requesting user- and IPUMS-defined time use variables",
  samples = "at2007",
  time_use_variables = list(
    my_time_use_variable,
    tu_var_spec("ACT_PCARE"),
    "ACT_SOCIAL"
  )
)
#> Unsubmitted IPUMS ATUS extract 
#> Description: Requesting user- and IPUMS-defined time use variables
#> 
#> Samples: (1 total) at2007
#> Time Use Variables: (3 total) MYTIMEUSEVAR, ACT_PCARE, ACT_SOCIAL
```

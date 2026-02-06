# Convert labelled data values to NA

Convert data values in a
[`labelled`](https://haven.tidyverse.org/reference/labelled.html) vector
to `NA` based on the value labels associated with that vector. Ignores
values that do not have a label.

## Usage

``` r
lbl_na_if(x, .predicate)
```

## Arguments

- x:

  A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
  vector

- .predicate:

  A function taking `.val` and `.lbl` arguments that returns `TRUE` for
  all values that should be converted to `NA`.

  Can be provided as an anonymous function or formula. See Details
  section.

## Value

A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
vector

## Details

Several `lbl_*()` functions include arguments that can be passed a
function of `.val` and/or `.lbl`. These refer to the existing values and
labels in the input vector, respectively.

Use `.val` to refer to the *values* in the vector's value labels. Use
`.lbl` to refer to the *label names* in the vector's value labels.

Note that not all `lbl_*()` functions support both of these arguments.

## See also

Other lbl_helpers:
[`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md),
[`lbl_add()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_add.md),
[`lbl_clean()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_clean.md),
[`lbl_define()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_define.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_relabel.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/dev/reference/zap_ipums_attributes.md)

## Examples

``` r
x <- haven::labelled(
  c(10, 10, 11, 20, 30, 99, 30, 10),
  c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
)

# Convert labelled values greater than 90 to `NA`
lbl_na_if(x, function(.val, .lbl) .val >= 90)
#> <labelled<double>[8]>
#> [1] 10 10 11 20 30 NA 30 10
#> 
#> Labels:
#>  value                    label
#>     10                      Yes
#>     11 Yes - Logically Assigned
#>     20                       No
#>     30                    Maybe

# Can use purrr-style notation
lbl_na_if(x, ~ .lbl %in% c("Maybe"))
#> <labelled<double>[8]>
#> [1] 10 10 11 20 NA 99 NA 10
#> 
#> Labels:
#>  value                    label
#>     10                      Yes
#>     11 Yes - Logically Assigned
#>     20                       No
#>     99                      NIU

# Or refer to named function
na_function <- function(.val, .lbl) .val >= 90
lbl_na_if(x, na_function)
#> <labelled<double>[8]>
#> [1] 10 10 11 20 30 NA 30 10
#> 
#> Labels:
#>  value                    label
#>     10                      Yes
#>     11 Yes - Logically Assigned
#>     20                       No
#>     30                    Maybe
```

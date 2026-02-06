# Modify value labels for a labelled vector

Update the mapping between values and labels in a
[`labelled`](https://haven.tidyverse.org/reference/labelled.html)
vector. These functions allow you to simultaneously update data values
and the existing value labels. Modifying data values directly does not
result in updated value labels.

Use `lbl_relabel()` to manually specify new value/label mappings. This
allows for the addition of new labels.

Use `lbl_collapse()` to collapse detailed labels into more general
categories. Values can be grouped together and associated with
individual labels that already exist in the `labelled` vector.

Unlabelled values will be converted to `NA`.

## Usage

``` r
lbl_relabel(x, ...)

lbl_collapse(x, .fun)
```

## Arguments

- x:

  A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
  vector

- ...:

  Arbitrary number of two-sided formulas.

  The left hand side should be a label placeholder created with
  [`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md) or a
  value that already exists in the data.

  The right hand side should be a function taking `.val` and `.lbl`
  arguments that evaluates to `TRUE` for all cases that should receive
  the label specified on the left hand side.

  Can be provided as an anonymous function or formula. See Details
  section.

- .fun:

  A function taking `.val` and `.lbl` arguments that returns the value
  associated with an existing label in the vector. Input values to this
  function will be relabeled with the label of the function's output
  value.

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
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_na_if.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/dev/reference/zap_ipums_attributes.md)

## Examples

``` r
x <- haven::labelled(
  c(10, 10, 11, 20, 21, 30, 99, 30, 10),
  c(
    Yes = 10, `Yes - Logically Assigned` = 11,
    No = 20, Unlikely = 21, Maybe = 30, NIU = 99
  )
)

# Convert cases with value 11 to value 10 and associate with 10's label
lbl_relabel(x, 10 ~ .val == 11)
#> <labelled<double>[9]>
#> [1] 10 10 10 20 21 30 99 30 10
#> 
#> Labels:
#>  value    label
#>     10      Yes
#>     20       No
#>     21 Unlikely
#>     30    Maybe
#>     99      NIU
lbl_relabel(x, lbl("Yes") ~ .val == 11)
#> <labelled<double>[9]>
#> [1] 10 10 10 20 21 30 99 30 10
#> 
#> Labels:
#>  value    label
#>     10      Yes
#>     20       No
#>     21 Unlikely
#>     30    Maybe
#>     99      NIU

# To relabel using new value/label pairs, use `lbl()` to define a new pair
lbl_relabel(
  x,
  lbl(10, "Yes/Yes-ish") ~ .val %in% c(10, 11),
  lbl(90, "???") ~ .val == 99 | .lbl == "Maybe"
)
#> <labelled<double>[9]>
#> [1] 10 10 10 20 21 90 90 90 10
#> 
#> Labels:
#>  value       label
#>     10 Yes/Yes-ish
#>     20          No
#>     21    Unlikely
#>     90         ???

# Collapse labels to create new label groups
lbl_collapse(x, ~ (.val %/% 10) * 10)
#> <labelled<double>[9]>
#> [1] 10 10 10 20 20 30 90 30 10
#> 
#> Labels:
#>  value label
#>     10   Yes
#>     20    No
#>     30 Maybe
#>     90   NIU

# These are equivalent
lbl_collapse(x, ~ ifelse(.val == 10, 11, .val))
#> <labelled<double>[9]>
#> [1] 11 11 11 20 21 30 99 30 11
#> 
#> Labels:
#>  value                    label
#>     11 Yes - Logically Assigned
#>     20                       No
#>     21                 Unlikely
#>     30                    Maybe
#>     99                      NIU
lbl_relabel(x, 11 ~ .val == 10)
#> <labelled<double>[9]>
#> [1] 11 11 11 20 21 30 99 30 11
#> 
#> Labels:
#>  value                    label
#>     11 Yes - Logically Assigned
#>     20                       No
#>     21                 Unlikely
#>     30                    Maybe
#>     99                      NIU
```

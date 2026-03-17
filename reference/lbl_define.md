# Define labels for an unlabelled vector

Create a
[`labelled`](https://haven.tidyverse.org/reference/labelled.html) vector
from an unlabelled vector using
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
syntax, allowing for the grouping of multiple values into a single
label. Values not assigned a label remain unlabelled.

## Usage

``` r
lbl_define(x, ...)
```

## Arguments

- x:

  An unlabelled vector

- ...:

  Arbitrary number of two-sided formulas.

  The left hand side should be a label placeholder created with
  [`lbl()`](https://tech.popdata.org/ipumsr/reference/lbl.md).

  The right hand side should be a function taking `.val` that evaluates
  to `TRUE` for all cases that should receive the label specified on the
  left hand side.

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
[`lbl()`](https://tech.popdata.org/ipumsr/reference/lbl.md),
[`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md),
[`lbl_clean()`](https://tech.popdata.org/ipumsr/reference/lbl_clean.md),
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/reference/zap_ipums_attributes.md)

## Examples

``` r
age <- c(10, 12, 16, 18, 20, 22, 25, 27)

# Group age values into two label groups.
# Values not captured by the right hand side functions remain unlabelled
lbl_define(
  age,
  lbl(1, "Pre-college age") ~ .val < 18,
  lbl(2, "College age") ~ .val >= 18 & .val <= 22
)
#> <labelled<double>[8]>
#> [1]  1  1  1  2  2  2 25 27
#> 
#> Labels:
#>  value           label
#>      1 Pre-college age
#>      2     College age
```

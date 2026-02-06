# Remove label attributes from a data frame or labelled vector

Remove all label attributes (value labels, variable labels, and variable
descriptions) from a data frame or vector.

## Usage

``` r
zap_ipums_attributes(x)
```

## Arguments

- x:

  A data frame or
  [labelled](https://haven.tidyverse.org/reference/labelled.html) vector
  (for instance, from a data frame column)

## Value

An object of the same type as `x` without `"val_labels"`, `"var_label`",
and `"var_desc"` attributes.

## See also

Other lbl_helpers:
[`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md),
[`lbl_add()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_add.md),
[`lbl_clean()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_clean.md),
[`lbl_define()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_define.md),
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_na_if.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_relabel.md)

## Examples

``` r
cps <- read_ipums_micro(ipums_example("cps_00157.xml"))
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

attributes(cps$YEAR)
#> $label
#> [1] "Survey year"
#> 
#> $var_desc
#> [1] "YEAR reports the year in which the survey was conducted.  YEARP is repeated on person records."
#> 
attributes(zap_ipums_attributes(cps$YEAR))
#> NULL

cps <- zap_ipums_attributes(cps)
attributes(cps$YEAR)
#> NULL
attributes(cps$INCTOT)
#> NULL
```

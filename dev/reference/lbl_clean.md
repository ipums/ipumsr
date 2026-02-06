# Clean unused labels

Remove labels that do not appear in the data. When converting labelled
values to a factor, this avoids the creation of additional factor
levels.

## Usage

``` r
lbl_clean(x)
```

## Arguments

- x:

  A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
  vector

## Value

A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
vector

## See also

Other lbl_helpers:
[`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md),
[`lbl_add()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_add.md),
[`lbl_define()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_define.md),
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_na_if.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_relabel.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/dev/reference/zap_ipums_attributes.md)

## Examples

``` r
x <- haven::labelled(
  c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  c(Q1 = 1, Q2 = 2, Q3 = 3, Q4 = 4)
)

lbl_clean(x)
#> <labelled<double>[9]>
#> [1] 1 2 3 1 2 3 1 2 3
#> 
#> Labels:
#>  value label
#>      1    Q1
#>      2    Q2
#>      3    Q3

# Compare the factor levels of the normal and cleaned labels after coercion
as_factor(lbl_clean(x))
#> [1] Q1 Q2 Q3 Q1 Q2 Q3 Q1 Q2 Q3
#> Levels: Q1 Q2 Q3

as_factor(x)
#> [1] Q1 Q2 Q3 Q1 Q2 Q3 Q1 Q2 Q3
#> Levels: Q1 Q2 Q3 Q4
```

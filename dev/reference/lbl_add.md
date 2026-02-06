# Add labels for unlabelled values

Add labels for values that don't already have them in a
[`labelled`](https://haven.tidyverse.org/reference/labelled.html)
vector.

## Usage

``` r
lbl_add(x, ...)

lbl_add_vals(x, labeller = as.character, vals = NULL)
```

## Arguments

- x:

  A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
  vector

- ...:

  Arbitrary number of label placeholders created with
  [`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md)
  indicating the value/label pairs to add.

- labeller:

  A function that takes values being added as an argument and returns
  the labels to associate with those values. By default, uses the values
  themselves after converting to character.

- vals:

  Vector of values to be labelled. If `NULL`, labels all unlabelled
  values that exist in the data.

## Value

A [`labelled`](https://haven.tidyverse.org/reference/labelled.html)
vector

## See also

Other lbl_helpers:
[`lbl()`](https://tech.popdata.org/ipumsr/dev/reference/lbl.md),
[`lbl_clean()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_clean.md),
[`lbl_define()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_define.md),
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_na_if.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/dev/reference/lbl_relabel.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/dev/reference/zap_ipums_attributes.md)

## Examples

``` r
x <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)

# Add new labels manually
lbl_add(
  x,
  lbl(100, "$100"),
  lbl(105, "$105"),
  lbl(200, "$200"),
  lbl(230, "$230")
)
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100    $100
#>    105    $105
#>    200    $200
#>    230    $230
#>    990 Unknown
#>    999     NIU

# Add labels for all unlabelled values
lbl_add_vals(x)
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100     100
#>    105     105
#>    200     200
#>    230     230
#>    990 Unknown
#>    999     NIU

# Update label names while adding
lbl_add_vals(x, labeller = ~ paste0("$", .))
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100    $100
#>    105    $105
#>    200    $200
#>    230    $230
#>    990 Unknown
#>    999     NIU

# Add labels for select values
lbl_add_vals(x, vals = c(100, 200))
#> <labelled<double>[6]>
#> [1] 100 200 105 990 999 230
#> 
#> Labels:
#>  value   label
#>    100     100
#>    200     200
#>    990 Unknown
#>    999     NIU
```

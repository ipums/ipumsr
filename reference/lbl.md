# Make a label placeholder object

Define a new label/value pair. For use in functions like
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md)
and [`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md).

## Usage

``` r
lbl(...)
```

## Arguments

- ...:

  Either one or two arguments specifying the label (`.lbl`) and value
  (`.val`) to use in the new label pair.

  If arguments are named, they must be named `.val` and/or `.lbl`.

  If a single unnamed value is passed, it is used as the `.lbl` for the
  new label. If two unnamed values are passed, they are used as the
  `.val` and `.lbl`, respectively.

## Value

A `label_placeholder` object

## Details

Several `lbl_*()` functions include arguments that can be passed a
function of `.val` and/or `.lbl`. These refer to the existing values and
labels in the input vector, respectively.

Use `.val` to refer to the *values* in the vector's value labels. Use
`.lbl` to refer to the *label names* in the vector's value labels.

Note that not all `lbl_*()` functions support both of these arguments.

## See also

Other lbl_helpers:
[`lbl_add()`](https://tech.popdata.org/ipumsr/reference/lbl_add.md),
[`lbl_clean()`](https://tech.popdata.org/ipumsr/reference/lbl_clean.md),
[`lbl_define()`](https://tech.popdata.org/ipumsr/reference/lbl_define.md),
[`lbl_na_if()`](https://tech.popdata.org/ipumsr/reference/lbl_na_if.md),
[`lbl_relabel()`](https://tech.popdata.org/ipumsr/reference/lbl_relabel.md),
[`zap_ipums_attributes()`](https://tech.popdata.org/ipumsr/reference/zap_ipums_attributes.md)

## Examples

``` r
# Label placeholder with no associated value
lbl("New label")
#> $.val
#> NULL
#> 
#> $.lbl
#> [1] "New label"
#> 
#> attr(,"class")
#> [1] "lbl_placeholder"

# Label placeholder with a value/label pair
lbl(10, "New label")
#> $.val
#> [1] 10
#> 
#> $.lbl
#> [1] "New label"
#> 
#> attr(,"class")
#> [1] "lbl_placeholder"

# Use placeholders as inputs to other label handlers
x <- haven::labelled(
  c(100, 200, 105, 990, 999, 230),
  c(`Unknown` = 990, NIU = 999)
)

x <- lbl_add(
  x,
  lbl(100, "$100"),
  lbl(105, "$105"),
  lbl(200, "$200"),
  lbl(230, "$230")
)

lbl_relabel(x, lbl(9999, "Missing") ~ .val > 900)
#> <labelled<double>[6]>
#> [1]  100  200  105 9999 9999  230
#> 
#> Labels:
#>  value   label
#>    100    $100
#>    105    $105
#>    200    $200
#>    230    $230
#>   9999 Missing
```

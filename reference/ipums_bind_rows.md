# Bind multiple data frames by row, preserving labelled attributes

Analogous to
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
but preserves the labelled attributes provided with IPUMS data.

## Usage

``` r
ipums_bind_rows(..., .id = NULL)
```

## Arguments

- ...:

  Data frames or
  [`tibbles`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  to combine. Each argument can be a data frame or a list of data
  frames. When binding, columns are matched by name. Missing columns
  will be filled with `NA`.

- .id:

  The name of an optional identifier column. Provide a string to create
  an output column that identifies each input. The column will use names
  if available, otherwise it will use positions.

## Value

Returns the same type as the first input. Either a `data.frame`,
`tbl_df`, or `grouped_df`

## Examples

``` r
file <- ipums_example("nhgis0712_csv.zip")

d1 <- read_ipums_agg(
  file,
  file_select = 1,
  verbose = FALSE
)

d2 <- read_ipums_agg(
  file,
  file_select = 2,
  verbose = FALSE
)

# Variables have associated label attributes:
ipums_var_label(d1$PMSAA)
#> [1] "Primary Metropolitan Statistical Area Code"

# Preserve labels when binding data sources:
d <- ipums_bind_rows(d1, d2)
ipums_var_label(d$PMSAA)
#> [1] "Primary Metropolitan Statistical Area Code"

# dplyr `bind_rows()` drops labels:
d <- dplyr::bind_rows(d1, d2)
ipums_var_label(d$PMSAA)
#> [1] NA
```

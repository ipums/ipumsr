# Join tabular data to geographic boundaries

These functions are analogous to dplyr's
[joins](https://dplyr.tidyverse.org/reference/mutate-joins.html), except
that:

- They operate on a data frame and an
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object

- They retain the variable attributes provided in IPUMS files and loaded
  by ipumsr data-reading functions

- They handle minor incompatibilities between attributes in spatial and
  tabular data that emerge in some IPUMS files

## Usage

``` r
ipums_shape_left_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_right_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_inner_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)

ipums_shape_full_join(
  data,
  shape_data,
  by,
  suffix = c("", "SHAPE"),
  verbose = TRUE
)
```

## Arguments

- data:

  A tibble or data frame. Typically, this will contain data that has
  been aggregated to a specific geographic level.

- shape_data:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  loaded with
  [`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md).

- by:

  Character vector of variables to join by. See
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for syntax.

- suffix:

  If there are non-joined duplicate variables in the two data sources,
  these suffixes will be added to the output to disambiguate them.
  Should be a character vector of length 2.

  Defaults to adding the `"SHAPE"` suffix to duplicated variables in
  `shape_file`.

- verbose:

  If `TRUE`, display information about any geometries that were
  unmatched during the join.

## Value

An `sf` object containing the joined data

## Examples

``` r
data <- read_ipums_agg(
  ipums_example("nhgis0972_csv.zip"),
  verbose = FALSE
)

sf_data <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))
joined_data <- ipums_shape_inner_join(data, sf_data, by = "GISJOIN")

colnames(joined_data)
#>  [1] "GISJOIN"    "YEAR"       "STUSAB"     "CMSA"       "DIVISIONA" 
#>  [6] "MSA_CMSAA"  "PMSA"       "PMSAA"      "REGIONA"    "STATEA"    
#> [11] "AREALAND"   "AREAWAT"    "ANPSADPI"   "FUNCSTAT"   "INTPTLAT"  
#> [16] "INTPTLNG"   "PSADC"      "D6Z001"     "D6Z002"     "D6Z003"    
#> [21] "D6Z004"     "D6Z005"     "D6Z006"     "D6Z007"     "D6Z008"    
#> [26] "PMSASHAPE"  "MSACMSA"    "ALTCMSA"    "GISJOIN2"   "SHAPE_AREA"
#> [31] "SHAPE_LEN"  "GISJOIN3"   "geometry"  
```

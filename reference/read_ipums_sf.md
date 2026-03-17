# Read spatial data from an IPUMS extract

Read a spatial data file (also referred to as a GIS file or shapefile)
from an IPUMS extract into an `sf` object from the
[sf](https://r-spatial.github.io/sf/) package.

## Usage

``` r
read_ipums_sf(
  shape_file,
  file_select = NULL,
  vars = NULL,
  encoding = NULL,
  bind_multiple = FALSE,
  add_layer_var = NULL,
  verbose = FALSE
)
```

## Arguments

- shape_file:

  Path to a single .shp file or a .zip archive containing at least one
  .shp file. See Details section.

- file_select:

  If `shape_file` is a .zip archive that contains multiple files, an
  expression identifying the files to load. Accepts a character string
  specifying the file name, a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md),
  or index position. If multiple files are selected, `bind_multiple`
  must be equal to `TRUE`.

- vars:

  Names of variables to include in the output. Accepts a character
  vector of names or a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md).
  If `NULL`, includes all variables in the file.

- encoding:

  Encoding to use when reading the shape file. If `NULL`, defaults to
  `"latin1"` unless the file includes a .cpg metadata file with encoding
  information. The default value should generally be appropriate.

- bind_multiple:

  If `TRUE` and `shape_file` contains multiple .shp files, row-bind the
  files into a single `sf` object. Useful when `shape_file` contains
  multiple files that represent the same geographic units for different
  extents (e.g. block-level data for multiple states).

- add_layer_var:

  If `TRUE`, add a variable to the output data indicating the file that
  each row originates from. Defaults to `FALSE` unless
  `bind_multiple = TRUE` and multiple files exist in `shape_file`.

  The column name will always be prefixed with `"layer"`, but will be
  adjusted to avoid name conflicts if another column named `"layer"`
  already exists in the data.

- verbose:

  If `TRUE` report additional progress information on load.

## Value

An [sf](https://r-spatial.github.io/sf/reference/sf.html) object

## Details

Some IPUMS products provide shapefiles in a "nested" .zip archive. That
is, each shapefile (including a .shp as well as accompanying files) is
compressed in its own archive, and the collection of all shapefiles
provided in an extract is also compressed into a single .zip archive.

`read_ipums_sf()` is designed to handle this structure. However, if any
files are altered such that an internal .zip archive contains *multiple*
shapefiles, this function will throw an error. If this is the case, you
may need to manually unzip the downloaded file before loading it into R.

## See also

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
or
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
to read tabular data from an IPUMS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Example shapefile from NHGIS
shape_ex1 <- ipums_example("nhgis0972_shape_small.zip")
data_ex1 <- read_ipums_agg(ipums_example("nhgis0972_csv.zip"), verbose = FALSE)

sf_data <- read_ipums_sf(shape_ex1)

sf_data
#> Simple feature collection with 71 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -2336182 ymin: -1247086 xmax: 2075339 ymax: 1476544
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 71 × 9
#>    PMSA  MSACMSA ALTCMSA GISJOIN GISJOIN2   SHAPE_AREA SHAPE_LEN GISJOIN3 
#>    <chr> <chr>   <chr>   <chr>   <chr>           <dbl>     <dbl> <chr>    
#>  1 3280  3282    41      G3280   3280      2840869482.   320921. G32823280
#>  2 5760  5602    70      G5760   5760       237428573.   126226. G56025760
#>  3 1145  3362    42      G1145   1145      3730749183.   489789. G33621145
#>  4 1920  1922    31      G1920   1920     12068105590.   543164. G19221920
#>  5 0080  1692    28      G0080   0080      2401347006.   218892. G16920080
#>  6 1640  1642    21      G1640   1640      5608404797.   415671. G16421640
#>  7 2960  1602    14      G2960   2960      2387760183.   241350. G16022960
#>  8 5190  5602    70      G5190   5190      2939483018.   872897. G56025190
#>  9 1125  2082    34      G1125   1125      1946034315.   199097. G20821125
#> 10 1120  1122    07      G1120   1120      4715670489.   922769. G11221120
#> # ℹ 61 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>

# To combine spatial data with tabular data without losing the attributes
# included in the tabular data, use an ipums shape join:
ipums_shape_full_join(data_ex1, sf_data, by = "GISJOIN")
#> Simple feature collection with 71 features and 32 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -2336182 ymin: -1247086 xmax: 2075339 ymax: 1476544
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 71 × 33
#>    GISJOIN  YEAR STUSAB CMSA  DIVISIONA MSA_CMSAA PMSA      PMSAA REGIONA STATEA
#>    <chr>   <dbl> <chr>  <chr> <lgl>         <dbl> <chr>     <chr> <lgl>   <lgl> 
#>  1 G3280    1990 CT     41    NA             3282 Hartford… 3280  NA      NA    
#>  2 G5760    1990 CT     70    NA             5602 Norwalk,… 5760  NA      NA    
#>  3 G1145    1990 TX     42    NA             3362 Brazoria… 1145  NA      NA    
#>  4 G1920    1990 TX     31    NA             1922 Dallas, … 1920  NA      NA    
#>  5 G0080    1990 OH     28    NA             1692 Akron, O… 0080  NA      NA    
#>  6 G1640    1990 ##     21    NA             1642 Cincinna… 1640  NA      NA    
#>  7 G2960    1990 IN     14    NA             1602 Gary--Ha… 2960  NA      NA    
#>  8 G5190    1990 NJ     70    NA             5602 Monmouth… 5190  NA      NA    
#>  9 G1125    1990 CO     34    NA             2082 Boulder-… 1125  NA      NA    
#> 10 G1120    1990 MA     07    NA             1122 Boston, … 1120  NA      NA    
#> # ℹ 61 more rows
#> # ℹ 23 more variables: AREALAND <chr>, AREAWAT <chr>, ANPSADPI <chr>,
#> #   FUNCSTAT <chr>, INTPTLAT <dbl>, INTPTLNG <dbl>, PSADC <dbl>, D6Z001 <dbl>,
#> #   D6Z002 <dbl>, D6Z003 <dbl>, D6Z004 <dbl>, D6Z005 <dbl>, D6Z006 <dbl>,
#> #   D6Z007 <dbl>, D6Z008 <dbl>, PMSASHAPE <chr>, MSACMSA <chr>, ALTCMSA <chr>,
#> #   GISJOIN2 <chr>, SHAPE_AREA <dbl>, SHAPE_LEN <dbl>, GISJOIN3 <chr>,
#> #   geometry <MULTIPOLYGON [m]>

shape_ex2 <- ipums_example("nhgis0712_shape_small.zip")

# Shapefiles are provided in .zip archives that may contain multiple
# files. Select a single file with `file_select`:
read_ipums_sf(shape_ex2, file_select = matches("us_pmsa_1990"))
#> Simple feature collection with 71 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -2336182 ymin: -1247086 xmax: 2075339 ymax: 1476544
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 71 × 9
#>    PMSA  MSACMSA ALTCMSA GISJOIN GISJOIN2   SHAPE_AREA SHAPE_LEN GISJOIN3 
#>    <chr> <chr>   <chr>   <chr>   <chr>           <dbl>     <dbl> <chr>    
#>  1 3280  3282    41      G3280   3280      2840869482.   320921. G32823280
#>  2 5760  5602    70      G5760   5760       237428573.   126226. G56025760
#>  3 1145  3362    42      G1145   1145      3730749183.   489789. G33621145
#>  4 1920  1922    31      G1920   1920     12068105590.   543164. G19221920
#>  5 0080  1692    28      G0080   0080      2401347006.   218892. G16920080
#>  6 1640  1642    21      G1640   1640      5608404797.   415671. G16421640
#>  7 2960  1602    14      G2960   2960      2387760183.   241350. G16022960
#>  8 5190  5602    70      G5190   5190      2939483018.   872897. G56025190
#>  9 1125  2082    34      G1125   1125      1946034315.   199097. G20821125
#> 10 1120  1122    07      G1120   1120      4715670489.   922769. G11221120
#> # ℹ 61 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>

# Or row-bind files with `bind_multiple`. This may be useful for files of
# the same geographic level that cover different extents
read_ipums_sf(
  shape_ex2,
  file_select = matches("us_pmsa"),
  bind_multiple = TRUE
)
#> Simple feature collection with 144 features and 9 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -2336182 ymin: -1247086 xmax: 2075339 ymax: 1493412
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 144 × 10
#>    layer    PMSA  MSACMSA ALTCMSA GISJOIN GISJOIN2 SHAPE_AREA SHAPE_LEN GISJOIN3
#>    <chr>    <chr> <chr>   <chr>   <chr>   <chr>         <dbl>     <dbl> <chr>   
#>  1 US_pmsa… 3280  3282    41      G3280   3280        2.84e 9   320921. G328232…
#>  2 US_pmsa… 5760  5602    70      G5760   5760        2.37e 8   126226. G560257…
#>  3 US_pmsa… 1145  3362    42      G1145   1145        3.73e 9   489789. G336211…
#>  4 US_pmsa… 1920  1922    31      G1920   1920        1.21e10   543164. G192219…
#>  5 US_pmsa… 0080  1692    28      G0080   0080        2.40e 9   218892. G169200…
#>  6 US_pmsa… 1640  1642    21      G1640   1640        5.61e 9   415671. G164216…
#>  7 US_pmsa… 2960  1602    14      G2960   2960        2.39e 9   241350. G160229…
#>  8 US_pmsa… 5190  5602    70      G5190   5190        2.94e 9   872897. G560251…
#>  9 US_pmsa… 1125  2082    34      G1125   1125        1.95e 9   199097. G208211…
#> 10 US_pmsa… 1120  1122    07      G1120   1120        4.72e 9   922769. G112211…
#> # ℹ 134 more rows
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>
```

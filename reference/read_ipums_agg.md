# Read data from an IPUMS aggregate data extract

Read a .csv file from an extract downloaded from an IPUMS aggregate data
collection (IPUMS NHGIS or IPUMS IHGIS).

To read spatial data from an NHGIS extract, use
[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md).

## Usage

``` r
read_ipums_agg(
  data_file,
  file_select = NULL,
  vars = NULL,
  col_types = NULL,
  n_max = Inf,
  guess_max = min(n_max, 1000),
  var_attrs = c("val_labels", "var_label", "var_desc"),
  remove_extra_header = TRUE,
  file_encoding = NULL,
  verbose = TRUE
)
```

## Arguments

- data_file:

  Path to a .zip archive containing an IPUMS NHGIS or IPUMS IHGIS
  extract or a single .csv file from such an extract.

- file_select:

  If `data_file` is a .zip archive that contains multiple files, an
  expression identifying the file to load. Accepts a character vector
  specifying the file name, a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md),
  or an index position. This must uniquely identify a file.

- vars:

  Names of variables to include in the output. Accepts a vector of names
  or a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md).
  If `NULL`, includes all variables in the file.

- col_types:

  One of `NULL`, a
  [`cols()`](https://readr.tidyverse.org/reference/cols.html)
  specification or a string. If `NULL`, all column types will be
  inferred from the values in the first `guess_max` rows of each column.
  Alternatively, you can use a compact string representation to specify
  column types:

  - c = character

  - i = integer

  - n = number

  - d = double

  - l = logical

  - f = factor

  - D = date

  - T = date time

  - t = time

  - ? = guess

  - \_ or - = skip

  See
  [`read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
  for more details.

- n_max:

  Maximum number of lines to read.

- guess_max:

  For .csv files, maximum number of lines to use for guessing column
  types. Will never use more than the number of lines read.

- var_attrs:

  Variable attributes to add from the codebook (.txt) file included in
  the extract. Defaults to all available attributes.

  See
  [`set_ipums_var_attributes()`](https://tech.popdata.org/ipumsr/reference/set_ipums_var_attributes.md)
  for more details.

- remove_extra_header:

  If `TRUE`, remove the additional descriptive header row included in
  some NHGIS .csv files.

  This header row is not usually needed as it contains similar
  information to that included in the `"label"` attribute of each data
  column (if `var_attrs` includes `"var_label"`).

- file_encoding:

  Encoding for the file to be loaded. For NHGIS extracts, defaults to
  ISO-8859-1. For IHGIS extracts, defaults to UTF-8. If the default
  encoding produces unexpected characters, adjust the encoding here.

- verbose:

  Logical controlling whether to display output when loading data. If
  `TRUE`, displays IPUMS conditions, a progress bar, and column types.
  Otherwise, all are suppressed.

  Will be overridden by `readr.show_progress` and `readr.show_col_types`
  options, if they are set.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the data found in `data_file`

## See also

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/reference/read_nhgis_codebook.md)
or
[`read_ihgis_codebook()`](https://tech.popdata.org/ipumsr/reference/read_ihgis_codebook.md)
to read metadata about an IPUMS aggregate data extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
nhgis_file <- ipums_example("nhgis0972_csv.zip")
ihgis_file <- ipums_example("ihgis0014.zip")

# Provide the .zip archive directly to load the data inside:
read_ipums_agg(nhgis_file)
#> Use of data from IPUMS NHGIS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> Rows: 71 Columns: 25
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (9): GISJOIN, STUSAB, CMSA, PMSA, PMSAA, AREALAND, AREAWAT, ANPSADPI, F...
#> dbl (13): YEAR, MSA_CMSAA, INTPTLAT, INTPTLNG, PSADC, D6Z001, D6Z002, D6Z003...
#> lgl  (3): DIVISIONA, REGIONA, STATEA
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> # A tibble: 71 × 25
#>    GISJOIN  YEAR STUSAB CMSA  DIVISIONA MSA_CMSAA PMSA      PMSAA REGIONA STATEA
#>    <chr>   <dbl> <chr>  <chr> <lgl>         <dbl> <chr>     <chr> <lgl>   <lgl> 
#>  1 G0080    1990 OH     28    NA             1692 Akron, O… 0080  NA      NA    
#>  2 G0360    1990 CA     49    NA             4472 Anaheim-… 0360  NA      NA    
#>  3 G0440    1990 MI     35    NA             2162 Ann Arbo… 0440  NA      NA    
#>  4 G0620    1990 IL     14    NA             1602 Aurora--… 0620  NA      NA    
#>  5 G0845    1990 PA     78    NA             6282 Beaver C… 0845  NA      NA    
#>  6 G0875    1990 NJ     70    NA             5602 Bergen--… 0875  NA      NA    
#>  7 G1120    1990 MA     07    NA             1122 Boston, … 1120  NA      NA    
#>  8 G1125    1990 CO     34    NA             2082 Boulder-… 1125  NA      NA    
#>  9 G1145    1990 TX     42    NA             3362 Brazoria… 1145  NA      NA    
#> 10 G1160    1990 CT     70    NA             5602 Bridgepo… 1160  NA      NA    
#> # ℹ 61 more rows
#> # ℹ 15 more variables: AREALAND <chr>, AREAWAT <chr>, ANPSADPI <chr>,
#> #   FUNCSTAT <chr>, INTPTLAT <dbl>, INTPTLNG <dbl>, PSADC <dbl>, D6Z001 <dbl>,
#> #   D6Z002 <dbl>, D6Z003 <dbl>, D6Z004 <dbl>, D6Z005 <dbl>, D6Z006 <dbl>,
#> #   D6Z007 <dbl>, D6Z008 <dbl>

# For extracts that contain multiple files, use `file_select` to specify
# a single file to load. This accepts a tidyselect expression:
read_ipums_agg(ihgis_file, file_select = matches("AAA_g0"), verbose = FALSE)
#> # A tibble: 1 × 11
#>   GISJOIN g0      AAA001 AAA002 AAA003 AAA004 AAA005 AAA006 AAA007 AAA008 AAA009
#>   <chr>   <chr>    <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1 KZ      Kazakh… 1.50e7 1.60e7   107. 8.46e6 8.66e6   102. 6.53e6 7.35e6   113.

# Or an index position:
read_ipums_agg(ihgis_file, file_select = 2, verbose = FALSE)
#> # A tibble: 16 × 12
#>    GISJOIN g0      g1    AAA001 AAA002 AAA003 AAA004 AAA005 AAA006 AAA007 AAA008
#>    <chr>   <chr>   <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 KZ01    Kazakh… Akmo… 8.27e5 7.37e5   89.1 3.81e5 3.42e5   89.9 4.47e5 3.96e5
#>  2 KZ02    Kazakh… Akto… 6.83e5 7.58e5  111   3.78e5 4.61e5  122.  3.05e5 2.97e5
#>  3 KZ03    Kazakh… Alma… 1.13e6 1.37e6  121.  1.13e6 1.37e6  121.  0      0     
#>  4 KZ04    Kazakh… Alma… 1.56e6 1.81e6  116.  4.65e5 4.17e5   89.7 1.09e6 1.39e6
#>  5 KZ05    Kazakh… Asta… 3.28e5 6.13e5  187.  3.28e5 6.13e5  187.  0      0     
#>  6 KZ06    Kazakh… Atyr… 4.40e5 5.10e5  116.  2.43e5 2.39e5   98.5 1.98e5 2.71e5
#>  7 KZ07    Kazakh… East… 1.53e6 1.40e6   91.2 8.98e5 8.01e5   89.2 6.33e5 5.95e5
#>  8 KZ08    Kazakh… Kara… 1.41e6 1.34e6   95.1 1.16e6 1.04e6   89.8 2.52e5 3.01e5
#>  9 KZ09    Kazakh… Kost… 1.02e6 8.86e5   87.1 5.54e5 4.40e5   79.3 4.63e5 4.46e5
#> 10 KZ10    Kazakh… Kyzy… 6.25e5 6.79e5  109.  3.90e5 2.84e5   72.9 2.35e5 3.95e5
#> 11 KZ11    Kazakh… Mang… 3.15e5 4.85e5  154.  2.47e5 2.63e5  107.  6.78e4 2.22e5
#> 12 KZ12    Kazakh… Nort… 7.26e5 5.97e5   82.2 2.75e5 2.37e5   86.4 4.51e5 3.59e5
#> 13 KZ13    Kazakh… Pavl… 8.07e5 7.42e5   92   5.11e5 5.05e5   98.7 2.96e5 2.38e5
#> 14 KZ14    Kazakh… Sout… 1.98e6 2.47e6  125.  7.93e5 9.72e5  122.  1.19e6 1.50e6
#> 15 KZ15    Kazakh… West… 6.17e5 5.99e5   97.1 2.52e5 2.78e5  110.  3.65e5 3.21e5
#> 16 KZ16    Kazakh… Zham… 9.89e5 1.02e6  103.  4.52e5 4.05e5   89.5 5.37e5 6.18e5
#> # ℹ 1 more variable: AAA009 <dbl>

# Variable metadata is automatically attached to data, if available
ihgis_data <- read_ipums_agg(ihgis_file, file_select = 2, verbose = FALSE)
ipums_var_info(ihgis_data)
#> # A tibble: 12 × 4
#>    var_name var_label                             var_desc            val_labels
#>    <chr>    <chr>                                 <chr>               <list>    
#>  1 GISJOIN  GIS join match code                   NA                  <tibble>  
#>  2 g0       Nation                                NA                  <tibble>  
#>  3 g1       Regions/Oblasts                       NA                  <tibble>  
#>  4 AAA001   Total population : 1999               Table AAA: Urban a… <tibble>  
#>  5 AAA002   Total population : 2009               Table AAA: Urban a… <tibble>  
#>  6 AAA003   Total population in 2009 as % of 1999 Table AAA: Urban a… <tibble>  
#>  7 AAA004   Urban population : 1999               Table AAA: Urban a… <tibble>  
#>  8 AAA005   Urban population : 2009               Table AAA: Urban a… <tibble>  
#>  9 AAA006   Urban population in 2009 as % of 1999 Table AAA: Urban a… <tibble>  
#> 10 AAA007   Rural population : 1999               Table AAA: Urban a… <tibble>  
#> 11 AAA008   Rural population : 2009               Table AAA: Urban a… <tibble>  
#> 12 AAA009   Rural population in 2009 as % of 1999 Table AAA: Urban a… <tibble>  

# Column types are inferred from the data. You can
# manually specify column types with `col_types`. This may be useful for
# geographic codes, which should typically be interpreted as character values
read_ipums_agg(nhgis_file, col_types = list(MSA_CMSAA = "c"), verbose = FALSE)
#> # A tibble: 71 × 25
#>    GISJOIN  YEAR STUSAB CMSA  DIVISIONA MSA_CMSAA PMSA      PMSAA REGIONA STATEA
#>    <chr>   <dbl> <chr>  <chr> <lgl>     <chr>     <chr>     <chr> <lgl>   <lgl> 
#>  1 G0080    1990 OH     28    NA        1692      Akron, O… 0080  NA      NA    
#>  2 G0360    1990 CA     49    NA        4472      Anaheim-… 0360  NA      NA    
#>  3 G0440    1990 MI     35    NA        2162      Ann Arbo… 0440  NA      NA    
#>  4 G0620    1990 IL     14    NA        1602      Aurora--… 0620  NA      NA    
#>  5 G0845    1990 PA     78    NA        6282      Beaver C… 0845  NA      NA    
#>  6 G0875    1990 NJ     70    NA        5602      Bergen--… 0875  NA      NA    
#>  7 G1120    1990 MA     07    NA        1122      Boston, … 1120  NA      NA    
#>  8 G1125    1990 CO     34    NA        2082      Boulder-… 1125  NA      NA    
#>  9 G1145    1990 TX     42    NA        3362      Brazoria… 1145  NA      NA    
#> 10 G1160    1990 CT     70    NA        5602      Bridgepo… 1160  NA      NA    
#> # ℹ 61 more rows
#> # ℹ 15 more variables: AREALAND <chr>, AREAWAT <chr>, ANPSADPI <chr>,
#> #   FUNCSTAT <chr>, INTPTLAT <dbl>, INTPTLNG <dbl>, PSADC <dbl>, D6Z001 <dbl>,
#> #   D6Z002 <dbl>, D6Z003 <dbl>, D6Z004 <dbl>, D6Z005 <dbl>, D6Z006 <dbl>,
#> #   D6Z007 <dbl>, D6Z008 <dbl>

# You can also read in a subset of the data file:
read_ipums_agg(
  nhgis_file,
  n_max = 15,
  vars = c(GISJOIN, YEAR, D6Z002),
  verbose = FALSE
)
#> # A tibble: 15 × 3
#>    GISJOIN  YEAR D6Z002
#>    <chr>   <dbl>  <dbl>
#>  1 G0080    1990  11593
#>  2 G0360    1990  95737
#>  3 G0440    1990   8988
#>  4 G0620    1990   8982
#>  5 G0845    1990   1814
#>  6 G0875    1990  20476
#>  7 G1120    1990  58143
#>  8 G1125    1990   9467
#>  9 G1145    1990   6774
#> 10 G1160    1990   9710
#> 11 G1170    1990   3209
#> 12 G1200    1990   3551
#> 13 G1280    1990  12072
#> 14 G1600    1990 111582
#> 15 G1640    1990  37225
```

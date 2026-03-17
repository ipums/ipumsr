# Read tabular data from an NHGIS extract

**\[deprecated\]**

Read a .csv or fixed-width (.dat) file downloaded from the NHGIS extract
system.

This function has been deprecated in favor of
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md),
which can read .csv files from both IPUMS aggregate data collections
(IPUMS NHGIS and IPUMS IHGIS). Please use that function instead.

Note that fixed-width file reading is not supported in
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
and will likely be retired with `read_nhgis()`. We therefore encourage
you to create NHGIS extracts in .csv format going forward. For
previously-submitted fixed-width extracts, we suggest regenerating them
in .csv format and loading them with
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md).
Use the `data_format` argument of
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to create a .csv extract for submission via the IPUMS API.

To read spatial data from an NHGIS extract, use
[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md).

## Usage

``` r
read_nhgis(
  data_file,
  file_select = NULL,
  vars = NULL,
  col_types = NULL,
  n_max = Inf,
  guess_max = min(n_max, 1000),
  do_file = NULL,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  remove_extra_header = TRUE,
  verbose = TRUE
)
```

## Arguments

- data_file:

  Path to a .zip archive containing an NHGIS extract or a single file
  from an NHGIS extract.

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

- do_file:

  For fixed-width files, path to the .do file associated with the
  provided `data_file`. The .do file contains the parsing instructions
  for the data file.

  By default, looks in the same path as `data_file` for a .do file with
  the same name. See Details section below.

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

- verbose:

  Logical controlling whether to display output when loading data. If
  `TRUE`, displays IPUMS conditions, a progress bar, and column types.
  Otherwise, all are suppressed.

  Will be overridden by `readr.show_progress` and `readr.show_col_types`
  options, if they are set.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the data found in `data_file`

## Details

The .do file that is included when downloading an NHGIS fixed-width
extract contains the necessary metadata (e.g. column positions and
implicit decimals) to correctly parse the data file. `read_nhgis()` uses
this information to parse and recode the fixed-width data appropriately.

If you no longer have access to the .do file, consider resubmitting the
extract that produced the data. You can also change the desired data
format to produce a .csv file, which does not require additional
metadata files to be loaded.

For more about resubmitting an existing extract via the IPUMS API, see
[`vignette("ipums-api", package = "ipumsr")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## See also

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/reference/read_nhgis_codebook.md)
to read metadata about an IPUMS NHGIS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Example files
csv_file <- ipums_example("nhgis0972_csv.zip")
fw_file <- ipums_example("nhgis0730_fixed.zip")

# Previously:
read_nhgis(csv_file)
#> Warning: `read_nhgis()` was deprecated in ipumsr 0.9.0.
#> ℹ Please use `read_ipums_agg()` instead.
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

# For CSV files, please update to use the following:
read_ipums_agg(csv_file)
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

# Fixed-width files are parsed with the correct column positions
# and column types automatically:
read_nhgis(fw_file, file_select = contains("ts"), verbose = FALSE)
#> # A tibble: 84 × 28
#>    GISJOIN STATE         STATEFP STATENH A00AA1790 A00AA1800 A00AA1810 A00AA1820
#>    <chr>   <chr>         <chr>   <chr>       <dbl>     <dbl>     <dbl>     <dbl>
#>  1 G010    Alabama       01      010            NA        NA        NA    127901
#>  2 G020    Alaska        02      020            NA        NA        NA        NA
#>  3 G025    Alaska Terri… NA      025            NA        NA        NA        NA
#>  4 G040    Arizona       04      040            NA        NA        NA        NA
#>  5 G045    Arizona Terr… NA      045            NA        NA        NA        NA
#>  6 G050    Arkansas      05      050            NA        NA        NA        NA
#>  7 G055    Arkansas Ter… NA      055            NA        NA        NA     14273
#>  8 G060    California    06      060            NA        NA        NA        NA
#>  9 G080    Colorado      08      080            NA        NA        NA        NA
#> 10 G085    Colorado Ter… NA      085            NA        NA        NA        NA
#> # ℹ 74 more rows
#> # ℹ 20 more variables: A00AA1830 <dbl>, A00AA1840 <dbl>, A00AA1850 <dbl>,
#> #   A00AA1860 <dbl>, A00AA1870 <dbl>, A00AA1880 <dbl>, A00AA1890 <dbl>,
#> #   A00AA1900 <dbl>, A00AA1910 <dbl>, A00AA1920 <dbl>, A00AA1930 <dbl>,
#> #   A00AA1940 <dbl>, A00AA1950 <dbl>, A00AA1960 <dbl>, A00AA1970 <dbl>,
#> #   A00AA1980 <dbl>, A00AA1990 <dbl>, A00AA2000 <dbl>, A00AA2010 <dbl>,
#> #   A00AA2020 <dbl>
```

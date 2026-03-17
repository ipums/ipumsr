# Read metadata from an NHGIS codebook (.txt) file

**\[experimental\]**

Read the variable metadata contained in the .txt codebook file included
with NHGIS extracts into an
[`ipums_ddi`](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
object.

Because NHGIS variable metadata do not adhere to all the standards of
microdata DDI files, some of the `ipums_ddi` fields will not be
populated.

This function is marked as experimental while we determine whether there
may be a more robust way to standardize codebook reading across IPUMS
aggregate data collections.

## Usage

``` r
read_nhgis_codebook(cb_file, file_select = NULL, raw = FALSE)
```

## Arguments

- cb_file:

  Path to a .zip archive containing an NHGIS extract or to an NHGIS
  codebook (.txt) file.

- file_select:

  If `cb_file` is a .zip archive or directory that contains multiple
  codebook files, an expression identifying the file to read. Accepts a
  character string specifying the file name, a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md),
  or an index position of the file. Ignored if `cb_file` is the path to
  a single codebook file.

- raw:

  If `TRUE`, return a character vector containing the lines of `cb_file`
  rather than an `ipums_ddi` object. Defaults to `FALSE`.

## Value

If `raw = FALSE`, an `ipums_ddi` object with metadata about the
variables contained in the data for the extract associated with the
given `cb_file`.

If `raw = TRUE`, a character vector with one element for each line of
the given `cb_file`.

## See also

[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
to read tabular data from an IPUMS NHGIS extract.

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Example file
nhgis_file <- ipums_example("nhgis0972_csv.zip")

# Read codebook as an `ipums_ddi` object:
codebook <- read_nhgis_codebook(nhgis_file)

# Variable-level metadata about the contents of the data file:
ipums_var_info(codebook)
#> # A tibble: 25 × 10
#>    var_name  var_label      var_desc val_labels code_instr start end   imp_decim
#>    <chr>     <chr>          <chr>    <list>     <chr>      <lgl> <lgl>     <dbl>
#>  1 GISJOIN   GIS Join Matc… ""       <tibble>   ""         NA    NA            0
#>  2 YEAR      Data File Year ""       <tibble>   ""         NA    NA            0
#>  3 STUSAB    State/US Abbr… ""       <tibble>   ""         NA    NA            0
#>  4 CMSA      Consolidated … ""       <tibble>   ""         NA    NA            0
#>  5 DIVISIONA Division Code  ""       <tibble>   ""         NA    NA            0
#>  6 MSA_CMSAA Metropolitan … ""       <tibble>   ""         NA    NA            0
#>  7 PMSA      Primary Metro… ""       <tibble>   ""         NA    NA            0
#>  8 PMSAA     Primary Metro… ""       <tibble>   ""         NA    NA            0
#>  9 REGIONA   Region Code    ""       <tibble>   ""         NA    NA            0
#> 10 STATEA    State Code     ""       <tibble>   ""         NA    NA            0
#> # ℹ 15 more rows
#> # ℹ 2 more variables: var_type <chr>, rectypes <lgl>

ipums_var_label(codebook, "PMSA")
#> [1] "Primary Metropolitan Statistical Area Name"

# If variable metadata have been lost from a data source, reattach from
# the corresponding `ipums_ddi` object:
nhgis_data <- read_ipums_agg(nhgis_file, verbose = FALSE)

nhgis_data <- zap_ipums_attributes(nhgis_data)
ipums_var_label(nhgis_data$PMSA)
#> [1] NA

nhgis_data <- set_ipums_var_attributes(nhgis_data, codebook)
ipums_var_label(nhgis_data$PMSA)
#> [1] "Primary Metropolitan Statistical Area Name"

# You can also load the codebook in raw format to display in the console
codebook_raw <- read_nhgis_codebook(nhgis_file, raw = TRUE)

# Use `cat` for human-readable output
cat(codebook_raw[1:20], sep = "\n")
#> --------------------------------------------------------------------------------
#> Codebook for NHGIS data file 'nhgis0972_ds135_1990_pmsa'
#> --------------------------------------------------------------------------------
#>  
#> Contents
#>     - Data Summary
#>     - Data Dictionary
#>     - Citation and Use
#>  
#> Additional documentation on NHGIS data sources is available at: 
#>     https://www.nhgis.org/documentation/tabular-data 
#>  
#> --------------------------------------------------------------------------------
#> Data Summary
#> --------------------------------------------------------------------------------
#>  
#> Year:             1990
#> Geographic level: Consolidated Metropolitan Statistical Area--Primary Metropolitan Statistical Area
#> Dataset:          1990 Census: SSTF 9 - Housing Characteristics of New Units
#>    NHGIS code:    1990_SSTF09
```

# Read metadata from an IHGIS extract's codebook files

**\[experimental\]**

Read the variable metadata contained in an IHGIS extract into an
[`ipums_ddi`](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object.

Because IHGIS variable metadata do not adhere to all the standards of
microdata DDI files, some of the `ipums_ddi` fields will not be
populated.

This function is marked as experimental while we determine whether there
may be a more robust way to standardize codebook reading across IPUMS
aggregate data collections.

## Usage

``` r
read_ihgis_codebook(cb_file, tbls_file = NULL, raw = FALSE)
```

## Arguments

- cb_file:

  Path to a .zip archive containing an IHGIS extract, an IHGIS data
  dictionary (`_datadict.csv`) file, or an IHGIS codebook (.txt) file.

- tbls_file:

  If `cb_file` is the path to an IHGIS data dictionary .csv file, path
  to the `_tables.csv` metadata file from the same IHGIS extract. If
  these files are in the same directory, this file will be automatically
  loaded. If you have moved this file, provide the path to it here.

- raw:

  If `TRUE` return a character vector containing the lines of `cb_file`
  rather than an `ipums_ddi` object. Defaults to `FALSE`.

  If `TRUE`, `cb_file` must be a .zip archive or a .txt codebook file.

## Value

If `raw = FALSE`, an `ipums_ddi` object with metadata about the
variables contained in the data for the extract associated with the
given `cb_file`.

If `raw = TRUE`, a character vector with one element for each line of
the given `cb_file`.

## Details

IHGIS extracts store variable and geographic metadata in multiple files:

- `_datadict.csv` contains the data dictionary with metadata about the
  variables included across all files in the extract.

- `_tables.csv` contains metadata about all IHGIS tables included in the
  extract.

- `_geog.csv` contains metadata about the tabulation geographies
  included for any tables in the extract.

- `_codebook.txt` contains table and variable metadata in human readable
  form and contains citation information for IHGIS data.

By default, `read_ihgis_codebook()` uses information from all these
files and assumes they exist in the provided extract (.zip) file or
directory. If you have unzipped your IHGIS extract and moved the
`_tables.csv` file, you will need to provide the path to that file in
the `tbls_file` argument. Certain variable metadata can still be loaded
without the `_geog.csv` or `_codebook.txt` files. However, if
`raw = TRUE`, the `_codebook.txt` file must be present in the .zip
archive or provided to `cb_file`.

If you no longer have access to these files, consider resubmitting the
extract request that produced the data.

Note that IHGIS codebooks contain metadata for all the datasets
contained in a given extract. Individual data files from the extract may
not contain all of the variables shown in the output of
`read_ihgis_codebook()`.

## Examples

``` r
ihgis_file <- ipums_example("ihgis0014.zip")

ihgis_cb <- read_ihgis_codebook(ihgis_file)

# Variable labels and descriptions
ihgis_cb$var_info
#> # A tibble: 18 × 10
#>    var_name var_label       var_desc val_labels code_instr start end   imp_decim
#>    <chr>    <chr>           <chr>    <list>     <chr>      <lgl> <lgl>     <dbl>
#>  1 GISJOIN  GIS join match… NA       <tibble>   ""         NA    NA            0
#>  2 g0       Nation          NA       <NULL>     NA         NA    NA           NA
#>  3 g1       Regions/Oblasts NA       <NULL>     NA         NA    NA           NA
#>  4 AAA001   Total populati… Table A… <tibble>   ""         NA    NA            0
#>  5 AAA002   Total populati… Table A… <tibble>   ""         NA    NA            0
#>  6 AAA003   Total populati… Table A… <tibble>   ""         NA    NA            0
#>  7 AAA004   Urban populati… Table A… <tibble>   ""         NA    NA            0
#>  8 AAA005   Urban populati… Table A… <tibble>   ""         NA    NA            0
#>  9 AAA006   Urban populati… Table A… <tibble>   ""         NA    NA            0
#> 10 AAA007   Rural populati… Table A… <tibble>   ""         NA    NA            0
#> 11 AAA008   Rural populati… Table A… <tibble>   ""         NA    NA            0
#> 12 AAA009   Rural populati… Table A… <tibble>   ""         NA    NA            0
#> 13 AAB001   1999 : Below a… Table A… <tibble>   ""         NA    NA            0
#> 14 AAB002   2009 : Below a… Table A… <tibble>   ""         NA    NA            0
#> 15 AAB003   1999 : Below w… Table A… <tibble>   ""         NA    NA            0
#> 16 AAB004   2009 : Below w… Table A… <tibble>   ""         NA    NA            0
#> 17 AAB005   1999 : Above w… Table A… <tibble>   ""         NA    NA            0
#> 18 AAB006   2009 : Above w… Table A… <tibble>   ""         NA    NA            0
#> # ℹ 2 more variables: var_type <chr>, rectypes <lgl>

# Citation information
ihgis_cb$conditions
#> [1] "\nSteven Manson, Tracy A. Kugler, Kathryn Grace, Jonathan Schroeder, David Van Riper, Steven Ruggles. IPUMS International Historical Geographic Information System: Version 3 [dataset]. Minneapolis, MN: IPUMS. 2025. http://doi.org/10.18128/D120.V3\nWorks based on data tabulated from IPUMS International must also cite IPUMS International:\nMinnesota Population Center. Integrated Public Use Microdata Series, International: Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2018. https://doi.org/10.18128/D020.V7.1\n"

# If variable metadata have been lost from a data source, reattach from
# the corresponding `ipums_ddi` object:
ihgis_data <- read_ipums_agg(
  ihgis_file,
  file_select = matches("AAA_g0"),
  verbose = FALSE
)

ihgis_data <- zap_ipums_attributes(ihgis_data)
ipums_var_label(ihgis_data$AAA001)
#> [1] NA

ihgis_data <- set_ipums_var_attributes(ihgis_data, ihgis_cb)
ipums_var_label(ihgis_data$AAA001)
#> [1] "Total population : 1999"

# Load in raw format
ihgis_cb_raw <- read_ihgis_codebook(ihgis_file, raw = TRUE)

# Use `cat()` to display in the R console in human readable format
cat(ihgis_cb_raw[1:21], sep = "\n")
#> ---------------------------------------------------------------
#> Codebook for IHGIS extract 0014 
#> ---------------------------------------------------------------
#> 
#> Contents
#>     - Citation and Use
#>     - Datasets
#>     - Tables
#>     - Variables
#> 
#> Additional documentation on IHGIS data sources is available at:
#>     https://ihgis.ipums.org
#> 
#> ---------------------------------------------------------------
#> Citation and Use of IHGIS Data
#> ---------------------------------------------------------------
#> 
#> Steven Manson, Tracy A. Kugler, Kathryn Grace, Jonathan Schroeder, David Van Riper, Steven Ruggles. IPUMS International Historical Geographic Information System: Version 3 [dataset]. Minneapolis, MN: IPUMS. 2025. http://doi.org/10.18128/D120.V3
#> Works based on data tabulated from IPUMS International must also cite IPUMS International:
#> Minnesota Population Center. Integrated Public Use Microdata Series, International: Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2018. https://doi.org/10.18128/D020.V7.1
#> 
```

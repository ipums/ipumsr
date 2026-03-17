# List files contained within a zipped IPUMS extract

Identify the files that can be read from an IPUMS extract.

## Usage

``` r
ipums_list_files(file, file_select = NULL, types = NULL)
```

## Arguments

- file:

  Path to a .zip archive containing the IPUMS extract to be examined.

- file_select:

  If the path in `file` contains multiple files, a [tidyselect
  selection](https://tech.popdata.org/ipumsr/reference/selection_language.md)
  identifying the files to be included in the output. Only files that
  match the provided expression will be included.

  While less useful, this can also be provided as a string specifying an
  exact file name or an integer to match files by index position.

- types:

  One or more of `"data"`, `"shape"`, or `"codebook"` indicating the
  type of files to include in the output. `"data"` refers to tabular
  data sources, `"shape"` refers to spatial data sources, and
  `"codebook"` refers to metadata text files that accompany data files.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the types and names of the available files.

## See also

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
or
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
to read tabular data from an IPUMS extract.

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

## Examples

``` r
nhgis_file <- ipums_example("nhgis0712_csv.zip")

# 2 available data files in this extract (with codebooks)
ipums_list_files(nhgis_file)
#> # A tibble: 4 × 2
#>   type     file                                                
#>   <chr>    <chr>                                               
#> 1 data     nhgis0712_csv/nhgis0712_ds135_1990_pmsa.csv         
#> 2 data     nhgis0712_csv/nhgis0712_ds136_1990_pmsa.csv         
#> 3 codebook nhgis0712_csv/nhgis0712_ds135_1990_pmsa_codebook.txt
#> 4 codebook nhgis0712_csv/nhgis0712_ds136_1990_pmsa_codebook.txt

# Look for files that match a particular pattern:
ipums_list_files(nhgis_file, file_select = matches("ds136"))
#> # A tibble: 2 × 2
#>   type     file                                                
#>   <chr>    <chr>                                               
#> 1 data     nhgis0712_csv/nhgis0712_ds136_1990_pmsa.csv         
#> 2 codebook nhgis0712_csv/nhgis0712_ds136_1990_pmsa_codebook.txt
```

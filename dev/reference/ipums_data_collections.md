# List IPUMS data collections

List IPUMS data collections with their corresponding codes used by the
IPUMS API. Note that some data collections do not yet have API support.

Currently, ipumsr supports extract definitions for the following
collections:

- IPUMS USA (`"usa"`)

- IPUMS CPS (`"cps"`)

- IPUMS International (`"ipumsi"`)

- IPUMS Time Use (`"atus"`, `"ahtus"`, `"mtus"`)

- IPUMS Health Surveys (`"nhis"`, `"meps"`)

- IPUMS NHGIS (`"nhgis"`)

- IPUMS IHGIS (`"ihgis"`)

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

## Usage

``` r
ipums_data_collections()
```

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with four columns containing the full collection name, the type of data
the collection provides, the collection code used by the IPUMS API, and
the status of API support for the collection.

## Examples

``` r
ipums_data_collections()
#> # A tibble: 14 × 4
#>    collection_name     collection_type code_for_api api_support
#>    <chr>               <chr>           <chr>        <lgl>      
#>  1 IPUMS USA           microdata       usa          TRUE       
#>  2 IPUMS CPS           microdata       cps          TRUE       
#>  3 IPUMS International microdata       ipumsi       TRUE       
#>  4 IPUMS NHGIS         aggregate data  nhgis        TRUE       
#>  5 IPUMS IHGIS         aggregate data  ihgis        TRUE       
#>  6 IPUMS ATUS          microdata       atus         TRUE       
#>  7 IPUMS AHTUS         microdata       ahtus        TRUE       
#>  8 IPUMS MTUS          microdata       mtus         TRUE       
#>  9 IPUMS DHS           microdata       dhs          TRUE       
#> 10 IPUMS PMA           microdata       pma          FALSE      
#> 11 IPUMS MICS          microdata       mics         FALSE      
#> 12 IPUMS NHIS          microdata       nhis         TRUE       
#> 13 IPUMS MEPS          microdata       meps         TRUE       
#> 14 IPUMS Higher Ed     microdata       highered     FALSE      
```

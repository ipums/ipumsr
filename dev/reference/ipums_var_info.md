# Get contextual information about variables in an IPUMS data source

Summarize the variable metadata for the variables found in an
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object or data frame. Provides descriptions of variable content
(`var_label` and `var_desc`) as well as labels of particular values for
each variable (`val_labels`).

`ipums_var_info()` produces a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
summary of multiple variables at once.

`ipums_var_label()`, `ipums_var_desc()`, and `ipums_val_labels()`
provide specific metadata for a single variable.

## Usage

``` r
ipums_var_info(object, vars = NULL)

ipums_var_label(object, var = NULL)

ipums_var_desc(object, var = NULL)

ipums_val_labels(object, var = NULL)
```

## Arguments

- object:

  An
  [ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
  object, a data frame containing variable metadata (as produced by most
  ipumsr data-reading functions), or a
  [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  vector from a single column in such a data frame.

- vars, var:

  A [tidyselect
  selection](https://tech.popdata.org/ipumsr/dev/reference/selection_language.md)
  identifying the variable(s) to include in the output. Only
  `ipums_var_info()` allows for the selection of multiple variables.

## Value

For `ipums_var_info()`, a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing variable information.

Otherwise, a length-1 character vector with the requested variable
information.

## Details

For `ipums_var_info()`, if the provided `object` is a
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
vector (i.e. a single column from a data frame), the summary output will
include the variable label, variable description, and value labels, if
applicable.

If it is a data frame, the same information will be provided for all
variables present in the data or to those indicated in `vars`.

If it is an
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object, the summary will also include information used when reading the
data from disk, including start/end positions for columns in the
fixed-width file, implied decimals, and variable types.

Providing an `ipums_ddi` object is the most robust way to access
variable metadata, as many data processing operations will remove these
attributes from data frame-like objects.

## See also

[`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md)
or
[`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/dev/reference/read_nhgis_codebook.md)
to read IPUMS metadata files.

## Examples

``` r
ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))

# Info for all variables in a data source
ipums_var_info(ddi)
#> # A tibble: 8 × 10
#>   var_name var_label        var_desc val_labels code_instr start   end imp_decim
#>   <chr>    <chr>            <chr>    <list>     <chr>      <dbl> <dbl>     <dbl>
#> 1 YEAR     Survey year      "YEAR r… <tibble>   "YEAR is …     1     4         0
#> 2 SERIAL   Household seria… "SERIAL… <tibble>   "SERIAL i…     5     9         0
#> 3 MONTH    Month            "MONTH … <tibble>    NA           10    11         0
#> 4 ASECWTH  Annual Social a… "ASECWT… <tibble>   "ASECWTH …    12    22         4
#> 5 STATEFIP State (FIPS cod… "STATEF… <tibble>    NA           23    24         0
#> 6 PERNUM   Person number i… "PERNUM… <tibble>   "PERNUM i…    25    26         0
#> 7 ASECWT   Annual Social a… "ASECWT… <tibble>   "ASECWT i…    27    37         4
#> 8 INCTOT   Total personal … "INCTOT… <tibble>   "99999999…    38    46         0
#> # ℹ 2 more variables: var_type <chr>, rectypes <lgl>

# Metadata for individual variables
ipums_var_desc(ddi, MONTH)
#> [1] "MONTH indicates the calendar month of the CPS interview."

ipums_var_label(ddi, MONTH)
#> [1] "Month"

ipums_val_labels(ddi, MONTH)
#> # A tibble: 12 × 2
#>      val lbl      
#>    <dbl> <chr>    
#>  1     1 January  
#>  2     2 February 
#>  3     3 March    
#>  4     4 April    
#>  5     5 May      
#>  6     6 June     
#>  7     7 July     
#>  8     8 August   
#>  9     9 September
#> 10    10 October  
#> 11    11 November 
#> 12    12 December 

# NHGIS also supports variable-level metadata, though many fields
# are not relevant and remain blank:
cb <- read_nhgis_codebook(ipums_example("nhgis0972_csv.zip"))

ipums_var_info(cb)
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
```

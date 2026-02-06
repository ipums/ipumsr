# Read metadata about an IPUMS microdata extract from a DDI codebook (.xml) file

Reads the metadata about an IPUMS extract from a [DDI
codebook](https://ddialliance.org/introduction-to-ddi) into an
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object.

These metadata contains parsing instructions for the associated
fixed-width data file, contextual labels for variables and values in the
data, and general extract information.

See *Downloading IPUMS files* below for information about downloading
IPUMS DDI codebook files.

## Usage

``` r
read_ipums_ddi(ddi_file, lower_vars = FALSE)
```

## Arguments

- ddi_file:

  Path to a DDI .xml file downloaded from
  [IPUMS](https://www.ipums.org/). See *Downloading IPUMS files* below.

- lower_vars:

  Logical indicating whether to convert variable names to lowercase.
  Defaults to `FALSE` for consistency with IPUMS conventions.

## Value

An
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object with metadata information.

## Downloading IPUMS files

The DDI codebook (.xml) file provided with IPUMS microdata extracts can
be downloaded through the IPUMS extract interface or (for some
collections) within R using the IPUMS API.

If using the IPUMS extract interface:

- Download the DDI codebook by right clicking on the **DDI** link in the
  **Codebook** column of the extract interface and selecting **Save
  as...** (on Safari, you may have to select **Download Linked File
  As...**). Be sure that the codebook is downloaded in .xml format.

If using the IPUMS API:

- For supported collections, use
  [`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
  to download a completed extract via the IPUMS API. This automatically
  downloads both the DDI codebook and the data file from the extract and
  returns the path to the codebook file.

## See also

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md),
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_chunked.md)
and
[`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_yield.md)
to read data from IPUMS microdata extracts.

[`ipums_var_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_var_info.md)
and
[`ipums_file_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_file_info.md)
to view metadata about an
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Example codebook file
ddi_file <- ipums_example("cps_00157.xml")

# Load data into an `ipums_ddi` obj
ddi <- read_ipums_ddi(ddi_file)

# Use the object to load its associated data
cps <- read_ipums_micro(ddi)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

head(cps)
#> # A tibble: 6 × 8
#>    YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT            
#>   <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>         
#> 1  1962     80 3 [March]   1476. 55 [Wisconsin]      1  1476.      4883         
#> 2  1962     80 3 [March]   1476. 55 [Wisconsin]      2  1471.      5800         
#> 3  1962     80 3 [March]   1476. 55 [Wisconsin]      3  1579. 999999998 [Missin…
#> 4  1962     82 3 [March]   1598. 27 [Minnesota]      1  1598.     14015         
#> 5  1962     83 3 [March]   1707. 27 [Minnesota]      1  1707.     16552         
#> 6  1962     84 3 [March]   1790. 27 [Minnesota]      1  1790.      6375         

# Or get metadata information directly
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

ipums_file_info(ddi)[1:2]
#> $ipums_project
#> [1] "IPUMS CPS"
#> 
#> $extract_date
#> [1] "2023-07-10"
#> 

# If variable metadata have been lost from a data source, reattach from
# its corresponding `ipums_ddi` object:
cps <- zap_ipums_attributes(cps)

ipums_var_label(cps$STATEFIP)
#> [1] NA

cps <- set_ipums_var_attributes(cps, ddi$var_info)

ipums_var_label(cps$STATEFIP)
#> [1] "State (FIPS code)"
```

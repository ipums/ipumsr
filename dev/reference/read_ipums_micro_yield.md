# Read data from an IPUMS microdata extract in yields

Read a microdata dataset downloaded from the IPUMS extract system into
an object that can read and operate on a group ("yield") of lines at a
time. Use these functions to read a file that is too large to store in
memory at a single time. They represent a more flexible implementation
of
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_chunked.md)
using R6.

Two files are required to load IPUMS microdata extracts:

- A [DDI codebook](https://ddialliance.org/introduction-to-ddi) file
  (.xml) used to parse the extract's data file

- A data file (either .dat.gz or .csv.gz)

See *Downloading IPUMS files* below for more information about
downloading these files.

`read_ipums_micro_yield()` and `read_ipums_micro_list_yield()` differ in
their handling of extracts that contain multiple record types. See *Data
structures* below.

Note that these functions only support fixed-width (.dat) data files.

## Usage

``` r
read_ipums_micro_yield(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)

read_ipums_micro_list_yield(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)
```

## Arguments

- ddi:

  Either a path to a DDI .xml file downloaded from
  [IPUMS](https://www.ipums.org/), or an
  [ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
  object parsed by
  [`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md).
  See *Downloading IPUMS files* below.

- vars:

  Names of variables to include in the output. Accepts a vector of names
  or a [tidyselect
  selection](https://tech.popdata.org/ipumsr/dev/reference/selection_language.md).
  If `NULL`, includes all variables in the file.

  For hierarchical data, the `RECTYPE` variable is always included even
  if unspecified.

- data_file:

  Path to the data (.gz) file associated with the provided `ddi` file.
  By default, looks for the data file in the same directory as the DDI
  file. If the data file has been moved, specify its location here.

- verbose:

  Logical indicating whether to display IPUMS conditions and progress
  information.

- var_attrs:

  Variable attributes from the DDI to add to the columns of the output
  data. Defaults to all available attributes. See
  [`set_ipums_var_attributes()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_var_attributes.md)
  for more details.

- lower_vars:

  If reading a DDI from a file, a logical indicating whether to convert
  variable names to lowercase. Defaults to `FALSE` for consistency with
  IPUMS conventions.

  This argument will be ignored if argument `ddi` is an
  [ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
  object. Use
  [`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md)
  to convert variable names to lowercase when reading a DDI file.

  If `lower_vars = TRUE` and `vars` is specified, `vars` should
  reference the lowercase column names.

## Value

A HipYield R6 object (see Details section)

## Methods summary:

These functions return a HipYield R6 object with the following methods:

- `yield(n = 10000)` reads the next "yield" from the data.

  For `read_ipums_micro_yield()`, returns a
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  with up to `n` rows.

  For `read_ipums_micro_list_yield()`, returns a list of tibbles with a
  total of up to `n` rows across list elements.

  If fewer than `n` rows are left in the data, returns all remaining
  rows. If no rows are left in the data, returns `NULL`.

- `reset()` resets the data so that the next yield will read data from
  the start.

- `is_done()` returns a logical indicating whether all rows in the file
  have been read.

- `cur_pos` contains the next row number that will be read (1-indexed).

## Data structures

Files from IPUMS projects that contain data for multiple types of
records (e.g. household records and person records) may be either
rectangular or hierarchical.

Rectangular data are transformed such that each row of data represents
only one type of record. For instance, each row will represent a person
record, and all household-level information for that person will be
included in the same row.

Hierarchical data have records of different types interspersed in a
single file. For instance, a household record will be included in its
own row followed by the person records associated with that household.

Hierarchical data can be read in two different formats:

- `read_ipums_micro_yield()` produces an object that yields data as a
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  whose rows represent single records, regardless of record type.
  Variables that do not apply to a particular record type will be filled
  with `NA` in rows of that record type. For instance, a person-specific
  variable will be missing in all rows associated with household
  records.

- `read_ipums_micro_list_yield()` produces an object that yields data as
  a list of `tibble` objects, where each list element contains only one
  record type. Each list element is named with its corresponding record
  type. In this case, when using `yield()`, `n` refers to the total
  number of rows *across* record types, rather than in each record type.

## Downloading IPUMS files

You must download both the DDI codebook and the data file from the IPUMS
extract system to load the data into R. `read_ipums_micro_*()` functions
assume that the data file and codebook share a common base file name and
are present in the same directory. If this is not the case, provide a
separate path to the data file with the `data_file` argument.

If using the IPUMS extract interface:

- Download the data file by clicking **Download .dat** under **Download
  Data**.

- Download the DDI codebook by right clicking on the **DDI** link in the
  **Codebook** column of the extract interface and selecting **Save
  as...** (on Safari, you may have to select **Download Linked File
  as...**). Be sure that the codebook is downloaded in .xml format.

If using the IPUMS API:

- For supported collections, use
  [`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
  to download a completed extract via the IPUMS API. This automatically
  downloads both the DDI codebook and the data file from the extract and
  returns the path to the codebook file.

## See also

[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_chunked.md)
to read data from large IPUMS microdata extracts in chunks.

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md)
to read data from an IPUMS microdata extract.

[`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md)
to read metadata associated with an IPUMS microdata extract.

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Create an IpumsLongYield object
long_yield <- read_ipums_micro_yield(ipums_example("cps_00157.xml"))
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

# Yield the first 10 rows of the data
long_yield$yield(10)
#> # A tibble: 10 × 8
#>     YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT           
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>        
#>  1  1962     80 3 [March]   1476. 55 [Wisconsin]      1  1476.      4883        
#>  2  1962     80 3 [March]   1476. 55 [Wisconsin]      2  1471.      5800        
#>  3  1962     80 3 [March]   1476. 55 [Wisconsin]      3  1579. 999999998 [Missi…
#>  4  1962     82 3 [March]   1598. 27 [Minnesota]      1  1598.     14015        
#>  5  1962     83 3 [March]   1707. 27 [Minnesota]      1  1707.     16552        
#>  6  1962     84 3 [March]   1790. 27 [Minnesota]      1  1790.      6375        
#>  7  1962    107 3 [March]   4355. 19 [Iowa]           1  4355. 999999999 [N.I.U…
#>  8  1962    107 3 [March]   4355. 19 [Iowa]           2  1386.         0        
#>  9  1962    107 3 [March]   4355. 19 [Iowa]           3  1629.       600        
#> 10  1962    107 3 [March]   4355. 19 [Iowa]           4  1432. 999999999 [N.I.U…

# Yield the next 20 rows of the data
long_yield$yield(20)
#> # A tibble: 20 × 8
#>     YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT           
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>        
#>  1  1962    108 3 [March]   1479. 19 [Iowa]           1  1479.     12300        
#>  2  1962    108 3 [March]   1479. 19 [Iowa]           2  1482.         0        
#>  3  1962    122 3 [March]   3603. 27 [Minnesota]      1  3603.     15550        
#>  4  1962    122 3 [March]   3603. 27 [Minnesota]      2  3603.         0        
#>  5  1962    122 3 [March]   3603. 27 [Minnesota]      3  4243.      3443        
#>  6  1962    122 3 [March]   3603. 27 [Minnesota]      4  3920.       255        
#>  7  1962    122 3 [March]   3603. 27 [Minnesota]      5  3689.       135        
#>  8  1962    124 3 [March]   4104. 55 [Wisconsin]      1  4104.     15000        
#>  9  1962    124 3 [March]   4104. 55 [Wisconsin]      2  1487.      3550        
#> 10  1962    124 3 [March]   4104. 55 [Wisconsin]      3  1450.       692        
#> 11  1962    124 3 [March]   4104. 55 [Wisconsin]      4  1441.         0        
#> 12  1962    125 3 [March]   2182. 55 [Wisconsin]      1  2182.      4470        
#> 13  1962    126 3 [March]   1826. 55 [Wisconsin]      1  1826. 999999999 [N.I.U…
#> 14  1962    126 3 [March]   1826. 55 [Wisconsin]      2  1629.         0        
#> 15  1962    761 3 [March]   1751. 19 [Iowa]           1  1751.      7300        
#> 16  1962    761 3 [March]   1751. 19 [Iowa]           2  1751.      3700        
#> 17  1962    762 3 [March]   1874. 19 [Iowa]           1  1874.      2534        
#> 18  1962    762 3 [March]   1874. 19 [Iowa]           2  1874.         0        
#> 19  1962    763 3 [March]   1874. 19 [Iowa]           1  1874.      1591        
#> 20  1962    764 3 [March]   1724. 19 [Iowa]           1  1724.      8002        

# Check the current position after yielding 30 rows
long_yield$cur_pos
#> [1] 31

# Reset to the beginning of the file
long_yield$reset()

# Use a loop to flexibly process the data in pieces. Count all Minnesotans:
total_mn <- 0

while (!long_yield$is_done()) {
  cur_data <- long_yield$yield(1000)
  total_mn <- total_mn + sum(as_factor(cur_data$STATEFIP) == "Minnesota")
}

total_mn
#> [1] 2362

# Can also read hierarchical data as list:
list_yield <- read_ipums_micro_list_yield(ipums_example("cps_00159.xml"))
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

# Yield size is based on total rows for all list elements
list_yield$yield(10)
#> $HOUSEHOLD
#> # A tibble: 4 × 6
#>   RECTYPE               YEAR SERIAL MONTH     ASECWTH STATEFIP      
#>   <chr+lbl>            <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>     
#> 1 H [Household Record]  1962     80 3 [March]   1476. 55 [Wisconsin]
#> 2 H [Household Record]  1962     82 3 [March]   1598. 27 [Minnesota]
#> 3 H [Household Record]  1962     83 3 [March]   1707. 27 [Minnesota]
#> 4 H [Household Record]  1962     84 3 [March]   1790. 27 [Minnesota]
#> 
#> $PERSON
#> # A tibble: 6 × 6
#>   RECTYPE            YEAR SERIAL PERNUM ASECWT INCTOT                           
#>   <chr+lbl>         <dbl>  <dbl>  <dbl>  <dbl> <dbl+lbl>                        
#> 1 P [Person Record]  1962     80      1  1476.      4883                        
#> 2 P [Person Record]  1962     80      2  1471.      5800                        
#> 3 P [Person Record]  1962     80      3  1579. 999999998 [Missing. (1962-1964 o…
#> 4 P [Person Record]  1962     82      1  1598.     14015                        
#> 5 P [Person Record]  1962     83      1  1707.     16552                        
#> 6 P [Person Record]  1962     84      1  1790.      6375                        
#> 
```

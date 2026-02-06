# Read data from an IPUMS microdata extract

Read a microdata dataset downloaded from the IPUMS extract system.

Two files are required to load IPUMS microdata extracts:

- A [DDI codebook](https://ddialliance.org/introduction-to-ddi) file
  (.xml) used to parse the extract's data file

- A data file (either .dat.gz or .csv.gz)

See *Downloading IPUMS files* below for more information about
downloading these files.

`read_ipums_micro()` and `read_ipums_micro_list()` differ in their
handling of extracts that contain multiple record types. See *Data
structures* below.

Note that Stata, SAS, and SPSS file formats are not supported by ipumsr
readers. Convert your extract to fixed-width or CSV format, or see
[haven](https://haven.tidyverse.org/index.html) for help loading these
files.

## Usage

``` r
read_ipums_micro(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)

read_ipums_micro_list(
  ddi,
  vars = NULL,
  n_max = Inf,
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

- n_max:

  The maximum number of lines to read. For `read_ipums_micro_list()`,
  this applies before splitting records into list components.

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

`read_ipums_micro()` returns a single
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
object.

`read_ipums_micro_list()` returns a list of `tibble` objects with one
entry for each record type.

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

- `read_ipums_micro()` reads data into a
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  where each row represents a single record, regardless of record type.
  Variables that do not apply to a particular record type will be filled
  with `NA` in rows of that record type. For instance, a person-specific
  variable will be missing in all rows associated with household
  records.

- `read_ipums_micro_list()` reads data into a list of `tibble` objects,
  where each list element contains only one record type. Each list
  element is named with its corresponding record type.

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
and
[`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_yield.md)
to read data from large IPUMS microdata extracts in chunks.

[`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_ddi.md)
to read metadata associated with an IPUMS microdata extract.

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
# Codebook for rectangular example file
cps_rect_ddi_file <- ipums_example("cps_00157.xml")

# Load data based on codebook file info
cps <- read_ipums_micro(cps_rect_ddi_file)
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

# Can also load data from a pre-existing `ipums_ddi` object
# (This may be useful to retain codebook metadata even if lost from data
# during processing)
ddi <- read_ipums_ddi(cps_rect_ddi_file)
cps <- read_ipums_micro(ddi, verbose = FALSE)

# Codebook for hierarchical example file
cps_hier_ddi_file <- ipums_example("cps_00159.xml")

# Read in "long" format to get a single data frame
read_ipums_micro(cps_hier_ddi_file, verbose = FALSE)
#> # A tibble: 11,053 × 9
#>    RECTYPE    YEAR SERIAL MONTH    ASECWTH STATEFIP PERNUM ASECWT INCTOT        
#>    <chr+lbl> <dbl>  <dbl> <int+lb>   <dbl> <int+lb>  <dbl>  <dbl> <dbl+lbl>     
#>  1 H [House…  1962     80  3 [Mar…   1476. 55 [Wis…     NA    NA  NA            
#>  2 P [Perso…  1962     80 NA           NA  NA            1  1476.  4.88 e3      
#>  3 P [Perso…  1962     80 NA           NA  NA            2  1471.  5.8  e3      
#>  4 P [Perso…  1962     80 NA           NA  NA            3  1579.  1.000e9 [Mis…
#>  5 H [House…  1962     82  3 [Mar…   1598. 27 [Min…     NA    NA  NA            
#>  6 P [Perso…  1962     82 NA           NA  NA            1  1598.  1.40 e4      
#>  7 H [House…  1962     83  3 [Mar…   1707. 27 [Min…     NA    NA  NA            
#>  8 P [Perso…  1962     83 NA           NA  NA            1  1707.  1.66 e4      
#>  9 H [House…  1962     84  3 [Mar…   1790. 27 [Min…     NA    NA  NA            
#> 10 P [Perso…  1962     84 NA           NA  NA            1  1790.  6.38 e3      
#> # ℹ 11,043 more rows

# Read in "list" format and you get a list of multiple data frames
cps_list <- read_ipums_micro_list(cps_hier_ddi_file)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

head(cps_list$PERSON)
#> # A tibble: 6 × 6
#>   RECTYPE            YEAR SERIAL PERNUM ASECWT INCTOT                           
#>   <chr+lbl>         <dbl>  <dbl>  <dbl>  <dbl> <dbl+lbl>                        
#> 1 P [Person Record]  1962     80      1  1476.      4883                        
#> 2 P [Person Record]  1962     80      2  1471.      5800                        
#> 3 P [Person Record]  1962     80      3  1579. 999999998 [Missing. (1962-1964 o…
#> 4 P [Person Record]  1962     82      1  1598.     14015                        
#> 5 P [Person Record]  1962     83      1  1707.     16552                        
#> 6 P [Person Record]  1962     84      1  1790.      6375                        

head(cps_list$HOUSEHOLD)
#> # A tibble: 6 × 6
#>   RECTYPE               YEAR SERIAL MONTH     ASECWTH STATEFIP      
#>   <chr+lbl>            <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>     
#> 1 H [Household Record]  1962     80 3 [March]   1476. 55 [Wisconsin]
#> 2 H [Household Record]  1962     82 3 [March]   1598. 27 [Minnesota]
#> 3 H [Household Record]  1962     83 3 [March]   1707. 27 [Minnesota]
#> 4 H [Household Record]  1962     84 3 [March]   1790. 27 [Minnesota]
#> 5 H [Household Record]  1962    107 3 [March]   4355. 19 [Iowa]     
#> 6 H [Household Record]  1962    108 3 [March]   1479. 19 [Iowa]     

# Use the `%<-%` operator from zeallot to unpack into separate objects
c(household, person) %<-% read_ipums_micro_list(cps_hier_ddi_file)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

head(person)
#> # A tibble: 6 × 6
#>   RECTYPE            YEAR SERIAL PERNUM ASECWT INCTOT                           
#>   <chr+lbl>         <dbl>  <dbl>  <dbl>  <dbl> <dbl+lbl>                        
#> 1 P [Person Record]  1962     80      1  1476.      4883                        
#> 2 P [Person Record]  1962     80      2  1471.      5800                        
#> 3 P [Person Record]  1962     80      3  1579. 999999998 [Missing. (1962-1964 o…
#> 4 P [Person Record]  1962     82      1  1598.     14015                        
#> 5 P [Person Record]  1962     83      1  1707.     16552                        
#> 6 P [Person Record]  1962     84      1  1790.      6375                        

head(household)
#> # A tibble: 6 × 6
#>   RECTYPE               YEAR SERIAL MONTH     ASECWTH STATEFIP      
#>   <chr+lbl>            <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>     
#> 1 H [Household Record]  1962     80 3 [March]   1476. 55 [Wisconsin]
#> 2 H [Household Record]  1962     82 3 [March]   1598. 27 [Minnesota]
#> 3 H [Household Record]  1962     83 3 [March]   1707. 27 [Minnesota]
#> 4 H [Household Record]  1962     84 3 [March]   1790. 27 [Minnesota]
#> 5 H [Household Record]  1962    107 3 [March]   4355. 19 [Iowa]     
#> 6 H [Household Record]  1962    108 3 [March]   1479. 19 [Iowa]     
```

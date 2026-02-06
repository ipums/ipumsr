# Reading IPUMS Data

Once you have downloaded an IPUMS extract, the next step is to load its
data into R for analysis.

For more information about IPUMS data and how to generate and download a
data extract, see the
[introduction](https://tech.popdata.org/ipumsr/dev/articles/ipums.md) to
IPUMS data.

## IPUMS extract structure

IPUMS extracts will be organized slightly differently for different
[IPUMS projects](https://www.ipums.org/overview). In general, all
projects will provide multiple files in a data extract. The files most
relevant to ipumsr are:

- The **metadata** file containing information about the variables
  included in the extract data
- One or more **data** files, depending on the project and
  specifications in the extract

Both of these files are necessary to properly load data into R.
Obviously, the data files contain the actual data values to be loaded.
But because these are often in fixed-width format, the metadata files
are required to correctly parse the data on load.

Even for .csv files, the metadata file allows for the addition of
contextual variable information to the loaded data. This makes it much
easier to interpret the values in the data variables and effectively use
them in your data processing pipeline. See the [value
labels](https://tech.popdata.org/ipumsr/dev/articles/value-labels.md)
vignette for more information on working with these labels.

## Reading IPUMS microdata extracts

Microdata extracts typically provide their metadata in a DDI (.xml) file
separate from the compressed data (.dat.gz) files.

Provide the path to the DDI file to
[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md)
to directly load its associated data file into R.

``` r
library(ipumsr)
library(dplyr)

# Example data
cps_ddi_file <- ipums_example("cps_00157.xml")

cps_data <- read_ipums_micro(cps_ddi_file)

head(cps_data)
#> # A tibble: 6 × 8
#>    YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT            
#>   <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>         
#> 1  1962     80 3 [March]   1476. 55 [Wisconsin]      1  1476.      4883         
#> 2  1962     80 3 [March]   1476. 55 [Wisconsin]      2  1471.      5800         
#> 3  1962     80 3 [March]   1476. 55 [Wisconsin]      3  1579. 999999998 [Missin…
#> 4  1962     82 3 [March]   1598. 27 [Minnesota]      1  1598.     14015         
#> 5  1962     83 3 [March]   1707. 27 [Minnesota]      1  1707.     16552         
#> 6  1962     84 3 [March]   1790. 27 [Minnesota]      1  1790.      6375
```

Note that you provide the path to the DDI file, *not* the data file.
This is because ipumsr needs to find both the DDI and data files to read
in your data, and the DDI file includes the name of the data file,
whereas the data file contains only the raw data.

The loaded data have been parsed correctly and include variable metadata
in each column. For a summary of the column contents, use
[`ipums_var_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_var_info.md):

``` r
ipums_var_info(cps_data)
#> # A tibble: 8 × 4
#>   var_name var_label                                         var_desc val_labels
#>   <chr>    <chr>                                             <chr>    <list>    
#> 1 YEAR     Survey year                                       "YEAR r… <tibble>  
#> 2 SERIAL   Household serial number                           "SERIAL… <tibble>  
#> 3 MONTH    Month                                             "MONTH … <tibble>  
#> 4 ASECWTH  Annual Social and Economic Supplement Household … "ASECWT… <tibble>  
#> 5 STATEFIP State (FIPS code)                                 "STATEF… <tibble>  
#> 6 PERNUM   Person number in sample unit                      "PERNUM… <tibble>  
#> 7 ASECWT   Annual Social and Economic Supplement Weight      "ASECWT… <tibble>  
#> 8 INCTOT   Total personal income                             "INCTOT… <tibble>
```

This information is also attached to specific columns. You can obtain it
with [`attributes()`](https://rdrr.io/r/base/attributes.html) or by
using ipumsr helpers:

``` r
attributes(cps_data$MONTH)
#> $labels
#>   January  February     March     April       May      June      July    August 
#>         1         2         3         4         5         6         7         8 
#> September   October  November  December 
#>         9        10        11        12 
#> 
#> $class
#> [1] "haven_labelled" "vctrs_vctr"     "integer"       
#> 
#> $label
#> [1] "Month"
#> 
#> $var_desc
#> [1] "MONTH indicates the calendar month of the CPS interview."
ipums_val_labels(cps_data$MONTH)
#> # A tibble: 12 × 2
#>      val lbl      
#>    <int> <chr>    
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
```

While this is the most straightforward way to load microdata, it’s often
advantageous to independently load the DDI file into an `ipums_ddi`
object containing the metadata:

``` r
cps_ddi <- read_ipums_ddi(cps_ddi_file)

cps_ddi
#> An IPUMS DDI for IPUMS CPS with 8 variables
#> Extract 'cps_00157.dat' created on 2023-07-10
#> User notes:  User-provided description: Reproducing cps00006
```

This is because many common data processing functions have the
side-effect of removing these attributes:

``` r
# This doesn't actually change the data...
cps_data2 <- cps_data %>%
  mutate(MONTH = ifelse(TRUE, MONTH, MONTH))

# but removes attributes!
ipums_val_labels(cps_data2$MONTH)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: val <dbl>, lbl <chr>
```

In this case, you can always use the separate DDI as a metadata
reference:

``` r
ipums_val_labels(cps_ddi, var = MONTH)
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
```

Or even reattach the metadata, assuming the variable names still match
those in the DDI:

``` r
cps_data2 <- set_ipums_var_attributes(cps_data2, cps_ddi)

ipums_val_labels(cps_data2$MONTH)
#> # A tibble: 12 × 2
#>      val lbl      
#>    <int> <chr>    
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
```

### Hierarchical extracts

IPUMS microdata can come in either *rectangular* or *hierarchical*
format.

Rectangular data are transformed such that every row of data represents
the same type of record. For instance, each row will represent a person
record, and all household-level information for that person will be
included in the same row. (This is the case for `cps_data` shown in the
[example above](#reading-microdata-extracts).)

Hierarchical data have records of different types interspersed in a
single file. For instance, a household record will be included in its
own row followed by the person records associated with that household.

Hierarchical data can be loaded in list format or long format.
[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md)
will read in long format:

``` r
cps_hier_ddi <- read_ipums_ddi(ipums_example("cps_00159.xml"))

read_ipums_micro(cps_hier_ddi)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
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
```

The long format consists of a single
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
that includes rows with varying record types. In this example, some rows
have a record type of “Household” and others have a record type of
“Person”. Variables that do not apply to a particular record type will
be filled with `NA` in rows of that record type.

To read data in list format, use
[`read_ipums_micro_list()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md).
This function returns a list where each element contains all the records
for a given record type:

``` r
read_ipums_micro_list(cps_hier_ddi)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.
#> $HOUSEHOLD
#> # A tibble: 3,385 × 6
#>    RECTYPE               YEAR SERIAL MONTH     ASECWTH STATEFIP      
#>    <chr+lbl>            <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>     
#>  1 H [Household Record]  1962     80 3 [March]   1476. 55 [Wisconsin]
#>  2 H [Household Record]  1962     82 3 [March]   1598. 27 [Minnesota]
#>  3 H [Household Record]  1962     83 3 [March]   1707. 27 [Minnesota]
#>  4 H [Household Record]  1962     84 3 [March]   1790. 27 [Minnesota]
#>  5 H [Household Record]  1962    107 3 [March]   4355. 19 [Iowa]     
#>  6 H [Household Record]  1962    108 3 [March]   1479. 19 [Iowa]     
#>  7 H [Household Record]  1962    122 3 [March]   3603. 27 [Minnesota]
#>  8 H [Household Record]  1962    124 3 [March]   4104. 55 [Wisconsin]
#>  9 H [Household Record]  1962    125 3 [March]   2182. 55 [Wisconsin]
#> 10 H [Household Record]  1962    126 3 [March]   1826. 55 [Wisconsin]
#> # ℹ 3,375 more rows
#> 
#> $PERSON
#> # A tibble: 7,668 × 6
#>    RECTYPE            YEAR SERIAL PERNUM ASECWT INCTOT                          
#>    <chr+lbl>         <dbl>  <dbl>  <dbl>  <dbl> <dbl+lbl>                       
#>  1 P [Person Record]  1962     80      1  1476.      4883                       
#>  2 P [Person Record]  1962     80      2  1471.      5800                       
#>  3 P [Person Record]  1962     80      3  1579. 999999998 [Missing. (1962-1964 …
#>  4 P [Person Record]  1962     82      1  1598.     14015                       
#>  5 P [Person Record]  1962     83      1  1707.     16552                       
#>  6 P [Person Record]  1962     84      1  1790.      6375                       
#>  7 P [Person Record]  1962    107      1  4355. 999999999 [N.I.U.]              
#>  8 P [Person Record]  1962    107      2  1386.         0                       
#>  9 P [Person Record]  1962    107      3  1629.       600                       
#> 10 P [Person Record]  1962    107      4  1432. 999999999 [N.I.U.]              
#> # ℹ 7,658 more rows
```

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md)
and
[`read_ipums_micro_list()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro.md)
also support partial loading by selecting only a subset of columns or a
limited number of rows. See the documentation for more details about
other options.

## Reading IPUMS aggregate data extracts

Unlike microdata projects, NHGIS and IHGIS extracts provide their data
and metadata files bundled into a single .zip archive.
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_agg.md)
anticipates this structure and can read data files directly from this
file without the need to manually extract the files:

``` r
nhgis_ex1 <- ipums_example("nhgis0972_csv.zip")

nhgis_data <- read_ipums_agg(nhgis_ex1)
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

nhgis_data
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
```

Within a zipped extract archive, tabular data from aggregate data
projects are typically provided in .csv format.
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_agg.md)
passes several arguments on to
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
to allow you to fine-tune the import of these files. For instance, if
the guessed column types are incorrect, use the `col_types` argument to
adjust. This is most likely to occur for columns that contain geographic
codes that end up stored as numeric values:

``` r
# Convert MSA codes to character format
read_ipums_agg(
  nhgis_ex1,
  col_types = c(MSA_CMSAA = "c"),
  verbose = FALSE
)
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
```

### Variable metadata

Like microdata extracts, the data include variable-level metadata, where
available:

``` r
attributes(nhgis_data$D6Z001)
#> $label
#> [1] "Total area: 1989 to March 1990"
#> 
#> $var_desc
#> [1] "Table D6Z: Year Structure Built (Universe: Housing Units)"
```

However, variable metadata for aggregate data files are slightly
different than those provided by microdata products. First, they come
from a .txt codebook file (for NHGIS) or a collection of metadata .csv
files (for IHGIS) rather than an .xml DDI file. Codebooks can still be
loaded into an `ipums_ddi` object, but fields that do not apply to
aggregate data will be empty. In general, aggregate data codebooks
provide only variable labels and descriptions, along with citation
information.

``` r
nhgis_cb <- read_nhgis_codebook(nhgis_ex1)

# Most useful metadata for NHGIS is for variable labels:
ipums_var_info(nhgis_cb) %>%
  select(var_name, var_label, var_desc)
#> # A tibble: 25 × 3
#>    var_name  var_label                                                  var_desc
#>    <chr>     <chr>                                                      <chr>   
#>  1 GISJOIN   GIS Join Match Code                                        ""      
#>  2 YEAR      Data File Year                                             ""      
#>  3 STUSAB    State/US Abbreviation                                      ""      
#>  4 CMSA      Consolidated Metropolitan Statistical Area                 ""      
#>  5 DIVISIONA Division Code                                              ""      
#>  6 MSA_CMSAA Metropolitan Statistical Area/Consolidated Metropolitan S… ""      
#>  7 PMSA      Primary Metropolitan Statistical Area Name                 ""      
#>  8 PMSAA     Primary Metropolitan Statistical Area Code                 ""      
#>  9 REGIONA   Region Code                                                ""      
#> 10 STATEA    State Code                                                 ""      
#> # ℹ 15 more rows
```

IHGIS codebooks are structured differently than NHGIS codebooks, so each
collection has its own codebook reader. Both produce an `ipums_ddi`
object.

``` r
ihgis_cb <- read_ihgis_codebook(ipums_example("ihgis0014.zip"))

ipums_var_info(ihgis_cb)
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
```

By design, aggregate data codebooks are human-readable, and it may be
easier to interpret their contents in raw format. To view the codebook
itself without converting to an `ipums_ddi` object, set `raw = TRUE`.

``` r
nhgis_cb <- read_nhgis_codebook(nhgis_ex1, raw = TRUE)

cat(nhgis_cb[1:20], sep = "\n")
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

### Handling multiple files

For more complicated extracts that include data from multiple data
sources, the provided .zip archive will contain multiple codebook and
data files.

You can view the files contained in an extract to determine if this is
the case:

``` r
nhgis_ex2 <- ipums_example("nhgis0731_csv.zip")

ipums_list_files(nhgis_ex2)
#> # A tibble: 4 × 2
#>   type     file                                                   
#>   <chr>    <chr>                                                  
#> 1 data     nhgis0731_csv/nhgis0731_ds239_20185_nation.csv         
#> 2 data     nhgis0731_csv/nhgis0731_ts_nominal_state.csv           
#> 3 codebook nhgis0731_csv/nhgis0731_ds239_20185_nation_codebook.txt
#> 4 codebook nhgis0731_csv/nhgis0731_ts_nominal_state_codebook.txt
```

In these cases, you can use the `file_select` argument to indicate which
file to load. `file_select` supports most features of the [tidyselect
selection
language](https://tidyselect.r-lib.org/reference/language.html). (See
[`?selection_language`](https://tech.popdata.org/ipumsr/dev/reference/selection_language.md)
for documentation of the features supported in ipumsr.)

``` r
nhgis_data2 <- read_ipums_agg(
  nhgis_ex2, 
  file_select = contains("nation")
)

nhgis_data3 <- read_ipums_agg(
  nhgis_ex2, 
  file_select = contains("ts_nominal_state")
)
```

The matching codebook should automatically be loaded and attached to the
data:

``` r
attributes(nhgis_data2$AJWBE001)
#> $label
#> [1] "Estimates: Total"
#> 
#> $var_desc
#> [1] "Table AJWB: Sex by Age (Universe: Total population)"

attributes(nhgis_data3$A00AA1790)
#> $label
#> [1] "1790: Persons: Total"
#> 
#> $var_desc
#> [1] "Table A00: Total Population"
```

(If for some reason the metadata are not loaded successfully, you can
load it separately with either
[`read_nhgis_codebook()`](https://tech.popdata.org/ipumsr/dev/reference/read_nhgis_codebook.md)
or
[`read_ihgis_codebook()`](https://tech.popdata.org/ipumsr/dev/reference/read_ihgis_codebook.md).)

`file_select` also accepts the full path or the index of the file to
load:

``` r
# Match by file name
read_ipums_agg(
  nhgis_ex2, 
  file_select = "nhgis0731_csv/nhgis0731_ds239_20185_nation.csv"
)

# Match first file in extract
read_ipums_agg(nhgis_ex2, file_select = 1)
```

## Reading spatial data

IPUMS distributes spatial data for several projects.

- For microdata projects and IPUMS IHGIS, spatial data are distributed
  in shapefiles on dedicated geography pages separate from the standard
  extract system. Look for a **Geography and GIS** link in the
  **Supplemental Data** section of the project’s website to find spatial
  data files and information.
- For NHGIS, spatial data can be obtained within the extract system.
  Shapefiles will be distributed in their own .zip archive alongside the
  .zip archive containing the extract’s tabular data (if any tabular
  data are requested).

Use
[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_sf.md)
to load spatial data from any of these sources as an `sf` object from
[sf](https://r-spatial.github.io/sf/).

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_sf.md)
also supports the loading of spatial files within .zip archives and the
`file_select` syntax for file selection when multiple internal files are
present.

``` r
nhgis_shp_file <- ipums_example("nhgis0972_shape_small.zip")

shp_data <- read_ipums_sf(nhgis_shp_file)

head(shp_data)
#> Simple feature collection with 6 features and 8 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -129888.4 ymin: -967051.1 xmax: 1948770 ymax: 751282.5
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 6 × 9
#>   PMSA  MSACMSA ALTCMSA GISJOIN GISJOIN2   SHAPE_AREA SHAPE_LEN GISJOIN3 
#>   <chr> <chr>   <chr>   <chr>   <chr>           <dbl>     <dbl> <chr>    
#> 1 3280  3282    41      G3280   3280      2840869482.   320921. G32823280
#> 2 5760  5602    70      G5760   5760       237428573.   126226. G56025760
#> 3 1145  3362    42      G1145   1145      3730749183.   489789. G33621145
#> 4 1920  1922    31      G1920   1920     12068105590.   543164. G19221920
#> 5 0080  1692    28      G0080   0080      2401347006.   218892. G16920080
#> 6 1640  1642    21      G1640   1640      5608404797.   415671. G16421640
#> # ℹ 1 more variable: geometry <MULTIPOLYGON [m]>
```

These data can then be joined to associated tabular data. To preserve
IPUMS attributes from the tabular data used in the join, use an
`ipums_shape_*_join()` function:

``` r
joined_data <- ipums_shape_left_join(
  nhgis_data,
  shp_data,
  by = "GISJOIN"
)

attributes(joined_data$MSA_CMSAA)
#> $label
#> [1] "Metropolitan Statistical Area/Consolidated Metropolitan Statistical Area Code"
#> 
#> $var_desc
#> [1] ""
```

For aggregate data collections, the join code typically corresponds to
the `GISJOIN` variable. However, for microdata collections, the variable
name used for a geographic level in the tabular data may differ from
that in the spatial data. Consult the documentation and metadata for
these files to identify the correct join columns and use the `by`
argument to join on these columns.

Once joined, data include both statistical and spatial information along
with the variable metadata.

### Harmonized vs. non-harmonized data

Longitudinal analysis of geographic data is complicated by the fact that
geographic boundaries shift over time. IPUMS therefore provides multiple
types of spatial data:

- Harmonized (also called “integrated” or “consistent”) files have been
  made consistent over time by combining geographies that share area for
  different time periods.
- Non-harmonized, or year-specific, files represent geographies at a
  specific point in time.

Furthermore, some NHGIS time series tables have been standardized such
that the statistics have been adjusted to apply to a year-specific
geographical boundary.

When using spatial data, it is important to consult the project-specific
documentation to ensure you are using the most appropriate boundaries
for your research question and the data included in your analysis. As
always, documentation for the IPUMS project you’re working with should
explain the different options available.

# Read data from an IPUMS microdata extract by chunk

Read a microdata dataset downloaded from the IPUMS extract system in
chunks.

Use these functions to read a file that is too large to store in memory
at a single time. The file is processed in chunks of a given size, with
a provided callback function applied to each chunk.

Two files are required to load IPUMS microdata extracts:

- A [DDI codebook](https://ddialliance.org) file (.xml) used to parse
  the extract's data file

- A data file (either .dat.gz or .csv.gz)

See *Downloading IPUMS files* below for more information about
downloading these files.

`read_ipums_micro_chunked()` and `read_ipums_micro_list_chunked()`
differ in their handling of extracts that contain multiple record types.
See *Data structures* below.

Note that Stata, SAS, and SPSS file formats are not supported by ipumsr
readers. Convert your extract to fixed-width or CSV format, or see
[haven](https://haven.tidyverse.org/index.html) for help loading these
files.

## Usage

``` r
read_ipums_micro_chunked(
  ddi,
  callback,
  chunk_size = 10000,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)

read_ipums_micro_list_chunked(
  ddi,
  callback,
  chunk_size = 10000,
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

- callback:

  An
  [ipums_callback](https://tech.popdata.org/ipumsr/dev/reference/ipums_callback.md)
  object, or a function that will be converted to an
  `IpumsSideEffectCallback` object. Callback functions should include
  both data (`x`) and position (`pos`) arguments. See examples.

- chunk_size:

  Integer number of observations to read per chunk. Higher values use
  more RAM, but typically result in faster processing. Defaults to
  10,000.

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

  Note that if reading in chunks from a .csv or .csv.gz file, the
  callback function will be called *before* variable names are converted
  to lowercase, and thus should reference uppercase variable names.

## Value

Depends on the provided callback object. See
[ipums_callback](https://tech.popdata.org/ipumsr/dev/reference/ipums_callback.md).

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

- `read_ipums_micro_chunked()` reads each chunk of data into a
  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  where each row represents a single record, regardless of record type.
  Variables that do not apply to a particular record type will be filled
  with `NA` in rows of that record type. For instance, a person-specific
  variable will be missing in all rows associated with household
  records. The provided `callback` function should therefore operate on
  a `tibble` object.

- `read_ipums_micro_list_chunked()` reads each chunk of data into a list
  of `tibble` objects, where each list element contains only one record
  type. Each list element is named with its corresponding record type.
  The provided `callback` function should therefore operate on a list
  object. In this case, the chunk size references the total number of
  rows *across* record types, rather than in each record type.

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

[`read_ipums_micro_yield()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_yield.md)
for more flexible handling of large IPUMS microdata files.

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
suppressMessages(library(dplyr))

# Example codebook file
cps_rect_ddi_file <- ipums_example("cps_00157.xml")

# Function to extract Minnesota cases from CPS example
# (This can also be accomplished by including case selections
# in an extract definition)
#
# Function must take `x` and `pos` to refer to data and row position,
# respectively.
filter_mn <- function(x, pos) {
  x[x$STATEFIP == 27, ]
}

# Initialize callback
filter_mn_callback <- IpumsDataFrameCallback$new(filter_mn)

# Process data in chunks, filtering to MN cases in each chunk
read_ipums_micro_chunked(
  cps_rect_ddi_file,
  callback = filter_mn_callback,
  chunk_size = 1000,
  verbose = FALSE
)
#> # A tibble: 2,362 × 8
#>     YEAR SERIAL MONTH     ASECWTH STATEFIP       PERNUM ASECWT INCTOT   
#>    <dbl>  <dbl> <int+lbl>   <dbl> <int+lbl>       <dbl>  <dbl> <dbl+lbl>
#>  1  1962     82 3 [March]   1598. 27 [Minnesota]      1  1598. 14015    
#>  2  1962     83 3 [March]   1707. 27 [Minnesota]      1  1707. 16552    
#>  3  1962     84 3 [March]   1790. 27 [Minnesota]      1  1790.  6375    
#>  4  1962    122 3 [March]   3603. 27 [Minnesota]      1  3603. 15550    
#>  5  1962    122 3 [March]   3603. 27 [Minnesota]      2  3603.     0    
#>  6  1962    122 3 [March]   3603. 27 [Minnesota]      3  4243.  3443    
#>  7  1962    122 3 [March]   3603. 27 [Minnesota]      4  3920.   255    
#>  8  1962    122 3 [March]   3603. 27 [Minnesota]      5  3689.   135    
#>  9  1962    857 3 [March]   1832. 27 [Minnesota]      1  1832.   624    
#> 10  1962    857 3 [March]   1832. 27 [Minnesota]      2  1832.  3600    
#> # ℹ 2,352 more rows

# Tabulate INCTOT average by state without storing full dataset in memory
read_ipums_micro_chunked(
  cps_rect_ddi_file,
  callback = IpumsDataFrameCallback$new(
    function(x, pos) {
      x %>%
        mutate(
          INCTOT = lbl_na_if(
            INCTOT,
            ~ grepl("Missing|N.I.U.", .lbl)
          )
        ) %>%
        filter(!is.na(INCTOT)) %>%
        group_by(STATEFIP = as_factor(STATEFIP)) %>%
        summarize(INCTOT_SUM = sum(INCTOT), n = n(), .groups = "drop")
    }
  ),
  chunk_size = 1000,
  verbose = FALSE
) %>%
  group_by(STATEFIP) %>%
  summarize(avg_inc = sum(INCTOT_SUM) / sum(n))
#> # A tibble: 5 × 2
#>   STATEFIP     avg_inc
#>   <fct>          <dbl>
#> 1 Iowa           2252.
#> 2 Minnesota      2500.
#> 3 North Dakota   2800.
#> 4 South Dakota   1641.
#> 5 Wisconsin      2733.

# `x` will be a list when using `read_ipums_micro_list_chunked()`
read_ipums_micro_list_chunked(
  ipums_example("cps_00159.xml"),
  callback = IpumsSideEffectCallback$new(function(x, pos) {
    print(
      paste0(
        nrow(x$PERSON), " persons and ",
        nrow(x$HOUSEHOLD), " households in this chunk."
      )
    )
  }),
  chunk_size = 1000,
  verbose = FALSE
)
#> [1] "699 persons and 301 households in this chunk."
#> [1] "701 persons and 299 households in this chunk."
#> [1] "693 persons and 307 households in this chunk."
#> [1] "685 persons and 315 households in this chunk."
#> [1] "696 persons and 304 households in this chunk."
#> [1] "691 persons and 309 households in this chunk."
#> [1] "695 persons and 305 households in this chunk."
#> [1] "691 persons and 309 households in this chunk."
#> [1] "694 persons and 306 households in this chunk."
#> [1] "692 persons and 308 households in this chunk."
#> [1] "692 persons and 308 households in this chunk."
#> [1] "39 persons and 14 households in this chunk."
#> NULL

# Using the biglm package, you can even run a regression without storing
# the full dataset in memory
if (requireNamespace("biglm")) {
  lm_results <- read_ipums_micro_chunked(
    ipums_example("cps_00160.xml"),
    IpumsBiglmCallback$new(
      INCTOT ~ AGE + HEALTH, # Model formula
      function(x, pos) {
        x %>%
          mutate(
            INCTOT = lbl_na_if(
              INCTOT,
              ~ grepl("Missing|N.I.U.", .lbl)
            ),
            HEALTH = as_factor(HEALTH)
          )
      }
    ),
    chunk_size = 1000,
    verbose = FALSE
  )

  summary(lm_results)
}
#> Loading required namespace: biglm
#> Large data regression model: biglm(INCTOT ~ AGE + HEALTH, data, ...)
#> Sample size =  8194 
#>                        Coef        (95%        CI)        SE      p
#> (Intercept)      25351.6183  21728.2210  28975.016 1811.6986 0.0000
#> AGE                499.7783    427.5196    572.037   36.1293 0.0000
#> HEALTHVery good  -2135.1060  -5431.8110   1161.599 1648.3525 0.1952
#> HEALTHGood      -10480.2543 -14052.9835  -6907.525 1786.3646 0.0000
#> HEALTHFair      -23091.1061 -28274.2254 -17907.987 2591.5596 0.0000
#> HEALTHPoor      -34341.0066 -42611.9852 -26070.028 4135.4893 0.0000
```

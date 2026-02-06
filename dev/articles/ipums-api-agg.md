# Aggregate Data API Requests

This vignette details the options available for requesting data and
metadata for IPUMS aggregate data projects via the IPUMS API. Supported
aggregate data projects include:

- IPUMS NHGIS
- IPUMS IHGIS

If you haven’t yet learned the basics of the IPUMS API workflow, you may
want to start with the [IPUMS API
introduction](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).
The code below assumes you have registered and set up your API key as
described there.

The IPUMS API also supports several microdata projects. For details
about obtaining IPUMS microdata using ipumsr, see the
[microdata-specific
vignette](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-micro.md).

Before getting started, we’ll load ipumsr and some helpful packages for
this demo:

``` r
library(ipumsr)
library(dplyr)
library(purrr)
```

## Basic IPUMS aggregate data concepts

IPUMS aggregate data collections support several different types of data
products:

- A *dataset* contains a collection of *data tables* that each
  correspond to a particular tabulated summary statistic. A dataset is
  distinguished by the years, geographic levels, and topics that it
  covers. For instance, 2021 1-year data from the American Community
  Survey (ACS) is encapsulated in a single dataset. In other cases, a
  single census product will be split into multiple datasets.

  Datasets are available for both NHGIS and IHGIS.

- A *time series table* is a longitudinal data source that links
  comparable statistics from multiple U.S. censuses in a single bundle.
  A table is comprised of one or more related time series, each of which
  describes a single summary statistic measured at multiple times for a
  given geographic level.

  Time series tables are available for NHGIS.

- A *shapefile* (or *GIS file*) contains geographic data for a given
  geographic level and year. Typically, these files are composed of
  polygon geometries containing the boundaries of census reporting
  areas.

  Shapefiles are available via API for NHGIS. Shapefiles from IHGIS can
  be downloaded directly from the [IHGIS
  website](https://ihgis.ipums.org/geography-gis).

## Metadata for aggregate data projects

Of course, to make a request for any of these data sources, we have to
know the codes that the API uses to refer to them. Fortunately, we can
browse the metadata for available IPUMS aggregate data sources with
[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md)
and
[`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md).

Users can view a catalog of all available data sources of a given data
type or detailed metadata for a specific data source indicated by name.

### Summary metadata

To see a catalog of all available sources for a given data product type,
use
[`get_metadata_catalog()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md).
This returns a data frame containing the available data sources of the
indicated `metadata_type`.

Note that `metadata_type` supports different options for different
collections. Use
[`catalog_types()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata_catalog.md)
to determine the supported metadata types for a given collection.

``` r
ds <- get_metadata_catalog("nhgis", metadata_type = "datasets")

head(ds)
#> # A tibble: 6 × 4
#>   name      group       description                              sequence
#>   <chr>     <chr>       <chr>                                       <int>
#> 1 1790_cPop 1790 Census Population Data [US, States & Counties]       101
#> 2 1800_cPop 1800 Census Population Data [US, States & Counties]       201
#> 3 1810_cPop 1810 Census Population Data [US, States & Counties]       301
#> 4 1820_cPop 1820 Census Population Data [US, States & Counties]       401
#> 5 1830_cPop 1830 Census Population Data [US, States & Counties]       501
#> 6 1840_cAg  1840 Census Agriculture Data [US, States & Counties]      601
```

We can use basic functions from [dplyr](https://dplyr.tidyverse.org) to
filter the metadata to those records of interest. For instance, if we
wanted to find all the data sources related to agriculture from the 1900
Census, we could filter on `group` and `description`:

``` r
ds %>%
  filter(
    group == "1900 Census",
    grepl("Agriculture", description)
  )
#> # A tibble: 2 × 4
#>   name       group       description                                    sequence
#>   <chr>      <chr>       <chr>                                             <int>
#> 1 1900_cAg   1900 Census Agriculture Data [US, States & Counties]           1401
#> 2 1900_cPHAM 1900 Census Population, Housing, Agriculture & Manufactur…     1403
```

The values listed in the `name` column correspond to the code that you
would use to request that dataset when creating an extract definition to
be submitted to the IPUMS API.

Similarly, for time series tables:

``` r
tst <- get_metadata_catalog("nhgis", "time_series_tables")
```

While some of the metadata fields are consistent across different data
types, some, like `geographic_integration`, are specific to time series
tables:

``` r
head(tst)
#> # A tibble: 6 × 7
#>   name  description         geographic_integration sequence time_series years   
#>   <chr> <chr>               <chr>                     <dbl> <list>      <list>  
#> 1 A00   Total Population    Nominal                    100. <tibble>    <tibble>
#> 2 AV0   Total Population    Nominal                    100. <tibble>    <tibble>
#> 3 B78   Total Population    Nominal                    100. <tibble>    <tibble>
#> 4 CL8   Total Population    Standardized to 2010       100. <tibble>    <tibble>
#> 5 A57   Persons by Urban/R… Nominal                    101. <tibble>    <tibble>
#> 6 A59   Persons by Urban/R… Nominal                    101. <tibble>    <tibble>
#> # ℹ 1 more variable: geog_levels <list>
```

Note that for time series tables, some metadata fields are stored in
list columns, where each entry is itself a data frame:

``` r
tst$years[[1]]
#> # A tibble: 24 × 3
#>    name  description sequence
#>    <chr> <chr>          <int>
#>  1 1790  1790               1
#>  2 1800  1800               2
#>  3 1810  1810               3
#>  4 1820  1820               4
#>  5 1830  1830               5
#>  6 1840  1840               6
#>  7 1850  1850               7
#>  8 1860  1860               8
#>  9 1870  1870              12
#> 10 1880  1880              22
#> # ℹ 14 more rows

tst$geog_levels[[1]]
#> # A tibble: 2 × 3
#>   name   description   sequence
#>   <chr>  <chr>            <int>
#> 1 state  State                4
#> 2 county State--County       25
```

To filter on these columns, we can use
[`map_lgl()`](https://purrr.tidyverse.org/reference/map.html) from
[purrr](https://purrr.tidyverse.org/). For instance, to find all time
series tables that include data from a particular year:

``` r
# Iterate over each `years` entry, identifying whether that entry
# contains "1840" in its `name` column.
tst %>%
  filter(map_lgl(years, ~ "1840" %in% .x$name))
#> # A tibble: 2 × 7
#>   name  description        geographic_integration sequence time_series years   
#>   <chr> <chr>              <chr>                     <dbl> <list>      <list>  
#> 1 A00   Total Population   Nominal                    100. <tibble>    <tibble>
#> 2 A08   Persons by Sex [2] Nominal                    102. <tibble>    <tibble>
#> # ℹ 1 more variable: geog_levels <list>
```

For more details on working with nested data frames, see this [tidyr
article](https://tidyr.tidyverse.org/articles/nest.html).

### Detailed metadata

Once we have identified a data source of interest, we can find out more
about its detailed options by providing its name to the corresponding
argument of
[`get_metadata()`](https://tech.popdata.org/ipumsr/dev/reference/get_metadata.md):

``` r
cAg_meta <- get_metadata("nhgis", dataset = "1900_cAg")
```

This provides a comprehensive list of the possible specifications for
the input data source. For instance, for the `1900_cAg` dataset, we have
66 tables to choose from, and 3 possible geographic levels:

``` r
cAg_meta$data_tables
#> # A tibble: 66 × 7
#>    name  description       universe nhgis_code sequence dataset_name n_variables
#>    <chr> <chr>             <chr>    <chr>         <int> <chr>              <int>
#>  1 NT1   Total Population  Persons  AWS               1 1900_cAg               1
#>  2 NT2   Number of Farms   Farms    AW3               2 1900_cAg               1
#>  3 NT3   Average Farm Size Farms    AXE               3 1900_cAg               1
#>  4 NT4   Farm Acreage      Farms    AXP               4 1900_cAg              10
#>  5 NT5   Farm Management   Farms    AXZ               5 1900_cAg               3
#>  6 NT6   Race of Farmer    Farms    AYA               6 1900_cAg               2
#>  7 NT7   Race of Farmer b… Farms    AYJ               7 1900_cAg              12
#>  8 NT8   Number of Farms   Farms    AYK               8 1900_cAg               1
#>  9 NT9   Farms with Build… Farms w… AYL               9 1900_cAg               1
#> 10 NT10  Acres of Farmland Farms    AWT              10 1900_cAg               1
#> # ℹ 56 more rows

cAg_meta$geog_levels
#> # A tibble: 3 × 4
#>   name   description   has_geog_extent_selection sequence
#>   <chr>  <chr>         <lgl>                        <int>
#> 1 nation Nation        FALSE                            1
#> 2 state  State         FALSE                            4
#> 3 county State--County FALSE                           25
```

You can also get detailed metadata for an individual data table. Since
data tables belong to specific datasets, both need to be specified to
identify a data table:

``` r
get_metadata("nhgis", dataset = "1900_cAg", data_table = "NT2")
#> $name
#> [1] "NT2"
#> 
#> $description
#> [1] "Number of Farms"
#> 
#> $universe
#> [1] "Farms"
#> 
#> $nhgis_code
#> [1] "AW3"
#> 
#> $sequence
#> [1] 2
#> 
#> $dataset_name
#> [1] "1900_cAg"
#> 
#> $variables
#> # A tibble: 1 × 2
#>   description nhgis_code
#>   <chr>       <chr>     
#> 1 Total       AW3001
```

Note that the `name` element is the one that contains the codes used for
interacting with the IPUMS API. (The `nhgis_code` element refers to the
prefix attached to individual variables in the output data, and the API
will throw an error if you use it in an extract definition.) For more
details on interpreting each of the provided metadata elements, see the
[IPUMS developer
documentation](https://developer.ipums.org/docs/v2/workflows/explore_metadata/).

Now that we have identified some of our options, we can go ahead and
define an extract request to submit to the IPUMS API.

## Defining an IPUMS aggregate data extract request

To create an extract definition for an IPUMS aggregate data project, use
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md).
When you define an extract request, you can specify the data to be
included in the extract and indicate the desired format and layout.

### Basic extract definitions

Let’s say we’re interested in getting state-level data on the number of
farms and their average size from the `1900_cAg` dataset that we
identified above. As we can see in the metadata, these data are
contained in tables `NT2` and `NT3`:

``` r
cAg_meta$data_tables
#> # A tibble: 66 × 7
#>    name  description       universe nhgis_code sequence dataset_name n_variables
#>    <chr> <chr>             <chr>    <chr>         <int> <chr>              <int>
#>  1 NT1   Total Population  Persons  AWS               1 1900_cAg               1
#>  2 NT2   Number of Farms   Farms    AW3               2 1900_cAg               1
#>  3 NT3   Average Farm Size Farms    AXE               3 1900_cAg               1
#>  4 NT4   Farm Acreage      Farms    AXP               4 1900_cAg              10
#>  5 NT5   Farm Management   Farms    AXZ               5 1900_cAg               3
#>  6 NT6   Race of Farmer    Farms    AYA               6 1900_cAg               2
#>  7 NT7   Race of Farmer b… Farms    AYJ               7 1900_cAg              12
#>  8 NT8   Number of Farms   Farms    AYK               8 1900_cAg               1
#>  9 NT9   Farms with Build… Farms w… AYL               9 1900_cAg               1
#> 10 NT10  Acres of Farmland Farms    AWT              10 1900_cAg               1
#> # ℹ 56 more rows
```

#### Dataset specifications

To request these data, we need to make an explicit *dataset
specification*.

For IPUMS NHGIS, all datasets must be associated with a selection of
data tables and geographic levels. For IHGIS, all datasets must be
associated with a selection of data tables and tabulation geographies.

We can use the
[`ds_spec()`](https://tech.popdata.org/ipumsr/dev/reference/ds_spec.md)
helper function to specify our selections for these parameters.
[`ds_spec()`](https://tech.popdata.org/ipumsr/dev/reference/ds_spec.md)
bundles all the selections for a given dataset together into a single
object (in this case, a `ds_spec` object):

``` r
dataset <- ds_spec(
  "1900_cAg",
  data_tables = c("NT1", "NT2"),
  geog_levels = "state"
)

str(dataset)
#> List of 3
#>  $ name       : chr "1900_cAg"
#>  $ data_tables: chr [1:2] "NT1" "NT2"
#>  $ geog_levels: chr "state"
#>  - attr(*, "class")= chr [1:3] "ds_spec" "ipums_spec" "list"
```

This dataset specification can then be provided to the extract
definition:

``` r
nhgis_ext <- define_extract_agg(
  "nhgis",
  description = "Example farm data in 1900",
  datasets = dataset
)

nhgis_ext
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example farm data in 1900
#> 
#> Dataset: 1900_cAg
#>   Tables: NT1, NT2
#>   Geog Levels: state
```

For NHGIS, dataset specifications can also include selections for
`years` and `breakdown_values`, but these are not available for all
datasets.

For IHGIS, datasets must include a selection of
`tabulation_geographies`:

``` r
define_extract_agg(
  "ihgis",
  description = "Example IHGIS extract",
  datasets = ds_spec(
    "KZ2009pop", 
    data_tables = "KZ2009pop.AAA",
    tabulation_geographies = "KZ2009pop.g0"
  )
)
#> Unsubmitted IPUMS IHGIS extract 
#> Description: Example IHGIS extract
#> 
#> Dataset: KZ2009pop
#>   Tables: KZ2009pop.AAA
#>   Tabulation Geogs: KZ2009pop.g0
```

#### Time series table specifications

Similarly, to make a request for time series tables, use the
[`tst_spec()`](https://tech.popdata.org/ipumsr/dev/reference/ds_spec.md)
helper. This makes a `tst_spec` object containing a time series table
specification.

Time series tables do not contain individual data tables, but do require
a geographic level selection, and allow an optional selection of years:

``` r
define_extract_agg(
  "nhgis",
  description = "Example time series table request",
  time_series_tables = tst_spec(
    "CW3",
    geog_levels = c("county", "tract"),
    years = c("1990", "2000")
  )
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example time series table request
#> 
#> Time Series Table: CW3
#>   Geog Levels: county, tract
#>   Years: 1990, 2000
```

#### Shapefile specifications

Shapefiles don’t have any additional specification options, and
therefore can be requested simply by providing their names:

``` r
define_extract_agg(
  "nhgis",
  description = "Example shapefiles request",
  shapefiles = c("us_county_2021_tl2021", "us_county_2020_tl2020")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Example shapefiles request
#> 
#> Shapefiles: us_county_2021_tl2021, us_county_2020_tl2020
```

IHGIS shapefiles are not available via API, but can be downloaded from
the [IHGIS website](https://ihgis.ipums.org/geography-gis).

#### Invalid specifications

An attempt to define an extract that includes unexpected specifications
or does not have all the required specifications for the given
collection will throw an error:

``` r
define_extract_agg(
  "nhgis",
  description = "Invalid extract",
  datasets = ds_spec("1900_STF1", "NP1", tabulation_geographies = "g0")
)
#> Error in `validate_ipums_spec()`:
#> ! Invalid `ds_spec` specification:
#> ✖ `geog_levels` must not contain missing values.
#> ✖ `tabulation_geographies` must be missing.
```

Note that it is still possible to make invalid extract requests (for
instance, by requesting a dataset or data table that doesn’t exist).
This kind of issue will be caught upon submission to the API, not upon
the creation of the extract definition.

### More complicated extract definitions

It’s possible to request data for multiple datasets (or time series
tables) in a single extract definition. To do so, pass a `list` of
`ds_spec` or `tst_spec` objects in
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md):

``` r
define_extract_agg(
  "nhgis",
  description = "Slightly more complicated extract request",
  datasets = list(
    ds_spec("2018_ACS1", "B01001", "state"),
    ds_spec("2019_ACS1", "B01001", "state")
  ),
  shapefiles = c("us_state_2018_tl2018", "us_state_2019_tl2019")
)
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Slightly more complicated extract request
#> 
#> Dataset: 2018_ACS1
#>   Tables: B01001
#>   Geog Levels: state
#> 
#> Dataset: 2019_ACS1
#>   Tables: B01001
#>   Geog Levels: state
#> 
#> Shapefiles: us_state_2018_tl2018, us_state_2019_tl2019
```

For extracts with multiple datasets or time series tables, it may be
easier to generate the specifications independently before creating your
extract request object. You can quickly create multiple `ds_spec`
objects by iterating across the specifications you want to include.
(This workflow works particularly well for ACS datasets, which often
have the same data table names across datasets.) Here, we use
[purrr](https://purrr.tidyverse.org/) to do so, but you could also use a
`for` loop:

``` r
ds_names <- c("2019_ACS1", "2018_ACS1")
tables <- c("B01001", "B01002")
geogs <- c("county", "state")

# For each dataset to include, create a specification with the
# data tabels and geog levels indicated above
datasets <- purrr::map(
  ds_names,
  ~ ds_spec(name = .x, data_tables = tables, geog_levels = geogs)
)

nhgis_ext <- define_extract_agg(
  "nhgis",
  description = "Slightly more complicated extract request",
  datasets = datasets
)

nhgis_ext
#> Unsubmitted IPUMS NHGIS extract 
#> Description: Slightly more complicated extract request
#> 
#> Dataset: 2019_ACS1
#>   Tables: B01001, B01002
#>   Geog Levels: county, state
#> 
#> Dataset: 2018_ACS1
#>   Tables: B01001, B01002
#>   Geog Levels: county, state
```

This workflow also makes it easy to quickly update the specifications in
the future. For instance, to add the 2017 ACS 1-year data to the extract
definition above, you’d only need to add `"2017_ACS1"` to the `ds_names`
variable. The iteration would automatically add the selected tables and
geog levels for the new dataset.

### Data layout and file format

IPUMS NHGIS extract definitions also support additional options to
modify the layout and format of the extract’s resulting data files.

For extracts that contain time series tables, the `tst_layout` argument
indicates how the longitudinal data should be organized.

For extracts that contain datasets with multiple breakdowns or data
types, use the `breakdown_and_data_type_layout` argument to specify a
layout . This is most common for data sources that contain both
estimates and margins of error, like the ACS.

See the documentation for
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md)
for more details on these options.

## Next steps

Once you have defined an extract request, you can submit the extract for
processing:

``` r
nhgis_ext_submitted <- submit_extract(nhgis_ext)
```

The workflow for submitting and monitoring an extract request and
downloading its files when complete is described in the [IPUMS API
introduction](https://tech.popdata.org/ipumsr/dev/articles/ipums-api.md).

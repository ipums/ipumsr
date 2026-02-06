# Introduction to the IPUMS API for R Users

The IPUMS API provides two asset types, both of which are supported by
ipumsr:

- **IPUMS extract** endpoints can be used to submit extract requests for
  processing and download completed extract files.
- **IPUMS metadata** endpoints can be used to discover and explore
  available IPUMS data as well as retrieve codes, names, and other
  extract parameters necessary to form extract requests.

Use of the IPUMS API enables the adoption of a programmatic workflow
that can help users to:

- Precisely recreate the specifications of previous extract requests,
  making analysis scripts reproducible and self-contained
- Save extract request definitions that can be shared with others
  without violating IPUMS conditions
- Integrate the extract download process with functions to load data
  into R
- Quickly identify and explore available IPUMS data sources

The basic workflow for interacting with the IPUMS API is as follows:

1.  [Define](#define) the parameters of an extract request
2.  [Submit](#submit) the extract request to the IPUMS API
3.  [Wait](#wait) for an extract to complete
4.  [Download](#download) a completed extract

Before getting started, we’ll load the necessary packages for the
examples in this vignette:

``` r
library(ipumsr)
library(dplyr)
library(purrr)
```

## API availability

IPUMS **extract** support is currently available via API for the
following collections:

- IPUMS microdata collections
  - IPUMS USA
  - IPUMS CPS
  - IPUMS International
  - IPUMS Time Use (ATUS, AHTUS, MTUS)
  - IPUMS Health Surveys (NHIS, MEPS)
- IPUMS aggregate data collections
  - IPUMS NHGIS
  - IPUMS IHGIS

Note that this support only includes data available via a collection’s
extract engine. Many collections provide supplemental data files via
direct download. Currently, API support for downloading these files is
limited to IPUMS NHGIS. See
[`download_supplemental_data()`](https://tech.popdata.org/ipumsr/dev/reference/download_supplemental_data.md)
for more information.

IPUMS **metadata** support is currently available via API for the IPUMS
aggregate data collections (NHGIS and IHGIS).

API support will continue to be added for more collections in the
future. You can check general API availability for all IPUMS collections
with
[`ipums_data_collections()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_data_collections.md).

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
#>  9 IPUMS DHS           microdata       dhs          FALSE      
#> 10 IPUMS PMA           microdata       pma          FALSE      
#> 11 IPUMS MICS          microdata       mics         FALSE      
#> 12 IPUMS NHIS          microdata       nhis         TRUE       
#> 13 IPUMS MEPS          microdata       meps         TRUE       
#> 14 IPUMS Higher Ed     microdata       highered     FALSE
```

Note that the tools in ipumsr may not necessarily support all the
functionality currently supported by the IPUMS API. See the [API
documentation](https://developer.ipums.org/docs/apiprogram/) for more
information about its latest features.

## Set up your API key

To interact with the IPUMS API, you’ll need to register for access with
the IPUMS project you’ll be using. If you have not yet registered, you
can find the link to register for each project at the top of its
website, which can be accessed from [ipums.org](https://www.ipums.org).

Once you’re registered, you’ll be able to [create an API
key](https://account.ipums.org/api_keys).

By default, ipumsr API functions assume that your key is stored in the
`IPUMS_API_KEY` environment variable. You can also provide your key
directly to these functions, but storing it in an environment variable
saves you some typing and helps prevent you from inadvertently sharing
your key with others (for instance, on GitHub).

You can save your API key to the `IPUMS_API_KEY` environment variable
with
[`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_api_key.md).
To save your key for use in future sessions, set `save = TRUE`. This
will add your API key to your `.Renviron` file in your user home
directory.

``` r
# Save key in .Renviron for use across sessions
set_ipums_api_key("paste-your-key-here", save = TRUE)
```

The rest of this vignette assumes you have obtained an API key and
stored it in the `IPUMS_API_KEY` environment variable.

## Define an extract request

ipumsr contains two extract definition functions used to specify the
parameters of a new extract request from scratch.

- For microdata projects, use
  [`define_extract_micro()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_micro.md).
- For aggregate data projects, use
  [`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md).

When you define an extract request, you can specify the data to be
included in the extract and indicate the desired format and layout.

For instance, the following defines a simple IPUMS USA extract request
for the `AGE`, `SEX`, `RACE`, `STATEFIP`, and `MARST` variables from the
2018 and 2019 American Community Survey (ACS):

``` r
usa_extract_definition <- define_extract_micro(
  collection = "usa",
  description = "USA extract for API vignette",
  samples = c("us2018a", "us2019a"),
  variables = c("AGE", "SEX", "RACE", "STATEFIP", "MARST")
)

usa_extract_definition
#> Unsubmitted IPUMS USA extract 
#> Description: USA extract for API vignette
#> 
#> Samples: (2 total) us2018a, us2019a
#> Variables: (5 total) AGE, SEX, RACE, STATEFIP, MARST
```

The exact extract definition options vary across collections, but all
collections can be used with the same general workflow. For more details
on the available extract definition options, see the associated
[microdata](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-micro.md)
and [aggregate
data](https://tech.popdata.org/ipumsr/dev/articles/ipums-api-agg.md)
vignettes.

For the purposes of demonstrating the overall workflow, we will continue
to work with the sample IPUMS USA extract definition created above.

### Extract request objects

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_micro.md)
and
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/dev/reference/define_extract_agg.md)
always produce an `ipums_extract` object, which can be handled by other
API functions (see
[`?ipums_extract`](https://tech.popdata.org/ipumsr/dev/reference/ipums_extract-class.md)).
Furthermore, these objects will have a subclass for the particular
collection with which they are associated.

``` r
class(usa_extract_definition)
#> [1] "usa_extract"   "micro_extract" "ipums_extract" "list"
```

Many of the specifications for a given extract request object can be
accessed by indexing the object:

``` r
names(usa_extract_definition$samples)
#> [1] "us2018a" "us2019a"

names(usa_extract_definition$variables)
#> [1] "AGE"      "SEX"      "RACE"     "STATEFIP" "MARST"

usa_extract_definition$data_format
#> [1] "fixed_width"
```

`ipums_extract` objects also contain information about the extract
request’s processing status and its assigned extract number, which
serves as an identifier for the extract request. Since this extract
request is still unsubmitted, it has no request number:

``` r
usa_extract_definition$status
#> [1] "unsubmitted"

usa_extract_definition$number
#> [1] NA
```

To obtain the data requested in the extract definition, we must first
submit it to the IPUMS API for processing.

## Submit an extract request

To submit an extract definition, use
[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md).

If no errors are detected in the extract definition, a submitted extract
request will be returned with its assigned number and status. Storing
the returned object can be useful for checking the extract request’s
status later.

``` r
usa_extract_submitted <- submit_extract(usa_extract_definition)
#> Successfully submitted IPUMS USA extract number 456
```

The extract number will be stored in the returned object:

``` r
usa_extract_submitted$number
#> [1] 456
usa_extract_submitted$status
#> [1] "queued"
```

Note that some fields of a submitted extract may be automatically
updated by the API upon submission. For instance, for microdata
extracts, additional preselected variables may be added to the extract
even if they weren’t specified explicitly in the extract definition.

``` r
names(usa_extract_submitted$variables)
#>  [1] "YEAR"     "SAMPLE"   "SERIAL"   "CBSERIAL" "HHWT"     "CLUSTER" 
#>  [7] "STATEFIP" "STRATA"   "GQ"       "PERNUM"   "PERWT"    "SEX"     
#> [13] "AGE"      "MARST"    "RACE"
```

If you forget to store the updated extract object returned by
[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md),
you can use the
[`get_last_extract_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_info.md)
helper to request the information for your most recent extract request
for a given collection:

``` r
usa_extract_submitted <- get_last_extract_info("usa")

usa_extract_submitted$number
#> [1] 456
```

## Wait for an extract request to complete

It may take some time for the IPUMS servers to process your extract
request. You can ensure that an extract has finished processing before
you attempt to download its files by using
[`wait_for_extract()`](https://tech.popdata.org/ipumsr/dev/reference/wait_for_extract.md).
This polls the API regularly until processing has completed (by default,
each interval increases by 10 seconds). It then returns an
`ipums_extract` object containing the completed extract definition.

``` r
usa_extract_complete <- wait_for_extract(usa_extract_submitted)
#> Checking extract status...
#> Waiting 10 seconds...
#> Checking extract status...
#> IPUMS USA extract 456 is ready to download.
```

``` r
usa_extract_complete$status
#> [1] "completed"
```

``` r
# `download_links` should be populated if the extract is ready for download
names(usa_extract_complete$download_links)
#> [1] "r_command_file"     "basic_codebook"     "data"              
#> [4] "stata_command_file" "sas_command_file"   "spss_command_file" 
#> [7] "ddi_codebook"
```

Note that
[`wait_for_extract()`](https://tech.popdata.org/ipumsr/dev/reference/wait_for_extract.md)
will tie up your R session until your extract is ready to download.
While this is fine in a strictly programmatic workflow, it may be
frustrating when working interactively, especially for large extracts or
when the IPUMS servers are busy.

In these cases, you can manually check whether an extract is ready for
download with
[`is_extract_ready()`](https://tech.popdata.org/ipumsr/dev/reference/wait_for_extract.md).
As long as this returns `TRUE`, you should be able to download your
extract’s files.

``` r
is_extract_ready(usa_extract_submitted)
#> [1] TRUE
```

For a more detailed status check, provide the extract’s collection and
number to
[`get_extract_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_info.md).
This returns an `ipums_extract` object reflecting the requested extract
definition with the most current status. The `status` of a submitted
extract will be one of `"queued"`, `"started"`, `"produced"`,
`"canceled"`, `"failed"`, or `"completed"`.

``` r
usa_extract_submitted <- get_extract_info(usa_extract_submitted)

usa_extract_submitted$status
#> [1] "completed"
```

Note that extracts are removed from the IPUMS servers after a set period
of time (72 hours for microdata collections, 2 weeks for IPUMS NHGIS).
Therefore, an extract that has a `"completed"` status may still be
unavailable for download.

[`is_extract_ready()`](https://tech.popdata.org/ipumsr/dev/reference/wait_for_extract.md)
will alert you if the extract has expired and needs to be resubmitted.
Simply use
[`submit_extract()`](https://tech.popdata.org/ipumsr/dev/reference/submit_extract.md)
to resubmit an extract request. Note that this will produce a *new*
extract (with a new extract number), even if the extract definition is
identical.

## Download an extract

Once your extract has finished processing, use
[`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
to download the extract’s data files to your local machine. This will
return the path to the downloaded file(s) required to load the data into
R.

For microdata collections, this will be the path to the DDI codebook
(.xml) file, which can be used to read the associated data (contained in
a .dat.gz file).

For NHGIS, this will be a path to the .zip archive containing the
requested data files and/or shapefiles.

``` r
# By default, downloads to your current working directory
filepath <- download_extract(usa_extract_submitted)
```

The files produced by
[`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
can be passed directly into the reader functions provided by ipumsr. For
instance, for microdata projects:

``` r
ddi <- read_ipums_ddi(filepath)
micro_data <- read_ipums_micro(ddi)
```

If instead you’re working with an aggregate data extract, use
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_agg.md)
(or
[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_sf.md)
for spatial data from IPUMS NHGIS extracts).

See the [associated
vignette](https://tech.popdata.org/ipumsr/dev/articles/ipums-read.md)
for more information about loading IPUMS data into R.

## Get info on past extracts

To retrieve the definition corresponding to a particular extract,
provide its collection and number to
[`get_extract_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_info.md).
These can be provided either as a single string of the form
`"collection:number"` or as a length-2 vector:
`c("collection", number)`. Several other API functions support this
syntax as well.

``` r
usa_extract <- get_extract_info("usa:47")

# Alternatively:
usa_extract <- get_extract_info(c("usa", 47))

usa_extract
#> Submitted IPUMS USA extract number 47
#> Description: Test extract
#> 
#> Samples: (1 total) us2017b
#> Variables: (8 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, GQ, PERNUM, PERWT
```

If you know you made a specific extract definition in the past, but you
can’t remember the exact number, you can use
[`get_extract_history()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_history.md)
to peruse your recent extract requests for a particular collection.

By default, this returns your 10 most recent extract requests as a list
of `ipums_extract` objects. You can adjust how many requests to retrieve
with the `how_many` argument:

``` r
usa_extracts <- get_extract_history("usa", how_many = 3)

usa_extracts
#> [[1]]
#> Submitted IPUMS USA extract number 456
#> Description: USA extract for API vignette
#> 
#> Samples: (2 total) us2018a, us2019a
#> Variables: (15 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, CLUSTER,...
#> 
#> [[2]]
#> Submitted IPUMS USA extract number 455
#> Description: Data from long ago
#> 
#> Samples: (1 total) us1880a
#> Variables: (12 total) YEAR, SAMPLE, SERIAL, HHWT, CLUSTER, STRATA, G...
#> 
#> [[3]]
#> Submitted IPUMS USA extract number 454
#> Description: Data from 2017 PRCS
#> 
#> Samples: (1 total) us2017b
#> Variables: (9 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, GQ, PERNU...
```

Because this is a list of `ipums_extract` objects, you can operate on
them with the API functions that have been introduced already.

``` r
is_extract_ready(usa_extracts[[2]])
#> [1] TRUE
```

You can also iterate through your extract history to find extracts with
particular characteristics. For instance, we can use
[`purrr::keep()`](https://purrr.tidyverse.org/reference/keep.html) to
find all extracts that contain a certain variable or are ready for
download:

``` r
purrr::keep(usa_extracts, ~ "MARST" %in% names(.x$variables))
#> [[1]]
#> Submitted IPUMS USA extract number 456
#> Description: USA extract for API vignette
#> 
#> Samples: (2 total) us2018a, us2019a
#> Variables: (15 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, CLUSTER,...

purrr::keep(usa_extracts, is_extract_ready)
#> [[1]]
#> Submitted IPUMS USA extract number 456
#> Description: USA extract for API vignette
#> 
#> Samples: (2 total) us2018a, us2019a
#> Variables: (15 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, CLUSTER,...
#> 
#> [[2]]
#> Submitted IPUMS USA extract number 455
#> Description: Data from long ago
#> 
#> Samples: (1 total) us1880a
#> Variables: (12 total) YEAR, SAMPLE, SERIAL, HHWT, CLUSTER, STRATA, G...
#> 
#> [[3]]
#> Submitted IPUMS USA extract number 454
#> Description: Data from 2017 PRCS
#> 
#> Samples: (1 total) us2017b
#> Variables: (9 total) YEAR, SAMPLE, SERIAL, CBSERIAL, HHWT, GQ, PERNU...
```

Or we can use the
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) family
to browse certain values:

``` r
purrr::map_chr(usa_extracts, ~ .x$description)
#> [1] "USA extract for API vignette" "Data from long ago"          
#> [3] "Data from 2017 PRCS"
```

If you regularly use only a single IPUMS collection, you can save
yourself some typing by setting that collection as your default.
[`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_default_collection.md)
will save a specified collection to the value of the
`IPUMS_DEFAULT_COLLECTION` environment variable. If you have a default
collection set, API functions will use that collection in all requests,
assuming no other collection is specified.

``` r
# Set `save = TRUE` to store across sessions
set_ipums_default_collection("usa")
```

``` r
# Check the default collection:
Sys.getenv("IPUMS_DEFAULT_COLLECTION")
#> [1] "usa"
```

``` r
# Most recent USA extract:
usa_last <- get_last_extract_info()

# Request info on extract request "usa:10"
usa_extract_10 <- get_extract_info(10)

# You can still request other collections as usual:
cps_extract_10 <- get_extract_info("cps:10")
```

## Share an extract definition

One exciting feature enabled by the IPUMS API is the ability to share a
standardized extract definition with other IPUMS users so that they can
create an identical extract request themselves. The terms of use for
most IPUMS collections prohibit the public redistribution of IPUMS data,
but don’t prohibit the sharing of data extract definitions.

ipumsr facilitates this type of sharing with
[`save_extract_as_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md)
and
[`define_extract_from_json()`](https://tech.popdata.org/ipumsr/dev/reference/save_extract_as_json.md),
which write and read `ipums_extract` objects to and from a standardized
JSON-formatted file.

``` r
usa_extract_10 <- get_extract_info("usa:10")
save_extract_as_json(usa_extract_10, file = "usa_extract_10.json")
```

At this point, you can send `"usa_extract_10.json"` to another user to
allow them to create a duplicate `ipums_extract` object, which they can
load and submit to the API themselves (as long as they have [API
access](#set-key)).

``` r
clone_of_usa_extract_10 <- define_extract_from_json("usa_extract_10.json")
usa_extract_10_resubmitted <- submit_extract(clone_of_usa_extract_10)
```

Note that the code in the previous chunk assumes that the file is saved
in the current working directory. If it’s saved somewhere else, replace
`"usa_extract_10.json"` with the full path to the file.

## Revise a previous extract request

Occasionally, you may want to modify an existing extract definition
(e.g. to update an analysis with new data). The easiest way to do so is
to add the new specifications to the `define_extract_*()` code that
produced the original extract definition. This is why we highly
recommend that you save this code somewhere where it can be accessed and
updated in the future.

However, there are cases where the original extract definition code does
not exist (e.g. if the extract was created using the online IPUMS
extract system). In this case, the best approach is to view the extract
definition with
[`get_extract_info()`](https://tech.popdata.org/ipumsr/dev/reference/get_extract_info.md)
and create a new extract definition (using a `define_extract_*()`
function) that reproduces that definition along with the desired
modifications. While this may be a bit tedious for complex extract
definitions, it is a one-time investment that will make any future
updates to the extract definition much easier.

Previously, we encouraged users to use the helpers
[`add_to_extract()`](https://tech.popdata.org/ipumsr/dev/reference/add_to_extract.md)
and
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/dev/reference/remove_from_extract.md)
when modifying extracts. We now encourage you to re-write extract
definitions because they improve reproducibility: extract definition
code will always be more clear and stable if it is written explicitly,
rather than based only on an old extract number. These two functions may
be retired in the future.

## Putting it all together

The core API functions in ipumsr are compatible with one another such
that they can be combined into a single pipeline that requests,
downloads, and reads your extract data into an R data frame:

``` r
usa_data <- define_extract_micro(
  "usa",
  "USA extract for API vignette",
  samples = c("us2018a", "us2019a"),
  variables = c("AGE", "SEX", "RACE", "STATEFIP")
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() %>%
  read_ipums_micro()
```

Note that for NHGIS extracts that contain both data and shapefiles, a
single file will need to be selected before reading, as
[`download_extract()`](https://tech.popdata.org/ipumsr/dev/reference/download_extract.md)
will return the path to each file. For instance, for a hypothetical
`nhgis_extract` that contains both tabular and spatial data:

``` r
nhgis_data <- download_extract(nhgis_extract) %>%
  purrr::pluck("data") %>% # Select only the tabular data file to read
  read_ipums_agg()
```

Not only does this API workflow allow you to obtain IPUMS data without
ever leaving your R environment, but it also allows you to retain a
reproducible record of your process. This makes it much easier to
document your workflow, collaborate with other researchers, and update
your analysis in the future.

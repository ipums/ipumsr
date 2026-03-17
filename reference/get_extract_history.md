# Browse definitions of previously submitted extract requests

Retrieve definitions of an arbitrary number of previously submitted
extract requests for a given IPUMS collection, starting from the most
recent extract request.

To check the status of a particular extract request, use
[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md).

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
get_extract_history(
  collection = NULL,
  how_many = 10,
  delay = 0,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
```

## Arguments

- collection:

  Character string of the IPUMS collection for which to retrieve extract
  history. Defaults to the current default collection, if it exists. See
  [`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/reference/set_ipums_default_collection.md).

  For a list of codes used to refer to each collection, see
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/reference/ipums_data_collections.md).

- how_many:

  The number of extract requests for which to retrieve information.
  Defaults to the 10 most recent extracts.

- delay:

  Number of seconds to delay between successive API requests, if
  multiple requests are needed to retrieve all records.

  A delay is highly unlikely to be necessary and is intended only as a
  fallback in the event that you cannot retrieve your extract history
  without exceeding the API rate limit.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

A list of
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
objects

## See also

[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
to get the current status of a specific extract request.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get information for most recent extract requests.
# By default gets the most recent 10 extracts
get_extract_history("usa")

# Return only the most recent 3 extract definitions
get_extract_history("cps", how_many = 3)

# To get the most recent extract (for instance, if you have forgotten its
# extract number), use `get_last_extract_info()`
get_last_extract_info("nhgis")
} # }

# To browse your extract history by particular criteria, you can
# loop through the extract objects. We'll create a sample list of 2 extracts:
extract1 <- define_extract_micro(
  collection = "usa",
  description = "2013 ACS",
  samples = "us2013a",
  variables = var_spec(
    "SEX",
    case_selections = "2",
    data_quality_flags = TRUE
  )
)

extract2 <- define_extract_micro(
  collection = "usa",
  description = "2014 ACS",
  samples = "us2014a",
  variables = list(
    var_spec("RACE"),
    var_spec(
      "SEX",
      case_selections = "1",
      data_quality_flags = FALSE
    )
  )
)

extracts <- list(extract1, extract2)

# `purrr::keep()`` is particularly useful for filtering:
purrr::keep(extracts, ~ "RACE" %in% names(.x$variables))
#> [[1]]
#> Unsubmitted IPUMS USA extract 
#> Description: 2014 ACS
#> 
#> Samples: (1 total) us2014a
#> Variables: (2 total) RACE, SEX
#> 

purrr::keep(extracts, ~ grepl("2014 ACS", .x$description))
#> [[1]]
#> Unsubmitted IPUMS USA extract 
#> Description: 2014 ACS
#> 
#> Samples: (1 total) us2014a
#> Variables: (2 total) RACE, SEX
#> 

# You can also filter on variable-specific criteria
purrr::keep(extracts, ~ isTRUE(.x$variables[["SEX"]]$data_quality_flags))
#> [[1]]
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS
#> 
#> Samples: (1 total) us2013a
#> Variables: (1 total) SEX
#> 

# To filter based on all variables in an extract, you'll need to
# create a nested loop. For instance, to find all extracts that have
# any variables with data_quality_flags:
purrr::keep(
  extracts,
  function(extract) {
    any(purrr::map_lgl(
      names(extract$variables),
      function(var) isTRUE(extract$variables[[var]]$data_quality_flags)
    ))
  }
)
#> [[1]]
#> Unsubmitted IPUMS USA extract 
#> Description: 2013 ACS
#> 
#> Samples: (1 total) us2013a
#> Variables: (1 total) SEX
#> 

# To peruse your extract history without filtering, `purrr::map()` is more
# useful
purrr::map(extracts, ~ names(.x$variables))
#> [[1]]
#> [1] "SEX"
#> 
#> [[2]]
#> [1] "RACE" "SEX" 
#> 

purrr::map(extracts, ~ names(.x$samples))
#> [[1]]
#> [1] "us2013a"
#> 
#> [[2]]
#> [1] "us2014a"
#> 

purrr::map(extracts, ~ .x$variables[["RACE"]]$case_selections)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 

# Once you have identified a past extract, you can easily download or
# resubmit it
if (FALSE) { # \dontrun{
extracts <- get_extract_history("nhgis")

extract <- purrr::keep(
  extracts,
  ~ "CW3" %in% names(.x$time_series_tables)
)

download_extract(extract[[1]])
} # }
```

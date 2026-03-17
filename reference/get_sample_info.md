# List available samples for IPUMS microdata collections

Retrieve sample IDs and descriptions for IPUMS microdata collections.

Currently supported microdata collections are:

- IPUMS USA (`"usa"`)

- IPUMS CPS (`"cps"`)

- IPUMS International (`"ipumsi"`)

- IPUMS Time Use (`"atus"`, `"ahtus"`, `"mtus"`)

- IPUMS Health Surveys (`"nhis"`, `"meps"`)

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
get_sample_info(
  collection = NULL,
  delay = 0,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
```

## Arguments

- collection:

  Character string indicating the IPUMS microdata collection for which
  to retrieve sample information.

- delay:

  Number of seconds to delay between successive API requests, if
  multiple requests are needed to retrieve all records.

  A delay is highly unlikely to be necessary and is intended only as a
  fallback in the event that you cannot retrieve all metadata records
  without exceeding the API rate limit.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing sample IDs and descriptions for the indicated collection.

## See also

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
to create an IPUMS microdata extract definition.

## Examples

``` r
if (FALSE) { # \dontrun{
get_sample_info("usa")
get_sample_info("cps")
get_sample_info("ipumsi")
get_sample_info("atus")
get_sample_info("meps")
get_sample_info("dhs")
} # }
```

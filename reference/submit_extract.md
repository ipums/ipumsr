# Submit an extract request via the IPUMS API

Submit an extract request via the IPUMS API and return an
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
object containing the extract definition with a newly-assigned extract
request number.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
submit_extract(extract, api_key = Sys.getenv("IPUMS_API_KEY"))
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

An
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
object containing the extract definition and newly-assigned extract
number of the submitted extract.

Note that some unspecified extract fields may be populated with default
values and therefore change slightly upon submission.

## See also

[`wait_for_extract()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
to wait for an extract to finish processing.

[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
and
[`is_extract_ready()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
to check the status of an extract request.

[`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
to download an extract's data files.

## Examples

``` r
my_extract <- define_extract_micro(
  collection = "cps",
  description = "2018-2019 CPS Data",
  samples = c("cps2018_05s", "cps2019_05s"),
  variables = c("SEX", "AGE", "YEAR")
)

if (FALSE) { # \dontrun{
# Store your submitted extract request to obtain the extract number
submitted_extract <- submit_extract(my_extract)

submitted_extract$number

# This is useful for checking the extract request status
get_extract_info(submitted_extract)

# You can always get the latest status, even if you forget to store the
# submitted extract request object
submitted_extract <- get_last_extract_info("cps")

# You can also check if submitted extract is ready
is_extract_ready(submitted_extract)

# Or have R check periodically and download when ready
downloadable_extract <- wait_for_extract(submitted_extract)
} # }
```

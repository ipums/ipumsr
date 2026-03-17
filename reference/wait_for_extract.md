# Wait for an extract request to finish processing

Wait for an extract request to finish by periodically checking its
status via the IPUMS API until it is complete.

`is_extract_ready()` is a convenience function to check if an extract is
ready to download without committing your R session to waiting for
extract completion.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
wait_for_extract(
  extract,
  initial_delay_seconds = 0,
  max_delay_seconds = 300,
  timeout_seconds = 10800,
  verbose = TRUE,
  api_key = Sys.getenv("IPUMS_API_KEY")
)

is_extract_ready(extract, api_key = Sys.getenv("IPUMS_API_KEY"))
```

## Arguments

- extract:

  One of:

  - An
    [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
    object

  - The data collection and extract number formatted as a string of the
    form `"collection:number"` or as a vector of the form
    `c("collection", number)`

  - An extract number to be associated with your default IPUMS
    collection. See
    [`set_ipums_default_collection()`](https://tech.popdata.org/ipumsr/reference/set_ipums_default_collection.md)

  For a list of codes used to refer to each collection, see
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/reference/ipums_data_collections.md).

- initial_delay_seconds:

  Seconds to wait before first status check. The wait time will
  automatically increase by 10 seconds between each successive check.

- max_delay_seconds:

  Maximum interval to wait between status checks. When the wait interval
  reaches this value, checks will continue to occur at
  `max_delay_seconds` intervals until the extract is complete or
  `timeout_seconds` is reached. Defaults to 300 seconds (5 minutes).

- timeout_seconds:

  Maximum total number of seconds to continue waiting for the extract
  before throwing an error. Defaults to 10,800 seconds (3 hours).

- verbose:

  If `TRUE`, print status updates to the R console at the beginning of
  each wait interval and upon extract completion. Defaults to `TRUE`.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

For `wait_for_extract()`, an
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
object containing the extract definition and the URLs from which to
download extract files.

For `is_extract_ready()`, a logical value indicating whether the extract
is ready to download.

## Details

The `status` of a submitted extract will be one of `"queued"`,
`"started"`, `"produced"`, `"canceled"`, `"failed"`, or `"completed"`.

To be ready to download, an extract must have a `"completed"` status.
However, some requests that are `"completed"` may still be unavailable
for download, as extracts expire and are removed from IPUMS servers
after a set period of time (72 hours for microdata collections, 2 weeks
for IPUMS NHGIS).

Therefore, these functions also check the `download_links` field of the
extract request to determine if data are available for download. If an
extract has expired (that is, it has completed but its download links
are no longer available), these functions will warn that the extract
request must be resubmitted.

## See also

[`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md)
to download an extract's data files.

[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
to obtain the definition of a submitted extract request.

## Examples

``` r
my_extract <- define_extract_micro(
  collection = "ipumsi",
  description = "Botswana data",
  samples = c("bw2001a", "bw2011a"),
  variables = c("SEX", "AGE", "YEAR")
)

if (FALSE) { # \dontrun{
submitted_extract <- submit_extract(my_extract)

# Wait for a particular extract request to complete by providing its
# associated `ipums_extract` object:
downloadable_extract <- wait_for_extract(submitted_extract)

# Or by specifying the collection and number for the extract request:
downloadable_extract <- wait_for_extract("ipumsi:1")

# If you have a default collection, you can use the extract number alone:
set_ipums_default_collection("ipumsi")

downloadable_extract <- wait_for_extract(1)

# Use `download_extract()` to download the completed extract:
files <- download_extract(downloadable_extract)

# Use `is_extract_ready()` if you don't want to tie up your R session by
# waiting for completion
is_extract_ready("usa:1")
} # }
```

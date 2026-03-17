# `ipums_extract` class

The `ipums_extract` class provides a data structure for storing the
extract definition and status of an IPUMS data extract request. Both
submitted and unsubmitted extract requests are stored in `ipums_extract`
objects.

`ipums_extract` objects are further divided into microdata and aggregate
data classes, and will also include a collection-specific extract
subclass to accommodate differences in extract options and content
across collections.

Currently supported collections are:

- IPUMS microdata

  - [IPUMS USA](https://usa.ipums.org/)

  - [IPUMS CPS](https://cps.ipums.org/)

  - [IPUMS International](https://international.ipums.org/)

  - IPUMS Time Use ([ATUS](https://www.atusdata.org/atus/),
    [AHTUS](https://www.ahtusdata.org/ahtus/),
    [MTUS](https://www.mtusdata.org/mtus/))

  - IPUMS Health Surveys ([NHIS](https://nhis.ipums.org/),
    [MEPS](https://meps.ipums.org/))

- IPUMS aggregate data

  - [IPUMS NHGIS](https://www.nhgis.org/)

  - [IPUMS IHGIS](https://ihgis.ipums.org/)

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Properties

Objects of class `ipums_extract` have:

- A `class` attribute of the form
  `c("{collection}_extract", "{collection_type}_extract", "ipums_extract")`.
  For instance, `c("cps_extract", "micro_extract", "ipums_extract")`.

- A base type of `"list"`.

- A `names` attribute that is a character vector the same length as the
  underlying list.

All `ipums_extract` objects will include several core fields identifying
the extract and its status:

- `collection`: the collection for the extract request.

- `description`: the description of the extract request.

- `submitted`: logical indicating whether the extract request has been
  submitted to the IPUMS API for processing.

- `download_links`: links to the downloadable data, if the extract
  request was completed at the time it was last checked.

- `number`: the number of the extract request. With `collection`, this
  uniquely identifies an extract request for a given user.

- `status`: status of the extract request at the time it was last
  checked. One of `"unsubmitted"`, `"queued"`, `"started"`,
  `"produced"`, `"canceled"`, `"failed"`, or `"completed"`.

## Creating or obtaining an extract

- Create an `ipums_extract` object from scratch with the appropriate
  `define_extract_*()` function.

  - For microdata extracts, use
    [`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)

  - For aggregate data extracts, use
    [`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)

- Use
  [`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
  to get the definition and latest status of a previously-submitted
  extract request.

- Use
  [`get_extract_history()`](https://tech.popdata.org/ipumsr/reference/get_extract_history.md)
  to get the definitions and latest status of multiple
  previously-submitted extract requests.

## Submitting an extract

- Use
  [`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
  to submit an extract request for processing through the IPUMS API.

- Use
  [`wait_for_extract()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
  to periodically check the status of a submitted extract request until
  it is ready to download.

- Use
  [`is_extract_ready()`](https://tech.popdata.org/ipumsr/reference/wait_for_extract.md)
  to manually check whether a submitted extract request is ready to
  download.

## Downloading an extract

- Download the data contained in a completed extract with
  [`download_extract()`](https://tech.popdata.org/ipumsr/reference/download_extract.md).

## Saving an extract

- Save an extract to a JSON-formatted file with
  [`save_extract_as_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md).

- Create an `ipums_extract` object from a saved JSON-formatted
  definition with
  [`define_extract_from_json()`](https://tech.popdata.org/ipumsr/reference/save_extract_as_json.md).

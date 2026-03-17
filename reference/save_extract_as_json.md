# Store an extract definition in JSON format

Write an
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
object to a JSON file, or read an extract definition from such a file.

Use these functions to store a copy of an extract definition outside of
your R environment and/or share an extract definition with another
registered IPUMS user.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
save_extract_as_json(extract, file, overwrite = FALSE)

define_extract_from_json(extract_json)
```

## Arguments

- extract:

  An
  [`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
  object.

- file:

  File path to which to write the JSON-formatted extract definition.

- overwrite:

  If `TRUE`, overwrite `file` if it already exists. Defaults to `FALSE`.

- extract_json:

  Path to a file containing a JSON-formatted extract definition.

## Value

An
[`ipums_extract`](https://tech.popdata.org/ipumsr/reference/ipums_extract-class.md)
object.

## API Version Compatibility

As of v0.6.0, ipumsr only supports IPUMS API version 2. If you have
stored an extract definition made using version beta or version 1 of the
IPUMS API, you will not be able to load it using
`define_extract_from_json()`. The API version for the request should be
stored in the saved JSON file. (If there is no `"api_version"` or
`"version"` field in the JSON file, the request was likely made under
version beta or version 1.)

If the extract definition was originally made under your user account
and you know its corresponding extract number, use
[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
to obtain a definition compliant with IPUMS API version 2. You can then
save this definition to JSON with `save_extract_as_json()`.

Otherwise, you will need to update the JSON file to be compliant with
IPUMS API version 2. In general, this should only require renaming all
JSON fields written in `snake_case` to `camelCase`. For instance,
`"data_tables"` would become `"dataTables"`, `"data_format"` would
become `"dataFormat"`, and so on. You will also need to change the
`"api_version"` field to `"version"` and set it equal to `2`. If you are
unable to create a valid extract by modifying the file, you may have to
recreate the definition manually using the
[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
or
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md).

See the IPUMS developer documentation for more details on [API
versioning](https://developer.ipums.org/docs/apiprogram/versioning/) and
[breaking
changes](https://developer.ipums.org/docs/apiprogram/changelog/)
introduced in version 2.

## See also

[`define_extract_micro()`](https://tech.popdata.org/ipumsr/reference/define_extract_micro.md)
or
[`define_extract_agg()`](https://tech.popdata.org/ipumsr/reference/define_extract_agg.md)
to define an extract request manually

[`get_extract_info()`](https://tech.popdata.org/ipumsr/reference/get_extract_info.md)
to obtain a past extract to save.

[`submit_extract()`](https://tech.popdata.org/ipumsr/reference/submit_extract.md)
to submit an extract request for processing.

[`add_to_extract()`](https://tech.popdata.org/ipumsr/reference/add_to_extract.md)
and
[`remove_from_extract()`](https://tech.popdata.org/ipumsr/reference/remove_from_extract.md)
to revise an extract definition.

## Examples

``` r
my_extract <- define_extract_micro(
  collection = "usa",
  description = "2013-2014 ACS Data",
  samples = c("us2013a", "us2014a"),
  variables = c("SEX", "AGE", "YEAR")
)

extract_json_path <- file.path(tempdir(), "usa_extract.json")
save_extract_as_json(my_extract, file = extract_json_path)

copy_of_my_extract <- define_extract_from_json(extract_json_path)

identical(my_extract, copy_of_my_extract)
#> [1] TRUE

file.remove(extract_json_path)
#> [1] TRUE
```

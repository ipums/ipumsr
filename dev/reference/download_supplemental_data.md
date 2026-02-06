# Download IPUMS supplemental data files

Some IPUMS collections provide supplemental data files that are
available outside of the IPUMS extract system. Use this function to
download these files.

Currently, only IPUMS NHGIS files are supported.

In general, files found on an IPUMS project website that include
`secure-assets` in their URL are available as supplemental data. See the
[IPUMS developer
documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/nhgis/)
for more information on available endpoints.

## Usage

``` r
download_supplemental_data(
  collection,
  path,
  download_dir = getwd(),
  overwrite = FALSE,
  progress = TRUE,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
```

## Arguments

- collection:

  Code for the IPUMS collection represented by this extract request.
  Currently, only `"nhgis"` is supported.

- path:

  Path to the supplemental data file to download. See examples.

- download_dir:

  Path to the directory where the files should be written. Defaults to
  current working directory.

- overwrite:

  If `TRUE`, overwrite files with the same name that already exist in
  `download_dir`. Defaults to `FALSE`.

- progress:

  If `TRUE`, output progress bar showing the status of the download
  request. Defaults to `TRUE`.

- api_key:

  API key associated with your user account. Defaults to the value of
  the `IPUMS_API_KEY` environment variable. See
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/dev/reference/set_ipums_api_key.md).

## Value

The path to the downloaded supplemental data file

## Examples

``` r
if (FALSE) { # \dontrun{
# Download a state-level tract to county crosswalk from NHGIS
file <- download_supplemental_data(
  "nhgis",
  "crosswalks/nhgis_tr1990_co2010_state/nhgis_tr1990_co2010_10.zip"
)

read_ipums_agg(file)

# Download 1980 Minnesota block boundary file
file <- download_supplemental_data(
  "nhgis",
  "blocks-1980/MN_block_1980.zip"
)

read_ipums_sf(file)
} # }
```

# Download a completed IPUMS data extract

Download IPUMS data extract files via the IPUMS API and save them on
your computer.

Learn more about the IPUMS API in
[`vignette("ipums-api")`](https://tech.popdata.org/ipumsr/articles/ipums-api.md).

## Usage

``` r
download_extract(
  extract,
  download_dir = getwd(),
  overwrite = FALSE,
  progress = TRUE,
  api_key = Sys.getenv("IPUMS_API_KEY")
)
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
  [`set_ipums_api_key()`](https://tech.popdata.org/ipumsr/reference/set_ipums_api_key.md).

## Value

The path(s) to the files required to read the data requested in the
extract, invisibly.

For NHGIS, paths will be named with either `"data"` (for tabular data
files) or `"shape"` (for spatial data files) to indicate the type of
data the file contains.

## Details

For NHGIS extracts, data files and GIS files (shapefiles) will be saved
in separate .zip archives. `download_extract()` will return a character
vector including the file paths to all downloaded files.

For microdata extracts, only the file path to the downloaded .xml DDI
file will be returned, as it is sufficient for reading the data provided
in the associated .dat.gz data file.

## See also

[`read_ipums_micro()`](https://tech.popdata.org/ipumsr/reference/read_ipums_micro.md)
or
[`read_ipums_agg()`](https://tech.popdata.org/ipumsr/reference/read_ipums_agg.md)
to read tabular data from an IPUMS extract.

[`read_ipums_sf()`](https://tech.popdata.org/ipumsr/reference/read_ipums_sf.md)
to read spatial data from an IPUMS extract.

[`ipums_list_files()`](https://tech.popdata.org/ipumsr/reference/ipums_list_files.md)
to list files in an IPUMS extract.

## Examples

``` r
usa_extract <- define_extract_micro(
  collection = "usa",
  description = "2013-2014 ACS Data",
  samples = c("us2013a", "us2014a"),
  variables = c("SEX", "AGE", "YEAR")
)

if (FALSE) { # \dontrun{
submitted_extract <- submit_extract(usa_extract)

downloadable_extract <- wait_for_extract(submitted_extract)

# For microdata, the path to the DDI .xml codebook file is provided.
usa_xml_file <- download_extract(downloadable_extract)

# Load with a `read_ipums_micro_*()` function
usa_data <- read_ipums_micro(usa_xml_file)

# You can also download previous extracts with their collection and number:
nhgis_files <- download_extract("nhgis:1")

# NHGIS extracts return a path to both the tabular and spatial data files,
# as applicable.
nhgis_data <- read_ipums_agg(data = nhgis_files["data"])

# Load NHGIS spatial data
nhgis_geog <- read_ipums_sf(data = nhgis_files["shape"])
} # }
```

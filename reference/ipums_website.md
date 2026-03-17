# Launch a browser window to an IPUMS metadata page

Launch the documentation webpage for a given IPUMS project and variable.
The project can be provided in the form of an
[`ipums_ddi`](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
object or can be manually specified.

This provides access to more extensive variable metadata than may be
contained within an `ipums_ddi` object itself.

Note that some IPUMS projects (e.g. IPUMS NHGIS) do not have
variable-specific pages. In these cases, `ipums_website()` will launch
the project's main data selection page.

## Usage

``` r
ipums_website(
  x,
  var = NULL,
  launch = TRUE,
  verbose = TRUE,
  homepage_if_missing = FALSE
)
```

## Arguments

- x:

  An
  [`ipums_ddi`](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
  object or the name of an IPUMS project. See
  [`ipums_data_collections()`](https://tech.popdata.org/ipumsr/reference/ipums_data_collections.md)
  for supported projects.

- var:

  Name of the variable to load. If `NULL`, provides the URL to the
  project's main data selection site.

- launch:

  If `TRUE`, launch a browser window to the metadata webpage. Otherwise,
  return the URL for the webpage.

- verbose:

  If `TRUE`, produce warnings when invalid URL specifications are
  detected.

- homepage_if_missing:

  If `TRUE`, return the IPUMS homepage if the IPUMS project in `x` is
  not recognized.

## Value

The URL to the IPUMS webpage for the indicated project and variable
(invisibly if `launch = TRUE`)

## Details

If `launch = TRUE`, you will need a valid registration for the specified
project to successfully launch the webpage.

Not all IPUMS variables are found at webpages that exactly match the
variable names that are included in completed extract files (and
`ipums_ddi` objects). Therefore, there may be some projects and
variables for which `ipums_website()` will launch the page for a
different variable or an invalid page.

## Examples

``` r
ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))

if (FALSE) { # \dontrun{
# Launch webpage for particular variable
ipums_website(ddi, "MONTH")
} # }

# Can also specify an IPUMS project instead of an `ipums_ddi` object
ipums_website("IPUMS CPS", var = "RECTYPE", launch = FALSE)
#> [1] "https://cps.ipums.org/cps-action/variables/RECTYPE"

# Shorthand project names from `ipums_data_collections()` are also accepted:
ipums_website("ipumsi", var = "YEAR", launch = FALSE)
#> [1] "https://international.ipums.org/international-action/variables/YEAR"
```

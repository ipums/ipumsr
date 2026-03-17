# View a static webpage with variable metadata from an IPUMS extract

For a given
[`ipums_ddi`](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
object or data frame, display metadata about its contents in the RStudio
viewer pane. This includes extract-level information as well as metadata
for the variables included in the input object.

It is also possible to save the output to an external HTML file without
launching the RStudio viewer.

## Usage

``` r
ipums_view(x, out_file = NULL, launch = TRUE)
```

## Arguments

- x:

  An `ipums_ddi` object or a data frame with IPUMS attributes attached.

  Note that file-level information (e.g. extract notes) is only
  available when `x` is an `ipums_ddi` object.

- out_file:

  Optional location to save the output HTML file. If `NULL`, makes a
  temporary file.

- launch:

  Logical indicating whether to launch the HTML file in the RStudio
  viewer pane. If `TRUE`, RStudio and rstudioapi must be available.

## Value

The file path to the output HTML file (invisibly, if `launch = TRUE`)

## Details

`ipums_view()` requires that the htmltools, shiny, and DT packages are
installed. If `launch = TRUE`, RStudio and the rstudioapi package must
also be available.

Note that if `launch = FALSE` and `out_file` is unspecified, the output
file will be written to a temporary directory. Some operating systems
may be unable to open the HTML file from the temporary directory; we
suggest that you manually specify the `out_file` location in this case.

## Examples

``` r
ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))

if (FALSE) { # \dontrun{
ipums_view(ddi)
ipums_view(ddi, "codebook.html", launch = FALSE)
} # }
```

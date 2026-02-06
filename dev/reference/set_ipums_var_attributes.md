# Add IPUMS variable attributes to a data frame

Add variable attributes from an
[ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
object to a data frame. These provide contextual information about the
variables and values contained in the data columns.

Most ipumsr data-reading functions automatically add these attributes.
However, some data processing operations may remove attributes, or you
may wish to store data in an external database that does not support
these attributes. In these cases, use this function to manually attach
this information.

## Usage

``` r
set_ipums_var_attributes(
  data,
  var_info,
  var_attrs = c("val_labels", "var_label", "var_desc")
)
```

## Arguments

- data:

  [`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
  or data frame

- var_info:

  An
  [ipums_ddi](https://tech.popdata.org/ipumsr/dev/reference/ipums_ddi-class.md)
  object or a data frame containing variable information. Variable
  information can be obtained by calling
  [`ipums_var_info()`](https://tech.popdata.org/ipumsr/dev/reference/ipums_var_info.md)
  on an `ipums_ddi` object.

- var_attrs:

  Variable attributes from the DDI to add to the columns of the output
  data. Defaults to all available attributes.

## Value

`data`, with variable attributes attached

## Details

Attribute `val_labels` adds the
[`haven_labelled`](https://haven.tidyverse.org/reference/labelled.html)
class and the corresponding value labels for applicable variables. For
more about the `haven_labelled` class, see
[`vignette("semantics", package = "haven")`](https://haven.tidyverse.org/articles/semantics.html).

Attribute `var_label` adds a short summary of the variable's contents to
the `"label"` attribute. This label is viewable in the RStudio Viewer.

Attribute `var_desc` adds a longer description of the variable's
contents to the `"var_desc"` attribute, when available.

Variable information is attached to the data by column name. If column
names in `data` do not match those found in `var_info`, attributes will
not be added.

## Examples

``` r
ddi_file <- ipums_example("cps_00157.xml")

# Load metadata into `ipums_ddi` object
ddi <- read_ipums_ddi(ddi_file)

# Load data
cps <- read_ipums_micro(ddi)
#> Use of data from IPUMS CPS is subject to conditions including that users should cite the data appropriately. Use command `ipums_conditions()` for more details.

# Data includes variable metadata:
ipums_var_desc(cps$INCTOT)
#> [1] "INCTOT indicates each respondent's total pre-tax personal income or losses from all sources for the previous calendar year.  Amounts are expressed as they were reported to the interviewer; users must adjust for inflation using Consumer Price Index adjustment factors."

# Some operations remove attributes, even if they do not alter the data:
cps$INCTOT <- ifelse(TRUE, cps$INCTOT, NA)
ipums_var_desc(cps$INCTOT)
#> [1] NA

# We can reattach metadata from the separate `ipums_ddi` object:
cps <- set_ipums_var_attributes(cps, ddi)
ipums_var_desc(cps$INCTOT)
#> [1] "INCTOT indicates each respondent's total pre-tax personal income or losses from all sources for the previous calendar year.  Amounts are expressed as they were reported to the interviewer; users must adjust for inflation using Consumer Price Index adjustment factors."
```

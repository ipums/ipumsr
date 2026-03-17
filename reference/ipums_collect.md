# Collect data into R session with IPUMS attributes

Convenience wrapper around dplyr's
[`collect()`](https://dplyr.tidyverse.org/reference/compute.html) and
[`set_ipums_var_attributes()`](https://tech.popdata.org/ipumsr/reference/set_ipums_var_attributes.md).
Use this to attach variable labels when collecting data from a database.

## Usage

``` r
ipums_collect(data, ddi, var_attrs = c("val_labels", "var_label", "var_desc"))
```

## Arguments

- data:

  A dplyr `tbl` object (generally a `tbl_lazy` object stored in a
  database).

- ddi:

  An
  [ipums_ddi](https://tech.popdata.org/ipumsr/reference/ipums_ddi-class.md)
  object created with
  [`read_ipums_ddi()`](https://tech.popdata.org/ipumsr/reference/read_ipums_ddi.md).

- var_attrs:

  Variable attributes to add to the output. Defaults to all available
  attributes. See
  [`set_ipums_var_attributes()`](https://tech.popdata.org/ipumsr/reference/set_ipums_var_attributes.md)
  for more details.

## Value

A local
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with the requested attributes attached.

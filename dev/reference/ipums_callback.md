# Callback classes

These classes are used to define callback behaviors for use with
[`read_ipums_micro_chunked()`](https://tech.popdata.org/ipumsr/dev/reference/read_ipums_micro_chunked.md).
They are based on the
[`callback`](https://readr.tidyverse.org/reference/callback.html)
classes from readr, but have been adapted to include handling of
implicit decimal values and variable/value labeling for use with IPUMS
microdata extracts.

## Details

- IpumsSideEffectCallback:

  Callback function that is used only for side effects, no results are
  returned.

  Initialize with a function that takes 2 arguments. The first argument
  (`x`) should correspond to the data chunk and the second (`pos`)
  should correspond to the position of the first observation in the
  chunk.

  If the function returns `FALSE`, no more chunks will be read.

- IpumsDataFrameCallback:

  Callback function that combines the results from each chunk into a
  single output `data.frame` (or similar) object.

  Initialize the same way as you would `IpumsSideEffectCallback`. The
  provided function should return an object that inherits from
  `data.frame`.

  The results from each application of the callback function will be
  added to the output `data.frame`.

- IpumsListCallback:

  Callback function that returns a list, where each element contains the
  result from a single chunk.

  Initialize the same was as you would `IpumsSideEffectCallback`.

- IpumsBiglmCallback:

  Callback function that performs a linear regression on a dataset by
  chunks using the biglm package.

  Initialize with a function that takes 2 arguments: The first argument
  should correspond to a [formula](https://rdrr.io/r/stats/formula.html)
  specifying the regression model. The second should correspond to a
  function that prepares the data before running the regression
  analysis. This function follows the conventions of the functions used
  in other callbacks. Any additional arguments passed to this function
  are passed to biglm.

- IpumsChunkCallback:

  (Advanced) Callback interface definition. All callback functions for
  IPUMS data should inherit from this class, and should use private
  method `ipumsify` on the data to handle implicit decimals and value
  labels.

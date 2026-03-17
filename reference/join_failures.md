# Report on observations dropped during a join

Helper to display observations that were not matched when joining
tabular and spatial data.

## Usage

``` r
join_failures(join_results)
```

## Arguments

- join_results:

  A data frame that has just been created by an [ipums shape
  join](https://tech.popdata.org/ipumsr/reference/ipums_shape_join.md).

## Value

A list of data frames, where the first element (`shape`) includes the
observations dropped from the shapefile and the second (`data`) includes
the observations dropped from the data file.

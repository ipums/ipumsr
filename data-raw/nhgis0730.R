# Generate sample fixed-width data for testing
#
# This script produces:
#   * inst/extdata/nhgis0730_fixed.zip
#
# This provides two fixed-width files for testing. One file requires
# data recoding (i.e. some variables in the file have implied decimal places)
# and another does not. Both provide information on column positions.
#
# It also includes a TST which can be used for testing on TST data and
# codebook reading.
#
# Note that if this code is re-run to produce new files, the files will be
# updated with new extract numbers. Code that references these files will
# need to be updated accordingly.

define_extract_nhgis(
  description = "Data for fixed-width testing",
  datasets = ds_spec(
    "2014_2018_ACS5a",
    data_tables = c("B01001", "B01002"),
    geog_levels = "nation"
  ),
  time_series_tables = tst_spec("A00", geog_levels = "state"),
  data_format = "fixed_width"
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

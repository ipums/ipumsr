# Generate sample csv data for comparison to fixed-width data
#
# This script produces:
#   * inst/extdata/nhgis0731_csv.zip
#
# This provides two csv files for the same data as are contained in
# nhgis0730_fixed.zip, which can be used for comparing data loading results
# across formats.
#
# Note that if this code is re-run to produce new files, the files will be
# updated with new extract numbers. Code that references these files will
# need to be updated accordingly.

define_extract_agg_data(
  "nhgis",
  description = "Data for fixed-width testing",
  datasets = ds_spec(
    "2014_2018_ACS5a",
    data_tables = c("B01001", "B01002"),
    geog_levels = "nation"
  ),
  time_series_tables = tst_spec("A00", "state"),
  data_format = "csv_no_header"
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

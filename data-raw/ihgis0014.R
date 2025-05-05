# Generate sample IHGIS data for examples, tests, and vignettes.
#
# This script produces:
#   * inst/extdata/ihgis0014.zip
#
# If data files are updated, file name references should be changed throughout
# the package. File names must be consistent with the contents of the .xml
# files, so renaming files for consistency is not advised.

define_extract_agg(
  collection = "ihgis",
  description = "Reproducing ihgis0014",
  datasets = ds_spec(
    "KZ2009pop",
    data_tables = c("KZ2009pop.AAA", "KZ2009pop.AAB"),
    tabulation_geographies = c("KZ2009pop.g0", "KZ2009pop.g1")
  )
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

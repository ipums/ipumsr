
# Generate sample fixed-width data for testing
#
# This script produces:
#   * inst/extdata/nhgis0722_fixed.zip
#
# Which (other than data format) includes the same extract specs as:
#   * inst/extdata/nhgis0707_csv.zip
#
# Note that if this code is re-run to produce new files, the files will be
# updated with new extract numbers. Code that references these files will
# need to be updated accordingly.

# NHGIS 1990 PMSA Data ------------

define_extract_nhgis(
  description = "Reproducing nhgis0707 example data in fixed-width format",
  datasets = "1990_SSTF09",
  data_tables = "NH004",
  geog_levels = "pmsa",
  data_format = "fixed_width"
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

# Make simplified versions of GIS files from NHGIS extracts for use in
# package examples and tests.
#
# NHGIS GIS files are typically too large to be included on GitHub or on CRAN,
# but these files are useful for examples and testing. We simplify the GIS
# files and repackage them to reduce file size while maintaining the same
# file structure.
#
# This pipeline has been updated to improve reproducibility of example data.
#
# This script produces:
#   * ipumsexamples/inst/extdata/nhgis0706_shape.zip
#   * ipumsexamples/inst/extdata/nhgis0706_csv.zip
#
# Which are updated versions of:
#   * ipumsexamples/inst/extdata/nhgis0024_shape.zip
#   * ipumsexamples/inst/extdata/nhgis0024_csv.zip
#
# The extract specifications are identical between these versions, but
# NHGIS has since added new content to the data files for many NHGIS datasets,
# so the files may not be identical.
#
# The original versions were made using the extract GUI.
#
# Note that if this code is re-run to produce new files, the files will be
# updated with new extract numbers. Code that references these files will
# need to be updated accordingly.

# NHGIS 2010 Block Data ------------

# Used in ipumsexamples example data

files <- define_extract_nhgis(
  description = "Reproducing nhgis0024 example data",
  datasets = "2010_SF1a",
  data_tables = "P13",
  geog_levels = "block",
  geographic_extents = c("090", "440"),
  shapefiles = c(
    "090_block_2010_tl2010",
    "440_block_2010_tl2010"
  )
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("ipumsexamples/inst/extdata/")

simplify_nhgis_gis_file(files[2])

invisible(
  file.rename(
    files[2],
    fostr_replace(files[2], "_shape.zip", "_shape_small.zip")
  )
)

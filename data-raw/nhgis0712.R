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
#   * inst/extdata/nhgis0712_shape.zip
#   * inst/extdata/nhgis0712_csv.zip
#
# Note that if this code is re-run to produce new files, the files will be
# updated with new extract numbers. Code that references these files will
# need to be updated accordingly.

# NHGIS PMSA data. Multiple files in both data and shapefiles ------------

files <- define_extract_nhgis(
  description = "New ipumsr example data: multiple datasets and shapefiles",
  datasets = list(
    ds_spec("1990_SSTF09", "NP001", "pmsa"),
    ds_spec("1990_SSTF10", "NP001", "pmsa")
  ),
  shapefiles = c(
    "us_pmsa_1990_tl2000",
    "us_pmsa_2000_tl2000",
    # Included for testing because POINT geometry will mismatch with PMSA data:
    "us_state_cenpop_2000_cenpop2000"
  )
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata/")

simplify_nhgis_gis_file(files[2])

invisible(
  file.rename(
    files[2],
    fostr_replace(files[2], "_shape.zip", "_shape_small.zip")
  )
)

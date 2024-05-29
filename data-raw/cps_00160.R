# Generate sample CPS data for examples, tests, and vignettes.
#
# This script produces:
#   * inst/extdata/cps_00160.xml
#   * inst/extdata/cps_00160.dat.gz
#
# This file contains additional variables for use in `lbl` function demos
#
# If data files are updated, file name references should be changed throughout
# the package. File names must be consistent with the contents of the .xml
# files, so renaming files for consistency is not advised.

define_extract_micro(
  collection = "cps",
  description = "Reproducing cps00015",
  samples = "cps2016_03s",
  variables = list(
    var_spec(
      "STATEFIP",
      case_selections = c("27", "19", "55", "46", "38")
    ),
    "INCTOT",
    "EDUC",
    "AGE",
    "HEALTH",
    "MIGRATE1"
  ),
  data_format = "fixed_width",
  data_structure = "rectangular",
  rectangular_on = "P",
  case_select_who = "individuals",
  data_quality_flags = FALSE
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

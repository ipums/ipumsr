# Generate sample CPS data for examples, tests, and vignettes.
#
# This script produces:
#   * inst/extdata/cps_00158.xml
#   * inst/extdata/cps_00158.csv.gz
#
# These files have the same contents as cps_00157.xml / cps_00157.dat.gz, but
# are in .csv format.
#
# If data files are updated, file name references should be changed throughout
# the package. File names must be consistent with the contents of the .xml
# files, so renaming files for consistency is not advised.

define_extract_micro(
  collection = "cps",
  description = "Reproducing cps00006 csv",
  samples = c("cps1962_03s", "cps1963_03s"),
  variables = list(
    var_spec(
      "STATEFIP",
      case_selections = c("27", "19", "55", "46", "38")
    ),
    "INCTOT"
  ),
  data_format = "csv",
  data_structure = "rectangular",
  rectangular_on = "P",
  case_select_who = "individuals",
  data_quality_flags = FALSE
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")

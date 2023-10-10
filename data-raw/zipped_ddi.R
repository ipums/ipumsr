# Create zip archive containing multiple DDI files.
#
# This is intended to enable testing of ability to read DDI files through
# zip archives.
#
# Previously, we manually zipped temporary files within the relevant
# unit tests that required zip archives.
#
# However, `zip()` behavior varies on different OS/platforms and occasionally
# will produce unexpected errors in certain environments. In particular,
# some checks on CRAN systems were failing with these tests implicated as
# potentially at fault.
#
# Alternatively, we now generate these archives as fixtures to avoid needing
# to create them on the fly during unit testing.

xml1 <- xml2::read_xml(ipums_example("cps_00157.xml"))
xml2 <- xml2::read_xml(ipums_example("cps_00159.xml"))

out_dir <- vcr::vcr_test_path("fixtures")

xml2::write_xml(xml1, "xml1.xml")
xml2::write_xml(xml2, "xml2.xml")

utils::zip(
  zipfile = file.path(out_dir, "zipped_ddi.zip"),
  files = c("xml1.xml", "xml2.xml"),
  flags = "-q"
)

file.remove("xml1.xml")
file.remove("xml2.xml")

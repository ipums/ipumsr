# Create directory containing multiple files of different extensions.
#
# This is intended to enable testing of ability to read files from a directory
# (i.e. the anticipated structure if a user manually unzips an extract,
# particularly from NHGIS).
#
# Previously, we manually created temporary files within the relevant
# unit tests that required sample files. However, this produces transient errors
# on different OS/platforms. In particular, some checks on CRAN systems were
# failing with these tests implicated as potentially at fault.
#
# Alternatively, we now generate these sample files as a stable test fixture to
# avoid needing to create them on the fly during unit testing.

out_dir <- file.path(vcr::vcr_test_path("fixtures"), "nhgis_unzipped")

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

csv_tmpfile1 <- file.path(out_dir, "test1.csv")
csv_tmpfile2 <- file.path(out_dir, "test2.csv")

readr::write_csv(tibble::tibble(a = c("a", "b")), csv_tmpfile1)
readr::write_csv(tibble::tibble(a = c("a", "b")), csv_tmpfile2)

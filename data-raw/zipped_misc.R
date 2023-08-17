# Create zip archive containing multiple files of different extensions.
#
# This is intended to enable testing of ability to read files through
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

out_dir <- vcr::vcr_test_path("fixtures")

file_names <- c("abc.txt", "test.csv", "test1.txt", "test2.txt")

purrr::walk(file_names, file.create)

utils::zip(
  zipfile = file.path(out_dir, "zipped_misc.zip"),
  files = file_names,
  flags = "-q"
)

purrr::walk(file_names, file.remove)

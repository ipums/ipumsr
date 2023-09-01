test_that("Reading through directory is deprecated", {
  test_path <- vcr::vcr_test_path("fixtures")

  lifecycle::expect_deprecated(
    x <- read_ipums_ddi(test_path, file_select = 1),
    "Reading files through a directory"
  )

  # Confirm that we can still read using `file_select` for now
  expect_s3_class(x, "ipums_ddi")
  expect_s3_class(
    suppressWarnings(
      read_nhgis(
        vcr::vcr_test_path("fixtures", "nhgis_unzipped"),
        file_select = 1
      )
    ),
    "tbl_df"
  )

  # Still should get correct errors
  expect_error(
    suppressWarnings(read_ipums_ddi(test_path)),
    "Multiple files found"
  )
  expect_error(
    suppressWarnings(read_nhgis(test_path)),
    "No .csv or .dat files found"
  )

  # Suppress file not found errors, which we expect but are not of interest
  # for these tests
  lifecycle::expect_deprecated(
    try(read_nhgis_codebook(test_path), silent = TRUE),
    "Reading files through a directory"
  )
  lifecycle::expect_deprecated(
    try(read_nhgis(test_path), silent = TRUE),
    "Reading files through a directory"
  )
  lifecycle::expect_deprecated(
    try(read_ipums_sf(test_path), silent = TRUE),
    "Reading files through a directory"
  )
})

test_that("Get correct file path when loading from directory", {
  test_path <- vcr::vcr_test_path("fixtures")

  lifecycle::expect_deprecated(
    x <- read_ipums_ddi(test_path, file_select = 1)
  )

  expect_equal(x$file_path, test_path)
})

test_that("Reading through directory is deprecated", {
  test_path <- vcr::vcr_test_path("fixtures")

  expect_s3_class(
    suppressWarnings(
      read_nhgis(
        vcr::vcr_test_path("fixtures", "nhgis_unzipped"),
        file_select = 1,
        verbose = FALSE
      )
    ),
    "tbl_df"
  )

  # Still should get correct errors
  expect_error(
    suppressWarnings(read_nhgis(test_path, verbose = FALSE)),
    "No .csv or .dat files found"
  )

  # Suppress file not found errors, which we expect but are not of interest
  # for these tests
  lifecycle::expect_deprecated(
    try(read_nhgis_codebook(test_path), silent = TRUE),
    "Reading files through a directory"
  )
  lifecycle::expect_deprecated(
    try(read_ipums_agg(test_path), silent = TRUE),
    "Reading files through a directory"
  )
  lifecycle::expect_deprecated(
    try(read_ipums_sf(test_path), silent = TRUE),
    "Reading files through a directory"
  )
})

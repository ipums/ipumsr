test_that("can read DDI with labeled string variable", {
  ddi <- read_ipums_ddi(
    file.path(vcr::vcr_test_path("fixtures"), "mtus_00002.xml")
  )
  sample_val_labels <- ddi$var_info[
    ddi$var_info$var_name == "SAMPLE",
    "val_labels",
    drop = TRUE
  ][[1]]
  expect_type(sample_val_labels$val, "character")
})

test_that("We ignore layer selection for direct .xml files", {
  ddi1 <- read_ipums_ddi(
    file.path(vcr::vcr_test_path("fixtures"), "mtus_00002.xml")
  )
  lifecycle::expect_deprecated(
    ddi2 <- read_ipums_ddi(
      file.path(vcr::vcr_test_path("fixtures"), "mtus_00002.xml"),
      file_select = 4
    ),
    "The `file_select` argument"
  )
  expect_identical(ddi1, ddi2)
})

test_that("Reading DDI through zip is deprecated", {
  zipped_ddi <- file.path(vcr::vcr_test_path("fixtures"), "zipped_ddi.zip")

  lifecycle::expect_deprecated(
    try(read_ipums_ddi(zipped_ddi), silent = TRUE),
    "Reading DDI files through a zip archive"
  )
})

test_that("Can read ddi from zip archives", {
  zipped_ddi <- file.path(vcr::vcr_test_path("fixtures"), "zipped_ddi.zip")
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_error(
    suppressWarnings(read_ipums_ddi(zipped_ddi)),
    "Multiple files found"
  )

  expect_error(
    read_ipums_ddi(zipped_ddi, file_select = contains("not-in-file")),
    "The provided `file_select` did not select any of the available files"
  )

  expect_identical(
    read_ipums_ddi(zipped_ddi, file_select = 1)$var_info,
    read_ipums_ddi(ipums_example("cps_00157.xml"))$var_info
  )
  expect_identical(
    read_ipums_ddi(zipped_ddi, file_select = 2)$var_info,
    read_ipums_ddi(ipums_example("cps_00159.xml"))$var_info
  )

  expect_identical(
    read_ipums_ddi(zipped_ddi, file_select = contains("xml1"))$var_info,
    read_ipums_ddi(ipums_example("cps_00157.xml"))$var_info
  )
  expect_identical(
    read_ipums_ddi(zipped_ddi, file_select = contains("xml2"))$var_info,
    read_ipums_ddi(ipums_example("cps_00159.xml"))$var_info
  )
})

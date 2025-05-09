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

test_that("Error on unexpected DDI file", {
  zipped_ddi <- file.path(vcr::vcr_test_path("fixtures"), "zipped_ddi.zip")

  expect_error(
    read_ipums_ddi(zipped_ddi),
    "Expected `ddi_file` to be the path to an .xml file."
  )
  expect_error(
    read_ipums_ddi(vcr::vcr_test_path("fixtures")),
    "Expected file `.+` to be a file path, but got a directory"
  )
})

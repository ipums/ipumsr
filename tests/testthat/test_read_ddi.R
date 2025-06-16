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

test_that("can read labeled values from codInstr tags", {
  ddi1 <- read_ipums_ddi(
    file.path(vcr::vcr_test_path("fixtures"), "cps_00111.xml")
  )
  ddi2 <- read_ipums_ddi(
    file.path(vcr::vcr_test_path("fixtures"), "ipumsi_00987.xml")
  )
  county_labels <- ipums_val_labels(ddi1, "COUNTY")
  oincfarm_labels <- ipums_val_labels(ddi1, "OINCFARM")
  popdensgeo1_labels <- ipums_val_labels(ddi2, "POPDENSGEO1")

  expect_equal(nrow(county_labels), 1)
  expect_equal(nrow(oincfarm_labels), 1)
  expect_equal(nrow(popdensgeo1_labels), 1)

  expect_equal(county_labels$val, 0)
  expect_equal(county_labels$lbl, "Not identified")

  expect_equal(oincfarm_labels$val, 99999999)
  expect_equal(oincfarm_labels$lbl, "N.I.U. (Not in Universe).")

  expect_equal(popdensgeo1_labels$val, 0)
  expect_equal(popdensgeo1_labels$lbl, "Unknown.")
})

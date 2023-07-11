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
  ddi2 <- read_ipums_ddi(
    file.path(vcr::vcr_test_path("fixtures"), "mtus_00002.xml"),
    file_select = 4
  )
  expect_identical(ddi1, ddi2)
})

test_that("Can read ddi from zip archives and directories", {
  xml1 <- xml2::read_xml(ipums_example("cps_00157.xml"))
  xml2 <- xml2::read_xml(ipums_example("cps_00159.xml"))

  tmp <- tempfile()

  on.exit(unlink(tmp, recursive = TRUE), add = TRUE, after = FALSE)

  dir.create(tmp)

  xml2::write_xml(xml1, file.path(tmp, "xml1.xml"))
  xml2::write_xml(xml2, file.path(tmp, "xml2.xml"))

  expect_error(
    read_ipums_ddi(tmp),
    "Multiple files found"
  )

  expect_error(
    read_ipums_ddi(tmp, file_select = contains("not-in-file")),
    "The provided `file_select` did not select any of the available files"
  )

  expect_identical(
    read_ipums_ddi(tmp, file_select = 1)$var_info,
    read_ipums_ddi(ipums_example("cps_00157.xml"))$var_info
  )
  expect_identical(
    read_ipums_ddi(tmp, file_select = 2)$var_info,
    read_ipums_ddi(ipums_example("cps_00159.xml"))$var_info
  )

  zip(
    file.path(tmp, "test.zip"),
    c(file.path(tmp, "xml1.xml"), file.path(tmp, "xml2.xml")),
    flags = "-q"
  )

  expect_error(
    read_ipums_ddi(file.path(tmp, "test.zip")),
    "Multiple files found"
  )

  expect_identical(
    read_ipums_ddi(
      file.path(tmp, "test.zip"),
      file_select = contains("xml1")
    )$var_info,
    read_ipums_ddi(ipums_example("cps_00157.xml"))$var_info
  )
  expect_identical(
    read_ipums_ddi(
      file.path(tmp, "test.zip"),
      file_select = contains("xml2")
    )$var_info,
    read_ipums_ddi(ipums_example("cps_00159.xml"))$var_info
  )
})

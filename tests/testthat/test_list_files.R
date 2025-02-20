test_that("Can list files from zipped data extract", {
  file <- ipums_example("nhgis0972_csv.zip")

  all_files <- ipums_list_files(file)
  d_files <- ipums_list_files(file, types = "data")
  cb_file <- ipums_list_files(file, file_select = contains("pmsa_codebook"))

  expect_equal(all_files$type, c("data", "codebook"))
  expect_equal(
    all_files$file,
    c("nhgis0972_csv/nhgis0972_ds135_1990_pmsa.csv",
      "nhgis0972_csv/nhgis0972_ds135_1990_pmsa_codebook.txt")
  )
  expect_equal(d_files$type, "data")
  expect_equal(d_files$file, "nhgis0972_csv/nhgis0972_ds135_1990_pmsa.csv")
  expect_equal(
    cb_file$file,
    "nhgis0972_csv/nhgis0972_ds135_1990_pmsa_codebook.txt"
  )
  expect_equal(nrow(ipums_list_files(file, file_select = "foo")), 0)
})

test_that("Can list files from zipped shapefile", {
  file <- ipums_example("nhgis0972_shape_small.zip")
  all_files <- ipums_list_files(file)
  expect_equal(all_files$type, "shape")
  expect_equal(
    all_files$file,
    "nhgis0972_shape/nhgis0972_shapefile_tl2000_us_pmsa_1990.zip"
  )
})

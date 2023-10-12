nhgis_csv <- ipums_example("nhgis0972_csv.zip")
nhgis_shp <- ipums_example("nhgis0972_shape_small.zip")

test_that("Can list files from zipped data extract", {
  if (!file.exists(nhgis_csv)) {
    skip("Couldn't find nhgis csv.")
  }

  all_files <- ipums_list_files(nhgis_csv)
  expect_equal(all_files$type, "data")
  expect_equal(all_files$file, "nhgis0972_csv/nhgis0972_ds135_1990_pmsa.csv")
})

test_that("Can list files from zipped shapefile", {
  if (!file.exists(nhgis_shp)) {
    skip("Couldn't find nhgis shape")
  }

  all_files <- ipums_list_files(nhgis_shp)
  expect_equal(all_files$type, "shape")
  expect_equal(
    all_files$file,
    "nhgis0972_shape/nhgis0972_shapefile_tl2000_us_pmsa_1990.zip"
  )
})

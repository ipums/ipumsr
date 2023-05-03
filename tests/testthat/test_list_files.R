nhgis_csv <- ipums_example("nhgis0972_csv.zip")
nhgis_shp <- ipums_example("nhgis0972_shape_small.zip")

test_that("Listing files from nhgis csv zip works", {
  if (!file.exists(nhgis_csv)) {
    skip("Couldn't find nhgis csv.")
  }

  all_files <- ipums_list_files(nhgis_csv)
  expect_equal(all_files$type, "data")
  expect_equal(all_files$file, "nhgis0972_csv/nhgis0972_ds135_1990_pmsa.csv")

  lifecycle::expect_deprecated(
    data_files <- ipums_list_data(nhgis_csv)
  )
  expect_equal(data_files$file, "nhgis0972_csv/nhgis0972_ds135_1990_pmsa.csv")

  lifecycle::expect_deprecated(
    shape_files <- ipums_list_shape(nhgis_csv)
  )
  expect_equal(nrow(shape_files), 0)

  lifecycle::expect_deprecated(
    raster_files <- ipums_list_raster(nhgis_csv)
  )
  expect_equal(nrow(raster_files), 0)
})

test_that("Listing files from nhgis shape zip works", {
  if (!file.exists(nhgis_shp)) {
    skip("Couldn't find nhgis shape")
  }

  all_files <- ipums_list_files(nhgis_shp)
  expect_equal(all_files$type, "shape")
  expect_equal(
    all_files$file,
    "nhgis0972_shape/nhgis0972_shapefile_tl2000_us_pmsa_1990.zip"
  )

  withr::with_options(list(lifecycle_verbosity = "quiet"), {
    shape_files <- ipums_list_shape(nhgis_shp)
    expect_equal(
      shape_files$file,
      "nhgis0972_shape/nhgis0972_shapefile_tl2000_us_pmsa_1990.zip"
    )

    data_files <- ipums_list_data(nhgis_shp)
    expect_equal(nrow(data_files), 0)

    raster_files <- ipums_list_raster(nhgis_shp)
    expect_equal(nrow(raster_files), 0)
  })
})

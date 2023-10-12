test_that("Can read single shapefile", {
  skip_if_not_installed("sf")

  expect_silent(
    shp <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))
  )

  expect_equal(nrow(shp), 71)
  expect_equal(ncol(shp), 9)
  expect_equal(sort(shp$PMSA)[1:2], c("0080", "0360"))
  expect_s3_class(shp, c("sf", "tbl_df", "tbl", "data.frame"))
})

test_that("Can row bind multiple sf files", {
  skip_if_not_installed("sf")

  nhgis_multi_shp <- ipums_example("nhgis0712_shape_small.zip")

  shp2 <- read_ipums_sf(nhgis_multi_shp, file_select = 2)
  shp3 <- read_ipums_sf(nhgis_multi_shp, file_select = 3)

  expect_silent(
    shp <- read_ipums_sf(
      nhgis_multi_shp,
      file_select = 2:3,
      bind_multiple = TRUE
    )
  )

  expect_identical(
    dplyr::bind_rows(shp2, shp3),
    dplyr::select(shp, -"layer")
  )

  expect_equal(unique(shp$layer), c("US_pmsa_1990", "US_pmsa_2000"))
  expect_s3_class(shp, c("sf", "tbl_df", "tbl", "data.frame"))
})

test_that("Can read extract at multiple file levels", {
  skip_if_not_installed("sf")

  # Read standard zip format ------------

  shp <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  # Read zip of shp ------------

  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzipped <- utils::unzip(
    ipums_example("nhgis0972_shape_small.zip"),
    exdir = temp_dir
  )
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE, after = FALSE)

  shp_unzip <- read_ipums_sf(unzipped)

  expect_identical(shp, shp_unzip)

  # Read dir of zip of shp ------------

  shp_unzip_dir <- read_ipums_sf(file.path(temp_dir, "nhgis0972_shape"))

  expect_identical(shp_unzip, shp_unzip_dir)

  # Informative error when reading a directory with no shapefiles
  expect_error(
    read_ipums_sf(temp_dir),
    "No .shp or .zip files found in the provided `shape_file`"
  )

  # Read shp directly ------------

  unzipped_shp <- utils::unzip(
    unzipped,
    exdir = temp_dir
  )

  unzipped_shp <- unzipped_shp[grepl(".shp$", unzipped_shp)]

  shp_direct <- read_ipums_sf(unzipped_shp)
  dir_of_shp <- read_ipums_sf(dirname(unzipped_shp))

  expect_identical(shp_unzip_dir, shp_direct)
  expect_identical(shp_direct, dir_of_shp)
})

test_that("We get informative errors when reading shapefiles", {
  skip_if_not_installed("sf")

  nhgis_multi_shp <- ipums_example("nhgis0712_shape_small.zip")

  # Multiple spatial files includes hint:
  expect_error(
    read_ipums_sf(nhgis_multi_shp, file_select = 1:2),
    "Multiple files found.+To combine files"
  )

  expect_error(
    read_ipums_sf(nhgis_multi_shp, file_select = 4),
    "Can't subset files past the end.+Available files:"
  )

  expect_warning(
    read_ipums_sf(
      nhgis_multi_shp,
      file_select = 1:2,
      bind_multiple = TRUE
    ),
    "Some variables had inconsistent types across files:"
  )
})

test_that("Careful rbind handles various data types", {
  skip_if_not_installed("sf")

  g1 <- sf::st_sfc(sf::st_point(1:2))
  g2 <- sf::st_sfc(sf::st_point(3:4))
  g3 <- sf::st_sfc(
    sf::st_polygon(
      list(
        matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE),
        matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE),
        matrix(c(5, 5, 5, 6, 6, 6, 6, 5, 5, 5), ncol = 2, byrow = TRUE)
      )
    )
  )

  sf1 <- sf::st_sf(layer = "A", geometry = g1, stringsAsFactors = FALSE)
  sf2 <- sf::st_sf(layer = FALSE, layer.1 = 1, geometry = g2)
  sf3 <- sf::st_sf(b = 3, layer.1 = FALSE, layer = 4, geometry = g3)

  expect_warning(
    careful_sf_rbind(list(a = sf1, b = sf2, c = sf3)),
    "Adding layer information to column \"layer.2\""
  )

  expect_warning(
    sf_bind <- careful_sf_rbind(list(a = sf1, b = sf2, c = sf3)),
    paste0(
      "Some variables .+ ",
      "\"geometry\" \\(c\\(\"sfc_POINT\", \"sfc\"\\) vs..+",
      "\"layer\" \\(character vs. logical vs. numeric\\).+",
      "\"layer.1\" \\(numeric vs. logical\\)"
    )
  )

  expect_null(
    suppressWarnings(
      careful_sf_rbind(
        list(a = sf1, b = sf2, sf3),
        add_layer_var = FALSE
      )$layer.2
    )
  )

  expect_equal(
    purrr::map(sf_bind, class),
    list(
      layer.2 = "character",
      layer = "character",
      layer.1 = "numeric",
      b = "numeric",
      geometry = c("sfc_GEOMETRY", "sfc")
    )
  )

  expect_equal(nrow(sf_bind), nrow(sf1) + nrow(sf2) + nrow(sf3))
  expect_equal(
    ncol(sf_bind),
    length(union(union(colnames(sf1), colnames(sf2)), colnames(sf3))) + 1
  )
})

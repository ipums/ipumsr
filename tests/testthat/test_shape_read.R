nhgis_single_shp <- ipums_example("nhgis0972_shape_small.zip")
nhgis_multi_shp <- ipums_example("nhgis0712_shape_small.zip")

rows <- 71
vars_data <- 25
vars_data_shape_sf <- 9
vars_data_shape_sp <- 8
pmsa_first2_codes <- c("0080", "0360")

test_that("Can read single shapefile: sf", {
  skip_if_not_installed("sf")

  expect_silent(
    shp <- read_ipums_sf(nhgis_single_shp)
  )

  expect_equal(nrow(shp), rows)
  expect_equal(ncol(shp), vars_data_shape_sf)
  expect_equal(sort(shp$PMSA)[1:2], pmsa_first2_codes)
  expect_s3_class(shp, c("sf", "tbl_df", "tbl", "data.frame"))
})

test_that("Can read NHGIS extract: single shapefile (sp)", {
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  lifecycle::expect_deprecated(
    shp <- read_ipums_sp(nhgis_single_shp, verbose = FALSE)
  )

  expect_error(
    read_ipums_sp(
      nhgis_single_shp,
      shape_layer = matches("fake-layer"),
      verbose = FALSE
    ),
    "`shape_layer`"
  )

  expect_equal(nrow(shp@data), rows)
  expect_equal(ncol(shp@data), vars_data_shape_sp)
  expect_equal(sort(shp$PMSA)[1:2], pmsa_first2_codes)
  expect_s4_class(shp, "SpatialPolygonsDataFrame")
})

test_that("Can row bind multiple sf files", {
  skip_if_not_installed("sf")

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

test_that("Can row bind multiple sp files", {
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  withr::with_options(list(lifecycle_verbosity = "quiet", warn = -1), {
    shp1 <- read_ipums_sp(nhgis_multi_shp, shape_layer = 2, verbose = FALSE)
    shp2 <- read_ipums_sp(nhgis_multi_shp, shape_layer = 3, verbose = FALSE)

    shp <- read_ipums_sp(
      nhgis_multi_shp,
      shape_layer = 2:3,
      bind_multiple = TRUE,
      verbose = FALSE
    )
  })

  expect_identical(
    rbind(shp1@data, shp2@data),
    dplyr::select(shp@data, -c("layer"))
  )

  expect_equal(unique(shp$layer), c("US_pmsa_1990", "US_pmsa_2000"))
  expect_s4_class(shp, "SpatialPolygonsDataFrame")
})

test_that("Can read extract at multiple file levels", {
  skip_if_not_installed("sf")

  # Read standard zip format ------------

  shp <- read_ipums_sf(nhgis_single_shp)

  # Read zip of shp ------------

  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzipped <- utils::unzip(nhgis_single_shp, exdir = temp_dir)
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


test_that("sf and sp geometries are consistent with each other", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  nhgis_sf <- read_ipums_sf(nhgis_single_shp)

  withr::with_options(list(lifecycle_verbosity = "quiet", warn = -1), {
    nhgis_sp <- read_ipums_sp(nhgis_single_shp, verbose = FALSE)
  })

  check_geo <- nhgis_sf$GISJOIN

  expect_equal(
    dplyr::filter(nhgis_sf, GISJOIN == check_geo)$geometry[[1]][[1]][[1]],
    subset(nhgis_sp, GISJOIN == check_geo)@polygons[[1]]@Polygons[[1]]@coords
  )
  expect_equal(
    dplyr::filter(nhgis_sf, GISJOIN == check_geo)$geometry[[2]][[1]][[1]],
    subset(nhgis_sp, GISJOIN == check_geo)@polygons[[2]]@Polygons[[1]]@coords
  )
})

# test_that("We can pass arguments to underlying reader functions", {
#   skip_if_not_installed("sf")
#   skip_if_not_installed("rgdal")
#   skip_if_not_installed("sp")
#
#   expect_output(
#     shp_sf <- read_ipums_sf(
#       nhgis_single_shp,
#       quiet = FALSE,
#       promote_to_multi = FALSE
#     )
#   )
#
#   # Make sure we handle case where user passes `options` to `...`
#   expect_warning(
#     read_ipums_sf(
#       nhgis_single_shp,
#       options = c("ENCODING=UTF-8", "SHPT=NO")
#     ),
#     "Encoding specified in both `encoding` and `options`.+",
#     "driver ESRI Shapefile does not support open option SHPT"
#   )
#
#   expect_true(any(sf::st_geometry_type(shp_sf) == "POLYGON"))
#
#   withr::with_options(list(lifecycle_verbosity = "quiet"), {
#     expect_warning(
#       shp_sp <- read_ipums_sp(nhgis_single_shp, verbose = FALSE),
#       "OGR support is provided"
#     )
#   })
# })

test_that("We get informative errors when reading shapefiles", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  # Multiple spatial files includes hint:
  expect_error(
    read_ipums_sf(nhgis_multi_shp, file_select = 1:2),
    "Multiple files found.+To combine files"
  )

  expect_error(
    read_ipums_sf(nhgis_multi_shp, file_select = 4),
    "Can't subset files past the end.+Available files:"
  )

  # geometry differences don't trigger the variable warning used in sf
  # implementation, but lower level error will still catch incompatibilities

  withr::with_options(list(lifecycle_verbosity = "quiet", warn = -1), {
    expect_error(
      read_ipums_sp(
        nhgis_multi_shp,
        shape_layer = 1:2,
        bind_multiple = TRUE,
        verbose = FALSE
      ),
      "no method.+for coercing"
    )
  })

  expect_warning(
    read_ipums_sf(
      nhgis_multi_shp,
      file_select = 1:2,
      bind_multiple = TRUE
    ),
    "Some variables had inconsistent types across files:"
  )
})

test_that("Careful rbind handles various data types (sf)", {
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

test_that("Careful rbind handles various data types (sp)", {
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  sp1 <- sp::SpatialPointsDataFrame(
    cbind(1:2, 3:4),
    data.frame(a = c(1L, 2L), b = c("a", "b")),
    match.ID = TRUE
  )

  sp2 <- sp::SpatialPointsDataFrame(
    cbind(5:6, 7:8),
    data.frame(a = c(TRUE, FALSE), layer = c("c", "d")),
    match.ID = TRUE
  )

  poly <- sp::SpatialPolygons(
    list(
      sp::Polygons(
        list(sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))),
        "s1"
      )
    )
  )

  sp3 <- sp::SpatialPolygonsDataFrame(
    poly,
    data = data.frame(a = 1L, b = "c", d = TRUE, row.names = row.names(poly))
  )

  expect_error(
    careful_sp_rbind(list(a = sp1, b = sp3)),
    "Unexpected variable type in shape file."
  )

  expect_error(
    careful_sp_rbind(list(a = sp1, b = sp2), add_layer_var = TRUE),
    "Cannot combine shape files because variable types don't match"
  )
})

test_that("Can read and combine with read_nhgis_sf() (deprecated)", {
  skip_if_not_installed("sf")

  nhgis_data_file <- ipums_example("nhgis0712_csv.zip")

  data <- read_nhgis(
    nhgis_data_file,
    file_select = contains("ds136_1990_pmsa"),
    verbose = FALSE
  )

  shape_data_sf <- read_ipums_sf(
    nhgis_multi_shp,
    contains("pmsa_1990")
  )

  expect_error(
    suppressWarnings(
      read_nhgis_sf(nhgis_data_file, nhgis_single_shp, verbose = FALSE)
    ),
    "`data_layer`"
  )
  expect_error(
    read_nhgis_sf(
      nhgis_data_file,
      nhgis_single_shp,
      data_layer = contains("ds136_1990_pmsa"),
      shape_layer = contains("fake-layer"),
      verbose = FALSE
    ),
    "`shape_layer`"
  )

  lifecycle::expect_deprecated(
    data_shp_sf <- read_nhgis_sf(
      nhgis_data_file,
      nhgis_single_shp,
      data_layer = contains("ds136_1990_pmsa"),
      shape_layer = contains("pmsa_1990"),
      verbose = FALSE
    )
  )

  expect_s3_class(data_shp_sf, "sf")
  expect_equal(nrow(data_shp_sf), nrow(data))
  expect_equal(
    ncol(data_shp_sf),
    ncol(data) + ncol(shape_data_sf) -
      length(intersect(colnames(data), colnames(shape_data_sf)))
  )
})

test_that("Can read and combine with read_nhgis_sp() (deprecated)", {
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  nhgis_data_file <- ipums_example("nhgis0712_csv.zip")

  data <- read_nhgis(
    nhgis_data_file,
    file_select = contains("ds136_1990_pmsa"),
    verbose = FALSE
  )

  expect_error(
    suppressWarnings(
      read_nhgis_sp(
        nhgis_data_file,
        nhgis_single_shp,
        verbose = FALSE
      )
    ),
    "`data_layer`"
  )

  expect_error(
    read_nhgis_sp(
      nhgis_data_file,
      nhgis_single_shp,
      data_layer = contains("ds136_1990_pmsa"),
      shape_layer = contains("fake-layer"),
      verbose = FALSE
    ),
    "`shape_layer`"
  )

  lifecycle::expect_deprecated(
    data_shp_sp <- read_nhgis_sp(
      nhgis_data_file,
      nhgis_single_shp,
      data_layer = contains("ds136_1990_pmsa"),
      shape_layer = contains("pmsa_1990"),
      verbose = FALSE
    )
  )

  expect_s4_class(data_shp_sp, "SpatialPolygonsDataFrame")
  expect_equal(nrow(data_shp_sp@data), nrow(data))
  expect_equal(
    ncol(data_shp_sp),
    ncol(data) + ncol(data_shp_sp@data) -
      length(intersect(colnames(data), colnames(data_shp_sp@data)))
  )
})

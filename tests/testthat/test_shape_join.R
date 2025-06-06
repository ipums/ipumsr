data <- read_ipums_agg(ipums_example("nhgis0972_csv.zip"), verbose = FALSE)

test_that("Basic join works", {
  skip_if_not_installed("sf")
  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  expect_message(expect_null(join_failures(joined)))
  expect_equal(nrow(data), nrow(joined))
  expect_equal(attr(joined, "sf_column"), "geometry")
})

test_that("suffix argument works", {
  skip_if_not_installed("sf")
  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  data$test <- 1
  shape$test <- 2

  joined <- ipums_shape_inner_join(
    data,
    shape,
    by = "GISJOIN",
    suffix = c("_d", "_s")
  )
  expect_equal(data$test, joined$test_d)
  expect_equal(shape$test, joined$test_s)
})


test_that("complicated `by` works", {
  skip_if_not_installed("sf")
  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  joined_regular <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  data$join_split1 <- fostr_sub(data$GISJOIN, 1, 1)
  data$join_split2 <- as.numeric(fostr_sub(data$GISJOIN, 2, -1))

  shape$join_split1 <- fostr_sub(shape$GISJOIN, 1, 1)
  shape$join_split_xxx <- fostr_sub(shape$GISJOIN, 2, -1)
  shape$GISJOIN <- NULL
  # Next line added as a workaround for apparent bug in sf:::rename.sf,
  # introduced in version 0.9.5
  shape <- dplyr::select(shape, starts_with("join_"), everything(), geometry)

  joined <- ipums_shape_inner_join(
    data, shape,
    by = c("join_split1", "join_split2" = "join_split_xxx")
  )

  joined <- dplyr::select(joined, -dplyr::one_of("join_split1", "join_split2"))
  expect_true(identical(joined, joined_regular))
})

test_that("error for missing a by variable", {
  skip_if_not_installed("sf")
  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  shape$GISJOIN <- NULL
  expect_error(joined <- ipums_shape_inner_join(data, shape, by = "GISJOIN"))
})

test_that("Join failures are mentioned", {
  skip_if_not_installed("sf")
  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))

  joined_regular <- ipums_shape_inner_join(data, shape, by = "GISJOIN")

  data$GISJOIN[data$GISJOIN == "G1120"] <- "ABC"
  shape$GISJOIN[shape$GISJOIN == "G1120"] <- "XYZ"

  capture.output(
    joined_fail <- ipums_shape_inner_join(data, shape, by = "GISJOIN")
  )

  jf <- join_failures(joined_fail)
  expect_equal(nrow(jf$data), 1)
  expect_equal(jf$data$GISJOIN, "ABC", ignore_attr = TRUE)
  expect_equal(nrow(jf$shape), 1)
  expect_equal(jf$shape$GISJOIN, "XYZ", ignore_attr = TRUE)

  filtered_regular <- dplyr::filter(joined_regular, .data$GISJOIN != "G1120")

  expect_equal(joined_fail$GISJOIN, filtered_regular$GISJOIN)
  expect_equal(joined_fail$D6Z001, filtered_regular$D6Z001)
  expect_equal(joined_fail$GISJOIN2, filtered_regular$GISJOIN2)
})


test_that("Character -> Integer conversion works (#16)", {
  skip_if_not_installed("sf")
  data$id <- as.integer(fostr_sub(data$GISJOIN, 2, -1))
  attr(data$id, "vardesc") <- "Test ipums attribute"

  shape <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))
  shape$id_shape <- fostr_sub(shape$GISJOIN, 2, -1)
  # Next line added as a workaround for apparent bug in sf:::rename.sf,
  # introduced in version 0.9.5
  shape <- dplyr::select(shape, id_shape, everything(), geometry)

  joined <- ipums_shape_inner_join(data, shape, by = c("id" = "id_shape"))

  expect_message(expect_null(join_failures(joined)))
  expect_equal(nrow(data), nrow(joined))
  expect_equal(attr(joined, "sf_column"), "geometry")
})

context("NHGIS")

nhgis_single_csv <- ipums_example("nhgis0707_csv.zip")
nhgis_single_fwf <- ipums_example("nhgis0722_fixed.zip")
nhgis_single_shp <- ipums_example("nhgis0707_shape_small.zip")

nhgis_multi_ds <-  ipums_example("nhgis0712_csv.zip")
nhgis_multi_shp <- ipums_example("nhgis0712_shape_small.zip")

rows <- 71
vars_data <- 25

# Read single files -------------------------------

test_that("Can read NHGIS extract: single dataset", {

  expect_output(
    nhgis_csv <- read_nhgis(nhgis_single_csv),
    "Use of data from NHGIS is subject"
  )

  expect_warning(
    nhgis_fwf <- read_nhgis(nhgis_single_fwf, show_conditions = FALSE),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  expect_equal(nrow(nhgis_csv), rows)
  expect_equal(ncol(nhgis_csv), vars_data)
  expect_equal(attr(nhgis_csv$D6Z001, "label"), "1989 to March 1990")
  expect_equal(attr(nhgis_csv$D6Z001, "var_desc"), "Year Structure Built (D6Z)")
  expect_equal(
    nhgis_csv$PMSA[1:2],
    c("Akron, OH PMSA", "Anaheim--Santa Ana, CA PMSA")
  )
  expect_s3_class(nhgis_csv, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

  expect_equal(nrow(nhgis_fwf), rows)
  expect_equal(ncol(nhgis_fwf), 14)
  expect_equal(colnames(nhgis_fwf), paste0("X", 1:ncol(nhgis_fwf)))

})

test_that("Can read NHGIS extract: single time series table", {

  # TODO: Update this example
  nhgis_timeseries <- system.file(
    "extdata",
    "nhgis0043_csv.zip",
    package = "ipumsexamples"
  )

  if (!file.exists(nhgis_timeseries)) {
    skip("Couldn't find nhgis time series. ipumsexamples likely not installed.")
  }

  data <- read_nhgis(nhgis_timeseries, show_conditions = FALSE)

  expect_equal(nrow(data), 1L)
  expect_equal(data$GISJOIN[[1]], "G1")
  expect_equal(data$NATION[[1]], "United States")
  expect_equal(data$B78AA1980[[1]], 226545805)
  expect_equal(attr(data$B78AA125, "label"), "2008-2012: Persons: Total")
  expect_equal(attr(data$B78AA125, "var_desc"), "Total Population (B78)")

})

# Select files when multiple exist -------------------------------

test_that("Can select data files by index", {

  nhgis1 <- read_nhgis(nhgis_multi_ds, data_layer = 1, show_conditions = FALSE)
  nhgis2 <- read_nhgis(nhgis_multi_ds, data_layer = 2, show_conditions = FALSE)

  expect_equal(dim(nhgis1), c(rows, 18))
  expect_equal(dim(nhgis2), c(rows, 18))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("ECI001" %in% colnames(nhgis1))
  expect_true("EH3001" %in% colnames(nhgis2))

})

test_that("Can select data files with tidyselect", {

  nhgis1 <- read_nhgis(
    nhgis_multi_ds,
    data_layer = contains("135"),
    show_conditions = FALSE
  )

  nhgis2 <- read_nhgis(
    nhgis_multi_ds,
    data_layer = contains("136"),
    show_conditions = FALSE
  )

  expect_equal(dim(nhgis1), c(rows, 18))
  expect_equal(dim(nhgis2), c(rows, 18))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("ECI001" %in% colnames(nhgis1))
  expect_true("EH3001" %in% colnames(nhgis2))

})

# Can read data when provided in zip, dir, and shp formats -------

# TODO: Docs need to show that some args to readOGR are already specified
test_that("We can pass arguments to underlying reader functions", {

  # TODO: Docs need to show that attr_vars doesn't work if col_names = FALSE
  # Worth checking if there are other ipums things that break with ... args
  expect_silent(
    nhgis_data <- read_nhgis(
      nhgis_single_csv,
      show_conditions = FALSE,
      show_col_types = FALSE,
      progress = FALSE,
      col_names = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      skip = 2
    )
  )

  expect_warning(
    nhgis_data_fwf <- read_nhgis(
      nhgis_single_fwf,
      show_conditions = FALSE,
      col_positions = readr::fwf_widths(c(4, 2, 2, 1)),
      col_types = "ccil"
    ),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  expect_equal(nrow(nhgis_data), rows - 1)
  expect_equal(colnames(nhgis_data), paste0("X", 1:vars_data))
  expect_equal(unique(purrr::map_chr(nhgis_data, class)), "character")

  expect_equal(nrow(nhgis_data_fwf), rows)
  expect_equal(ncol(nhgis_data_fwf), 4)
  expect_equal(
    purrr::map_chr(nhgis_data_fwf, class),
    c(X1 = "character", X2 = "character", X3 = "integer", X4 = "logical")
  )

})

test_that("We get informative error messages when reading NHGIS extracts", {

  expect_error(
    read_nhgis("FAKE_FILE.zip", show_conditions = FALSE),
    "Could not find file"
  )

  expect_error(
    read_nhgis("C:/FAKE_FOLDER/FAKE_FILE.zip", show_conditions = FALSE),
    "Could not find file, check the path"
  )

  expect_error(
    nhgis <- read_nhgis(nhgis_multi_ds, show_conditions = FALSE),
    "Multiple files found"
  )
  expect_error(
    nhgis <- read_nhgis(
      nhgis_multi_ds,
      data_layer = contains("ds"),
      show_conditions = FALSE
    ),
    "Multiple files found"
  )
  expect_error(
    nhgis <- read_nhgis(
      nhgis_multi_ds,
      data_layer = 1:2,
      show_conditions = FALSE
    ),
    "Multiple files found"
  )

  expect_error(
    read_nhgis(nhgis_multi_ds, data_layer = 3, show_conditions = FALSE),
    "Can't subset files past the end.+Available files:"
  )
  expect_error(
    read_nhgis(
      nhgis_multi_ds,
      data_layer = contains("not-in-file"),
      show_conditions = FALSE
    ),
    "The provided `data_layer` did not select any of the available files:"
  )
})


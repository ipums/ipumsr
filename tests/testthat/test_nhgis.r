context("NHGIS")

nhgis_single_csv <- ipums_example("nhgis0707_csv.zip")
nhgis_single_shp <- ipums_example("nhgis0707_shape_small.zip")

nhgis_multi_ds <-  ipums_example("nhgis0731_csv.zip")
nhgis_multi_shp <- ipums_example("nhgis0712_shape_small.zip")

nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

rows <- 71
vars_data <- 25

# Read single files -------------------------------

test_that("Can read NHGIS extract: single dataset", {

  expect_message(
    nhgis_csv <- read_nhgis(nhgis_single_csv),
    "Use of data from NHGIS is subject"
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

})

test_that("Can read NHGIS extract: fixed-width files", {

  expect_error(
    read_nhgis(nhgis_multi_fwf, show_conditions = FALSE),
    "Multiple files found"
  )

  expect_silent(
    nhgis_fwf1 <- read_nhgis(
      nhgis_multi_fwf,
      data_layer = 1,
      show_conditions = FALSE,
      progress = FALSE
    )
  )

  expect_silent(
    nhgis_csv1 <- read_nhgis(
      nhgis_multi_ds,
      data_layer = 1,
      show_conditions = FALSE,
      progress = FALSE,
      show_col_types = FALSE
    )
  )

  # Test that .do file col typing is working.
  # These coltypes differ from what would be expected by readr defaults.
  expect_type(nhgis_fwf1$AIHHTLI, "character")
  expect_type(nhgis_fwf1$MEMI, "character")
  expect_type(nhgis_fwf1$PCI, "character")
  expect_type(nhgis_fwf1$NATIONA, "character")

  # Test that .do file recoding is working:
  # This should be coercible to integer because it was not rescaled on import:
  expect_true(
    dplyr::near(
      nhgis_fwf1$AJWBE001,
      as.integer(nhgis_fwf1$AJWBE001)
    )
  )
  # This was rescaled and should be a decimal:
  expect_false(
    dplyr::near(
      nhgis_fwf1$AJWCE001,
      as.integer(nhgis_fwf1$AJWCE001)
    )
  )

  expect_equal(nrow(nhgis_fwf1), nrow(nhgis_csv1))
  expect_equal(ncol(nhgis_fwf1), ncol(nhgis_csv1) - 1)

  expect_equal(
    colnames(nhgis_fwf1),
    read_nhgis_codebook(nhgis_multi_fwf, data_layer = 1)$var_info$var_name
  )

  # This has some coltype differences which are to be expected
  # as we can directly provide coltypes in read_nhgis_fwf()
  # whereas read_nhgis_csv() uses readr defaults
  expect_true(
    dplyr::all_equal(
      nhgis_fwf1,
      dplyr::mutate(
        nhgis_csv1[, 2:ncol(nhgis_csv1)],
        NATIONA = as.character(NATIONA)
      ),
      convert = TRUE
    )
  )

  expect_warning(
    read_nhgis(
      nhgis_multi_fwf,
      data_layer = 1,
      do_file = FALSE,
      show_conditions = FALSE,
      progress = FALSE,
      show_col_types = FALSE
    ),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  expect_warning(
    read_nhgis(
      nhgis_multi_fwf,
      data_layer = 1,
      do_file = "asdf",
      show_conditions = FALSE,
      progress = FALSE,
      show_col_types = FALSE
    ),
    "Could not find the provided `do_file`..+"
  )

  expect_warning(
    nhgis_fwf2 <- read_nhgis(
      nhgis_multi_fwf,
      data_layer = 1,
      do_file = "asdf",
      col_positions = readr::fwf_widths(1),
      show_conditions = FALSE,
      progress = FALSE,
      show_col_types = FALSE
    ),
    paste0(
      "Only one of `col_positions` or `do_file` can be provided. ",
      "Setting `do_file = FALSE`"
    )
  )

  expect_equal(nrow(nhgis_fwf1), nrow(nhgis_fwf2))
  expect_equal(ncol(nhgis_fwf2), 1)
  expect_equal(colnames(nhgis_fwf2), "X1")

  expect_equal(attr(nhgis_fwf1$YEAR, "label"), "Data File Year")
  expect_s3_class(nhgis_fwf1, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

})

test_that("Can read NHGIS extract: single time series table", {

  tst <- read_nhgis(
    nhgis_multi_fwf,
    data_layer = contains("ts"),
    progress = FALSE,
    show_conditions = FALSE
  )

  tst2 <- read_nhgis(
    nhgis_multi_ds,
    data_layer = contains("ts"),
    progress = FALSE,
    show_conditions = FALSE,
    show_col_types = FALSE
  )

  expect_true(dplyr::all_equal(tst, tst2))

  expect_equal(nrow(tst), 84)
  expect_equal(tst$GISJOIN[[1]], "G010")
  expect_equal(tst$STATE[[1]], "Alabama")
  expect_equal(tst$A00AA1820[[1]], 127901)

})

# Select files when multiple exist -------------------------------

test_that("Can select data files by index", {

  nhgis1 <- read_nhgis(nhgis_multi_ds, data_layer = 1, show_conditions = FALSE)
  nhgis2 <- read_nhgis(nhgis_multi_ds, data_layer = 2, show_conditions = FALSE)

  expect_equal(dim(nhgis1), c(1, 115))
  expect_equal(dim(nhgis2), c(84, 28))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("AJWBE001" %in% colnames(nhgis1))
  expect_true("A00AA1790" %in% colnames(nhgis2))

})

test_that("Can select data files with tidyselect", {

  nhgis1 <- read_nhgis(
    nhgis_multi_ds,
    data_layer = contains("ds239"),
    show_conditions = FALSE
  )

  nhgis2 <- read_nhgis(
    nhgis_multi_ds,
    data_layer = contains("ts_nominal"),
    show_conditions = FALSE
  )

  expect_equal(dim(nhgis1), c(1, 115))
  expect_equal(dim(nhgis2), c(84, 28))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("AJWBE001" %in% colnames(nhgis1))
  expect_true("A00AA1790" %in% colnames(nhgis2))

})

# Can read data when provided in zip, dir, and shp formats -------

# TODO: Docs need to show that some args to readOGR are already specified?
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
      nhgis_multi_fwf,
      data_layer = 1,
      show_conditions = FALSE,
      col_positions = readr::fwf_widths(c(4, 2, 2, 1)),
      col_types = "ccil"
    ),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  expect_equal(nrow(nhgis_data), rows - 1)
  expect_equal(colnames(nhgis_data), paste0("X", 1:vars_data))
  expect_equal(unique(purrr::map_chr(nhgis_data, class)), "character")

  expect_equal(nrow(nhgis_data_fwf), 1)
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
      data_layer = contains("nhgis"),
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

test_that("Can read NHGIS codebook", {

  expect_error(
    read_nhgis_codebook(nhgis_multi_ds),
    "Multiple files found"
  )

  # Single dataset ----------------

  d_csv <- read_nhgis(
    nhgis_single_csv,
    show_conditions = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )

  cb_csv <- read_nhgis_codebook(nhgis_single_csv)

  # FWF with ACS ----------------

  # Includes multiple data types which changes codebook slightly

  d_fwf <- read_nhgis(
    nhgis_multi_fwf,
    data_layer = 1,
    show_conditions = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )

  cb_fwf <- read_nhgis_codebook(nhgis_multi_fwf, data_layer = 1)

  # Time series table ----------

  d_fwf_tst <- read_nhgis(
    nhgis_multi_fwf,
    data_layer = 2,
    show_conditions = FALSE,
    progress = FALSE,
    show_col_types = FALSE
  )

  cb_fwf_ts <- read_nhgis_codebook(nhgis_multi_fwf, data_layer = 2)

  expect_equal(cb_csv$var_info$var_name, colnames(d_csv))
  expect_equal(cb_fwf$var_info$var_name, colnames(d_fwf))
  expect_equal(cb_fwf_ts$var_info$var_name, colnames(d_fwf_tst))

  # Raw cb lines:
  expect_equal(
    length(read_nhgis_codebook(nhgis_single_csv, raw = TRUE)),
    117
  )

})

# Difficult to test because Terra is being decommissioned,
# but creating this space if more thorough work is to be done later.
#
# TODO: I don't think any terra tests are being run because of
# problem with check() locating data in ipumsexamples
#
# TODO: read_ipums_codebook should likely be deprecated. Should
# talk about how to handle Terra decommissioning in R
test_that("Can read Terra codebook", {

  terra_area <- system.file(
    "extdata",
    "3485_bundle.zip",
    package = "ipumsexamples"
  )

  if (!file.exists(terra_area)) {
    skip("Couldn't find terra area ipumsexamples likely not installed.")
  }

  cb <- read_ipums_codebook(terra_area)

  expect_equal(dim(cb$var_info), c(4, 10))

})

test_that("Can read certain unzipped structures", {

  sps_tmpfile <- file.path(tempdir(), "test.sps")
  csv_tmpfile1 <- file.path(tempdir(), "test1.csv")
  csv_tmpfile2 <- file.path(tempdir(), "test2.csv")
  dat_tmpfile <- file.path(tempdir(), "test.dat")

  on.exit(unlink(sps_tmpfile), add = TRUE, after = FALSE)
  on.exit(unlink(csv_tmpfile1), add = TRUE, after = FALSE)
  on.exit(unlink(csv_tmpfile2), add = TRUE, after = FALSE)
  on.exit(unlink(dat_tmpfile), add = TRUE, after = FALSE)

  # Non-csv or dat file should not interfere with reading:
  readr::write_lines("A", sps_tmpfile)

  expect_error(
    read_nhgis(tempdir()),
    "No .csv or .dat files found"
  )

  # Single file should read normally, but should warn
  # that no codebook is available.
  readr::write_csv(tibble::tibble(a = c("a", "b")), csv_tmpfile1)

  expect_warning(
    x1 <- read_nhgis(tempdir()),
    "Unable to read codebook"
  )

  expect_message(
    x2 <- read_nhgis(file.path(tempdir(), "test1.csv"), var_attrs = NULL),
    "Use of data from NHGIS"
  )

  # NB: First row is removed because read_nhgis thinks this fake extract has an
  # extra header row. Not a concern here.
  expect_equal(x1$a, "b")
  expect_identical(x1, x2)

  # Multiple files should throw same errors even if unzipped:
  readr::write_csv(tibble::tibble(a = c("a", "b")), csv_tmpfile2)

  expect_error(
    read_nhgis(tempdir()),
    "Multiple files found"
  )

  expect_identical(
    read_nhgis(tempdir(), data_layer = 1, var_attrs = NULL),
    x1
  )

  # Both csv and dat is ambiguous:
  readr::write_csv(tibble::tibble(a = "a"), dat_tmpfile)

  expect_error(
    read_nhgis(tempdir()),
    "Both .csv and .dat files found.+Use the `file_type` argument"
  )

  suppressWarnings(
    x3 <- read_nhgis(tempdir(), file_type = "csv", data_layer = 1)
  )

  expect_identical(x3, x1)

  # Direct data file errors if mismatched extension
  expect_error(
    read_nhgis_fwf(csv_tmpfile1),
    "Expected `file` to match extension"
  )

  # Reading a direct data file should ignore data layer
  expect_identical(
    suppressWarnings(read_nhgis(csv_tmpfile1, data_layer = 3)),
    x2
  )

})

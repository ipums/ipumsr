nhgis_single_csv <- ipums_example("nhgis0972_csv.zip")
nhgis_single_shp <- ipums_example("nhgis0972_shape_small.zip")

nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")
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

  expect_message(
    read_nhgis(nhgis_single_csv, verbose = FALSE, show_col_types = TRUE),
    "Column specification"
  )

  expect_equal(nrow(nhgis_csv), rows)
  expect_equal(ncol(nhgis_csv), vars_data)
  expect_equal(
    attr(nhgis_csv$D6Z001, "label"),
    "Total area: 1989 to March 1990"
  )
  expect_equal(
    attr(nhgis_csv$D6Z001, "var_desc"),
    "Table D6Z: Year Structure Built (Universe: Housing Units)"
  )
  expect_equal(
    nhgis_csv$PMSA[1:2],
    c("Akron, OH PMSA", "Anaheim--Santa Ana, CA PMSA")
  )
  expect_s3_class(nhgis_csv, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
})

test_that("Can read NHGIS extract: fixed-width files", {
  expect_error(
    read_nhgis(nhgis_multi_fwf, verbose = FALSE),
    "Multiple files found"
  )

  nhgis_fwf1 <- read_nhgis(nhgis_multi_fwf, file_select = 1, verbose = FALSE)
  nhgis_csv1 <- read_nhgis(nhgis_multi_ds, file_select = 1, verbose = FALSE)

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
    read_nhgis_codebook(nhgis_multi_fwf, file_select = 1)$var_info$var_name
  )

  # This has some coltype differences which are to be expected
  # as we can directly provide coltypes in read_nhgis_fwf()
  # whereas read_nhgis_csv() uses readr defaults
  expect_identical(
    purrr::map(nhgis_fwf1, attributes),
    purrr::map(dplyr::select(nhgis_csv1, -"GISJOIN"), attributes)
  )

  expect_identical(
    purrr::map(zap_ipums_attributes(nhgis_fwf1), "identity"),
    purrr::map(
      zap_ipums_attributes(dplyr::select(nhgis_csv1, -"GISJOIN")),
      "identity"
    )
  )

  expect_warning(
    read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      do_file = FALSE,
      verbose = FALSE
    ),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  expect_warning(
    read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      do_file = "asdf",
      verbose = FALSE
    ),
    "Could not find the provided `do_file`..+"
  )

  expect_warning(
    nhgis_fwf2 <- read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      do_file = "asdf",
      col_positions = readr::fwf_widths(1),
      verbose = FALSE
    ),
    paste0(
      "Only one of `col_positions` or `do_file` can be provided. ",
      "Setting `do_file = FALSE`"
    )
  )

  expect_equal(nrow(nhgis_fwf1), nrow(nhgis_fwf2))
  expect_equal(ncol(nhgis_fwf2), 1)
  expect_equal(colnames(nhgis_fwf2), "X1")

  expect_equal(
    attributes(nhgis_fwf1$AJWBE001),
    list(
      label = "Estimates: Total",
      var_desc = "Table AJWB: Sex by Age (Universe: Total population)"
    )
  )

  expect_equal(attr(nhgis_fwf1$YEAR, "label"), "Data File Year")
  expect_s3_class(nhgis_fwf1, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
})

test_that("Can read NHGIS extract: single time series table", {
  tst <- read_nhgis(
    nhgis_multi_fwf,
    file_select = contains("ts"),
    verbose = FALSE
  )

  tst2 <- read_nhgis(
    nhgis_multi_ds,
    file_select = contains("ts"),
    verbose = FALSE
  )

  expect_identical(
    purrr::map(tst, attributes),
    purrr::map(tst2, attributes)
  )

  expect_identical(
    purrr::map(zap_ipums_attributes(tst), "identity"),
    purrr::map(zap_ipums_attributes(tst2), "identity")
  )

  expect_equal(nrow(tst), 84)
  expect_equal(tst$GISJOIN[[1]], "G010")
  expect_equal(tst$STATE[[1]], "Alabama")
  expect_equal(tst$A00AA1820[[1]], 127901)

  expect_equal(
    attributes(tst$A00AA2020),
    list(
      label = "2020: Persons: Total",
      var_desc = "Table A00: Total Population"
    )
  )
})

# Select files when multiple exist -------------------------------

test_that("Can select data files by index", {
  nhgis1 <- read_nhgis(nhgis_multi_ds, file_select = 1, verbose = FALSE)
  nhgis2 <- read_nhgis(nhgis_multi_ds, file_select = 2, verbose = FALSE)

  expect_equal(dim(nhgis1), c(1, 115))
  expect_equal(dim(nhgis2), c(84, 28))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("AJWBE001" %in% colnames(nhgis1))
  expect_true("A00AA1790" %in% colnames(nhgis2))

  expect_equal(
    attributes(nhgis1$GISJOIN),
    list(label = "GIS Join Match Code", var_desc = "")
  )

  expect_equal(
    attributes(nhgis2$GISJOIN),
    list(label = "GIS Join Match Code", var_desc = "")
  )
})

test_that("Can select data files with tidyselect", {
  nhgis1 <- read_nhgis(
    nhgis_multi_ds,
    file_select = contains("ds239"),
    verbose = FALSE
  )

  nhgis2 <- read_nhgis(
    nhgis_multi_ds,
    file_select = contains("ts_nominal"),
    verbose = FALSE
  )

  expect_equal(dim(nhgis1), c(1, 115))
  expect_equal(dim(nhgis2), c(84, 28))

  expect_false(identical(nhgis1, nhgis2))

  expect_true("AJWBE001" %in% colnames(nhgis1))
  expect_true("A00AA1790" %in% colnames(nhgis2))

  expect_equal(
    attributes(nhgis1$GISJOIN),
    list(label = "GIS Join Match Code", var_desc = "")
  )

  expect_equal(
    attributes(nhgis2$GISJOIN),
    list(label = "GIS Join Match Code", var_desc = "")
  )
})

test_that("Can still find codebook if file_select matches data file only", {
  nhgis <- read_nhgis(
    nhgis_multi_ds,
    file_select = matches("nation.csv"),
    verbose = FALSE
  )

  nhgis2 <- read_nhgis(
    nhgis_multi_ds,
    file_select = "nhgis0731_csv/nhgis0731_ds239_20185_nation.csv",
    verbose = FALSE
  )

  expect_true(all.equal(nhgis, nhgis2))

  expect_equal(
    attributes(nhgis$GISJOIN),
    list(label = "GIS Join Match Code", var_desc = "")
  )
})

# Can read data when provided in zip, dir, and shp formats -------

test_that("We can pass arguments to underlying reader functions", {
  expect_silent(
    nhgis_data <- read_nhgis(
      nhgis_single_csv,
      verbose = FALSE,
      col_names = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      skip = 1,
      remove_extra_header = TRUE
    )
  )

  expect_warning(
    nhgis_data_fwf <- read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      verbose = FALSE,
      col_positions = readr::fwf_widths(c(4, 2, 2, 1)),
      col_types = "ccil"
    ),
    "Data loaded from NHGIS fixed-width files may not be consistent"
  )

  nhgis_data2 <- read_nhgis(
    nhgis_single_csv,
    verbose = FALSE,
    col_names = c("A", "B"),
    remove_extra_header = FALSE
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

  expect_equal(colnames(nhgis_data2)[1:2], c("A", "B"))
  expect_equal(nhgis_data2[[1]][1], "GIS Join Match Code")
})

test_that("We get informative error messages when reading NHGIS extracts", {
  expect_error(
    read_nhgis("FAKE_FILE.zip", verbose = FALSE),
    "Could not find file"
  )

  expect_error(
    read_nhgis("~/Desktop/FAKE_FILE.zip", verbose = FALSE),
    "Could not find file .+/Desktop/FAKE_FILE.zip"
  )

  expect_error(
    nhgis <- read_nhgis(nhgis_multi_ds, verbose = FALSE),
    "Multiple files found, please use the `file_select`"
  )
  expect_error(
    nhgis <- read_nhgis(
      nhgis_multi_ds,
      file_select = contains("nhgis"),
      verbose = FALSE
    ),
    "Multiple files found, please use the `file_select`"
  )
  expect_error(
    nhgis <- read_nhgis(
      nhgis_multi_ds,
      file_select = 1:2,
      verbose = FALSE
    ),
    "Multiple files found, please use the `file_select`"
  )

  expect_error(
    read_nhgis(nhgis_multi_ds, file_select = 3, verbose = FALSE),
    "Can't subset files past the end.+Available files:"
  )
  expect_error(
    read_nhgis(
      nhgis_multi_ds,
      file_select = contains("not-in-file"),
      verbose = FALSE
    ),
    "The provided `file_select` did not select any of the available files:"
  )
})

test_that("Can read NHGIS codebook", {
  expect_error(
    read_nhgis_codebook(nhgis_multi_ds),
    "Multiple files found"
  )

  # Single dataset ----------------

  d_csv <- read_nhgis(nhgis_single_csv, verbose = FALSE)

  cb_csv <- read_nhgis_codebook(nhgis_single_csv)

  # FWF with ACS ----------------

  # Includes multiple data types which changes codebook slightly

  d_fwf <- read_nhgis(
    nhgis_multi_fwf,
    file_select = 1,
    verbose = FALSE
  )

  cb_fwf <- read_nhgis_codebook(nhgis_multi_fwf, file_select = 1)

  # Time series table ----------

  d_fwf_tst <- read_nhgis(
    nhgis_multi_fwf,
    file_select = 2,
    verbose = FALSE
  )

  cb_fwf_ts <- read_nhgis_codebook(nhgis_multi_fwf, file_select = 2)

  expect_equal(cb_csv$var_info$var_name, colnames(d_csv))
  expect_equal(cb_fwf$var_info$var_name, colnames(d_fwf))
  expect_equal(cb_fwf_ts$var_info$var_name, colnames(d_fwf_tst))

  # Raw cb lines:
  expect_equal(
    length(read_nhgis_codebook(nhgis_single_csv, raw = TRUE)),
    117
  )

  # Confirm that data types and universe are added to labels
  expect_equal(
    attr(d_fwf$AJWBE009, "label"),
    "Estimates: Male: 21 years",
  )
  expect_equal(
    attr(d_fwf$AJWBE009, "var_desc"),
    "Table AJWB: Sex by Age (Universe: Total population)"
  )
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
  expect_equal(x1, x2)

  # Multiple files should throw same errors even if unzipped:
  readr::write_csv(tibble::tibble(a = c("a", "b")), csv_tmpfile2)

  expect_error(
    read_nhgis(tempdir()),
    "Multiple files found"
  )

  expect_equal(
    read_nhgis(tempdir(), file_select = 1, var_attrs = NULL),
    x1
  )

  # Both csv and dat is ambiguous:
  readr::write_csv(tibble::tibble(a = "a"), dat_tmpfile)

  expect_error(
    read_nhgis(tempdir()),
    "Both .csv and .dat files found.+Use the `file_type` argument"
  )

  suppressWarnings(
    x3 <- read_nhgis(tempdir(), file_type = "csv", file_select = 1)
  )

  expect_equal(x3, x1)

  # Direct data file errors if mismatched extension
  expect_error(
    read_nhgis_fwf(csv_tmpfile1),
    "Expected `file` to match extension"
  )

  # Reading a direct data file should ignore data layer
  expect_equal(
    suppressWarnings(read_nhgis(csv_tmpfile1, file_select = 3)),
    x2
  )
})
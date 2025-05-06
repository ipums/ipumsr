test_that("read_nhgis() is deprecated", {
  lifecycle::expect_deprecated(
    read_nhgis(ipums_example("nhgis0972_csv.zip"), verbose = FALSE)
  )
})

test_that("Can read NHGIS extract: single dataset", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_single_csv <- ipums_example("nhgis0972_csv.zip")

  # Use snapshot since several forms of output are produced by default
  # (IPUMS conditions, col spec, and additional col spec info)
  expect_snapshot(
    nhgis_csv <- read_nhgis(nhgis_single_csv)
  )

  expect_equal(nrow(nhgis_csv), 71)
  expect_equal(ncol(nhgis_csv), 25)
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
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")
  nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

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

  expect_error(
    read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      do_file = FALSE,
      verbose = FALSE
    )
  )

  expect_error(
    read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      do_file = "asdf",
      verbose = FALSE
    ),
    "Could not find the provided `do_file`."
  )

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
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")
  nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

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

test_that("Can read IHGIS extract", {
  fp <- ipums_example("ihgis0014.zip")

  x <- read_ipums_agg(fp, file_select = matches("AAA_g0"), verbose = FALSE)

  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(1, 11))
  expect_equal(zap_ipums_attributes(x[["GISJOIN"]]), "KZ")
  expect_equal(zap_ipums_attributes(x[["g0"]]), "Kazakhstan")
  expect_true(all(fostr_detect(colnames(x)[3:ncol(x)], "AAA")))
  expect_identical(
    attributes(x$AAA001),
    list(
      label = "Total population : 1999",
      var_desc = "Table AAA: Urban and rural population (Universe: Total population)"
    )
  )
  expect_false(is.null(ipums_var_info(x)))

  x <- read_ipums_agg(
    fp,
    file_select = matches("AAA_g0"),
    vars = c(GISJOIN, "AAA003"),
    remove_extra_header = FALSE,
    var_attrs = "var_desc",
    verbose = FALSE
  )

  expect_s3_class(x, "tbl_df")
  expect_equal(dim(x), c(1, 2))
  expect_equal(zap_ipums_attributes(x[["GISJOIN"]]), "KZ")
  expect_equal(zap_ipums_attributes(x[["AAA003"]]), 106.9)
  expect_identical(
    attributes(x$AAA003),
    list(
      var_desc = "Table AAA: Urban and rural population (Universe: Total population)"
    )
  )
})

test_that("Can select data files from an extract", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")

  nhgis1 <- read_nhgis(nhgis_multi_ds, file_select = 1, verbose = FALSE)
  nhgis2 <- read_nhgis(nhgis_multi_ds, file_select = 2, verbose = FALSE)

  expect_equal(dim(nhgis1), c(1, 115))
  expect_equal(dim(nhgis2), c(84, 28))

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

  expect_identical(
    nhgis1,
    read_nhgis(
      nhgis_multi_ds,
      file_select = contains("ds239"),
      verbose = FALSE
    )
  )
  expect_identical(
    nhgis2,
    read_nhgis(
      nhgis_multi_ds,
      file_select = contains("ts_nominal"),
      verbose = FALSE
    )
  )
})

test_that("Don't show metadata files for IHGIS extract file selection", {
  fp <- ipums_example("ihgis0014.zip")

  expect_error(
    read_ipums_agg(fp),
    paste0(
      "Multiple files found.+KZ2009popAAA_g0.+KZ2009popAAA_g1.+",
      "KZ2009popAAB_g0.+KZ2009popAAB_g1"
    )
  )
  expect_error(
    read_ipums_agg(fp, file_select = 5),
    "There are only 4 files"
  )
  expect_error(
    read_ipums_agg(fp, file_select = matches("datadict")),
    "did not select any of the available files"
  )
})

test_that("Can still find codebook if file_select matches data file only", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")

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

test_that("Can still find codebook if direct data file path provided", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzipped <- utils::unzip(ipums_example("nhgis0972_csv.zip"), exdir = temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE, after = FALSE)

  data_file <- unzipped[fostr_detect(unzipped, ".csv$")]

  x <- read_nhgis(data_file, verbose = FALSE)

  expect_equal(
    ipums_var_info(x)$var_name,
    colnames(x)
  )

  expect_equal(
    ipums_var_info(x)$var_label,
    unname(purrr::map_chr(x, ~ attributes(.x)$label))
  )
})

test_that("We can specify available readr options in read_nhgis()", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

  expect_silent(
    nhgis_data <- read_nhgis(
      ipums_example("nhgis0972_csv.zip"),
      verbose = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      remove_extra_header = TRUE
    )
  )

  expect_warning(
    nhgis_data_fwf <- read_nhgis(
      nhgis_multi_fwf,
      file_select = 1,
      verbose = FALSE,
      vars = 1:4,
      col_types = "ccil"
    ),
    "One or more parsing issues"
  )

  nhgis_data_fwf2 <- read_nhgis(
    nhgis_multi_fwf,
    file_select = 1,
    verbose = FALSE,
    vars = c(YEAR, STUSAB)
  )

  expect_equal(nrow(nhgis_data), 71)
  expect_equal(ncol(nhgis_data), 25)
  expect_equal(unique(purrr::map_chr(nhgis_data, class)), "character")

  expect_equal(nrow(nhgis_data_fwf), 1)
  expect_equal(ncol(nhgis_data_fwf), 4)
  expect_equal(
    unname(purrr::map_chr(nhgis_data_fwf, class)),
    c("character", "character", "integer", "logical")
  )

  expect_equal(colnames(nhgis_data_fwf2), c("YEAR", "STUSAB"))

  expect_equal(
    nrow(
      read_nhgis(ipums_example("nhgis0972_csv.zip"), n_max = 10, verbose = FALSE)
    ),
    10
  )
})

test_that("We get informative error messages when reading NHGIS extracts", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  skip_if_not_installed("tidyselect", "1.2.1")

  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")
  nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

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
  expect_snapshot(
    read_nhgis(nhgis_multi_ds, file_select = 3, verbose = FALSE),
    error = TRUE
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
  rlang::local_options(lifecycle_verbosity = "quiet")

  nhgis_single_csv <- ipums_example("nhgis0972_csv.zip")
  nhgis_multi_ds <- ipums_example("nhgis0731_csv.zip")
  nhgis_multi_fwf <- ipums_example("nhgis0730_fixed.zip")

  expect_error(
    read_nhgis_codebook(nhgis_multi_ds),
    "Multiple files found"
  )

  # Single dataset

  d_csv <- read_nhgis(nhgis_single_csv, verbose = FALSE)

  cb_csv <- read_nhgis_codebook(nhgis_single_csv)

  # FWF with ACS

  # Includes multiple data types which changes codebook slightly

  d_fwf <- read_nhgis(
    nhgis_multi_fwf,
    file_select = 1,
    verbose = FALSE
  )

  cb_fwf <- read_nhgis_codebook(nhgis_multi_fwf, file_select = 1)

  # Time series table

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
    "Estimates: Male: 21 years"
  )
  expect_equal(
    attr(d_fwf$AJWBE009, "var_desc"),
    "Table AJWB: Sex by Age (Universe: Total population)"
  )
})

test_that("Can read IHGIS metadata from extract", {
  cb <- read_ihgis_codebook(ipums_example("ihgis0014.zip"))

  expect_s3_class(cb, "ipums_ddi")
  expect_equal(dim(cb$var_info), c(18, 10))
  expect_true(all(fostr_detect(cb$var_info$var_name, "GISJOIN|^g|AAA|AAB")))
  expect_true(
    all(
      fostr_detect(
        cb$var_info$var_desc[which(!fostr_detect(cb$var_info$var_name, "GISJOIN|^g"))]
        , "Table AA"
      )
    )
  )
  expect_equal(cb$var_info[4, ]$var_name, "AAA001")
  expect_equal(cb$var_info[4, ]$var_label, "Total population : 1999")
  expect_equal(cb$var_info[4, ]$var_desc, "Table AAA: Urban and rural population (Universe: Total population)")
})

test_that("Can read IHGIS metadata if no txt codebook", {
  fp <- vcr::vcr_test_path("fixtures", "ihgis0014")

  expect_warning(
    cb <- read_ihgis_codebook(fp),
    "Unable to load IPUMS conditions"
  )
  cb2 <- read_ihgis_codebook(ipums_example("ihgis0014.zip"))

  expect_null(cb$conditions)
  expect_false(is.null(cb2$conditions))
  expect_identical(cb$var_info, cb2$var_info)
})

test_that("Correctly handle missing files when loading IHGIS metadata", {
  fp1 <- vcr::vcr_test_path("fixtures", "ihgis0014")
  fp2 <- vcr::vcr_test_path("fixtures", "ihgis0014_incomplete")
  fp3 <- file.path(fp2, "ihgis0014_datadict.csv")

  expect_error(
    read_ihgis_codebook(ipums_example("nhgis0972_csv.zip")),
    "Could not find `_datadict\\.csv`"
  )
  expect_warning(
    d1 <- read_ihgis_codebook(fp1),
    "Unable to load IPUMS conditions"
  )
  expect_error(
    read_ihgis_codebook(fp2),
    "Could not find `_tables.csv`.+Use `tbls_file`"
  )
  expect_warning(
    lifecycle::expect_deprecated(read_ipums_agg(fp2, verbose = FALSE)),
    "Unable to read codebook"
  )
  expect_error(read_ihgis_codebook(fp3), "Could not find `_tables\\.csv`")
  expect_error(
    read_ihgis_codebook(fp2, tbls_file = "foobar/ihgis0014_tables.csv"),
    "Could not find file `foobar/ihgis0014_tables.csv`"
  )

  expect_warning(
    d2 <- read_ihgis_codebook(
      fp2,
      tbls_file = file.path(fp1, "ihgis0014_tables.csv")
    ),
    "Unable to load tabulation geography metadata for this file"
  )

  expect_equal(
    setdiff(d1$var_info$var_name, d2$var_info$var_name),
    c("g0", "g1")
  )
  expect_equal(dim(d1$var_info), c(18, 10))
  expect_equal(dim(d2$var_info), c(16, 10))
  expect_true(!is.null(d2$conditions))
})

test_that("Error when providing cb or data file to wrong IHGIS reader", {
  fp <- vcr::vcr_test_path("fixtures", "ihgis0014")

  expect_error(
    read_ihgis_codebook(file.path(fp, "KZ2009popAAA_g0.csv")),
    "Expected `cb_file` to be a zipped IPUMS extract or a `_datadict\\.csv`"
  )
  expect_error(
    read_ipums_agg(file.path(fp, "ihgis0014_datadict.csv")),
    "Unexpected data file"
  )
})

test_that("Can read raw IHGIS codebook", {
  fp <- vcr::vcr_test_path("fixtures", "ihgis0014_incomplete")

  expect_error(
    read_ihgis_codebook(file.path(fp, "ihgis0014_codebook.txt")),
    "Expected `cb_file` to be a zipped IPUMS extract or a `_datadict\\.csv`"
  )

  x <- read_ihgis_codebook(ipums_example("ihgis0014.zip"), raw = TRUE)

  expect_equal(length(x), 70)
  expect_equal(class(x), "character")
})

test_that("We get relevant subset of IHGIS metadata after attaching to data", {
  fp <- ipums_example("ihgis0014.zip")

  d1 <- read_ipums_agg(fp, file_select = 1, verbose = FALSE)
  d2 <- read_ipums_agg(fp, file_select = 1, verbose = FALSE, var_attrs = NULL)
  c1 <- read_ihgis_codebook(fp)

  expect_true(all(is.na(ipums_var_info(d2)$var_label)))

  expect_equal(dim(ipums_var_info(c1)), c(18, 10))
  expect_equal(nrow(ipums_var_info(d1)), ncol(d1))
  expect_equal(nrow(ipums_var_info(d1)), 11)

  expect_true(all(ipums_var_info(d1)$var_name %in% colnames(d1)))
  expect_true(
    all(
      fostr_detect(
        setdiff(ipums_var_info(c1)$var_name, ipums_var_info(d1)$var_name),
        "g1|^AAB"
      )
    )
  )
})

test_that("Can read unzipped NHGIS files", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  test_path <- vcr::vcr_test_path("fixtures", "nhgis_unzipped")

  x1 <- suppressWarnings(read_nhgis(test_path, file_select = 1, verbose = FALSE))
  x2 <- read_nhgis(
    file.path(test_path, "test1.csv"),
    var_attrs = NULL,
    verbose = FALSE
  )

  expect_equal(x1$a, "b")
  expect_equal(x1, x2)
})

test_that("Can read unzipped IHGIS files", {
  fp <- vcr::vcr_test_path("fixtures", "ihgis0014")

  # This is because this test case uses a file path with no txt codebook
  expect_warning(
    d <- read_ipums_agg(file.path(fp, "KZ2009popAAB_g1.csv"), verbose = FALSE),
    "Unable to load IPUMS conditions"
  )

  expect_identical(
    d,
    read_ipums_agg(
      ipums_example("ihgis0014.zip"),
      file_select = "KZ2009popAAB_g1.csv",
      verbose = FALSE
    )
  )
  expect_error(
    read_ipums_agg(file.path(fp, "foobar.csv"), verbose = FALSE),
    "Could not find file"
  )

  # Dir reading is currently deprecated but for now it should still work
  suppressWarnings(
    d2 <- read_ipums_agg(fp, file_select = matches("AAB_g1"), verbose = FALSE)
  )

  expect_identical(d, d2)
})

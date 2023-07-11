# Manually set these constants...
rows_h <- 3385
rows_p <- 7668

vars_h <- 6
vars_p <- 6
vars_all <- 9
vars_rect <- 8

YEAR_label <- "Survey year"
YEAR_var_desc <- paste0(
  "YEAR reports the year in which the survey was conducted.  ",
  "YEARP is repeated on person records."
)
STATEFIP_val_labels <- c(Alabama = 1, Alaska = 2)
YEAR_first5_values <- rep(1962, 5)

# imp_dec
ASECWTH_first5_values <- c(1475.59, 1475.59, 1475.59, 1597.61, 1706.65)

test_that("Can read Rectangular .dat.gz", {
  cps <- read_ipums_micro(
    ipums_example("cps_00157.xml"),
    data_file = ipums_example("cps_00157.dat.gz"),
    verbose = FALSE
  )

  expect_equal(nrow(cps), rows_p)
  expect_equal(ncol(cps), vars_rect)
  expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  expect_equal(cps$YEAR[1:5], YEAR_first5_values)
  expect_equal(cps$ASECWTH[1:5], ASECWTH_first5_values)
})

test_that("Can read Rectangular .dat", {
  temp_file <- paste0(tempfile(), ".dat")

  temp <- readr::read_lines(ipums_example("cps_00157.dat.gz"))
  readr::write_lines(temp, temp_file)
  on.exit(unlink(temp_file), add = TRUE, after = FALSE)

  cps <- read_ipums_micro(
    ipums_example("cps_00157.xml"),
    data_file = temp_file,
    verbose = FALSE
  )

  expect_equal(nrow(cps), rows_p)
  expect_equal(ncol(cps), vars_rect)
  expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  expect_equal(cps$YEAR[1:5], YEAR_first5_values)
  expect_equal(cps$ASECWTH[1:5], ASECWTH_first5_values)
})

test_that("Can read Rectangular .csv.gz", {
  cps <- read_ipums_micro(
    ipums_example("cps_00158.xml"),
    data_file = ipums_example("cps_00158.csv.gz"),
    verbose = FALSE
  )

  expect_equal(nrow(cps), rows_p)
  expect_equal(ncol(cps), vars_rect)
  expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  expect_equal(cps$YEAR[1:5], YEAR_first5_values)
  expect_equal(cps$ASECWTH[1:5], ASECWTH_first5_values)
})

test_that("Can read Hierarchical into long format", {
  cps <- read_ipums_micro(
    ipums_example("cps_00159.xml"),
    verbose = FALSE
  )

  expect_equal(nrow(cps), rows_h + rows_p)
  expect_equal(ncol(cps), vars_all)
  expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  expect_equal(cps$YEAR[1], YEAR_first5_values[1])
  expect_equal(cps$ASECWTH[1], ASECWTH_first5_values[1])
})

test_that("Can read Hierarchical into list format", {
  cps <- read_ipums_micro_list(
    ipums_example("cps_00159.xml"),
    verbose = FALSE
  )

  expect_equal(nrow(cps$HOUSEHOLD), rows_h)
  expect_equal(nrow(cps$PERSON), rows_p)
  expect_equal(ncol(cps$HOUSEHOLD), vars_h)
  expect_equal(ncol(cps$PERSON), vars_p)
  expect_equal(attr(cps$HOUSEHOLD[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps$PERSON[["YEAR"]], "label"), YEAR_label)
  expect_equal(attr(cps$HOUSEHOLD[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(attr(cps$PERSON[["YEAR"]], "var_desc"), YEAR_var_desc)
  expect_equal(
    attr(cps$HOUSEHOLD[["STATEFIP"]], "labels")[1:2],
    STATEFIP_val_labels
  )
  expect_equal(cps$HOUSEHOLD$YEAR[1], YEAR_first5_values[1])
  expect_equal(cps$HOUSEHOLD$ASECWTH[1], ASECWTH_first5_values[1])
})

test_that("Can't read Rectangular into list format", {
  temp_file <- paste0(tempfile(), ".dat")

  temp <- readr::read_lines(ipums_example("cps_00157.dat.gz"))
  readr::write_lines(temp, temp_file)
  on.exit(unlink(temp_file), add = TRUE, after = FALSE)

  expect_error(
    cps_list <- read_ipums_micro_list(
      ipums_example("cps_00157.xml"),
      data_file = temp_file,
      verbose = FALSE
    ),
    regexp = "must be hierarchical"
  )
})

test_that("Arguments n_max and vars work", {
  cps <- read_ipums_micro(
    ipums_example("cps_00159.xml"),
    n_max = 100,
    vars = c(RECTYPE, STATEFIP),
    verbose = FALSE
  )
  expect_equal(nrow(cps), 100)
  expect_equal(ncol(cps), 2)
})

test_that("Arguments n_max and vars work for csv files (#26)", {
  cps <- read_ipums_micro(
    ipums_example("cps_00158.xml"),
    ipums_example("cps_00158.csv.gz"),
    n_max = 100,
    vars = c(YEAR, SERIAL),
    verbose = FALSE
  )
  expect_equal(nrow(cps), 100)
  expect_equal(ncol(cps), 2)
})

test_that("Setting argument var_attrs to NULL works", {
  cps <- read_ipums_micro(
    ipums_example("cps_00157.xml"),
    data_file = ipums_example("cps_00157.dat.gz"),
    verbose = FALSE,
    var_attrs = NULL
  )

  no_var_attrs <- purrr::map_lgl(cps, ~ is.null(attributes(.)))
  expect_true(all(no_var_attrs))
})

test_that("Informative error when no ddi file", {
  expect_error(
    read_ipums_micro("FAKE_FILE.xml"),
    "Could not find file .+/FAKE_FILE.xml`"
  )
  expect_error(
    read_ipums_micro("C:/FAKE_FOLDER/FAKE_FILE.xml"),
    "Could not find file `C:/FAKE_FOLDER/FAKE_FILE.xml`"
  )
})

test_that("keyvar is loaded regardless of selection in hierarchical", {
  cps <- read_ipums_micro_list(
    ipums_example("cps_00159.xml"),
    verbose = FALSE,
    vars = c(STATEFIP, INCTOT)
  )
  expect_true("SERIAL" %in% names(cps$HOUSEHOLD))
  expect_true("SERIAL" %in% names(cps$PERSON))
})


test_that("Don't duplicate rectype vars in ATUS hierarchical extracts (#43)", {
  ddi_file <- file.path(vcr::vcr_test_path("fixtures"), "atus_00025.xml")
  ddi <- read_ipums_ddi(ddi_file)

  data <- dplyr::tibble(
    RECTYPE = c(1, 1, 2, 3, 3, 3),
    REGION = c(1, 2, 3, 4, 1, 2),
    x = 1:6
  )

  out <- set_ipums_var_attributes(data, ddi, var_attrs = "val_labels")
  # Removed duplicates from rectype
  expect_equal(
    ipums_val_labels(out$RECTYPE),
    tibble::tibble(
      val = as.numeric(1:5),
      lbl = c(
        "Household Record", "Person Record", "Activity Record",
        "Who Record", "Elder Care Record"
      )
    )
  )
  # Didn't affect the region labels
  expect_equal(
    ipums_val_labels(out$REGION),
    tibble::tibble(
      val = as.numeric(1:4),
      lbl = c("Northeast", "Midwest", "South", "West")
    )
  )
})


test_that("Empty vars selection is handled", {
  expect_error(
    cps <- read_ipums_micro(
      ipums_example("cps_00157.xml"),
      data_file = ipums_example("cps_00157.dat.gz"),
      vars = starts_with("MISSING"),
      verbose = FALSE
    ),
    regexp = "did not match any variables"
  )
})

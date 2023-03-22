library(dplyr)
library(purrr)


# Setup ----
usa_extract <- define_extract_usa(
  samples = "us2017b",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

cps_extract <- define_extract_cps(
  samples = c("cps1976_01s", "cps1976_02b"),
  variables = c("YEAR", "MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1"),
  description = "Compare age-sex-race breakdowns 1976",
  data_format = "fixed_width"
)

# Tests ----

# > Define extract ----
test_that("Can define a USA extract", {
  expect_s3_class(usa_extract, "usa_extract")
  expect_s3_class(usa_extract, "ipums_extract")
  expect_equal(usa_extract$variables[[1]], "YEAR")
  expect_equal(usa_extract$data_structure, "rectangular")
  expect_equal(usa_extract$rectangular_on, "P")
  expect_identical(usa_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(usa_extract$submitted)
  expect_equal(usa_extract$number, NA_integer_)
  expect_equal(usa_extract$status, "unsubmitted")
})


test_that("Can define a CPS extract", {
  expect_s3_class(cps_extract, "cps_extract")
  expect_s3_class(cps_extract, "ipums_extract")
  expect_equal(cps_extract$variables[[1]], "YEAR")
  expect_equal(cps_extract$data_structure, "rectangular")
  expect_equal(cps_extract$rectangular_on, "P")
  expect_identical(cps_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(cps_extract$submitted)
  expect_equal(cps_extract$number, NA_integer_)
  expect_equal(cps_extract$status, "unsubmitted")
})

test_that("Attempt to define a hierarchical extract throws an error", {
  expect_error(
    define_extract_usa(
      "Test", "us2017b", "YEAR",
      data_structure = "hierarchical"
    ),
    regexp = "must be equal to \"rectangular\""
  )
  expect_error(
    define_extract_cps(
      "Test", "us2017b", "YEAR",
      rectangular_on = "H"
    ),
    "Currently, the `rectangular_on` argument must be equal to \"P\""
  )
})


test_that("Attempt to rectangularize on H records throws an error", {
  expect_error(
    define_extract_usa(
      "Test", "us2017b", "YEAR",
      rectangular_on = "H"
    ),
    regexp = "`rectangular_on` argument must be equal to \"P\""
  )
})


test_that("ipums_extract validate method works", {
  expect_identical(validate_ipums_extract(usa_extract), usa_extract)
  expect_error(
    validate_ipums_extract(define_extract_usa()),
    "argument \"description\" is missing"
  )
  expect_error(
    validate_ipums_extract(define_extract_cps()),
    "argument \"description\" is missing"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa"
      )
    ),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = ""
      )
    ),
    paste0(
      "`data_structure` must not contain missing.+",
      "`data_format` must not contain missing.+",
      "`samples` must not contain missing.+",
      "`variables` must not contain missing.+"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "Test",
        samples = "Test",
        variables = "Test",
        data_format = "Test",
        data_structure = "Test"
      )
    ),
    paste0(
      "`data_structure` must be one of.+",
      "`data_format` must be one of "
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = "",
        data_structure = "hierarchical",
        rectangular_on = "B",
        samples = "Test",
        variables = "Test",
        data_format = "csv"
      )
    ),
    paste0(
      "`rectangular_on` must be missing when ",
      "`data_structure != \"rectangular\"`."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = "",
        data_structure = "rectangular",
        samples = "Test",
        variables = "Test",
        data_format = "csv"
      )
    ),
    paste0(
      "`rectangular_on` must not contain missing values when ",
      "`data_structure == \"rectangular\"`."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        data_structure = "hierarchical",
        samples = list("A"),
        variables = list("B"),
        data_format = "csv"
      )
    ),
    paste0(
      "`samples` must be of type `character`, not `list`.+",
      "`variables` must be of type `character`, not `list`"
    )
  )
})

test_that("extract_list_from_json reproduces extract specs", {
  usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")
  cps_json <- new_ipums_json(extract_to_request_json(cps_extract), "cps")

  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_s3_class(cps_json, c("cps_json", "ipums_json"))
  expect_identical(
    extract_list_from_json(usa_json)[[1]],
    usa_extract
  )
  expect_identical(
    extract_list_from_json(cps_json)[[1]],
    cps_extract
  )
})

# > Add to / remove from extract ----
test_that("Can add to a USA extract", {
  revised_extract <- add_to_extract(
    usa_extract,
    samples = "us2014a",
    variables = "RELATE"
  )
  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(
    revised_extract$samples,
    union(usa_extract$samples, "us2014a")
  )
  expect_equal(
    revised_extract$variables,
    union(usa_extract$variables, "RELATE")
  )
})


test_that("Can add to a CPS extract", {
  revised_extract <- add_to_extract(
    cps_extract,
    samples = "cps2019_03s",
    variables = "RELATE"
  )
  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(
    revised_extract$samples,
    union(cps_extract$samples, "cps2019_03s")
  )
  expect_equal(
    revised_extract$variables,
    union(cps_extract$variables, "RELATE")
  )
})

test_that("Can remove from a USA extract", {
  revised_extract <- add_to_extract(
    usa_extract,
    samples = "us2014a",
    variables = c("RELATE", "AGE", "SEX")
  )

  revised_extract <- remove_from_extract(
    revised_extract,
    samples = "us2017b",
    variables = c("AGE", "SEX")
  )

  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(revised_extract$samples, "us2014a")
  expect_equal(
    revised_extract$variables,
    c("YEAR", "RELATE")
  )
})


test_that("Can remove from a CPS extract", {
  revised_extract <- remove_from_extract(
    cps_extract,
    samples = "cps1976_02b",
    variables = c("MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1")
  )

  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(revised_extract$samples, "cps1976_01s")
  expect_equal(
    revised_extract$variables,
    c("YEAR")
  )
})


test_that("Unused revisions do not alter USA extract", {
  expect_identical(usa_extract, add_to_extract(usa_extract))
  expect_identical(usa_extract, remove_from_extract(usa_extract))
  expect_identical(
    usa_extract,
    suppressWarnings(
      add_to_extract(
        usa_extract,
        samples = usa_extract$samples
      )
    )
  )
  expect_identical(
    usa_extract,
    suppressWarnings(
      remove_from_extract(
        usa_extract,
        variables = "not in extract"
      )
    )
  )
})


test_that("Unused revisions do not alter CPS extract", {
  expect_identical(cps_extract, add_to_extract(cps_extract))
  expect_identical(cps_extract, remove_from_extract(cps_extract))
  expect_identical(
    cps_extract,
    suppressWarnings(
      add_to_extract(
        cps_extract,
        samples = cps_extract$samples
      )
    )
  )
  expect_identical(
    cps_extract,
    suppressWarnings(
      remove_from_extract(
        cps_extract,
        variables = "not in extract"
      )
    )
  )
})


test_that("Improper extract revisions throw warnings or errors", {
  expect_silent(
    add_to_extract(usa_extract, samples = "us2017b")
  )
  expect_silent(
    remove_from_extract(cps_extract, variables = "RELATE")
  )
  expect_warning(
    remove_from_extract(usa_extract,
      description = "description",
      invalid = "invalid"
    ),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed:.+`description`, `invalid`.+Use ",
      "`add_to_extract\\(\\)`"
    )
  )
  expect_warning(
    add_to_extract(cps_extract,
      description = "description",
      invalid = "invalid"
    ),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified:.+`invalid`"
    )
  )
  expect_silent(
    add_to_extract(cps_extract, description = "description")
  )
  expect_error(
    remove_from_extract(
      usa_extract,
      samples = usa_extract$samples,
      variables = usa_extract$variables
    ),
    paste0(
      "`samples` must not contain missing values.+",
      "`variables` must not contain missing values.+"
    )
  )
  expect_error(
    add_to_extract(usa_extract, data_format = "bad_format"),
    "`data_format` must be one of"
  )
  expect_error(
    add_to_extract(
      usa_extract,
      samples = "New Sample",
      data_format = c("csv", "bad_format"),
    ),
    "`data_format` must be length 1"
  )
})

test_that("Can combine extracts", {
  x1 <- define_extract_usa(
    description = "Combining",
    samples = c("S1", "S2"),
    variables = "V1",
    data_format = "csv"
  )

  x2 <- define_extract_usa(
    description = "",
    samples = "S3",
    variables = "V2",
    data_format = "csv"
  )

  x <- combine_extracts(x1, x2)

  expect_identical(
    x,
    define_extract_usa(
      description = "Combining",
      samples = c("S1", "S2", "S3"),
      variables = c("V1", "V2"),
      data_format = "csv"
    )
  )
})

# > Save as / define from JSON ----
test_that("We can export to and import from JSON", {
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(usa_extract, json_tmpfile)
  copy_of_usa_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(usa_extract, copy_of_usa_extract)
})

# Setup ------------------------------------------------------------------------

nhgis_extract <- define_extract_nhgis(
  description = "Extract for R client testing",
  dataset = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
  data_tables = c("B01001", "B01002"),
  time_series_table = "CW3",
  geog_levels = list("nation", "blck_grp", "state"),
  geographic_extents = c("110", "Pennsylvania"),
  tst_layout = "time_by_row_layout",
  shapefiles = "110_blck_grp_2019_tl2019",
  data_format = "csv_no_header"
)

nhgis_extract_shp <- define_extract_nhgis(
  shapefiles = "110_blck_grp_2019_tl2019"
)

# Tests ------------------------------------------------------------------------

# > Defining Extracts ----------------------------

test_that("Can define an NHGIS extract", {
  expect_s3_class(nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(
    nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    nhgis_extract$data_tables,
    list(
      "2014_2018_ACS5a" = c("B01001", "B01002"),
      "2015_2019_ACS5a" = c("B01001", "B01002")
    )
  )
  expect_equal(
    nhgis_extract$geog_levels,
    list(
      "2014_2018_ACS5a" = "nation",
      "2015_2019_ACS5a" = "blck_grp",
      "CW3" = "state"
    )
  )
  expect_equal(
    nhgis_extract$years,
    list(
      "2014_2018_ACS5a" = NULL,
      "2015_2019_ACS5a" = NULL
    )
  )
  expect_equal(
    nhgis_extract$breakdown_values,
    list(
      "2014_2018_ACS5a" = NULL,
      "2015_2019_ACS5a" = NULL
    )
  )
  expect_equal(nhgis_extract$time_series_tables, "CW3")
  expect_equal(nhgis_extract$tst_layout, "time_by_row_layout")
  expect_equal(nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(nhgis_extract$data_format, "csv_no_header")
  expect_identical(nhgis_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(nhgis_extract$submitted)
  expect_equal(nhgis_extract$number, NA_integer_)
  expect_equal(nhgis_extract$status, "unsubmitted")
  expect_true(is.null(nhgis_extract_shp$datasets))
})

test_that("NHGIS extracts get correct default values", {
  expect_equal(
    nhgis_extract$breakdown_and_data_type_layout,
    "single_file"
  )
  expect_equal(
    define_extract_nhgis(
      time_series_tables = "A00",
      geog_levels = "A1"
    )$tst_layout,
    "time_by_column_layout"
  )
  expect_equal(
    nhgis_extract_shp$breakdown_and_data_type_layout,
    NULL
  )
  expect_equal(
    nhgis_extract_shp$data_format,
    NULL
  )
})

test_that("nhgis_extract validate method works", {
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis")),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = c("A00", "A01"),
        data_tables = NA,
        geog_levels = list(NULL, "b"),
      )
    ),
    paste0(
      "All `datasets` must be associated with a value in `data_tables`.+",
      "All `datasets`, `time_series_tables` must be associated with a value in",
      " `geog_levels`.+",
      "`data_format` must not contain missing values when any `datasets` ",
      "or `time_series_tables` are specified."
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      time_series_tables = "Test",
      geog_levels = NA
    ),
    paste0(
      "`geog_levels` must not contain missing values for any of the ",
      "provided `datasets` or `time_series_tables`."
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      datasets = "Test",
      data_tables = "Test",
      geog_levels = "Test",
      data_format = "Test"
    ),
    "`data_format` must be one of"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = NULL
      )
    ),
    paste0(
      "`description` must not contain missing values"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        geog_levels = "Test"
      )
    ),
    paste0(
      "`geog_levels` must be missing when no `datasets` or ",
      "`time_series_tables` are specified"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = c("A00", "A01"),
        geog_levels = list(A00 = "a", A01 = "b", A01 = "c"),
        data_format = "csv_no_header"
      )
    ),
    paste0(
      "`geog_levels` must not be greater than the number of ",
      "`datasets` \\+ `time_series_tables`.+",
      "`tst_layout` must not contain missing values when any ",
      "`time_series_tables` are specified."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        data_format = "csv_header"
      )
    ),
    "`data_format` must be missing when no `datasets` or `time_series_tables`",
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = NA,
        time_series_tables = c("CW3", NA)
      )
    ),
    "None of `datasets`.+can contain missing values"
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = "A",
      geog_levels = "A",
      tst_layout = c("time_by_row_layout", "time_by_row_layout")
    ),
    paste0(
      "`tst_layout` must be length 1."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = "A",
        data_tables = c(A = "A"),
        geog_levels = list(A = "A"),
        data_format = "csv_no_header"
      )
    ),
    "`data_tables` must be of type `list`, not `character`"
  )
})

test_that("extract_list_from_json reproduces extract specs", {
  nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))
  expect_identical(
    extract_list_from_json(nhgis_json)[[1]],
    nhgis_extract
  )
})

# > Revising ------------------------------------

test_that("Can add new fields to an NHGIS extract", {
  revised <- add_to_extract(
    nhgis_extract,
    datasets = "New",
    data_tables = c("T1", "T2"),
    time_series_tables = c("CW4", "CW5"),
    geog_levels = list("G1", "G1", "G2"),
    years = "Y1",
    data_format = "csv_header",
    shapefiles = "New"
  )

  expect_equal(revised$datasets, c(nhgis_extract$datasets, "New"))
  expect_equal(
    revised$data_tables,
    c(nhgis_extract$data_tables, list(New = c("T1", "T2")))
  )
  expect_equal(
    revised$geog_levels,
    list(
      `2014_2018_ACS5a` = "nation",
      `2015_2019_ACS5a` = "blck_grp",
      New = "G1",
      CW3 = "state",
      CW4 = "G1",
      CW5 = "G2"
    )
  )
  expect_equal(
    revised$years,
    c(nhgis_extract$years, list(New = "Y1"))
  )
  expect_equal(
    revised$time_series_tables,
    c(nhgis_extract$time_series_tables, c("CW4", "CW5"))
  )
  expect_equal(revised$data_format, "csv_header")
  expect_equal(revised$shapefiles, c(nhgis_extract$shapefiles, "New"))
})

test_that("Can add subfields to existing parent fields", {
  revised <- add_to_extract(
    nhgis_extract,
    data_tables = c("T1", "T2"),
    time_series_tables = c("CW3", "CW4"),
    geog_levels = list(
      `2015_2019_ACS5a` = "G1",
      CW3 = "G1",
      CW4 = "G2"
    ),
    years = list("Y1", "Y2")
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$data_tables,
    list(
      `2014_2018_ACS5a` = c("B01001", "B01002", "T1", "T2"),
      `2015_2019_ACS5a` = c("B01001", "B01002", "T1", "T2")
    )
  )
  expect_equal(revised$time_series_tables, c("CW3", "CW4"))
  expect_equal(
    revised$geog_levels,
    list(
      `2014_2018_ACS5a` = c("nation"),
      `2015_2019_ACS5a` = c("blck_grp", "G1"),
      CW3 = c("state", "G1"),
      CW4 = "G2"
    )
  )
  expect_equal(
    revised$years,
    list(
      `2014_2018_ACS5a` = "Y1",
      `2015_2019_ACS5a` = "Y2"
    )
  )

  # Recycling handling for extracts with DS + TST
  expect_equal(
    add_to_extract(
      nhgis_extract,
      time_series_tables = "CW3",
      geog_levels = "C"
    )$geog_levels,
    list(
      "2014_2018_ACS5a" = "nation",
      "2015_2019_ACS5a" = "blck_grp",
      "CW3" = c("state", "C")
    )
  )

  expect_equal(
    add_to_extract(
      nhgis_extract,
      datasets = "2014_2018_ACS5a",
      time_series_tables = "CW3",
      geog_levels = "C"
    )$geog_levels,
    list(
      "2014_2018_ACS5a" = c("nation", "C"),
      "2015_2019_ACS5a" = "blck_grp",
      "CW3" = c("state", "C")
    )
  )
})

test_that("Can remove full fields from an NHGIS extract", {
  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2015_2019_ACS5a",
    time_series_tables = "CW3",
    geographic_extents = "DC"
  )

  expect_equal(revised$datasets, "2014_2018_ACS5a")

  expect_equal(
    revised$data_tables,
    list(`2014_2018_ACS5a` = c("B01001", "B01002"))
  )
  expect_equal(
    revised$geog_levels,
    list(`2014_2018_ACS5a` = c("nation"))
  )
  expect_equal(
    revised$years,
    list(`2014_2018_ACS5a` = NULL)
  )

  expect_null(revised$time_series_tables)
  expect_null(revised$tst_layout)

  expect_equal(revised$geographic_extents, "PA")
})

test_that("Can remove subfields from an NHGIS extract", {
  revised <- remove_from_extract(
    add_to_extract(
      nhgis_extract,
      geog_levels = list(`2014_2018_ACS5a` = "tract", CW3 = "tract"),
    ),
    data_tables = "B01001",
    geog_levels = list(
      `2014_2018_ACS5a` = "nation",
      CW3 = c("state", "blck_grp")
    ),
    geographic_extents = "420"
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$data_tables,
    list(
      `2014_2018_ACS5a` = "B01002",
      `2015_2019_ACS5a` = "B01002"
    )
  )
  expect_equal(revised$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(
    revised$geog_levels,
    list(
      `2014_2018_ACS5a` = "tract",
      `2015_2019_ACS5a` = "blck_grp",
      CW3 = "tract"
    )
  )
  expect_equal(revised$geographic_extents, "DC")
})

test_that("Removing parent fields occurs before evaluating subfields", {
  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2014_2018_ACS5a",
    data_tables = "B01001"
  )

  expect_equal(revised$datasets, "2015_2019_ACS5a")
  expect_equal(revised$data_tables, list(`2015_2019_ACS5a` = "B01002"))
})

test_that("Revisions do not alter unspecified extract fields", {
  extract1 <- define_extract_nhgis(
    shapefiles = "Test"
  )

  extract2 <- add_to_extract(
    extract1,
    datasets = "Test",
    data_tables = "Test",
    geog_levels = "Test"
  )

  # Test on an extract of multiple types
  expect_identical(nhgis_extract, add_to_extract(nhgis_extract))
  expect_identical(nhgis_extract, remove_from_extract(nhgis_extract))

  # Test on an extract of a single type
  expect_identical(extract1, add_to_extract(extract1))
  expect_identical(extract1, remove_from_extract(extract1))

  expect_null(extract2$time_series_tables)
  expect_null(extract2$tst_layout)

  expect_error(
    add_to_extract(extract1, geog_levels = "Test"),
    "`geog_levels` must be missing when no `datasets` or `time_series_tables`"
  )
})

test_that("Improper extract revisions throw warnings or errors", {
  expect_error(
    remove_from_extract(
      nhgis_extract,
      datasets = "2014_2018_ACS5a",
      data_tables = list("a", "b"),
      geog_levels = list(
        "2014_2018_ACS5a" = "a",
        "not_in_extract" = "blck_grp"
      ),
      years = list(C = "c")
    ),
    paste0(
      "`data_tables` must not be greater than the number of ",
      "`datasets`.+",
      "All names in `geog_levels` must be present in this extract\'s ",
      "`datasets`, `time_series_tables`.+",
      "All names in `years` must be present in this extract\'s `datasets`"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      datasets = c("New"),
      data_tables = list("A", "B"),
      geog_levels = list(New = "A", not_in_extract = "blck_grp"),
      years = list(C = "c")
    ),
    paste0(
      "`data_tables` must not be greater than the number of ",
      "`datasets`.+",
      "All names in `geog_levels` must be present in this extract\'s ",
      "`datasets`, `time_series_tables`.+",
      "All names in `years` must be present in this extract\'s `datasets`.+"
    )
  )
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      description = "Extract for R client testing",
      bad_field = "not in extract"
    ),
    regexp = paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed:.+`description`, `bad_field`"
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      geographic_extents = list("A", "B")
    ),
    "`geographic_extents` was provided as a list, but this parameter"
  )
  expect_silent(
    remove_from_extract(
      nhgis_extract,
      datasets = "DS",
      time_series_tables = "TST"
    )
  )
  expect_error(
    remove_from_extract(
      nhgis_extract,
      geog_levels = "nation"
    ),
    paste0(
      "`geog_levels` must not contain missing values"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract_shp,
      geog_levels = "nation"
    ),
    "`geog_levels` must be missing when no `datasets`"
  )
  expect_identical(
    remove_from_extract(
      nhgis_extract_shp,
      geog_levels = "nation"
    ),
    nhgis_extract_shp
  )
  expect_warning(
    add_to_extract(nhgis_extract, vars = "var", number = 45),
    paste0(
      "The following fields were either not found in the ",
      "provided extract or cannot be modified:.+`vars`, `number`"
    )
  )
  expect_identical(
    suppressWarnings(add_to_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
  expect_identical(
    suppressWarnings(remove_from_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
  expect_silent(
    add_to_extract(nhgis_extract, description = "Test")
  )
  expect_error(
    add_to_extract(nhgis_extract, data_format = c("csv_header", "fixed_width")),
    "`data_format` must be length 1"
  )
})

test_that("Can combine extracts", {
  x1 <- define_extract_nhgis(
    description = "Combining",
    shapefiles = "S1"
  )

  x2 <- define_extract_nhgis(
    datasets = c("A", "B"),
    data_tables = list(c("D1", "D2"), "D3"),
    time_series_tables = c("T1", "T2"),
    geog_levels = "G1",
    tst_layout = "time_by_file_layout"
  )

  x3 <- define_extract_nhgis(
    datasets = "C",
    data_tables = c("D2", "D4"),
    geog_levels = c("G2", "G3"),
    years = "Y1",
    shapefiles = "S1",
    data_format = "fixed_width",
  )

  x <- combine_extracts(x1, x2, x3)

  expect_identical(
    x,
    define_extract_nhgis(
      description = "Combining",
      datasets = c("A", "B", "C"),
      data_tables = list(c("D1", "D2"), "D3", c("D2", "D4")),
      years = list(C = "Y1"),
      time_series_tables = c("T1", "T2"),
      geog_levels = list(
        T1 = "G1", T2 = "G1", A = "G1", B = "G1", C = c("G2", "G3")
      ),
      tst_layout = "time_by_file_layout",
      shapefiles = "S1",
      data_format = "csv_no_header"
    )
  )

  expect_error(
    combine_extracts(
      x1, define_extract_usa("A", "B", "C")
    ),
    "same collection"
  )
})

# > JSON export -----------------------------------

test_that("We can export to and import from JSON for NHGIS", {
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(nhgis_extract, json_tmpfile)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(nhgis_extract, copy_of_nhgis_extract)
})

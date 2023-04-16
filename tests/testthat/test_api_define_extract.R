# Extract definitions ---------------------------

test_that("Can define a USA extract", {
  usa_extract <- test_usa_extract()

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
  cps_extract <- test_cps_extract()

  expect_s3_class(cps_extract, "cps_extract")
  expect_s3_class(cps_extract, "ipums_extract")

  expect_equal(cps_extract$variables[[1]], "YEAR")
  expect_equal(cps_extract$data_structure, "hierarchical")
  expect_null(cps_extract$rectangular_on)

  expect_identical(cps_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(cps_extract$submitted)
  expect_equal(cps_extract$number, NA_integer_)
  expect_equal(cps_extract$status, "unsubmitted")
})

test_that("Can define an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_s3_class(nhgis_extract, "nhgis_extract")
  expect_s3_class(nhgis_extract, "ipums_extract")

  expect_true(all(
    purrr::map_lgl(nhgis_extract$datasets, ~ inherits(.x, "ipums_dataset")))
  )
  expect_true(all(
    purrr::map_lgl(nhgis_extract$time_series_tables, ~ inherits(.x, "ipums_tst")))
  )

  expect_equal(
    purrr::map_chr(nhgis_extract$datasets, ~ .x$name),
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    purrr::map(nhgis_extract$datasets, ~ .x$data_tables),
    list(c("B01001", "B01002"), c("B01001", "B01002"))
  )
  expect_equal(
    purrr::map(nhgis_extract$datasets, ~ .x$geog_levels),
    list("nation", "blck_grp")
  )
  expect_equal(
    purrr::map(nhgis_extract$datasets, ~ .x$years),
    list(NULL, NULL)
  )
  expect_equal(
    purrr::map(nhgis_extract$datasets, ~ .x$breakdown_values),
    list(NULL, NULL)
  )

  expect_equal(
    purrr::map_chr(nhgis_extract$time_series_tables, ~ .x$name),
    c("CW3")
  )
  expect_equal(
    purrr::map(nhgis_extract$time_series_tables, ~ .x$geog_levels),
    list("state")
  )
  expect_equal(
    purrr::map(nhgis_extract$time_series_tables, ~ .x$years),
    list("1990")
  )

  expect_equal(nhgis_extract$tst_layout, "time_by_row_layout")
  expect_equal(nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(nhgis_extract$data_format, "csv_no_header")

  expect_identical(nhgis_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(nhgis_extract$submitted)
  expect_equal(nhgis_extract$number, NA_integer_)
  expect_equal(nhgis_extract$status, "unsubmitted")

  expect_null(nhgis_extract_shp$datasets)
  expect_null(nhgis_extract_shp$time_series_tables)
})

test_that("NHGIS extract fields get correct default values", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_equal(nhgis_extract$breakdown_and_data_type_layout, "single_file")
  expect_equal(
    define_extract_nhgis(
      time_series_tables = new_tst("A00", "A1")
    )$tst_layout,
    "time_by_column_layout"
  )
  expect_equal(
    define_extract_nhgis(
      time_series_tables = new_tst("A00", "A1")
    )$data_format,
    "csv_no_header"
  )

  expect_null(nhgis_extract_shp$breakdown_and_data_type_layout)
  expect_null(nhgis_extract_shp$data_format)
})

# Extract validation ------------------------

test_that("Attempt to define a rectangular_on = H throws error", {
  expect_error(
    define_extract_cps(
      "Test",
      samples = "us2017b",
      variables = "YEAR",
      rectangular_on = "H"
    ),
    "Currently, the `rectangular_on` argument must be equal to \"P\""
  )
})

test_that("Attempt to rectangularize on H records throws an error", {
  expect_error(
    define_extract_usa(
      "Test",
      samples = "us2017b",
      variables = "YEAR",
      rectangular_on = "H"
    ),
    "`rectangular_on` argument must be equal to \"P\""
  )
})

test_that("Microdata validation methods work", {
  usa_extract <- test_usa_extract()

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
    validate_ipums_extract(new_ipums_extract("usa")),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("usa", description = "")),
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

test_that("Can validate `nhgis_extract` object", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

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
        datasets = list(
          new_nested_field(
            "A00",
            data_tables = NA,
            geog_levels = list("A", "b"),
            years = 1990,
            class = "ipums_dataset"
          )
        )
      )
    ),
    paste0(
      "Invalid `ipums_dataset` object:.+",
      "`data_tables` must not contain missing values.+",
      "`geog_levels` must be of type `character`, not `list`.+",
      "`years` must be of type `character`, not `double`"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list(
          new_nested_field(
            "A00",
            data_tables = NA,
            geog_levels = list(NULL, "b"),
            class = "ipums_tst"
          )
        )
      )
    ),
    paste0(
      "Invalid `ipums_tst` object:.+",
      "Unrecognized fields: `data_tables`"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list(
          new_nested_field(
            NULL,
            geog_levels = list(NULL, "b"),
            class = "ipums_tst"
          )
        )
      )
    ),
    paste0(
      "Invalid `ipums_tst` object:.+",
      "`name` must not contain missing values.+",
      "`geog_levels` must not contain missing values"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list(
          new_nested_field(
            c("A", "B"),
            geog_levels = c("A", "B"),
            class = "ipums_tst"
          )
        )
      )
    ),
    "`name` must be length 1"
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      datasets = new_dataset("a", "b", "c"),
      data_format = "Test"
    ),
    "`data_format` must be one of"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis", description = NULL)),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list(new_tst("A00", "a")),
        data_format = "csv_no_header"
      )
    ),
    paste0(
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
        time_series_tables = list(new_tst("CW3", "A"))
      )
    ),
    "Expected `datasets` to be an `ipums_dataset` object"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list("a", "b", "c")
      )
    ),
    "Expected `time_series_tables` to be an `ipums_tst` object"
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = new_tst("A", "B"),
      tst_layout = c("time_by_row_layout", "time_by_row_layout")
    ),
    "`tst_layout` must be length 1."
  )
})

# Extract revisions --------------------------

test_that("Can add to a USA extract", {
  usa_extract <- test_usa_extract()

  revised_extract <- add_to_extract(
    usa_extract,
    samples = "us2014a",
    variables = "RELATE",
    data_structure = "hierarchical"
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
  expect_equal(revised_extract$data_structure, "hierarchical")
  expect_null(revised_extract$rectangular_on)

  expect_error(
    add_to_extract(
      usa_extract,
      samples = "cps2019_03s",
      variables = "RELATE",
      rectangular_on = "H"
    ),
    "must be equal to \"P\""
  )
})

test_that("Can add to a CPS extract", {
  cps_extract <- test_cps_extract()

  revised_extract <- add_to_extract(
    cps_extract,
    samples = "cps2019_03s",
    variables = "RELATE",
    data_structure = "rectangular"
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
  expect_equal(revised_extract$data_structure, "rectangular")
  expect_equal(revised_extract$rectangular_on, "P")

  expect_error(
    add_to_extract(
      cps_extract,
      samples = "cps2019_03s",
      variables = "RELATE",
      data_structure = "rectangular",
      rectangular_on = "H"
    ),
    "must be equal to \"P\""
  )
  expect_error(
    add_to_extract(
      cps_extract,
      samples = "cps2019_03s",
      variables = "RELATE",
      rectangular_on = "H"
    ),
    "`rectangular_on` must be missing"
  )
})

test_that("Can add new parent fields to an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  new_ds <- new_dataset("New", c("T1", "T2"), "G1")
  new_tst <- list(
    new_tst("CW4", "G2", "Y1"),
    new_tst("CW5", "G3", "Y1")
  )

  revised <- add_to_extract(
    nhgis_extract,
    datasets = new_ds,
    time_series_tables = new_tst,
    data_format = "csv_header",
    shapefiles = "New"
  )

  revised_shp <- add_to_extract(
    nhgis_extract_shp,
    datasets = new_ds
  )

  expect_equal(revised$datasets, c(nhgis_extract$datasets, list(new_ds)))
  expect_equal(
    revised$time_series_tables,
    c(nhgis_extract$time_series_tables, new_tst)
  )
  expect_equal(revised$data_format, "csv_header")
  expect_equal(revised$shapefiles, c(nhgis_extract$shapefiles, "New"))

  expect_equal(revised_shp$datasets, list(new_ds))
  expect_equal(revised_shp$shapefiles, nhgis_extract_shp$shapefiles)
  expect_null(revised_shp$time_series_tables)
})

test_that("Can add subfields to existing NHGIS extract parent fields", {
  nhgis_extract <- test_nhgis_extract()

  revised <- add_to_extract(
    nhgis_extract,
    datasets = list(
      new_dataset("2014_2018_ACS5a", c("T1", "T2"), "nation"),
      new_dataset("2015_2019_ACS5a", c("T1", "T2"), "G1", "Y2")
    ),
    time_series_tables = list(
      new_tst("CW3", "G1", "Y1"),
      new_tst("CW4", "G2")
    )
  )

  expect_equal(
    purrr::map(revised$datasets, ~ .x$name),
    purrr::map(nhgis_extract$datasets, ~ .x$name),
  )
  expect_equal(
    purrr::map(revised$datasets, ~ .x$data_tables),
    list(
      c("B01001", "B01002", "T1", "T2"),
      c("B01001", "B01002", "T1", "T2")
    )
  )
  expect_equal(
    purrr::map(revised$datasets, ~ .x$geog_levels),
    list(
      c("nation"),
      c("blck_grp", "G1")
    )
  )
  expect_equal(
    purrr::map(revised$datasets, ~ .x$years),
    list(NULL, "Y2")
  )

  expect_equal(
    purrr::map_chr(revised$time_series_tables, ~ .x$name),
    c("CW3", "CW4")
  )
  expect_equal(
    purrr::map(revised$time_series_tables, ~ .x$geog_levels),
    list(
      c("state", "G1"),
      c("G2")
    )
  )
  expect_equal(
    purrr::map(revised$time_series_tables, ~ .x$years),
    list(c("1990", "Y1"), NULL)
  )
})

test_that("Can remove from a USA extract", {
  usa_extract <- test_usa_extract()

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
  expect_equal(revised_extract$variables, c("YEAR", "RELATE"))
})

test_that("Can remove from a CPS extract", {
  cps_extract <- test_cps_extract()

  revised_extract <- remove_from_extract(
    cps_extract,
    samples = "cps1976_02b",
    variables = c("MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1")
  )

  expect_equal(revised_extract$status, "unsubmitted")
  expect_equal(revised_extract$samples, "cps1976_01s")
  expect_equal(revised_extract$variables, "YEAR")
})

test_that("Can remove parent fields from an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2015_2019_ACS5a",
    time_series_tables = "CW3",
    geographic_extents = "DC"
  )

  expect_equal(revised$datasets, nhgis_extract$datasets[1])
  expect_null(revised$time_series_tables)
  expect_null(revised$tst_layout)
  expect_equal(revised$geographic_extents, "PA")
})

# test_that("Can remove subfields from existing NHGIS extract parent fields", {
#   nhgis_extract <- test_nhgis_extract()
#
#   revised <- remove_from_extract(
#     add_to_extract(
#       nhgis_extract,
#       geog_levels = list(`2014_2018_ACS5a` = "tract", CW3 = "tract"),
#     ),
#     data_tables = "B01001",
#     geog_levels = list(
#       `2014_2018_ACS5a` = "nation",
#       CW3 = c("state", "blck_grp")
#     ),
#     years = "1990",
#     geographic_extents = "420"
#   )
#
#   expect_equal(revised$datasets, nhgis_extract$datasets)
#   expect_equal(
#     revised$data_tables,
#     list(
#       `2014_2018_ACS5a` = "B01002",
#       `2015_2019_ACS5a` = "B01002"
#     )
#   )
#   expect_equal(revised$time_series_tables, nhgis_extract$time_series_tables)
#   expect_equal(
#     revised$geog_levels,
#     list(
#       `2014_2018_ACS5a` = "tract",
#       `2015_2019_ACS5a` = "blck_grp",
#       CW3 = "tract"
#     )
#   )
#   expect_equal(
#     revised$years,
#     list(`2014_2018_ACS5a` = NULL, `2015_2019_ACS5a` = NULL, CW3 = NULL)
#   )
#   expect_equal(revised$geographic_extents, "DC")
# })

# test_that("Removing NHGIS parent fields occurs before removing subfields", {
#   nhgis_extract <- test_nhgis_extract()
#
#   revised <- remove_from_extract(
#     nhgis_extract,
#     datasets = "2014_2018_ACS5a",
#     data_tables = "B01001"
#   )
#
#   expect_equal(revised$datasets, "2015_2019_ACS5a")
#   expect_equal(revised$data_tables, list(`2015_2019_ACS5a` = "B01002"))
# })

test_that("Unused revisions do not alter USA extract", {
  usa_extract <- test_usa_extract()

  expect_identical(usa_extract, add_to_extract(usa_extract))
  expect_identical(usa_extract, remove_from_extract(usa_extract))
  expect_identical(
    usa_extract,
    add_to_extract(usa_extract, samples = usa_extract$samples)
  )
  expect_identical(
    usa_extract,
    remove_from_extract(usa_extract, variables = "not in extract")
  )
})

test_that("Unused revisions do not alter CPS extract", {
  cps_extract <- test_cps_extract()

  expect_identical(cps_extract, add_to_extract(cps_extract))
  expect_identical(cps_extract, remove_from_extract(cps_extract))
  expect_identical(
    cps_extract,
    add_to_extract(cps_extract, samples = cps_extract$samples)
  )
  expect_identical(
    cps_extract,
    remove_from_extract(cps_extract, variables = "not in extract")
  )
})

test_that("Unused revisions do not alter NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()
  extract1 <- define_extract_nhgis(shapefiles = "Test")

  extract2 <- add_to_extract(
    extract1,
    datasets = new_dataset("Test", "Test", "Test")
  )

  # Test on an extract of multiple types
  expect_identical(nhgis_extract, add_to_extract(nhgis_extract))
  expect_identical(nhgis_extract, remove_from_extract(nhgis_extract))

  # Test on an extract of a single type
  expect_identical(extract1, add_to_extract(extract1))
  expect_identical(extract1, remove_from_extract(extract1))

  expect_null(extract2$time_series_tables)
  expect_null(extract2$tst_layout)
})

test_that("Improper microdata extract revisions throw warnings or errors", {
  usa_extract <- test_usa_extract()
  cps_extract <- test_cps_extract()

  expect_silent(add_to_extract(usa_extract, samples = "us2017b"))
  expect_silent(remove_from_extract(cps_extract, variables = "RELATE"))
  expect_warning(
    remove_from_extract(
      usa_extract,
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
    add_to_extract(
      cps_extract,
      description = "description",
      invalid = "invalid"
    ),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified:.+`invalid`"
    )
  )
  expect_silent(add_to_extract(cps_extract, description = "description"))
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

test_that("Improper NHGIS extract revisions throw warnings or errors", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_warning(
    remove_from_extract(
      nhgis_extract,
      description = "Extract for R client testing",
      bad_field = "not in extract"
    ),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed:.+`description`, `bad_field`"
    )
  )
  expect_silent(
    remove_from_extract(
      nhgis_extract,
      datasets = "DS",
      time_series_tables = "TST"
    )
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

  expect_error(
    add_to_extract(
      nhgis_extract,
      datasets = list(
        new_dataset("A", "A", "A"),
        new_dataset("A", "B", "B")
      )
    ),
    "Cannot add two `ipums_dataset` objects of same name"
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      time_series_tables = list(
        new_tst("A", "A", "A"),
        new_tst("A", "B", "B")
      )
    ),
    "Cannot add two `ipums_tst` objects of same name"
  )
})

test_that("Can combine microdata extracts", {
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

  x_target <- define_extract_usa(
    description = "Combining",
    samples = c("S1", "S2", "S3"),
    variables = c("V1", "V2"),
    data_format = "csv"
  )

  x <- combine_extracts(x1, x2)

  expect_identical(x, x_target)
})


test_that("Can combine NHGIS extracts", {
  x1 <- define_extract_nhgis(
    description = "Combining",
    shapefiles = "S1"
  )

  x2 <- define_extract_nhgis(
    datasets = list(
      new_dataset("A", c("D1", "D2"), "G1"),
      new_dataset("B", "D3", "G2")
    ),
    time_series_tables = list(
      new_tst("T1", "G1"),
      new_tst("T2", "G1")
    ),
    tst_layout = "time_by_file_layout"
  )

  x3 <- define_extract_nhgis(
    datasets = new_dataset("C", c("D2", "D4"), c("G2", "G3"), years = "Y1"),
    shapefiles = "S1",
    data_format = "fixed_width"
  )

  x_target <- define_extract_nhgis(
    description = "Combining",
    datasets = list(
      new_dataset("A", c("D1", "D2"), "G1"),
      new_dataset("B", "D3", "G2"),
      new_dataset("C", c("D2", "D4"), c("G2", "G3"), "Y1")
    ),
    time_series_tables = list(
      new_tst("T1", "G1"),
      new_tst("T2", "G1")
    ),
    tst_layout = "time_by_file_layout",
    shapefiles = "S1",
    data_format = "csv_no_header"
  )

  x <- combine_extracts(x1, x2, x3)

  expect_identical(x, x_target)
  expect_error(
    combine_extracts(x1, define_extract_usa("A", "B", "C")),
    "same collection"
  )
})


# JSON Conversion ----------------

test_that("Can reproduce extract specs from JSON definition", {
  usa_extract <- test_usa_extract()
  cps_extract <- test_cps_extract()
  nhgis_extract <- test_nhgis_extract()

  usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")
  cps_json <- new_ipums_json(extract_to_request_json(cps_extract), "cps")
  nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")

  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_s3_class(cps_json, c("cps_json", "ipums_json"))
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))

  expect_identical(
    extract_list_from_json(usa_json)[[1]],
    usa_extract
  )
  expect_identical(
    extract_list_from_json(cps_json)[[1]],
    cps_extract
  )
  expect_identical(
    extract_list_from_json(nhgis_json)[[1]],
    nhgis_extract
  )
})

test_that("Can export to and import from JSON", {
  usa_extract <- test_usa_extract()
  nhgis_extract <- test_nhgis_extract()

  json_tmpfile_usa <- file.path(tempdir(), "usa_extract.json")
  json_tmpfile_nhgis <- file.path(tempdir(), "nhgis_extract.json")

  on.exit(unlink(json_tmpfile_usa), add = TRUE, after = FALSE)
  on.exit(unlink(json_tmpfile_nhgis), add = TRUE, after = FALSE)

  save_extract_as_json(usa_extract, json_tmpfile_usa)
  save_extract_as_json(nhgis_extract, json_tmpfile_nhgis)

  copy_of_usa_extract <- define_extract_from_json(json_tmpfile_usa)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile_nhgis)

  expect_identical(usa_extract, copy_of_usa_extract)
  expect_identical(nhgis_extract, copy_of_nhgis_extract)

  expect_error(
    save_extract_as_json(nhgis_extract, json_tmpfile_usa),
    "already exists"
  )

  save_extract_as_json(nhgis_extract, json_tmpfile_usa, overwrite = TRUE)

  expect_identical(
    copy_of_nhgis_extract,
    define_extract_from_json(json_tmpfile_usa)
  )
})

test_that("Throw error when defining from JSON with old API version", {
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)

  withr::with_envvar(c("IPUMS_API_VERSION" = "v1"), {
    save_extract_as_json(test_usa_extract(), json_tmpfile)
  })

  expect_error(
    define_extract_from_json(json_tmpfile),
    paste0(
      "`extract_json` was created with IPUMS API version \"v1\".+",
      "As of ipumsr 0.6.0, only IPUMS API version 2 is supported"
    )
  )
})

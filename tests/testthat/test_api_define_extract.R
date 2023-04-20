# Extract definitions ---------------------------

test_that("Can define a USA extract", {
  usa_extract <- test_usa_extract()

  expect_s3_class(usa_extract, "usa_extract")
  expect_s3_class(usa_extract, "ipums_extract")

  expect_identical(
    usa_extract$samples,
    set_nested_names(list(new_sample("us2017b")))
  )
  expect_identical(
    usa_extract$variables,
    set_nested_names(
      list(
        new_variable(
          "RACE",
          case_selections = c("801", "802"),
          case_selection_type = "detailed",
          preselected = FALSE
        ),
        new_variable("YEAR")
      )
    )
  )

  expect_equal(usa_extract$data_structure, "rectangular")
  expect_equal(usa_extract$rectangular_on, "P")
  expect_identical(usa_extract$download_links, EMPTY_NAMED_LIST)
  expect_false(usa_extract$submitted)
  expect_equal(usa_extract$number, NA_integer_)
  expect_equal(usa_extract$status, "unsubmitted")
})

test_that("Can define a CPS extract", {
  cps_extract <- test_cps_extract()

  expect_s3_class(cps_extract, "cps_extract")
  expect_s3_class(cps_extract, "ipums_extract")

  expect_identical(
    cps_extract$samples,
    set_nested_names(list(new_sample("cps2018_03s"), new_sample("cps2019_03s")))
  )
  expect_identical(
    cps_extract$variables,
    set_nested_names(
      list(
        new_variable(
          "AGE",
          attached_characteristics = "head",
          data_quality_flags = TRUE
        ),
        new_variable(
          "SEX",
          case_selections = "2",
          attached_characteristics = c("mother", "father")
        ),
        new_variable(
          "RACE",
          case_selections = c("810", "811", "812"),
          case_selection_type = "general"
        )
      )
    )
  )

  expect_equal(cps_extract$data_structure, "hierarchical")
  expect_null(cps_extract$rectangular_on)
  expect_identical(cps_extract$download_links, EMPTY_NAMED_LIST)
  expect_false(cps_extract$submitted)
  expect_equal(cps_extract$number, NA_integer_)
  expect_equal(cps_extract$status, "unsubmitted")
})

test_that("Can define an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_s3_class(nhgis_extract, "nhgis_extract")
  expect_s3_class(nhgis_extract, "ipums_extract")

  expect_identical(
    nhgis_extract$datasets,
    set_nested_names(
      list(
        new_dataset("2014_2018_ACS5a", c("B01001", "B01002"), "nation"),
        new_dataset("2015_2019_ACS5a", c("B01001", "B01002"), "blck_grp")
      )
    )
  )

  expect_true(
    all(purrr::map_lgl(nhgis_extract$datasets, ~ is.null(.x$years)))
  )
  expect_true(
    all(purrr::map_lgl(nhgis_extract$datasets, ~ is.null(.x$breakdown_values)))
  )

  expect_identical(
    nhgis_extract$time_series_tables,
    set_nested_names(
      list(
        new_tst("CW3", "state", "1990")
      )
    )
  )

  expect_equal(nhgis_extract$tst_layout, "time_by_row_layout")
  expect_equal(nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(nhgis_extract$data_format, "csv_no_header")

  expect_identical(nhgis_extract$download_links, EMPTY_NAMED_LIST)
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
    define_extract_nhgis(time_series_tables = new_tst("A00", "A1"))$tst_layout,
    "time_by_column_layout"
  )
  expect_equal(
    define_extract_nhgis(time_series_tables = new_tst("A00", "A1"))$data_format,
    "csv_no_header"
  )

  expect_null(nhgis_extract_shp$breakdown_and_data_type_layout)
  expect_null(nhgis_extract_shp$data_format)
})

test_that("Microdata variables get correct default values", {
  var1 <- new_variable("SEX")
  var2 <- new_variable("SEX", case_selections = "1")

  expect_equal(var1$name, "SEX")
  expect_null(var1$case_selections)
  expect_null(var1$attached_characteristics)
  expect_null(var1$data_quality_flags)
  expect_null(var1$case_selection_type)

  expect_equal(var2$case_selection_type, "general")
})

test_that("We coerce variables and samples to `ipums_nested` objects", {
  x <- define_extract_cps(
    description = "",
    samples = "A",
    variables = list("B")
  )

  expect_s3_class(x$samples[[1]], "ipums_sample")
  expect_s3_class(x$variables[[1]], "ipums_variable")
  expect_equal(names(x$samples), "A")
  expect_equal(names(x$variables), "B")

  # Do not coerce if named to guard against unexpected behavior when variables
  # are combined with c() instead of list(). (c() removes class so each
  # variable field will be coerced individually)
  expect_error(
    define_extract_cps(
      description = "",
      samples = "A",
      variables = c(new_variable("A"), new_variable("B"))
    ),
    "Expected `variables` to be an `ipums_variable`"
  )
})

# Extract validation ------------------------

test_that("Attempt to rectangularize on H records throws an error", {
  expect_error(
    define_extract_cps(
      "Test",
      samples = "us2017b",
      variables = "YEAR",
      rectangular_on = "H"
    ),
    "Currently, the `rectangular_on` argument must be equal to \"P\""
  )
  expect_error(
    define_extract_usa(
      "Test",
      samples = "us2017b",
      variables = "YEAR",
      rectangular_on = "H"
    ),
    "Currently, the `rectangular_on` argument must be equal to \"P\""
  )
})

test_that("Succesful validation returns identical object", {
  cps_extract <- test_cps_extract()
  usa_extract <- test_usa_extract()
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_identical(validate_ipums_extract(cps_extract), cps_extract)
  expect_identical(validate_ipums_extract(usa_extract), usa_extract)
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
})

test_that("Can validate core microdata extract fields", {
  expect_error(
    validate_ipums_extract(define_extract_usa()),
    "argument \"variables\" is missing"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("usa")),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("usa", description = "")),
    "A `usa_extract` must contain values for `samples`"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "Test",
        samples = list(new_sample("Test")),
        variables = list(new_variable("Test")),
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
        samples = list(new_sample("Test")),
        variables = list(new_variable("Test")),
        data_format = "csv"
      )
    ),
    paste0(
      "`rectangular_on` must be missing when ",
      "`data_structure` is \"hierarchical\"."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = "",
        data_structure = "rectangular",
        samples = list(new_sample("Test")),
        variables = list(new_variable("Test")),
        data_format = "csv"
      )
    ),
    paste0(
      "`rectangular_on` must not contain missing values when ",
      "`data_structure` is \"rectangular\"."
    )
  )
})

test_that("Can validate core NHGIS extract fields", {
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis")),
    "`description` must not contain missing values"
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
    define_extract_nhgis(
      time_series_tables = new_tst("A", "B"),
      tst_layout = c("time_by_row_layout", "time_by_row_layout")
    ),
    "`tst_layout` must be length 1."
  )
})

test_that("We require `ipums_*` objects in appropriate extract fields", {
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
    "Expected `samples` to be an `ipums_sample` object or a list of "
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        data_structure = "hierarchical",
        samples = list(new_sample("A")),
        variables = list("B"),
        data_format = "csv"
      )
    ),
    "Expected `variables` to be an `ipums_variable` object or a list of "
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
    "Expected `datasets` to be an `ipums_dataset` object or a list of"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = list("a", "b", "c")
      )
    ),
    "Expected `time_series_tables` to be an `ipums_tst` object or a list of"
  )
})

test_that("Can validate `ipums_*` objects within extracts", {
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        samples = list(new_sample("A")),
        variables = list(
          new_nested_field(
            "A",
            case_selection_type = "general",
            class = "ipums_variable"
          )
        )
      )
    ),
    "`case_selection_type` must be missing when `case_selection` is not provided."
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        samples = list(new_sample("A")),
        variables = list(
          new_nested_field(
            "A",
            case_selections = "1",
            case_selection_type = "foobar",
            class = "ipums_variable"
          )
        )
      )
    ),
    "`case_selection_type` must be one of \"general\", \"detailed\""
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        samples = list(new_sample("A")),
        variables = list(new_variable(
          "A",
          data_quality_flags = "foo",
          attached_characteristics = TRUE
        ))
      )
    ),
    paste0(
      "`attached_characteristics` must be of type `character`, not `logical`.+",
      "`data_quality_flags` must be of type `logical`, not `character`"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        variables = new_variable("A"),
        samples = list(new_nested_field("A", foo = "bar", class = "ipums_sample"))
      )
    ),
    "Invalid `ipums_sample` object:.+Unrecognized fields: `foo`"
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
})

# We must avoid doing this because the API will only process one of them,
# producing ambiguity for extracts that contain duplicates if their
# subfield specifications are different.
test_that("We avoid adding multiple `ipums_*` objects of same name", {
  expect_error(
    define_extract_usa(
      "",
      samples = "A",
      variables = c("A", "A")
    ),
    "Cannot add multiple `variables` of same name"
  )
  expect_error(
    define_extract_cps(
      "",
      samples = c("A", "A"),
      variables = "A"
    ),
    "Cannot add multiple `samples` of same name"
  )
  expect_error(
    define_extract_nhgis(
      datasets = list(new_dataset("A", "A", "A"), new_dataset("A", "B", "C"))
    ),
    "Cannot add multiple `datasets` of same name"
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = list(new_tst("A", "A", "A"), new_tst("A", "B"))
    ),
    "Cannot add multiple `time_series_tables` of same name"
  )
})

# Extract revisions --------------------------

test_that("Can add full fields to a microdata extract", {
  cps_extract <- test_cps_extract()

  revised_extract <- add_to_extract(
    cps_extract,
    samples = new_sample("cps2019_03s"),
    variables = list(
      new_variable("SEX", data_quality_flags = TRUE),
      new_variable("RELATE", case_selections = c("1", "2"))
    ),
    data_structure = "rectangular"
  )

  expect_equal(
    names(revised_extract$samples),
    union(names(cps_extract$samples), "cps2019_03s")
  )
  expect_equal(
    names(revised_extract$variables),
    union(names(cps_extract$variables), "RELATE")
  )
  expect_equal(
    revised_extract$variables$RELATE$case_selections,
    c("1", "2")
  )
  expect_equal(
    revised_extract$variables$SEX$data_quality_flags,
    TRUE
  )
  expect_equal(revised_extract$data_structure, "rectangular")
  expect_equal(revised_extract$rectangular_on, "P")
})

test_that("Can add full fields to an NHGIS extract", {
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

  expect_equal(revised$datasets, c(nhgis_extract$datasets, list(New = new_ds)))
  expect_equal(
    revised$time_series_tables,
    c(nhgis_extract$time_series_tables, purrr::set_names(new_tst, c("CW4", "CW5")))
  )
  expect_equal(revised$data_format, "csv_header")
  expect_equal(revised$shapefiles, c(nhgis_extract$shapefiles, "New"))

  expect_equal(revised_shp$datasets, list(New = new_ds))
  expect_equal(revised_shp$shapefiles, nhgis_extract_shp$shapefiles)
  expect_null(revised_shp$time_series_tables)
})

test_that("Can add subfields to existing `ipums_*` fields", {
  nhgis_extract <- test_nhgis_extract()
  cps_extract <- test_cps_extract()

  nhgis_revised <- add_to_extract(
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

  cps_revised <- add_to_extract(
    cps_extract,
    variables = new_variable(
      "RACE",
      case_selections = "813",
      case_selection_type = "detailed"
    )
  )

  expect_equal(
    purrr::map(nhgis_revised$datasets, ~ .x$name),
    purrr::map(nhgis_extract$datasets, ~ .x$name),
  )
  expect_equal(
    unname(purrr::map(nhgis_revised$datasets, ~ .x$data_tables)),
    list(
      c("B01001", "B01002", "T1", "T2"),
      c("B01001", "B01002", "T1", "T2")
    )
  )
  expect_equal(
    unname(purrr::map(nhgis_revised$datasets, ~ .x$geog_levels)),
    list(
      c("nation"),
      c("blck_grp", "G1")
    )
  )
  expect_equal(
    unname(purrr::map(nhgis_revised$datasets, ~ .x$years)),
    list(NULL, "Y2")
  )
  expect_equal(
    names(nhgis_revised$time_series_tables),
    c("CW3", "CW4")
  )
  expect_equal(
    unname(purrr::map(nhgis_revised$time_series_tables, ~ .x$geog_levels)),
    list(
      c("state", "G1"),
      c("G2")
    )
  )
  expect_equal(
    unname(purrr::map(nhgis_revised$time_series_tables, ~ .x$years)),
    list(c("1990", "Y1"), NULL)
  )

  expect_equal(
    names(cps_revised$variables),
    names(cps_extract$variables)
  )
  expect_equal(
    cps_revised$variables$RACE$case_selections,
    c(cps_extract$variables$RACE$case_selections, "813")
  )
  expect_equal(
    cps_revised$variables$RACE$case_selection_type,
    "detailed"
  )
})

test_that("Can remove from a microdata extract", {
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

  expect_equal(
    revised_extract$samples,
    list(us2014a = new_sample("us2014a"))
  )
  expect_equal(
    revised_extract$variables,
    list(
      RACE = new_variable(
        "RACE",
        case_selections = c("801", "802"),
        case_selection_type = "detailed",
        preselected = FALSE
      ),
      YEAR = new_variable("YEAR"),
      RELATE = new_variable("RELATE")
    )
  )
})

test_that("Can remove from an NHGIS extract", {
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

test_that("Unused revisions do not alter unsubmitted extracts", {
  usa_extract <- test_usa_extract()
  cps_extract <- test_cps_extract()
  nhgis_extract <- test_nhgis_extract()

  expect_identical(usa_extract, add_to_extract(usa_extract))
  expect_identical(cps_extract, remove_from_extract(cps_extract))
  expect_identical(
    usa_extract,
    add_to_extract(usa_extract, samples = usa_extract$samples)
  )
  expect_identical(
    cps_extract,
    remove_from_extract(cps_extract, variables = "not in extract")
  )

  # Test on an NHGIS extract of multiple types
  expect_identical(nhgis_extract, add_to_extract(nhgis_extract))
  expect_identical(nhgis_extract, remove_from_extract(nhgis_extract))

  # Test on an NHGIS extract of a single type
  # to ensure we do not alter missing datasets/tsts on revision of other fields
  nhgis_extract1 <- define_extract_nhgis(shapefiles = "Test")

  expect_identical(nhgis_extract1, add_to_extract(nhgis_extract1))
  expect_identical(nhgis_extract1, remove_from_extract(nhgis_extract1))

  nhgis_extract2 <- add_to_extract(
    nhgis_extract1,
    datasets = new_dataset("Test", "Test", "Test")
  )

  expect_null(nhgis_extract2$time_series_tables)
  expect_null(nhgis_extract2$tst_layout)

  expect_identical(
    suppressWarnings(add_to_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
})

test_that("Revisions update status of submitted extract", {
  vcr::use_cassette("recent-usa-extracts-list", {
    usa_extracts <- get_extract_history("usa")
  })

  last_extract <- usa_extracts[[1]]

  revised <- add_to_extract(last_extract)

  expect_false(last_extract$status == revised$status)
  expect_equal(revised$status, "unsubmitted")
  expect_true(is.na(revised$number))
  expect_false(revised$submitted)
  expect_true(is_empty(revised$download_links))

  # Should still leave specifications untouched
  expect_identical(last_extract$samples, revised$samples)
  expect_identical(last_extract$variables, revised$variables)
})

test_that("Improper extract revisions throw warnings or errors", {
  usa_extract <- test_usa_extract()
  nhgis_extract <- test_nhgis_extract()

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
      nhgis_extract,
      description = "description",
      invalid = "invalid"
    ),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified:.+`invalid`"
    )
  )
  expect_error(
    remove_from_extract(
      usa_extract,
      samples = names(usa_extract$samples),
      variables = names(usa_extract$variables)
    ),
    "A `usa_extract` must contain values for `samples`"
  )
  expect_error(
    remove_from_extract(
      usa_extract,
      samples = usa_extract$samples
    ),
    "Expected `samples` to be a character vector"
  )
  expect_error(
    add_to_extract(usa_extract, data_format = "bad_format"),
    "`data_format` must be one of"
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      shapefiles = "New shapefile",
      data_format = c("csv", "bad_format"),
    ),
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
      usa_extract,
      variables = c("A", "A")
    ),
    "Cannot add two `ipums_variable` objects of same name"
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
  nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")

  # .json method should handle appropriately if there is a collection field
  # available
  cps_json <- extract_to_request_json(cps_extract)

  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))
  expect_s3_class(cps_json, "json")

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
      "`extract_json` was created with IPUMS API version v1.+",
      "As of ipumsr 0.6.0, only IPUMS API version 2 is supported"
    )
  )
})

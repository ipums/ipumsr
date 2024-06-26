# Extract definitions ---------------------------

test_that("Can define a USA extract", {
  usa_extract <- test_usa_extract()

  expect_s3_class(usa_extract, "usa_extract")
  expect_s3_class(usa_extract, "ipums_extract")

  expect_identical(
    usa_extract$samples,
    set_nested_names(list(samp_spec("us2017b")))
  )
  expect_identical(
    usa_extract$variables,
    set_nested_names(
      list(
        var_spec(
          "RACE",
          case_selections = c("801", "802"),
          case_selection_type = "detailed",
          preselected = FALSE
        ),
        var_spec("YEAR")
      )
    )
  )

  expect_equal(usa_extract$data_structure, "rectangular")
  expect_equal(usa_extract$rectangular_on, "P")
  expect_equal(usa_extract$case_select_who, "households")
  expect_null(usa_extract$data_quality_flags)
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
    set_nested_names(list(samp_spec("cps2018_03s"), samp_spec("cps2019_03s")))
  )
  expect_identical(
    cps_extract$variables,
    set_nested_names(
      list(
        var_spec(
          "AGE",
          attached_characteristics = "head",
          data_quality_flags = FALSE
        ),
        var_spec(
          "SEX",
          case_selections = "2",
          attached_characteristics = c("mother", "father")
        ),
        var_spec(
          "RACE",
          case_selections = c("810", "811", "812"),
          case_selection_type = "general"
        )
      )
    )
  )

  expect_equal(cps_extract$data_structure, "hierarchical")
  expect_null(cps_extract$rectangular_on)
  expect_equal(cps_extract$case_select_who, "individuals")
  expect_equal(cps_extract$data_quality_flags, TRUE)
  expect_identical(cps_extract$download_links, EMPTY_NAMED_LIST)
  expect_false(cps_extract$submitted)
  expect_equal(cps_extract$number, NA_integer_)
  expect_equal(cps_extract$status, "unsubmitted")
})

test_that("Can define an IPUMSI extract", {
  ipumsi_extract <- test_ipumsi_extract()

  expect_identical(
    ipumsi_extract$samples,
    set_nested_names(list(samp_spec("mx2015a"), samp_spec("cl2017a")))
  )
  expect_identical(
    ipumsi_extract$variables,
    set_nested_names(
      list(
        var_spec(
          "AGE",
          case_selections = "010"
        ),
        var_spec(
          "SEX",
          attached_characteristics = "father"
        ),
        var_spec(
          "EDATTAIN"
        )
      )
    )
  )

  expect_equal(ipumsi_extract$data_structure, "rectangular")
  expect_equal(ipumsi_extract$rectangular_on, "P")
  expect_equal(ipumsi_extract$case_select_who, "individuals")
  expect_null(ipumsi_extract$data_quality_flags)
  expect_identical(ipumsi_extract$download_links, EMPTY_NAMED_LIST)
  expect_false(ipumsi_extract$submitted)
  expect_equal(ipumsi_extract$number, NA_integer_)
  expect_equal(ipumsi_extract$status, "unsubmitted")
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
        ds_spec("2014_2018_ACS5a", c("B01001", "B01002"), "nation"),
        ds_spec("2015_2019_ACS5a", c("B01001", "B01002"), "blck_grp")
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
        tst_spec("CW3", "state", "1990")
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

test_that("Can define an ATUS extract with ATUS-specific features", {
  atus_extract <- test_atus_extract()

  expect_equal(atus_extract$sample_members, "include_household_members")
  expect_equal(atus_extract$time_use_variables[[1]]$name, "ACT_PCARE")
  expect_false("owner" %in% names(atus_extract$time_use_variables[[1]]))
  expect_s3_class(atus_extract$time_use_variables[[1]], "tu_var_spec")
  expect_equal(
    atus_extract$time_use_variables[[2]]$owner,
    "example@example.com"
  )
})

test_that("Can define an ATUS extract with no variables", {
  atus_no_vars <- define_extract_micro(
    "atus",
    "No vars",
    "at2020",
    time_use_variables = "ACT_PCARE"
  )
  expect_null(atus_no_vars$variables)
})

test_that("NHGIS extract fields get correct default values", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_equal(nhgis_extract$breakdown_and_data_type_layout, "single_file")
  expect_equal(
    define_extract_nhgis(time_series_tables = tst_spec("A00", "A1"))$tst_layout,
    "time_by_column_layout"
  )
  expect_equal(
    define_extract_nhgis(time_series_tables = tst_spec("A00", "A1"))$data_format,
    "csv_no_header"
  )

  expect_null(nhgis_extract_shp$breakdown_and_data_type_layout)
  expect_null(nhgis_extract_shp$data_format)
})

test_that("Microdata variables get correct default values", {
  var1 <- var_spec("SEX")
  var2 <- var_spec("SEX", case_selections = "1")

  expect_equal(var1$name, "SEX")
  expect_null(var1$case_selections)
  expect_null(var1$attached_characteristics)
  expect_null(var1$data_quality_flags)
  expect_null(var1$case_selection_type)

  expect_equal(var2$case_selection_type, "general")
})

test_that("We cast vectors to `ipums_spec` objects", {
  x <- define_extract_micro(
    collection = "cps",
    description = "",
    samples = "A",
    variables = list("B", "C")
  )

  expect_s3_class(x$samples[[1]], "samp_spec")
  expect_s3_class(x$variables[[1]], "var_spec")
  expect_equal(names(x$samples), "A")
  expect_equal(names(x$variables), c("B", "C"))

  # It is not possible to distinguish a "vector" of spec objects from
  # a generic named vector. We use names to determine whether the input
  # to a spec argument is unexpected. But we do still coerce since the input
  # values are in a vector, which we can handle.
  expect_warning(
    x <- define_extract_micro(
      collection = "cps",
      description = "",
      samples = "A",
      variables = c(var_spec("A"), var_spec("B"))
    ),
    paste0(
      "Unexpected names in input when converting to `var_spec`.+",
      "You may have combined `var_spec` objects with `c\\(\\)`"
    )
  )

  expect_equal(names(x$variables), c("A", "B"))

  expect_warning(
    x <- define_extract_micro(
      collection = "atus",
      description = "",
      samples = "A",
      time_use_variables = c(tu_var_spec("A"), tu_var_spec("B"))
    ),
    paste0(
      "Unexpected names in input when converting to `tu_var_spec`.+",
      "You may have combined `tu_var_spec` objects with `c\\(\\)`"
    )
  )

  expect_equal(names(x$time_use_variables), c("A", "B"))
})

test_that("Vector to `ipums_spec` casting logic is as expected", {
  expect_equal(
    spec_cast(tst_spec("A", "B"), "tst_spec"),
    list(tst_spec("A", "B"))
  )
  expect_equal(
    spec_cast(list(tst_spec("A", "B")), "tst_spec"),
    list(tst_spec("A", "B"))
  )
  expect_equal(
    spec_cast(tst_spec("A", "B"), "ds_spec"),
    list(tst_spec("A", "B"))
  )
  expect_equal(
    spec_cast(c("A", "B"), "var_spec"),
    list(var_spec("A"), var_spec("B"))
  )
  expect_equal(
    spec_cast(list(var_spec("A"), "B"), "var_spec"),
    list(var_spec("A"), var_spec("B"))
  )
  expect_equal(
    spec_cast(c("A", "B"), "tu_var_spec"),
    list(tu_var_spec("A"), tu_var_spec("B"))
  )
  expect_equal(
    spec_cast(list(tu_var_spec("A"), "B"), "tu_var_spec"),
    list(tu_var_spec("A"), tu_var_spec("B"))
  )
  expect_equal(
    spec_cast(list(samp_spec("A"), samp_spec("B"), "C"), "samp_spec"),
    list(samp_spec("A"), samp_spec("B"), samp_spec("C"))
  )
  expect_warning(
    spec_cast(c(var_spec("A"), "B"), "var_spec")
  )
  expect_warning(
    spec_cast(c(a = "A", "B"), "var_spec")
  )
  expect_warning(
    spec_cast(c(tu_var_spec("A"), "B"), "tu_var_spec")
  )
})

# Extract validation ------------------------

test_that("Succesful validation returns identical object", {
  cps_extract <- test_cps_extract()
  usa_extract <- test_usa_extract()
  atus_extract <- test_atus_extract()
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_identical(validate_ipums_extract(cps_extract), cps_extract)
  expect_identical(validate_ipums_extract(usa_extract), usa_extract)
  expect_identical(validate_ipums_extract(atus_extract), atus_extract)
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
})

test_that("Can validate core microdata extract fields", {
  expect_error(
    validate_ipums_extract(define_extract_micro(collection = "usa")),
    "argument \"samples\" is missing"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("usa")),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(new_ipums_extract("usa", description = "")),
    "Extract definition must contain values for `samples`"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "Test",
        samples = list(samp_spec("Test")),
        variables = list(var_spec("Test")),
        sample_members = "Test",
        data_format = "Test",
        data_structure = "Test",
        case_select_who = "Test",
        data_quality_flags = c(TRUE, FALSE)
      )
    ),
    paste0(
      "`sample_members` must be one of.+",
      "`data_structure` must be one of.+",
      "`data_format` must be one of.+",
      "`case_select_who` must be one of.+",
      "`data_quality_flags` must be length"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = "",
        data_structure = "hierarchical",
        rectangular_on = "B",
        samples = list(samp_spec("Test")),
        variables = list(var_spec("Test")),
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
        samples = list(samp_spec("Test")),
        variables = list(var_spec("Test")),
        data_format = "csv"
      )
    ),
    paste0(
      "`rectangular_on` must not contain missing values when ",
      "`data_structure` is \"rectangular\"."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = "",
        samples = list(samp_spec("Test")),
        variables = list(var_spec("Test")),
        data_structure = "rectangular",
        rectangular_on = "H",
        data_format = "csv"
      )
    ),
    "`rectangular_on` must be one of"
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
      datasets = ds_spec("a", "b", "c"),
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
        time_series_tables = list(tst_spec("A00", "a")),
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
      time_series_tables = tst_spec("A", "B"),
      tst_layout = c("time_by_row_layout", "time_by_row_layout")
    ),
    "`tst_layout` must be length 1."
  )
})

test_that("We require `*_spec` objects in appropriate extract fields", {
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
    "Expected `samples` to be a `samp_spec` object or a list of "
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "cps",
        description = "",
        data_structure = "hierarchical",
        samples = list(samp_spec("A")),
        variables = list("B"),
        data_format = "csv"
      )
    ),
    "Expected `variables` to be a `var_spec` object or a list of "
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "atus",
        description = "",
        data_structure = "hierarchical",
        samples = list(samp_spec("A")),
        time_use_variables = list("B"),
        data_format = "csv"
      )
    ),
    "Expected `time_use_variables` to be a `tu_var_spec` object or a list of "
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "atus",
        description = "",
        data_structure = "hierarchical",
        samples = list(samp_spec("A")),
        time_use_variables = var_spec("B"),
        data_format = "csv"
      )
    ),
    "Expected `time_use_variables` to be a `tu_var_spec` object or a list of "
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = NA,
        time_series_tables = list(tst_spec("CW3", "A"))
      )
    ),
    "Expected `datasets` to be a `ds_spec` object or a list of"
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      time_series_tables = c("a", "b", "c")
    ),
    "Expected `time_series_tables` to be a `tst_spec` object or a list of"
  )
  expect_silent(
    define_extract_micro(
      collection = "cps",
      description = "test",
      samples = "sample",
      variables = list(
        "var1",
        var_spec("var2", attached_characteristics = "head")
      )
    )
  )
})

test_that("Can validate `*_spec` objects within extracts", {
  samps <- samp_spec("A")

  expect_error(
    define_extract_micro(
      collection = "cps",
      description = "",
      samples = samps,
      variables = var_spec("A", case_selection_type = "detailed")
    ),
    paste0(
      "`case_selection_type` must be missing when `case_selections` is not",
      " provided."
    )
  )
  expect_error(
    define_extract_micro(
      collection = "cps",
      description = "",
      samples = samps,
      variables = var_spec(
        "A",
        case_selections = "1",
        case_selection_type = "foobar"
      )
    ),
    "`case_selection_type` must be one of \"general\", \"detailed\""
  )
  expect_error(
    define_extract_micro(
      collection = "cps",
      description = "",
      samples = samps,
      variables = var_spec(
        "A",
        data_quality_flags = "foo",
        attached_characteristics = TRUE
      )
    ),
    paste0(
      "`attached_characteristics` must be of type `character`, not `logical`.+",
      "`data_quality_flags` must be of type `logical`, not `character`"
    )
  )
  expect_error(
    define_extract_micro(
      collection = "cps",
      description = "",
      variables = var_spec("A"),
      samples = list(new_ipums_spec("A", foo = "bar", class = "samp_spec"))
    ),
    "Invalid `samp_spec` specification:.+Unrecognized fields: `foo`"
  )
  expect_error(
    define_extract_micro(
      collection = "atus",
      description = "",
      samples = "at2020",
      time_use_variables = list(
        new_ipums_spec("A", foo = "bar", class = "tu_var_spec")
      )
    ),
    "Invalid `tu_var_spec` specification:.+Unrecognized fields: `foo`"
  )
  expect_error(
    define_extract_nhgis(
      datasets = ds_spec(
        "A00",
        data_tables = NA,
        geog_levels = list("A", "b"),
        years = 1990
      )
    ),
    paste0(
      "Invalid `ds_spec` specification:.+",
      "`data_tables` must not contain missing values.+",
      "`geog_levels` must be of type `character`, not `list`.+",
      "`years` must be of type `character`, not `double`"
    )
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = tst_spec(
        NULL,
        geog_levels = list(NULL, "b")
      )
    ),
    paste0(
      "Invalid `tst_spec` specification:.+",
      "`name` must not contain missing values.+",
      "`geog_levels` must not contain missing values"
    )
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = tst_spec(
        c("A", "B"),
        geog_levels = c("A", "B")
      )
    ),
    "`name` must be length 1"
  )
})

# We must avoid doing this because the API will only process one of them,
# producing ambiguity for extracts that contain duplicates if their
# subfield specifications are different.
test_that("We avoid adding multiple `*_spec` objects of same name", {
  expect_error(
    define_extract_micro(
      collection = "usa",
      "",
      samples = "A",
      variables = c("A", "A")
    ),
    "cannot contain multiple `variables` of same name"
  )
  expect_error(
    define_extract_micro(
      collection = "atus",
      "",
      samples = "A",
      time_use_variables = c("A", "A")
    ),
    "cannot contain multiple `time_use_variables` of same name"
  )
  expect_error(
    define_extract_micro(
      collection = "cps",
      "",
      samples = c("A", "A"),
      variables = "A"
    ),
    "cannot contain multiple `samples` of same name"
  )
  expect_error(
    define_extract_nhgis(
      datasets = list(ds_spec("A", "A", "A"), ds_spec("A", "B", "C"))
    ),
    "cannot contain multiple `datasets` of same name"
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = list(tst_spec("A", "A", "A"), tst_spec("A", "B"))
    ),
    "cannot contain multiple `time_series_tables` of same name"
  )
})

# Extract revisions --------------------------

test_that("Can add full fields to a microdata extract", {
  cps_extract <- test_cps_extract()
  atus_extract <- test_atus_extract()

  revised_cps <- add_to_extract(
    cps_extract,
    samples = samp_spec("cps2019_03s"),
    variables = list(
      var_spec("SEX", data_quality_flags = TRUE),
      var_spec("RELATE", case_selections = c("1", "2"))
    ),
    data_structure = "rectangular",
    case_select_who = "households",
    data_quality_flags = FALSE
  )

  expect_equal(
    names(revised_cps$samples),
    union(names(cps_extract$samples), "cps2019_03s")
  )
  expect_equal(
    names(revised_cps$variables),
    union(names(cps_extract$variables), "RELATE")
  )
  expect_equal(
    revised_cps$variables$RELATE$case_selections,
    c("1", "2")
  )
  expect_equal(
    revised_cps$variables$SEX$data_quality_flags,
    TRUE
  )
  expect_equal(revised_cps$data_structure, "rectangular")
  expect_equal(revised_cps$rectangular_on, "P")
  expect_equal(revised_cps$case_select_who, "households")
  expect_false(revised_cps$data_quality_flags)

  revised_atus <- add_to_extract(
    atus_extract,
    time_use_variables = tu_var_spec(
      "new_time_use_var",
      owner = "example@example.com"
    )
  )
  expect_contains(
    names(revised_atus$time_use_variables),
    "new_time_use_var"
  )
})

test_that("Can add full fields to hierarchical-on-round extracts", {
  meps_extract <- test_meps_extract()
  revised_meps_extract <- add_to_extract(
    meps_extract,
    data_quality_flags = TRUE
  )
  expect_equal(revised_meps_extract$rectangular_on, "R")
})

test_that("Can add full fields to an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  new_ds <- ds_spec("New", c("T1", "T2"), "G1")
  tst_spec <- list(
    tst_spec("CW4", "G2", "Y1"),
    tst_spec("CW5", "G3", "Y1")
  )

  revised <- add_to_extract(
    nhgis_extract,
    datasets = new_ds,
    time_series_tables = tst_spec,
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
    c(nhgis_extract$time_series_tables, purrr::set_names(tst_spec, c("CW4", "CW5")))
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
  atus_extract <- test_atus_extract()

  nhgis_revised <- add_to_extract(
    nhgis_extract,
    datasets = list(
      ds_spec("2014_2018_ACS5a", c("T1", "T2"), "nation"),
      ds_spec("2015_2019_ACS5a", c("T1", "T2"), "G1", "Y2")
    ),
    time_series_tables = list(
      tst_spec("CW3", "G1", "Y1"),
      tst_spec("CW4", "G2")
    )
  )

  expect_equal(
    purrr::map(nhgis_revised$datasets, ~ .x$name),
    purrr::map(nhgis_extract$datasets, ~ .x$name)
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

  cps_revised <- add_to_extract(
    cps_extract,
    variables = list(
      var_spec(
        "RACE",
        case_selections = "813",
        case_selection_type = "detailed"
      ),
      # Ensure we add case_selection_type when adding new case selections:
      var_spec("AGE", case_selections = "10")
    )
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
  expect_equal(
    cps_revised$variables$AGE$case_selections,
    "10"
  )
  expect_equal(
    cps_revised$variables$AGE$case_selection_type,
    "general"
  )

  atus_revised <- add_to_extract(
    atus_extract,
    time_use_variables = tu_var_spec("ACT_PCARE", owner = "example@example.com")
  )

  expect_equal(
    atus_revised$time_use_variables$ACT_PCARE$owner,
    "example@example.com"
  )
})

test_that("Can replace length-one fields in existing `ipums_spec` objects", {
  atus_extract <- test_atus_extract()

  revised_atus_extract <- add_to_extract(
    atus_extract,
    variables = var_spec("AGE", data_quality_flags = FALSE),
    time_use_variables = tu_var_spec(
      "my_time_use_var",
      owner = "newowner@example.com"
    )
  )

  expect_false(revised_atus_extract$variables$AGE$data_quality_flags)
  expect_equal(
    revised_atus_extract$time_use_variables$my_time_use_var$owner,
    "newowner@example.com"
  )

  usa_extract <- test_usa_extract()

  revised_usa_extract <- add_to_extract(
    usa_extract,
    variables = var_spec(
      "RACE",
      case_selection_type = "general",
      preselected = TRUE
    )
  )

  expect_equal(
    revised_usa_extract$variables$RACE$case_selection_type,
    "general"
  )
  expect_true(revised_usa_extract$variables$RACE$preselected)
})

test_that("Can remove full fields from a microdata extract", {
  usa_extract <- test_usa_extract()
  atus_extract <- test_atus_extract()

  revised_usa <- add_to_extract(
    usa_extract,
    samples = "us2014a",
    variables = c("RELATE", "AGE", "SEX")
  )

  revised_usa <- remove_from_extract(
    revised_usa,
    samples = "us2017b",
    variables = list(
      var_spec("AGE"),
      "SEX"
    )
  )

  expect_equal(
    revised_usa$samples,
    list(us2014a = samp_spec("us2014a"))
  )
  expect_equal(
    revised_usa$variables,
    list(
      RACE = var_spec(
        "RACE",
        case_selections = c("801", "802"),
        case_selection_type = "detailed",
        preselected = FALSE
      ),
      YEAR = var_spec("YEAR"),
      RELATE = var_spec("RELATE")
    )
  )

  revised_atus <- remove_from_extract(
    atus_extract,
    time_use_variables = "ACT_PCARE"
  )

  expect_equal(
    revised_atus$time_use_variables,
    list(
      my_time_use_var = tu_var_spec(
        "my_time_use_var",
        owner = "example@example.com"
      )
    )
  )
})

test_that("Can remove sample_members from ATUS extract", {
  revised_atus_extract <- remove_from_extract(
    test_atus_extract(),
    sample_members = "include_household_members"
  )

  expect_null(revised_atus_extract$sample_members)

  revised_atus_extract <- remove_from_extract(
    add_to_extract(
      test_atus_extract(),
      sample_members = "include_non_respondents"
    ),
    sample_members = "include_household_members"
  )

  expect_equal(revised_atus_extract$sample_members, "include_non_respondents")
})

test_that("Can remove full fields from hierarchical-on-round extracts", {
  meps_extract <- test_meps_extract()
  revised_meps_extract <- remove_from_extract(
    meps_extract,
    samples = "mp2005"
  )
  expect_equal(revised_meps_extract$rectangular_on, "R")
})

test_that("Can remove full fields from an NHGIS extract", {
  nhgis_extract <- test_nhgis_extract()

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = ds_spec("2015_2019_ACS5a"),
    time_series_tables = "CW3",
    geographic_extents = "110"
  )

  expect_equal(revised$datasets, nhgis_extract$datasets[1])
  expect_null(revised$time_series_tables)
  expect_null(revised$tst_layout)
  expect_equal(revised$geographic_extents, "100")
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
    datasets = ds_spec("Test", "Test", "Test")
  )

  expect_null(nhgis_extract2$time_series_tables)
  expect_null(nhgis_extract2$tst_layout)

  expect_identical(
    suppressWarnings(add_to_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
})

test_that("Can remove subfields from existing spec fields", {
  cps_extract <- test_cps_extract()
  nhgis_extract <- test_nhgis_extract()

  revised_nhgis <- remove_from_extract(
    nhgis_extract,
    datasets = ds_spec("2014_2018_ACS5a", data_tables = "B01001")
  )

  expect_equal(
    revised_nhgis$datasets[["2014_2018_ACS5a"]]$data_tables,
    "B01002"
  )
  expect_equal(
    revised_nhgis$datasets[["2015_2019_ACS5a"]],
    nhgis_extract$datasets[["2015_2019_ACS5a"]]
  )

  revised_cps <- remove_from_extract(
    cps_extract,
    variables = list(
      var_spec("AGE", data_quality_flags = FALSE),
      var_spec(
        "SEX",
        attached_characteristics = "mother",
        case_selections = "2"
      )
    )
  )

  expect_null(
    revised_cps$variables[["AGE"]]$data_quality_flags
  )
  expect_equal(
    revised_cps$variables[["SEX"]]$attached_characteristics,
    "father"
  )
  expect_null(revised_cps$variables[["SEX"]]$case_selections)
  expect_null(revised_cps$variables[["SEX"]]$case_selection_type)
})

test_that("Can remove full fields and subfields simultaneously", {
  cps_extract <- test_cps_extract()

  revised_cps <- remove_from_extract(
    cps_extract,
    variables = list(
      "SEX",
      var_spec("RACE", case_selections = c("810", "811"))
    )
  )

  expect_false("SEX" %in% names(revised_cps$variables))
  expect_equal(revised_cps$variables[["RACE"]]$case_selections, "812")
})

test_that("Improper extract revisions throw warnings or errors", {
  usa_extract <- test_usa_extract()
  nhgis_extract <- test_nhgis_extract()
  atus_extract <- test_atus_extract()

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
    "Extract definition must contain values for `samples`"
  )
  expect_error(
    remove_from_extract(
      usa_extract,
      samples = usa_extract$samples
    ),
    "Extract definition must contain values for `samples`"
  )
  expect_error(
    add_to_extract(nhgis_extract, datasets = ds_spec("A", "B")),
    "`geog_levels` must not contain missing values"
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
        ds_spec("A", "A", "A"),
        ds_spec("A", "B", "B")
      )
    ),
    "cannot contain multiple `datasets` of same name"
  )
  expect_error(
    add_to_extract(
      usa_extract,
      variables = c("A", "A")
    ),
    "cannot contain multiple `variables` of same name"
  )
  expect_warning(
    add_to_extract(usa_extract, variables = c("A", var_spec("B"))),
    paste0(
      "Unexpected names in input when converting to `var_spec`.+",
      "You may have combined `var_spec` objects with `c\\(\\)`"
    )
  )
  expect_warning(
    add_to_extract(atus_extract, time_use_variables = c("A", tu_var_spec("B"))),
    paste0(
      "Unexpected names in input when converting to `tu_var_spec`.+",
      "You may have combined `tu_var_spec` objects with `c\\(\\)`"
    )
  )
})

# JSON Conversion ----------------

test_that("Can reproduce extract specs from JSON definition", {
  usa_extract <- test_usa_extract()
  cps_extract <- test_cps_extract()
  nhgis_extract <- test_nhgis_extract()
  atus_extract <- test_atus_extract()

  usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")
  nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")

  # .json method should handle appropriately if there is a collection field
  # available
  cps_json <- extract_to_request_json(cps_extract)
  atus_json <- extract_to_request_json(atus_extract)

  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))
  expect_s3_class(cps_json, "json")
  expect_s3_class(atus_json, "json")

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
  expect_identical(
    extract_list_from_json(atus_json)[[1]],
    atus_extract
  )
})

test_that("Can export to and import from JSON", {
  usa_extract <- test_usa_extract()
  nhgis_extract <- test_nhgis_extract()
  atus_extract <- test_atus_extract()

  json_tmpfile_usa <- file.path(tempdir(), "usa_extract.json")
  json_tmpfile_nhgis <- file.path(tempdir(), "nhgis_extract.json")
  json_tmpfile_atus <- file.path(tempdir(), "atus_extract.json")

  on.exit(unlink(json_tmpfile_usa), add = TRUE, after = FALSE)
  on.exit(unlink(json_tmpfile_nhgis), add = TRUE, after = FALSE)
  on.exit(unlink(json_tmpfile_atus), add = TRUE, after = FALSE)

  save_extract_as_json(usa_extract, json_tmpfile_usa)
  save_extract_as_json(nhgis_extract, json_tmpfile_nhgis)
  save_extract_as_json(atus_extract, json_tmpfile_atus)

  copy_of_usa_extract <- define_extract_from_json(json_tmpfile_usa)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile_nhgis)
  copy_of_atus_extract <- define_extract_from_json(json_tmpfile_atus)

  expect_identical(usa_extract, copy_of_usa_extract)
  expect_identical(nhgis_extract, copy_of_nhgis_extract)
  expect_identical(atus_extract, copy_of_atus_extract)

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

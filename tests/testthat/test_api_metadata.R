test_that("We can get summary metadata", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("nhgis-metadata-summary", {
    shp_meta <- get_nhgis_metadata("shapefiles")
  })

  expect_true(tibble::is_tibble(shp_meta))
  expect_true(!is_empty(shp_meta))

  # Should have more than default number of records for endpoints with
  # more records and `n_records = NULL`
  expect_true(nrow(shp_meta) > 500)
})

test_that("We can filter summary metadata", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("nhgis-metadata-filtered", {
    expect_warning(
      tst_meta_filt <- get_nhgis_metadata(
        "time_series_tables",
        description = "Sex",
        years = c("1990", "2000"),
        geographic_integration = "Standard",
        foo = "bar",
        n_records = 30 # restrict to reduce cassette size
      ),
      "unrecognized metadata variables"
    )

    ds_meta_filt <- get_nhgis_metadata(
      "datasets",
      name = c("1790_cPop", "1800_cPop"),
      match_all = FALSE
    )
  })

  expect_true(all(grepl("[Ss]ex", tst_meta_filt$description)))
  expect_true(
    all(
      purrr::map_lgl(
        tst_meta_filt$years,
        ~ all(c("1990", "2000") %in% .x$name)
      )
    )
  )
  expect_true(all(grepl("[Ss]tandard", tst_meta_filt$geographic_integration)))
  expect_equal(nrow(ds_meta_filt), 2)
})

test_that("We can restrict number of records to retrieve", {
  vcr::use_cassette("nhgis-metadata-summary-small", {
    dt_meta <- get_nhgis_metadata("data_tables", n_records = 10)
  })

  expect_equal(nrow(dt_meta), 10)
  expect_equal(
    colnames(dt_meta),
    c(
      "name", "description", "universe", "nhgis_code",
      "sequence", "dataset_name", "n_variables"
    )
  )
})

test_that("We can iterate through pages to get all records", {
  vcr::use_cassette("nhgis-metadata-summary-iterate", {
    ds_meta <- get_summary_metadata(
      collection = "nhgis",
      type = "datasets",
      n_records = 100,
      get_all_records = TRUE # not exposed to user, only used in testing
    )
  })

  expect_true(tibble::is_tibble(ds_meta))
  expect_true(nrow(ds_meta) > 100)
})

test_that("We can get metadata for single dataset", {
  skip_if_no_api_access(have_api_access)

  ds <- "2010_SF1a"

  vcr::use_cassette("nhgis-metadata-single-dataset", {
    single_ds_meta <- get_nhgis_metadata(dataset = ds)
  })

  expect_true(is_list(single_ds_meta))
  expect_equal(length(single_ds_meta), 10)
  expect_equal(
    names(single_ds_meta),
    c(
      "name", "nhgis_id", "group", "description", "sequence",
      "has_multiple_data_types", "data_tables", "geog_levels",
      "geographic_instances", "breakdowns"
    )
  )
  expect_equal(single_ds_meta$name, ds)
  expect_true(
    all(
      tibble::is_tibble(single_ds_meta$data_tables),
      tibble::is_tibble(single_ds_meta$geog_levels),
      tibble::is_tibble(single_ds_meta$geographic_instances),
      tibble::is_tibble(single_ds_meta$breakdowns),
      tibble::is_tibble(single_ds_meta$breakdowns$breakdown_values[[1]])
    )
  )
})

test_that("We can get metadata for single time series table", {
  skip_if_no_api_access(have_api_access)

  tst <- "CM0"

  vcr::use_cassette("nhgis-metadata-single-tst", {
    single_tst_meta <- get_nhgis_metadata(time_series_table = tst)
  })

  expect_true(is_list(single_tst_meta))
  expect_equal(length(single_tst_meta), 7)
  expect_equal(
    names(single_tst_meta),
    c(
      "name", "description", "geographic_integration", "sequence",
      "time_series", "years", "geog_levels"
    )
  )
  expect_equal(single_tst_meta$name, tst)
  expect_true(
    all(
      tibble::is_tibble(single_tst_meta$time_series),
      tibble::is_tibble(single_tst_meta$years),
      tibble::is_tibble(single_tst_meta$geog_levels)
    )
  )
})

test_that("We can get metadata for single data table", {
  skip_if_no_api_access(have_api_access)

  ds <- "2010_SF1a"
  dt <- "P8"

  vcr::use_cassette("nhgis-metadata-single-source", {
    single_dt_meta <- get_nhgis_metadata(dataset = ds, data_table = dt)
  })

  expect_equal(length(single_dt_meta), 7)
  expect_equal(
    names(single_dt_meta),
    c(
      "name", "description", "universe", "nhgis_code",
      "sequence", "dataset_name", "variables"
    )
  )
  expect_equal(single_dt_meta$name, dt)
  expect_true(tibble::is_tibble(single_dt_meta$variables))
})

test_that("We throw errors on bad metadata specs prior to making request", {
  skip_if_no_api_access(have_api_access)

  # Only one source per metadata request
  expect_error(
    get_nhgis_metadata(dataset = c("A", "B")),
    "Can only retrieve metadata"
  )
  expect_error(
    get_nhgis_metadata(time_series_table = c("A", "B")),
    "Can only retrieve metadata"
  )
  expect_error(
    get_nhgis_metadata(data_table = "A", dataset = c("A", "B")),
    "Can only retrieve metadata"
  )

  # Table metadata needs dataset
  expect_error(
    get_nhgis_metadata(data_table = c("A", "B")),
    "`data_table` must be specified with a corresponding `dataset`"
  )
  expect_error(
    get_nhgis_metadata(data_table = "P8"),
    "`data_table` must be specified with a corresponding `dataset`"
  )

  # This produces a low-level curl error and therefore is not submitted
  # but on other OS it is submitted and would therefore need to be mocked.
  # TODO: if we truly want to handle these errors ourselves we will
  # likely need to validate the resulting request URL before submitting.
  # expect_error(
  #   get_nhgis_metadata(data_table = "bad table", dataset = "1980_STF1")
  # )
})

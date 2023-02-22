
# Setup ------------------------------------------------------------------------

if (have_api_access) {

  # Summary metadata
  vcr::use_cassette("nhgis-metadata-summary", {
    ds_meta <- get_nhgis_metadata("datasets")
    shp_meta <- get_nhgis_metadata("shapefiles")
  })

  # Filtered metadata
  vcr::use_cassette("nhgis-metadata-filtered", {
    tst_meta_filt <- suppressWarnings(
      get_nhgis_metadata(
        "time_series_tables",
        description = c("Sex", "Age"),
        years = c("1990", "2000"),
        geographic_integration = "Standard",
        foo = "bar"
      )
    )
    ds_meta_filt <- get_nhgis_metadata(
      "datasets",
      name = c("1790_cPop", "1800_cPop"),
      match_all = FALSE
    )
  })

  # Single-source metadata
  ds <- "2010_SF1a"
  dt <- "P8"
  tst <- "CM0"

  vcr::use_cassette("nhgis-metadata-single-source", {
    single_ds_meta <- get_nhgis_metadata(dataset = ds)
    single_dt_meta <- get_nhgis_metadata(dataset = ds, data_table = dt)
    single_tst_meta <- get_nhgis_metadata(time_series_table = tst)
  })

  # API Errors
  vcr::use_cassette("nhgis-metadata-errors", {
    ds_err <- catch_cnd(get_nhgis_metadata(dataset = "bad-dataset"))$message
    dt_err <- catch_cnd(
      get_nhgis_metadata(data_table = "bad-table", dataset = "1980_STF1")
    )$message
    tst_err <- catch_cnd(
      get_nhgis_metadata(time_series_table = "bad-tst")
    )$message
    filt_err <- catch_cnd(
      get_nhgis_metadata("time_series_tables", foo = "bar")
    )$message
  })

  # Create fake table metadata to simulate need for table metadata updates
  all_ds <- unique(table_metadata$dataset)
  last_ds <- all_ds[(length(all_ds) - 4):length(all_ds)]
  tmp_metadata <- table_metadata[!table_metadata$dataset %in% last_ds, ]

  vcr::use_cassette("nhgis-metadata-missing-api-key", {
    err_no_key <- catch_cnd(withr::with_envvar(
      new = c("IPUMS_API_KEY" = NA),
      get_nhgis_metadata("datasets")
    ))$message
    err_bad_key <- catch_cnd(withr::with_envvar(
      new = c("IPUMS_API_KEY" = "foobar"),
      get_nhgis_metadata("datasets")
    ))$message
  })

}

# Tests ------------------------------------------------------------------------

test_that("We can get summary metadata", {

  skip_if_no_api_access(have_api_access)

  expect_true(tibble::is_tibble(ds_meta))
  expect_true(!is_empty(ds_meta))

  expect_true(tibble::is_tibble(tst_meta_filt))
  expect_true(!is_empty(tst_meta_filt))

  expect_true(tibble::is_tibble(shp_meta))
  expect_true(!is_empty(shp_meta))

})

test_that("We can filter summary metadata", {

  skip_if_no_api_access(have_api_access)

  expect_true(all(grepl("[Ss]ex", tst_meta_filt$description)))
  expect_true(all(grepl("[Aa]ge", tst_meta_filt$description)))
  expect_true(
    all(purrr::map_lgl(tst_meta_filt$years, ~all(c("1990", "2000") %in% .x)))
  )
  expect_true(all(grepl("[Ss]tandard", tst_meta_filt$geographic_integration)))

  expect_equal(nrow(ds_meta_filt), 2)

})

test_that("We can get metadata for specific data sources", {

  skip_if_no_api_access(have_api_access)

  expect_true(is_list(single_ds_meta))
  expect_equal(length(single_ds_meta), 10)
  expect_equal(
    names(single_ds_meta),
    c("name", "nhgis_id", "group", "description", "sequence",
      "has_multiple_data_types", "data_tables", "geog_levels",
      "geographic_instances", "breakdowns")
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

  expect_true(is_list(single_tst_meta))
  expect_equal(length(single_tst_meta), 7)
  expect_equal(
    names(single_tst_meta),
    c("name", "description", "geographic_integration", "sequence",
      "time_series", "years", "geog_levels")
  )
  expect_equal(single_tst_meta$name, tst)
  expect_true(
    all(
      tibble::is_tibble(single_tst_meta$time_series),
      tibble::is_tibble(single_tst_meta$years),
      tibble::is_tibble(single_tst_meta$geog_levels)
    )
  )

  expect_equal(length(single_dt_meta), 6)
  expect_equal(
    names(single_dt_meta),
    c("name", "description", "universe", "nhgis_code",
      "sequence", "variables")
  )
  expect_equal(single_dt_meta$name, dt)
  expect_true(tibble::is_tibble(single_dt_meta$variables))

})

test_that("We throw errors on bad metadata requests", {

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

  # API errors
  expect_true(grepl("Couldn't find Dataset", ds_err))
  expect_true(grepl("Couldn't find DataTable", dt_err))
  expect_true(grepl("Couldn't find TimeSeriesTable", tst_err))
  expect_true(grepl("Access to this API has been disallowed", err_bad_key))
  expect_true(grepl("Authorization field missing", err_no_key))

  expect_error(
    get_nhgis_metadata(data_table = "bad table", dataset = "1980_STF1"),
    "bad/illegal format or missing URL"
  )

  # Missing columns
  expect_true(grepl("unrecognized metadata variables:.+`foo`", filt_err))

})

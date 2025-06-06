test_that("We can get summary metadata", {
  skip_if_no_api_access()

  vcr::use_cassette("nhgis-metadata-summary", {
    shp_meta <- get_metadata_catalog("nhgis", "shapefiles")
  })
  vcr::use_cassette("ihgis-metadata-summary", {
    ihgis_meta <- get_metadata_catalog("ihgis", "tabulation_geographies")
  })
  vcr::use_cassette("micro-metadata-summary", {
    cps_meta <- get_sample_info("cps")
  })

  expect_true(tibble::is_tibble(shp_meta))
  expect_true(!is_empty(shp_meta))
  expect_equal(shp_meta$name[[1]], "us_state_1790_tl2000")

  expect_true(tibble::is_tibble(ihgis_meta))
  expect_true(!is_empty(ihgis_meta))
  expect_equal(
    colnames(ihgis_meta),
    c(
      "name", "label", "hierarchical_level", "mean_population",
      "mean_area", "sequence", "unit_count"
    )
  )

  expect_true(tibble::is_tibble(cps_meta))
  expect_true(!is_empty(cps_meta))
  expect_equal(cps_meta$name[[1]], "cps1962_03s")
})

test_that("We can iterate through pages to get all records", {
  skip_if_no_api_access()

  page_size <- 100

  vcr::use_cassette("nhgis-metadata-summary-paged", {
    responses <- ipums_api_paged_request(
      url = api_request_url(
        collection = "nhgis",
        path = metadata_request_path("nhgis", "datasets"),
        queries = list(pageNumber = 1, pageSize = page_size)
      ),
      max_pages = Inf
    )
  })

  metadata <- convert_metadata(
    purrr::map_dfr(
      responses,
      function(res) {
        content <- jsonlite::fromJSON(
          httr::content(res, "text"),
          simplifyVector = TRUE
        )

        content$data
      }
    )
  )

  expect_true(tibble::is_tibble(metadata))
  expect_true(!is_empty(metadata))

  # Should have more records than the page_size if pagination worked
  # as expected
  expect_true(nrow(metadata) > page_size)
})

test_that("We can get metadata for single dataset", {
  skip_if_no_api_access()

  ds <- "2010_SF1a"

  vcr::use_cassette("nhgis-metadata-single-dataset", {
    single_ds_meta <- get_metadata("nhgis", dataset = ds)
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
  skip_if_no_api_access()

  tst <- "CM0"

  vcr::use_cassette("nhgis-metadata-single-tst", {
    single_tst_meta <- get_metadata("nhgis", time_series_table = tst)
  })

  expect_true(is_list(single_tst_meta))
  expect_equal(length(single_tst_meta), 8)
  expect_equal(
    names(single_tst_meta),
    c(
      "name", "description", "geographic_integration", "sequence",
      "time_series", "years", "geog_levels", "geographic_instances"
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
  skip_if_no_api_access()

  ds <- "2010_SF1a"
  dt <- "P8"

  vcr::use_cassette("nhgis-metadata-single-source", {
    single_dt_meta <- get_metadata("nhgis", dataset = ds, data_table = dt)
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

test_that("We can get metadata for a single IHGIS data table w/o dataset", {
  skip_if_no_api_access()

  vcr::use_cassette("ihgis-metadata-data-table", {
    ihgis_dt_meta <- get_metadata("ihgis", data_table = "AL2001pop.ABK")
  })

  expect_length(ihgis_dt_meta, 9)
  expect_equal(
    names(ihgis_dt_meta),
    c(
      "name", "dataset_name", "label", "universe", "table_num",
      "sequence", "tabulation_geographies", "footnotes", "variables"
    )
  )
  expect_true(tibble::is_tibble(ihgis_dt_meta$variables))
  expect_true(!is_empty(ihgis_dt_meta$variables))
})

test_that("We throw errors on bad metadata specs prior to making request", {
  # Only one source per metadata request
  expect_error(
    get_metadata("nhgis", dataset = c("A", "B")),
    "Can only retrieve metadata"
  )
  expect_error(
    get_metadata("nhgis", time_series_table = c("A", "B")),
    "Can only retrieve metadata"
  )
  expect_error(
    get_metadata("nhgis", data_table = "A", dataset = c("A", "B")),
    "Can only retrieve metadata"
  )

  # Table metadata needs dataset
  expect_error(
    get_metadata("nhgis", data_table = c("A", "B")),
    "`data_table` must be specified with a corresponding `dataset`"
  )
  expect_error(
    get_metadata("nhgis", data_table = "P8"),
    "`data_table` must be specified with a corresponding `dataset`"
  )

  # Invalid collection and/or catalog endpoints
  expect_error(
    get_metadata("foobar", dataset = "P8"),
    "Unrecognized collection"
  )
  expect_error(
    get_metadata_catalog("foobar", "datasets"),
    "Unrecognized collection"
  )
  expect_error(
    get_metadata_catalog("nhgis", "foobar"),
    paste0(
      "`metadata_type` must be one of \"datasets\", \"data_tables\", ",
      "\"time_series_tables\", or \"shapefiles\""
    )
  )
  expect_error(
    get_metadata_catalog("usa", "datasets"),
    "`metadata_type` must be one of \"samples\""
  )
})

test_that("get_metadata_nhgis() is deprecated", {
  vcr::use_cassette("nhgis-metadata-summary", {
    lifecycle::expect_deprecated(
      shp_meta <- get_metadata_nhgis("shapefiles")
    )
  })

  ds <- "2010_SF1a"

  vcr::use_cassette("nhgis-metadata-single-dataset", {
    lifecycle::expect_deprecated(
      single_ds_meta <- get_metadata_nhgis(dataset = ds)
    )
  })

  expect_true(tibble::is_tibble(shp_meta))
  expect_true(!is_empty(shp_meta))
  expect_equal(shp_meta$name[[1]], "us_state_1790_tl2000")

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

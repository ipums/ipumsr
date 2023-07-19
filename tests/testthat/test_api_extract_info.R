# Fixture clean-up -------------------------------------------------------------

# Clean up fixtures after testing
# Files will only be modified on the run that first produces them;
# otherwise, they remain unchanged.
on.exit(
  # Remove all but last API request for wait_for_extract() requests
  # to avoid waiting on subsequent runs
  modify_ready_extract_cassette_files(
    c(
      "ready-usa-extract.yml",
      "ready-cps-extract.yml",
      "ready-nhgis-extract.yml",
      "ready-nhgis-extract-shp.yml"
    )
  ),
  add = TRUE,
  after = FALSE
)

# Tests ------------------------------------------------------------------------

# Check extract status -----------------

test_that("Can check microdata extract status", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })

  extract_number <- submitted_usa_extract$number

  vcr::use_cassette("ready-usa-extract", {
    wait_for_extract(submitted_usa_extract)
  })

  # Use same cassette for each status check. If request is incompatible,
  # vcr should error.
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(submitted_usa_extract)
  })
  vcr::use_cassette("get-usa-extract-info", {
    extract_id1 <- get_extract_info(c("usa", extract_number))
  })
  vcr::use_cassette("get-usa-extract-info", {
    extract_id2 <- get_extract_info(paste0("usa:", extract_number))
  })

  expect_s3_class(extract, "usa_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")

  expect_identical(extract, extract_id1)
  expect_identical(extract, extract_id2)

  vcr::use_cassette("get-usa-extract-info", {
    is_ready <- is_extract_ready(submitted_usa_extract)
  })
  vcr::use_cassette("get-usa-extract-info", {
    is_ready_id1 <- is_extract_ready(c("usa", extract_number))
  })
  vcr::use_cassette("get-usa-extract-info", {
    is_ready_id2 <- is_extract_ready(paste0("usa:", extract_number))
  })

  expect_true(is_ready && is_ready_id1 && is_ready_id2)
})

test_that("Can check NHGIS extract status", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })

  extract_number <- submitted_nhgis_extract$number

  vcr::use_cassette("ready-nhgis-extract", {
    wait_for_extract(submitted_nhgis_extract)
  })

  vcr::use_cassette("get-nhgis-extract-info", {
    extract <- get_extract_info(submitted_nhgis_extract)
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    extract_id1 <- get_extract_info(c("nhgis", extract_number))
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    extract_id2 <- get_extract_info(paste0("nhgis:", extract_number))
  })

  expect_s3_class(extract, "nhgis_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")

  expect_identical(extract, extract_id1)
  expect_identical(extract, extract_id2)

  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready <- is_extract_ready(submitted_nhgis_extract)
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready_id1 <- is_extract_ready(c("nhgis", extract_number))
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready_id2 <- is_extract_ready(paste0("nhgis:", extract_number))
  })

  expect_true(is_ready && is_ready_id1 && is_ready_id2)
})

test_that("Cannot check status for an extract with no number", {
  expect_error(
    get_extract_info(
      define_extract_nhgis(
        datasets = ds_spec("a", "B", "C")
      )
    ),
    "Cannot get info for an `ipums_extract` object with missing extract number"
  )
  expect_error(
    get_extract_info("nhgis"),
    "Invalid `extract` argument"
  )
})

test_that("We avoid superfluous checks when getting extract status", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })

  # This should not make an API request because the extract is already
  # ready, and therefore should succeed despite an invalid key
  # TODO: how to reconcile this behavior with fact that someone may have
  # stored a "ready_nhgis_extract" object, and then it may have expired
  # before they use wait_for_extract()?
  expect_message(
    wait_for_extract(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+is ready to download"
  )

  # Simulate an expired extract:
  ready_nhgis_extract$status <- "completed"
  ready_nhgis_extract$download_links <- EMPTY_NAMED_LIST

  expect_warning(
    ready_extract <- is_extract_ready(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+has expired"
  )
  expect_error(
    wait_for_extract(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+has expired"
  )

  # Simulate a failed extract:
  ready_nhgis_extract$status <- "failed"
  ready_nhgis_extract$download_links <- EMPTY_NAMED_LIST

  expect_warning(
    ready_extract <- is_extract_ready(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+has failed"
  )
  expect_error(
    wait_for_extract(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+has failed"
  )

  expect_false(ready_extract)
})

test_that("Can parse default collection", {
  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "nhgis"), {
    extract_id <- standardize_extract_identifier(1)

    expect_equal(extract_id$collection, "nhgis")
    expect_equal(length(extract_id), 2)
    expect_equal(extract_id$number, 1)

    expect_equal(
      standardize_extract_identifier("usa:1"),
      list(collection = "usa", number = 1)
    )
    expect_error(
      standardize_extract_identifier(
        get_default_collection(),
        collection_ok = FALSE
      ),
      "Invalid `extract` argument"
    )
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "usa"), {
    extract_id <- standardize_extract_identifier(10)
    expect_equal(extract_id$collection, "usa")
    expect_equal(length(extract_id), 2)
    expect_equal(extract_id$number, 10)
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = NA), {
    expect_error(
      standardize_extract_identifier(1),
      "No default collection set"
    )
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "fake-collection"), {
    expect_error(
      get_default_collection(),
      paste0(
        "The default collection is set to \"fake-collection\", which is not a ",
        "supported IPUMS collection"
      )
    )
  })
})

test_that("Can get extract info for default collection", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_history("nhgis")
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "nhgis"), {
    vcr::use_cassette("submitted-nhgis-extract", {
      submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
    })

    extract_number <- submitted_nhgis_extract$number

    vcr::use_cassette("ready-nhgis-extract", {
      expect_equal(
        wait_for_extract(extract_number)$status,
        "completed"
      )
    })

    vcr::use_cassette("recent-nhgis-extracts-list", {
      recent_nhgis_extracts_list_default <- get_extract_history()
    })

    expect_identical(
      recent_nhgis_extracts_list_default,
      recent_nhgis_extracts_list
    )
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = NA), {
    expect_error(
      get_extract_history(),
      "No default collection set"
    )
  })
})

# Extract history ------------------------

test_that("Can get extract history for all records", {
  skip_if_no_api_access(have_api_access)

  # We reproduce the internal workings of `get_extract_history()`
  # so we can update the page size parameter, which is otherwise not exposed.
  # This enables us to ensure that multiple pages are requested.
  vcr::use_cassette("extract-history-all-records", {
    responses <- ipums_api_paged_request(
      url = api_request_url(
        collection = "cps",
        path = extract_request_path(),
        queries = list(pageNumber = 1, pageSize = 30)
      ),
      max_pages = Inf
    )
  })

  extracts <- purrr::flatten(
    purrr::map(
      responses,
      function(res) {
        extract_list_from_json(
          new_ipums_json(
            httr::content(res, "text"),
            collection = "cps"
          )
        )
      }
    )
  )

  expect_equal(length(extracts), 94)
  expect_equal(extracts[[94]][["number"]], 1)
})

test_that("Can get extract history for more records than page size", {
  skip_if_no_api_access(have_api_access)

  # We reproduce the internal workings of `get_extract_history()`
  # so we can update the page size parameter, which is otherwise not exposed.
  # This enables us to ensure that multiple pages are requested.
  how_many <- 91
  page_limit <- 30

  vcr::use_cassette("extract-history-multi-page-records", {
    responses <- ipums_api_paged_request(
      url = api_request_url(
        collection = "cps",
        path = extract_request_path(),
        queries = list(pageNumber = 1, pageSize = page_limit)
      ),
      max_pages = ceiling(how_many / page_limit)
    )
  })

  extracts <- purrr::flatten(
    purrr::map(
      responses,
      function(res) {
        extract_list_from_json(
          new_ipums_json(
            httr::content(res, "text"),
            collection = "cps"
          )
        )
      }
    )
  )

  if (length(extracts) > how_many) {
    extracts <- extracts[seq_len(how_many)]
  }

  expect_equal(length(extracts), how_many)

  # The appropriate last record number can be discerned on the most recent
  # record and the number of records retrieved
  expect_equal(
    extracts[[how_many]]$number,
    extracts[[1]]$number - how_many + 1
  )
})

test_that("Tibble of recent micro extracts has expected structure", {
  skip_if_no_api_access(have_api_access)

  usa_extract <- test_usa_extract()

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })

  submitted_number <- submitted_usa_extract$number

  vcr::use_cassette("ready-usa-extract", {
    ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  })

  vcr::use_cassette("recent-usa-extracts-tbl", {
    lifecycle::expect_deprecated(
      recent_usa_extracts_tbl <- get_recent_extracts_info_tbl("usa")
    )
  })

  recent_numbers <- recent_usa_extracts_tbl$number

  single_usa_extract_tbl <- recent_usa_extracts_tbl[
    recent_numbers == submitted_number,
  ]

  expected_columns <- c(
    "collection", "description", "data_structure",
    "rectangular_on", "data_format", "samples", "variables",
    "case_selections", "case_selection_type", "attached_characteristics",
    "var_data_quality_flags", "preselected", "data_quality_flags",
    "case_select_who", "submitted", "download_links", "number", "status"
  )

  expect_equal(nrow(recent_usa_extracts_tbl), 10)

  expect_setequal(names(recent_usa_extracts_tbl), expected_columns)

  expect_equal(single_usa_extract_tbl$number, ready_usa_extract$number)
  expect_equal(
    single_usa_extract_tbl$variables[[1]],
    names(ready_usa_extract$variables)
  )
  expect_equal(
    single_usa_extract_tbl$samples[[1]],
    names(ready_usa_extract$samples)
  )
  expect_equal(
    single_usa_extract_tbl$case_selections[[1]],
    purrr::map(ready_usa_extract$variables, ~ .x$case_selections)
  )
  expect_equal(
    single_usa_extract_tbl$case_selection_type[[1]],
    purrr::map(ready_usa_extract$variables, ~ .x$case_selection_type)
  )
  expect_equal(
    single_usa_extract_tbl$attached_characteristics[[1]],
    purrr::map(ready_usa_extract$variables, ~ .x$attached_characteristics)
  )
  expect_equal(
    single_usa_extract_tbl$var_data_quality_flags[[1]],
    purrr::map(ready_usa_extract$variables, ~ .x$data_quality_flags)
  )
})

test_that("Tibble of recent NHGIS extracts has expected structure", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })

  submitted_number <- submitted_nhgis_extract$number

  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    lifecycle::expect_deprecated(
      recent_nhgis_extracts_tbl <- get_recent_extracts_info_tbl("nhgis")
    )
  })

  recent_numbers <- recent_nhgis_extracts_tbl$number

  expected_columns <- c(
    "collection", "number", "description", "data_type",
    "name", "data_tables",
    "geog_levels", "years",
    "breakdown_values", "geographic_extents",
    "tst_layout",
    "breakdown_and_data_type_layout", "data_format",
    "submitted", "download_links", "status"
  )

  recent_nhgis_extract_submitted_ds <- recent_nhgis_extracts_tbl[
    which(recent_numbers == submitted_number &
            recent_nhgis_extracts_tbl$data_type == "datasets"),
  ]

  recent_nhgis_extract_submitted_tst <- recent_nhgis_extracts_tbl[
    which(recent_numbers == submitted_number &
            recent_nhgis_extracts_tbl$data_type == "time_series_tables"),
  ]

  recent_nhgis_extract_submitted_shp <- recent_nhgis_extracts_tbl[
    which(recent_numbers == submitted_number &
            recent_nhgis_extracts_tbl$data_type == "shapefiles"),
  ]

  expect_setequal(names(recent_nhgis_extracts_tbl), expected_columns)
  expect_equal(length(unique(recent_numbers)), 10)

  expect_equal(
    recent_nhgis_extract_submitted_ds$name,
    names(ready_nhgis_extract$datasets)
  )
  expect_equal(
    recent_nhgis_extract_submitted_ds$data_tables,
    unname(purrr::map(ready_nhgis_extract$datasets, ~ .x$data_tables))
  )
  expect_equal(
    recent_nhgis_extract_submitted_ds$geog_levels,
    unname(purrr::map(ready_nhgis_extract$datasets, ~ .x$geog_levels))
  )
  expect_equal(
    recent_nhgis_extract_submitted_ds$years,
    unname(purrr::map(ready_nhgis_extract$datasets, ~ .x$years))
  )
  expect_equal(
    recent_nhgis_extract_submitted_ds$breakdown_values,
    unname(purrr::map(ready_nhgis_extract$datasets, ~ .x$breakdown_values))
  )
  expect_equal(
    recent_nhgis_extract_submitted_tst$name,
    names(ready_nhgis_extract$time_series_tables)
  )
  expect_equal(
    recent_nhgis_extract_submitted_shp$name,
    ready_nhgis_extract$shapefiles
  )
})

test_that("Can get specific number of recent extracts in tibble format", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-usa-extracts-tbl-two", {
    lifecycle::expect_deprecated(
      two_recent_usa_extracts <- get_recent_extracts_info_tbl(
        "usa",
        how_many = 2
      )
    )
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl-two", {
    lifecycle::expect_deprecated(
      two_recent_nhgis_extracts <- get_recent_extracts_info_tbl(
        "nhgis",
        how_many = 2
      )
    )
  })

  expect_equal(nrow(two_recent_usa_extracts), 2)

  # NHGIS does not adhere to 1-row per extract, but only 2 should be
  # represented
  expect_equal(length(unique(two_recent_nhgis_extracts$number)), 2)
})

test_that("Error on invalid number of records", {
  expect_error(
    get_extract_history("usa", how_many = 0),
    "Must request a positive number of records."
  )
})

# Tibble <--> List conversion -------------------

test_that("Microdata tbl/list conversion works", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-usa-extracts-tbl", {
    lifecycle::expect_deprecated(
      recent_usa_extracts_tbl <- get_recent_extracts_info_tbl("usa")
    )
  })
  vcr::use_cassette("recent-usa-extracts-list", {
    recent_usa_extracts_list <- get_extract_history("usa")
  })

  lifecycle::expect_deprecated(
    converted_to_list <- extract_tbl_to_list(
      recent_usa_extracts_tbl,
      validate = FALSE
    )
  )

  lifecycle::expect_deprecated(
    converted_to_tbl <- extract_list_to_tbl(recent_usa_extracts_list)
  )

  expect_identical(recent_usa_extracts_list, converted_to_list)
  expect_identical(recent_usa_extracts_tbl, converted_to_tbl)
})

test_that("NHGIS tbl/list conversion works", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    lifecycle::expect_deprecated(
      recent_nhgis_extracts_tbl <- get_recent_extracts_info_tbl("nhgis")
    )
  })
  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_history("nhgis")
  })

  lifecycle::expect_deprecated(
    converted_to_list <- extract_tbl_to_list(recent_nhgis_extracts_tbl)
  )

  lifecycle::expect_deprecated(
    converted_to_tbl <- extract_list_to_tbl(recent_nhgis_extracts_list)
  )

  expect_identical(recent_nhgis_extracts_list, converted_to_list)
  expect_identical(recent_nhgis_extracts_tbl, converted_to_tbl)

  expect_error(
    extract_list_to_tbl(
      list(
        submitted_nhgis_extract,
        define_extract_usa("test", "test", "test")
      )
    ),
    "All extracts in `extract_list` must belong to same collection"
  )

  # `test_nhgis_extract()` does not include case where multiple DS with
  # different subfields. Including here for a test of this scenario:
  x <- define_extract_nhgis(
    datasets = list(
      ds_spec("D1", "A", "C"), ds_spec("D2", "B", "C")
    )
  )

  expect_identical(
    x,
    extract_tbl_to_list(extract_to_tbl(x))[[1]]
  )
})

# Test included because conversion has behaved strangely when missing at
# least one of datasets, time_series_tables, or shapefiles in past.
test_that("NHGIS shapefile-only tbl/list conversion works", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(test_nhgis_extract_shp())
  })
  vcr::use_cassette("ready-nhgis-extract-shp", {
    ready_nhgis_extract_shp <- wait_for_extract(
      c("nhgis", submitted_nhgis_extract_shp$number)
    )
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl-one", {
    lifecycle::expect_deprecated(
      nhgis_extract_tbl_shp <- get_recent_extracts_info_tbl(
        "nhgis",
        how_many = 1
      )
    )
  })

  lifecycle::expect_deprecated(
    extract <- extract_tbl_to_list(nhgis_extract_tbl_shp)
  )

  expect_identical(
    extract[[1]],
    ready_nhgis_extract_shp
  )
})

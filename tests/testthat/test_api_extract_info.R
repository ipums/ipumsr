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

test_that("Can check the status of a USA extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })
  vcr::use_cassette("ready-usa-extract", {
    wait_for_extract(submitted_usa_extract)
  })
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(submitted_usa_extract)
  })
  vcr::use_cassette("get-usa-extract-info", {
    is_ready <- is_extract_ready(submitted_usa_extract)
  })

  expect_s3_class(extract, "usa_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  expect_true(is_ready)
})

test_that("Can check the status of a CPS extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-cps-extract", {
    submitted_cps_extract <- submit_extract(test_cps_extract())
  })
  vcr::use_cassette("ready-cps-extract", {
    wait_for_extract(submitted_cps_extract)
  })
  vcr::use_cassette("get-cps-extract-info", {
    extract <- get_extract_info(submitted_cps_extract)
  })
  vcr::use_cassette("get-cps-extract-info", {
    is_ready <- is_extract_ready(submitted_cps_extract)
  })

  expect_s3_class(extract, "cps_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  expect_true(is_ready)
})

test_that("Can check NHGIS extract status by supplying extract object", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    wait_for_extract(submitted_nhgis_extract)
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(submitted_nhgis_extract)
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready <- is_extract_ready(submitted_nhgis_extract)
  })

  expect_s3_class(checked_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_true(is_ready)
})

test_that("Can check USA extract status by supplying collection and number", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })
  vcr::use_cassette("ready-usa-extract", {
    wait_for_extract(submitted_usa_extract)
  })

  vcr::use_cassette("get-usa-extract-info", {
    extract1 <- get_extract_info(c("usa", submitted_usa_extract$number))
  })
  vcr::use_cassette("get-usa-extract-info", {
    extract2 <- get_extract_info(paste0("usa:", submitted_usa_extract$number))
  })

  expect_s3_class(extract1, "usa_extract")
  expect_s3_class(extract1, "ipums_extract")
  expect_true(extract1$status == "completed")
  expect_identical(extract1, extract2)

  vcr::use_cassette("get-usa-extract-info", {
    is_ready1 <- is_extract_ready(c("usa", submitted_usa_extract$number))
  })
  vcr::use_cassette("get-usa-extract-info", {
    is_ready2 <- is_extract_ready(paste0("usa:", submitted_usa_extract$number))
  })

  expect_true(is_ready1)
  expect_true(is_ready2)
})

test_that("Can check CPS extract status by supplying collection and number", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-cps-extract", {
    submitted_cps_extract <- submit_extract(test_cps_extract())
  })
  vcr::use_cassette("ready-cps-extract", {
    wait_for_extract(submitted_cps_extract)
  })

  vcr::use_cassette("get-cps-extract-info", {
    extract1 <- get_extract_info(c("cps", submitted_cps_extract$number))
  })
  vcr::use_cassette("get-cps-extract-info", {
    extract2 <- get_extract_info(paste0("cps:", submitted_cps_extract$number))
  })

  expect_s3_class(extract1, "cps_extract")
  expect_s3_class(extract1, "ipums_extract")
  expect_true(extract1$status == "completed")
  expect_identical(extract1, extract2)

  vcr::use_cassette("get-cps-extract-info", {
    is_ready1 <- is_extract_ready(c("cps", submitted_cps_extract$number))
  })
  vcr::use_cassette("get-cps-extract-info", {
    is_ready2 <- is_extract_ready(paste0("cps:", submitted_cps_extract$number))
  })

  expect_true(is_ready1)
  expect_true(is_ready2)
})

test_that("Can check NHGIS extract status with collection and number", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    wait_for_extract(submitted_nhgis_extract)
  })

  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract1 <- get_extract_info(
      c("nhgis", submitted_nhgis_extract$number)
    )
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract2 <- get_extract_info(
      paste0("nhgis:", submitted_nhgis_extract$number)
    )
  })

  expect_s3_class(checked_nhgis_extract1, c("nhgis_extract", "ipums_extract"))
  expect_equal(checked_nhgis_extract1$status, "completed")
  expect_identical(checked_nhgis_extract1, checked_nhgis_extract2)

  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready1 <- is_extract_ready(c("nhgis", submitted_nhgis_extract$number))
  })
  vcr::use_cassette("get-nhgis-extract-info", {
    is_ready2 <- is_extract_ready(paste0("nhgis:", submitted_nhgis_extract$number))
  })

  expect_true(is_ready1)
  expect_true(is_ready2)
})

test_that("We throw error when extract object has no number", {
  expect_error(
    get_extract_info(
      define_extract_nhgis(
        datasets = "a",
        data_tables = "B",
        geog_levels = "C"
      )
    ),
    "Cannot get info for an `ipums_extract` object with missing extract number."
  )
})

test_that("We avoid superflous checks when getting extract status", {
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
    "IPUMS NHGIS extract.+has either expired or failed."
  )
  expect_error(
    wait_for_extract(ready_nhgis_extract, api_key = NULL),
    "IPUMS NHGIS extract.+has either expired or failed"
  )

  expect_false(ready_extract)
})

test_that("Can use default collection when getting extract info", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_info("nhgis")
  })

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "nhgis"), {
    extract_id <- standardize_extract_identifier(1)

    expect_equal(extract_id$collection, "nhgis")
    expect_equal(length(extract_id), 2)
    expect_equal(extract_id$number, 1)

    vcr::use_cassette("submitted-nhgis-extract", {
      submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
    })
    vcr::use_cassette("ready-nhgis-extract", {
      expect_equal(
        wait_for_extract(submitted_nhgis_extract$number)$status,
        "completed"
      )
    })

    vcr::use_cassette("recent-nhgis-extracts-list", {
      recent_nhgis_extracts_list_default <- get_extract_info()
    })

    expect_identical(
      recent_nhgis_extracts_list_default,
      recent_nhgis_extracts_list
    )

    expect_error(
      standardize_extract_identifier(get_default_collection()),
      "Invalid `extract` argument"
    )

    # Can override:
    expect_equal(
      standardize_extract_identifier("usa:1"),
      list(collection = "usa", number = 1)
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
    expect_error(
      get_extract_info(),
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

test_that("We parse API errors on bad requests", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_info("nhgis")
  })

  bad_extract <- new_ipums_extract(
    "nhgis",
    datasets = "foo",
    data_tables = "bar",
    geog_levels = "baz"
  )

  vcr::use_cassette("nhgis-extract-errors", {
    expect_error(
      get_last_extract_info("nhgis", api_key = "foobar"),
      "API Key is either missing or invalid"
    )
    expect_error(
      get_extract_info(
        c("nhgis", recent_nhgis_extracts_list[[1]]$number + 1)
      ),
      paste0(
        "number ",
        recent_nhgis_extracts_list[[1]]$number + 1,
        " does not exist"
      )
    )
    expect_error(
      ipums_api_json_request(
        "POST",
        collection = "nhgis",
        path = NULL,
        body = extract_to_request_json(bad_extract),
        api_key = Sys.getenv("IPUMS_API_KEY")
      ),
      "Datasets invalid"
    )
  })
})

# Recent extracts ------------------------

test_that("Tibble of recent USA extracts contains expected columns", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-usa-extracts-tbl", {
    recent_usa_extracts_tbl <- get_extract_info("usa", table = TRUE)
  })

  expected_columns <- c(
    "collection", "description", "data_structure",
    "rectangular_on", "data_format", "samples", "variables",
    "submitted", "download_links", "number", "status"
  )

  expect_setequal(names(recent_usa_extracts_tbl), expected_columns)
})

test_that("Tibble of recent CPS extracts contains expected columns", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-cps-extracts-tbl", {
    recent_cps_extracts_tbl <- get_extract_info("cps", table = TRUE)
  })

  expected_columns <- c(
    "collection", "description", "data_structure",
    "rectangular_on", "data_format", "samples", "variables",
    "submitted", "download_links", "number", "status"
  )

  expect_setequal(names(recent_cps_extracts_tbl), expected_columns)
})

test_that("Tibble of recent NHGIS extracts has expected structure", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    recent_nhgis_extracts_tbl <- get_extract_info("nhgis", table = TRUE)
  })

  expected_columns <- c(
    "collection", "number", "description", "data_type",
    "name", "data_tables",
    "geog_levels", "years",
    "breakdown_values", "geographic_extents",
    "tst_layout",
    "breakdown_and_data_type_layout", "data_format",
    "submitted", "download_links", "status"
  )

  recent_nhgis_extract_submitted <- recent_nhgis_extracts_tbl[
    which(recent_nhgis_extracts_tbl$number == submitted_nhgis_extract$number &
            recent_nhgis_extracts_tbl$data_type == "datasets"),
  ]

  row_level_nhgis_tbl <- collapse_nhgis_extract_tbl(recent_nhgis_extracts_tbl)

  row_level_nhgis_tbl_submitted <- row_level_nhgis_tbl[
    which(row_level_nhgis_tbl$number == submitted_nhgis_extract$number),
  ]

  expect_setequal(names(recent_nhgis_extracts_tbl), expected_columns)
  expect_equal(length(unique(recent_nhgis_extracts_tbl$number)), 10)
  expect_equal(nrow(row_level_nhgis_tbl), 10)

  expect_equal(
    recent_nhgis_extract_submitted$name,
    ready_nhgis_extract$datasets
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$datasets[[1]],
    ready_nhgis_extract$datasets
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$data_tables[[1]],
    ready_nhgis_extract$data_tables
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$geog_levels[[1]],
    ready_nhgis_extract$geog_levels
  )
  # When NULL, values are not recycled in the tbl format:
  expect_equal(
    row_level_nhgis_tbl_submitted$years[[1]],
    ready_nhgis_extract$years
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$breakdown_values[[1]],
    ready_nhgis_extract$breakdown_values
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$time_series_tables[[1]],
    ready_nhgis_extract$time_series_tables
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$shapefiles[[1]],
    ready_nhgis_extract$shapefiles
  )
})

test_that("Can limit number of recent USA extracts to get info on", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-usa-extracts-tbl-two", {
    two_recent_usa_extracts <- get_extract_info(
      "usa",
      how_many = 2,
      table = TRUE
    )
  })
  expect_equal(nrow(two_recent_usa_extracts), 2)
})

test_that("Can limit number of recent CPS extracts to get info on", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-cps-extracts-tbl-two", {
    two_recent_cps_extracts <- get_extract_info(
      "cps",
      how_many = 2,
      table = TRUE
    )
  })
  expect_equal(nrow(two_recent_cps_extracts), 2)
})

test_that("Can limit number of recent extracts to get info on", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-nhgis-extracts-tbl-two", {
    two_recent_nhgis_extracts <- get_extract_info(
      "nhgis",
      how_many = 2,
      table = TRUE
    )
  })
  expect_equal(length(unique(two_recent_nhgis_extracts$number)), 2)
  expect_equal(nrow(collapse_nhgis_extract_tbl(two_recent_nhgis_extracts)), 2)
})

# Tibble <--> List conversion -------------------

test_that("tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-usa-extracts-tbl", {
    recent_usa_extracts_tbl <- get_extract_info("usa", table = TRUE)
  })
  vcr::use_cassette("recent-usa-extracts-list", {
    recent_usa_extracts_list <- get_extract_info("usa", table = FALSE)
  })

  converted_to_list <- extract_tbl_to_list(
    recent_usa_extracts_tbl,
    validate = FALSE
  )

  converted_to_tbl <- extract_list_to_tbl(recent_usa_extracts_list)

  expect_identical(recent_usa_extracts_list, converted_to_list)
  expect_identical(recent_usa_extracts_tbl, converted_to_tbl)
})

test_that("NHGIS tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    recent_nhgis_extracts_tbl <- get_extract_info("nhgis", table = TRUE)
  })
  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_info("nhgis", table = FALSE)
  })

  converted_to_list <- extract_tbl_to_list(recent_nhgis_extracts_tbl)
  converted_to_tbl <- extract_list_to_tbl(recent_nhgis_extracts_list)

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

  # `nhgis_extract` does not include case where multiple DS with different
  # subfields. Including here for a test of this scenario:
  x <- define_extract_nhgis(
    datasets = c("D1", "D2"),
    data_tables = list("A", "B"),
    geog_levels = "C"
  )

  expect_identical(
    x,
    extract_tbl_to_list(extract_to_tbl(x))[[1]]
  )
})

test_that("Shapefile-only can be converted from tbl to list", {
  skip_if_no_api_access(have_api_access)

  download_dir <- file.path(tempdir(), "ipums-api-downloads")

  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(test_nhgis_extract_shp())
  })
  vcr::use_cassette("ready-nhgis-extract-shp", {
    ready_nhgis_extract_shp <- wait_for_extract(
      c("nhgis", submitted_nhgis_extract_shp$number)
    )
  })
  vcr::use_cassette("recent-nhgis-extracts-tbl-one", {
    nhgis_extract_tbl_shp <- get_extract_info(
      "nhgis",
      how_many = 1,
      table = TRUE
    )
  })

  expect_identical(
    extract_tbl_to_list(nhgis_extract_tbl_shp)[[1]],
    ready_nhgis_extract_shp
  )
})

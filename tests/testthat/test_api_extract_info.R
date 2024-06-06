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
      "ready-atus-extract.yml",
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
    suppressMessages(
      submitted_usa_extract <- submit_extract(test_usa_extract())
    )
  })

  extract_number <- submitted_usa_extract$number

  vcr::use_cassette("ready-usa-extract", {
    wait_for_extract(submitted_usa_extract, verbose = FALSE)
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

  atus_extract <- test_atus_extract_submittable()

  vcr::use_cassette("submitted-atus-extract", {
    suppressMessages(
      submitted_atus_extract <- submit_extract(atus_extract)
    )
  })

  vcr::use_cassette("ready-atus-extract", {
    wait_for_extract(submitted_atus_extract, verbose = FALSE)
  })

  vcr::use_cassette("get-atus-extract-info", {
    completed_atus_extract <- get_extract_info(submitted_atus_extract)
  })

  expect_s3_class(completed_atus_extract$time_use_variables[[1]], "tu_var_spec")

  expect_equal(
    names(atus_extract$time_use_variables),
    names(completed_atus_extract$time_use_variables)
  )

  expect_equal(
    completed_atus_extract$time_use_variables$screentime$owner,
    "burkx031@umn.edu"
  )

  expect_setequal(
    atus_extract$sample_members,
    completed_atus_extract$sample_members
  )
})

test_that("Can check NHGIS extract status", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    suppressMessages(
      submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
    )
  })

  extract_number <- submitted_nhgis_extract$number

  vcr::use_cassette("ready-nhgis-extract", {
    wait_for_extract(submitted_nhgis_extract, verbose = FALSE)
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
    suppressMessages(
      submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
    )
  })
  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(
      submitted_nhgis_extract,
      verbose = FALSE
    )
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
      suppressMessages(
        submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
      )
    })

    extract_number <- submitted_nhgis_extract$number

    vcr::use_cassette("ready-nhgis-extract", {
      expect_equal(
        wait_for_extract(extract_number, verbose = FALSE)$status,
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

test_that("Error on invalid number of records", {
  expect_error(
    get_extract_history("usa", how_many = 0),
    "Must request a positive number of records."
  )
})

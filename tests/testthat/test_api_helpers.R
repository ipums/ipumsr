# Print extract ------------------------

test_that("Microdata print methods work", {
  usa_extract <- test_usa_extract()
  cps_extract <- test_cps_extract()

  expect_output(print(usa_extract), "Unsubmitted IPUMS USA extract")
  expect_output(print(cps_extract), "Unsubmitted IPUMS CPS extract")
})

test_that("NHGIS print method works", {
  nhgis_extract <- test_nhgis_extract()
  nhgis_extract_shp <- test_nhgis_extract_shp()

  expect_output(
    print(nhgis_extract),
    paste0(
      "Unsubmitted IPUMS NHGIS extract ",
      "\nDescription: Extract for R client testing",
      "\n",
      "\nDataset: 2014_2018_ACS5a",
      "\n  Tables: B01001, B01002",
      "\n  Geog Levels: nation",
      "\n",
      "\nDataset: 2015_2019_ACS5a",
      "\n  Tables: B01001, B01002",
      "\n  Geog Levels: blck_grp",
      "\n",
      "\nGeographic extents: DC, PA",
      "\n",
      "\nTime Series Table: CW3",
      "\n  Geog Levels: state",
      "\n  Years: 1990",
      "\n",
      "\nShapefiles: 110_blck_grp_2019_tl2019"
    )
  )
  expect_output(
    print(nhgis_extract_shp),
    paste0(
      "Unsubmitted IPUMS NHGIS extract ",
      "\nDescription: ",
      "\n",
      "\nShapefiles: 110_blck_grp_2019_tl2019"
    )
  )
  expect_output(
    print(
      define_extract_nhgis(
        datasets = "DS",
        data_tables = "DT",
        geog_levels = "DG",
        years = "Y1",
        breakdown_values = "B1"
      )
    ),
    paste0(
      "Unsubmitted IPUMS NHGIS extract ",
      "\nDescription: ",
      "\n",
      "\nDataset: DS",
      "\n  Tables: DT",
      "\n  Geog Levels: DG",
      "\n  Years: Y1",
      "\n  Breakdowns: B1"
    )
  )
})

test_that("NHGIS extract print coloring works", {
  local_reproducible_output(crayon = TRUE)
  expect_equal(
    format_field_for_printing(
      parent_field = list("Dataset: " = "A"),
      subfields = list(
        "Tables: " = "B",
        "Geog Levels: " = "C"
      ),
      parent_style = extract_field_styler(nhgis_print_color("dataset"), "bold"),
      subfield_style = extract_field_styler("bold"),
      padding = 1
    ),
    paste0(
      "\n\033[34m\033[1mDataset: ",
      "\033[22m\033[39mA\n  \033[1mTables: ",
      "\033[22mB\n  \033[1mGeog Levels: \033[22mC"
    )
  )

  expect_match(
    format_field_for_printing(
      parent_field = list("Dataset: " = "A"),
      subfields = list(
        "Tables: " = "B",
        "Geog Levels: " = "C"
      )
    ),
    paste0(
      "\n\n\033[0mDataset: ",
      "\033[0m\033[22m\033[23m\033[24m",
      "\033[27m\033[28m\033[29m\033[39m\033[49mA\n  ",
      "\033[0mTables: ",
      "\033[0m\033[22m\033[23m\033[24m\033[27m\033",
      "[28m\033[29m\033[39m\033[49mB\n  ",
      "\033[0mGeog Levels: \033[0m\033[22m\033[23m\033",
      "[24m\033[27m\033[28m\033[29m\033[39m\033[49mC",
      collapse = ""
    ),
    fixed = TRUE
  )

  local_reproducible_output(crayon = FALSE)
  expect_equal(
    format_field_for_printing(
      parent_field = list("Dataset: " = "A"),
      subfields = list(
        "Tables: " = "B",
        "Geog Levels: " = "C"
      ),
      parent_style = extract_field_styler(nhgis_print_color("dataset"), "bold"),
      subfield_style = extract_field_styler("bold"),
      padding = 2
    ),
    "\n\nDataset: A\n  Tables: B\n  Geog Levels: C"
  )
})

# IPUMS environment variables -------------------

test_that("set_ipums_envvar sets environment variable", {
  skip_if_not_installed("withr")

  current_ipums_api_key <- Sys.getenv("IPUMS_API_KEY")

  # Reset envvars to original state upon test completion
  withr::defer(Sys.setenv(IPUMS_API_KEY = current_ipums_api_key))

  # Ensure no envvar value exists before setting
  Sys.setenv(IPUMS_API_KEY = "")
  set_ipums_envvar(IPUMS_API_KEY = "testapikey")

  expect_equal(Sys.getenv("IPUMS_API_KEY"), "testapikey")
})

test_that("set_ipums_envvar sets environment variable and saves to .Renviron", {
  skip_if_not_installed("withr")

  current_ipums_default_collection <- Sys.getenv("IPUMS_DEFAULT_COLLECTION")
  current_ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
  current_home_dir <- Sys.getenv("HOME")
  temp_renviron_file <- file.path(tempdir(), ".Renviron")

  # Reset envvars to original state upon test completion
  withr::defer(Sys.setenv(HOME = current_home_dir))
  withr::defer(Sys.setenv(IPUMS_API_KEY = current_ipums_api_key))
  withr::defer(
    Sys.setenv(IPUMS_DEFAULT_COLLECTION = current_ipums_default_collection)
  )
  withr::defer(file.remove(temp_renviron_file))

  Sys.setenv(IPUMS_API_KEY = "")
  Sys.setenv(HOME = tempdir())
  set_ipums_envvar(IPUMS_API_KEY = "testapikey", save = TRUE)
  set_ipums_envvar(IPUMS_DEFAULT_COLLECTION = "testcollect", overwrite = TRUE)

  renviron_lines <- readLines(temp_renviron_file)

  expect_equal(Sys.getenv("IPUMS_API_KEY"), "testapikey")
  expect_true("IPUMS_API_KEY=\"testapikey\"" %in% renviron_lines)
  expect_equal(Sys.getenv("IPUMS_DEFAULT_COLLECTION"), "testcollect")
  expect_true("IPUMS_DEFAULT_COLLECTION=\"testcollect\"" %in% renviron_lines)
})

test_that("set_ipums_envvar works with existing .Renviron file", {
  skip_if_not_installed("withr")

  current_ipums_default_collection <- Sys.getenv("IPUMS_DEFAULT_COLLECTION")
  current_home_dir <- Sys.getenv("HOME")
  temp_renviron_file <- file.path(tempdir(), ".Renviron")
  temp_renviron_file_backup <- file.path(tempdir(), ".Renviron_backup")

  # Reset envvars to original state upon test completion
  withr::defer(file.remove(temp_renviron_file_backup))
  withr::defer(file.remove(temp_renviron_file))
  withr::defer(Sys.setenv(HOME = current_home_dir))
  withr::defer(
    Sys.setenv(IPUMS_DEFAULT_COLLECTION = current_ipums_default_collection)
  )

  Sys.setenv(IPUMS_DEFAULT_COLLECTION = "")
  Sys.setenv(HOME = tempdir())
  writeLines("OTHER_ENV_VAR=\"value\"", con = temp_renviron_file)
  set_ipums_envvar(IPUMS_DEFAULT_COLLECTION = "usa", save = TRUE)

  renviron_lines <- readLines(temp_renviron_file)
  renviron_backup_lines <- readLines(temp_renviron_file_backup)

  expect_true(file.exists(temp_renviron_file_backup))
  expect_equal(Sys.getenv("IPUMS_DEFAULT_COLLECTION"), "usa")
  expect_true("IPUMS_DEFAULT_COLLECTION=\"usa\"" %in% renviron_lines)

  expect_error(
    set_ipums_envvar(IPUMS_DEFAULT_COLLECTION = "nhgis", save = TRUE),
    "IPUMS_DEFAULT_COLLECTION already exists"
  )
  expect_message(
    set_ipums_envvar(
      IPUMS_DEFAULT_COLLECTION = "nhgis",
      overwrite = TRUE
    ),
    "Existing \\.Renviron file copied"
  )

  renviron_lines <- readLines(temp_renviron_file)
  renviron_backup_lines <- readLines(temp_renviron_file_backup)

  expect_false("IPUMS_DEFAULT_COLLECTION=\"usa\"" %in% renviron_lines)
  expect_true("IPUMS_DEFAULT_COLLECTION=\"usa\"" %in% renviron_backup_lines)

  expect_true("IPUMS_DEFAULT_COLLECTION=\"nhgis\"" %in% renviron_lines)
  expect_false("IPUMS_DEFAULT_COLLECTION=\"nhgis\"" %in% renviron_backup_lines)

  expect_true("OTHER_ENV_VAR=\"value\"" %in% renviron_backup_lines)

  expect_equal(Sys.getenv("IPUMS_DEFAULT_COLLECTION"), "nhgis")

  unset_ipums_envvar("IPUMS_DEFAULT_COLLECTION")

  expect_equal(Sys.getenv("IPUMS_DEFAULT_COLLECTION"), "")
  expect_false("IPUMS_DEFAULT_COLLECTION" %in% renviron_lines)
})

# API Request Errors ----------------------------------------

test_that("We handle API auth errors for extract and metadata endpoints", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("api-errors-authorization", {
    expect_error(
      withr::with_envvar(new = c("IPUMS_API_KEY" = NA), {
        get_nhgis_metadata("datasets")
      }),
      "API key is either missing or invalid"
    )
    expect_error(
      get_last_extract_info("usa", api_key = "foobar"),
      "API key is either missing or invalid"
    )
  })
})

test_that("Can parse API request error details in basic requests", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("api-errors-invalid-extract", {
    expect_error(
      ipums_api_extracts_request(
        "POST",
        url = api_request_url(
          collection = "usa",
          path = extract_request_path()
        ),
        body = extract_to_request_json(
          new_ipums_extract("usa", samples = "foo")
        ),
        api_key = Sys.getenv("IPUMS_API_KEY")
      ),
      paste0(
        "API request failed.+",
        "dataStructure.+did not contain.+",
        "variables.+did not match"
      )
    )
    expect_error(
      ipums_api_extracts_request(
        "POST",
        url = api_request_url(
          collection = "nhgis",
          path = extract_request_path()
        ),
        body = extract_to_request_json(
          new_ipums_extract(
            "nhgis",
            datasets = "foo",
            data_tables = "bar",
            geog_levels = "baz"
          )
        ),
        api_key = Sys.getenv("IPUMS_API_KEY")
      ),
      paste0(
        "API request failed.+",
        "Datasets invalid.+",
        "Data format invalid"
      )
    )
    expect_error(
      ipums_api_request(
        "GET",
        api_request_url("nhgis", "foo"),
        body = FALSE
      ),
      "API request failed with status 404.$"
    )
  })

  vcr::use_cassette("api-errors-invalid-metadata", {
    expect_error(
      get_summary_metadata("nhgis", type = "foo"),
      "API request failed with status 404.$"
    )
    expect_error(
      get_nhgis_metadata(dataset = "foo"),
      "API request failed.+Couldn\'t find Dataset"
    )
    expect_error(
      get_nhgis_metadata(data_table = "foo", dataset = "1980_STF1"),
      "API request failed.+Couldn\'t find DataTable"
    )
    expect_error(
      get_nhgis_metadata(time_series_table = "foo"),
      "API request failed.+Couldn\'t find TimeSeriesTable"
    )
  })
})

test_that("Can parse API request error details in paged requests", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("api-errors-paged-extract", {
    expect_error(
      ipums_api_paged_request(
        url = api_request_url(
          collection = "nhgis",
          path = extract_request_path(),
          queries = list(pageSize = 3000)
        )
      ),
      "API request failed.+Invalid pageSize: 3000"
    )
  })
})

test_that("We inform user about invalid extract number request", {
  skip_if_no_api_access(have_api_access)

  # API itself returns empty-bodied response for an invalid extract number
  # for a given collection, but we want to inform user that the error
  # resulted from their extract number not existing.
  vcr::use_cassette("api-errors-invalid-extract-number", {
    most_recent <- get_last_extract_info("nhgis")

    expect_error(
      get_extract_info(c("nhgis", most_recent$number + 1)),
      paste0(
        "number ", most_recent$number + 1,
        " does not exist"
      )
    )
  })
})

test_that("We inform user about expired extract for invalid download request", {
  skip_if_no_api_access(have_api_access)

  # Currently, the API downloads a file containing the 404 error, so for now
  # ensure that we only write this to a temp file.
  # TODO: In the future we will want to ensure we don't download on an erroneous
  # request.
  file <- file.path(tempdir(), "foo.zip")

  on.exit(unlink(file), add = TRUE, after = FALSE)

  expect_error(
    ipums_api_download_request(
      url = "https://api.ipums.org/downloads/nhgis/api/v1/extracts/foo.zip",
      file_path =  file,
      overwrite = TRUE
    ),
    "API request failed.+The extract may have expired"
  )
})

test_that("We catch invalid collection specifications during requests", {
  skip_if_no_api_access(have_api_access)

  # Ideally we'd catch before request, as API message suggests all ipums
  # collections are available.
  expect_error(
    api_request_url(collection = "foo", path = extract_request_path()),
    "No API version found for collection \"foo\""
  )

  # But ensure that the error is still caught by the API if `api_request_url()`
  # is not used to form URL.
  vcr::use_cassette("api-errors-invalid-collection", {
    expect_error(
      ipums_api_request(
        "GET",
        url = "https://api.ipums.org/extracts/?collection=foo&version=2",
        body = FALSE
      ),
      "API request failed.+The \'collection\' query parameter is invalid"
    )
  })
})

# Misc ------------------------------------------

test_that("We can get correct API version info for each collection", {
  has_support <- dplyr::filter(
    ipums_data_collections(),
    .data$api_support
  )

  expect_equal(
    has_support$code_for_api,
    c("usa", "cps", "nhgis")
  )
  expect_equal(ipums_api_version(), 2)
  expect_equal(check_api_support("nhgis"), "nhgis")
  expect_error(
    get_extract_history("fake-collection"),
    "No API version found"
  )
})

test_that("standardize_extract_identifier handles unusual cases", {
  expect_equal(
    standardize_extract_identifier("nhgis:1L"),
    list(collection = "nhgis", number = 1)
  )
  expect_error(
    standardize_extract_identifier("usa:1.2"),
    "Unable to interpret extract number 1.2 as integer"
  )
  expect_error(
    standardize_extract_identifier("fake-collection:1"),
    "No API version found for collection \"fake-collection\""
  )
  expect_error(
    standardize_extract_identifier("fake-collection", collection_ok = TRUE),
    "No API version found for collection \"fake-collection\""
  )
  expect_equal(
    standardize_extract_identifier("nhgis", collection_ok = TRUE),
    list(collection = "nhgis", number = NA)
  )
})

test_that("Can toggle demo API URL", {
  skip_if_not_installed("withr")

  withr::with_envvar(c("IPUMS_API_INSTANCE" = NA), {
    expect_equal(
      api_base_url(),
      "https://api.ipums.org/"
    )
  })
  withr::with_envvar(c("IPUMS_API_INSTANCE" = "demo"), {
    expect_equal(
      api_base_url(),
      "https://demo.api.ipums.org/"
    )
  })
  withr::with_envvar(c("IPUMS_API_INSTANCE" = "foobar"), {
    expect_equal(
      api_base_url(),
      "https://api.ipums.org/"
    )
  })
})

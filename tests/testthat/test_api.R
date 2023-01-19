library(dplyr)
library(purrr)


# Setup ----
usa_extract <- define_extract_usa(
  samples = "us2017b",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

cps_extract <- define_extract_cps(
  samples = c("cps1976_01s", "cps1976_02b"),
  variables = c("YEAR", "MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1"),
  description = "Compare age-sex-race breakdowns 1976",
  data_format = "fixed_width"
)

if (have_api_access) {
  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })

  vcr::use_cassette("submitted-cps-extract", {
    submitted_cps_extract <- submit_extract(cps_extract)
  })

  vcr::use_cassette("ready-usa-extract", {
    ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  })

  vcr::use_cassette("ready-cps-extract", {
    ready_cps_extract <- wait_for_extract(submitted_cps_extract)
  })

  # Modify ready-<collection>-extract.yml files to only include the final http
  # request, so that they return the ready-to-download extract immediately on
  # subsequent runs.
  modify_ready_extract_cassette_file("ready-usa-extract.yml")
  modify_ready_extract_cassette_file("ready-cps-extract.yml")


  vcr::use_cassette("recent-usa-extracts-list", {
    recent_usa_extracts_list <- get_extract_info("usa")
  })

  vcr::use_cassette("recent-cps-extracts-list", {
    recent_cps_extracts_list <- get_extract_info("cps")
  })

  vcr::use_cassette("recent-usa-extracts-tbl", {
    recent_usa_extracts_tbl <- get_extract_info("usa", table = TRUE)
  })

  vcr::use_cassette("recent-cps-extracts-tbl", {
    recent_cps_extracts_tbl <- get_extract_info("cps", table = TRUE)
  })
}


# Tests ----


# > Define extract ----
test_that("Can define a USA extract", {
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
  expect_s3_class(cps_extract, "cps_extract")
  expect_s3_class(cps_extract, "ipums_extract")
  expect_equal(cps_extract$variables[[1]], "YEAR")
  expect_equal(cps_extract$data_structure, "rectangular")
  expect_equal(cps_extract$rectangular_on, "P")
  expect_identical(cps_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(cps_extract$submitted)
  expect_equal(cps_extract$number, NA_integer_)
  expect_equal(cps_extract$status, "unsubmitted")
})

test_that("Attempt to define a hierarchical extract throws an error", {
  expect_error(
    define_extract_usa(
      "Test", "us2017b", "YEAR", data_structure = "hierarchical"
    ),
    regexp = "must be equal to \"rectangular\""
  )
  expect_error(
    define_extract_cps(
      "Test", "us2017b", "YEAR", rectangular_on = "H"
    ),
    "Currently, the `rectangular_on` argument must be equal to \"P\""
  )
})


test_that("Attempt to rectangularize on H records throws an error", {
  expect_error(
    define_extract_usa(
      "Test", "us2017b", "YEAR", rectangular_on = "H"
    ),
    regexp = "`rectangular_on` argument must be equal to \"P\""
  )
})


# > Submit extract ----
test_that("Can submit a USA extract", {
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_usa_extract, "usa_extract")
  expect_s3_class(submitted_usa_extract, "ipums_extract")
  expect_equal(submitted_usa_extract$collection, "usa")
  expect_true(submitted_usa_extract$submitted)
  expect_equal(submitted_usa_extract$status, "queued")
  expect_identical(
    submitted_usa_extract$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})


test_that("Can submit a CPS extract", {
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_cps_extract, "cps_extract")
  expect_s3_class(submitted_cps_extract, "ipums_extract")
  expect_equal(submitted_cps_extract$collection, "cps")
  expect_true(submitted_cps_extract$submitted)
  expect_equal(submitted_cps_extract$status, "queued")
  expect_identical(
    submitted_cps_extract$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})


# > Print extract ----
test_that("usa_extract print method works", {
  expect_output(print(usa_extract), regexp = "Unsubmitted IPUMS USA extract")
})


test_that("cps_extract print method works", {
  expect_output(print(cps_extract), regexp = "Unsubmitted IPUMS CPS extract")
})

test_that("ipums_extract validate method works", {
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
    validate_ipums_extract(
      new_ipums_extract(
        "usa"
      )
    ),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "usa",
        description = ""
      )
    ),
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

test_that("Can check the status of a USA extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(submitted_usa_extract)
  })
  expect_s3_class(extract, "usa_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(submitted_usa_extract)
  })
  expect_true(is_ready)
})


test_that("Can check the status of a CPS extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-cps-extract-info", {
    extract <- get_extract_info(submitted_cps_extract)
  })
  expect_s3_class(extract, "cps_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-cps-extract-ready", {
    is_ready <- is_extract_ready(submitted_cps_extract)
  })
  expect_true(is_ready)
})


test_that("Can check the status of a USA extract by supplying collection and number", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(c("usa", submitted_usa_extract$number))
  })
  expect_s3_class(extract, "usa_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(c("usa", submitted_usa_extract$number))
  })
  expect_true(is_ready)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(paste0("usa:", submitted_usa_extract$number))
  })
  expect_s3_class(extract, "usa_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(paste0("usa:", submitted_usa_extract$number))
  })
  expect_true(is_ready)
})


test_that("Can check the status of a CPS extract by supplying collection and number", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-cps-extract-info", {
    extract <- get_extract_info(c("cps", submitted_cps_extract$number))
  })
  expect_s3_class(extract, "cps_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-cps-extract-ready", {
    is_ready <- is_extract_ready(c("cps", submitted_cps_extract$number))
  })
  expect_true(is_ready)
  vcr::use_cassette("get-cps-extract-info", {
    extract <- get_extract_info(paste0("cps:", submitted_cps_extract$number))
  })
  expect_s3_class(extract, "cps_extract")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-cps-extract-ready", {
    is_ready <- is_extract_ready(paste0("cps:", submitted_cps_extract$number))
  })
  expect_true(is_ready)
})

test_that("extract_list_from_json reproduces extract specs", {

  usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")
  cps_json <- new_ipums_json(extract_to_request_json(cps_extract), "cps")

  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_s3_class(cps_json, c("cps_json", "ipums_json"))
  expect_identical(
    extract_list_from_json(usa_json)[[1]],
    usa_extract
  )
  expect_identical(
    extract_list_from_json(cps_json)[[1]],
    cps_extract
  )

})

# > Get recent extracts info ----
test_that("Tibble of recent USA extracts contains expected columns", {
  skip_if_no_api_access(have_api_access)
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_usa_extracts_tbl), expected_columns)
})


test_that("Tibble of recent CPS extracts contains expected columns", {
  skip_if_no_api_access(have_api_access)
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_cps_extracts_tbl), expected_columns)
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


# > Download extract ----
if (have_api_access) {
  download_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-usa-extract-ipums-extract.yml"
  )

  already_existed <- file.exists(download_extract_cassette_file)
}


download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)
on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

tryCatch(
  vcr::use_cassette("download-usa-extract-ipums-extract", {
    test_that("Can download a USA extract by supplying extract object", {
      skip_if_no_api_access(have_api_access)
      expect_message(
        ddi_file_path <- download_extract(
          ready_usa_extract,
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)


if (have_api_access) {
  if (!already_existed) {
    convert_paths_in_cassette_file_to_relative(download_extract_cassette_file)
  }

  download_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-usa-extract-collection-number.yml"
  )

  already_existed <- file.exists(download_extract_cassette_file)
}

tryCatch(
  vcr::use_cassette("download-usa-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download a USA extract by supplying collection and number as vector", {
      expect_message(
        ddi_file_path <- download_extract(
          c("usa", ready_usa_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)

if (have_api_access) {
  if (!already_existed) {
    convert_paths_in_cassette_file_to_relative(download_extract_cassette_file)
  }
}

tryCatch(
  vcr::use_cassette("download-usa-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download a USA extract by supplying collection and number as string", {
      expect_message(
        ddi_file_path <- download_extract(
          paste0("usa:", ready_usa_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)


# > Submit extract errors ----
test_that("An extract request with missing collection returns correct error", {
  expect_error(
    submit_extract(ipumsr:::new_ipums_extract()),
    paste0(
      "`collection` must not contain missing.+",
      "`description` must not contain missing"
    )
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    submit_extract(ipumsr:::new_ipums_extract(collection = "usa")),
    "`description` must not contain missing values"
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    submit_extract(
      ipumsr:::new_ipums_extract(collection = "usa", description = "Test")
    ),
    paste0(
      "`data_structure` must not contain missing values.+",
      "`data_format` must not contain missing values.+",
      "`samples` must not contain missing values.+",
      "`variables` must not contain missing values"
    )
  )
})


# > Add to / remove from extract ----
test_that("Can add to a USA extract", {
  revised_extract <- add_to_extract(
    usa_extract,
    samples = "us2014a",
    variables = "RELATE"
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
})


test_that("Can add to a CPS extract", {
  revised_extract <- add_to_extract(
    cps_extract,
    samples = "cps2019_03s",
    variables = "RELATE"
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
})


test_that("Can add to a submitted USA extract", {
  skip_if_no_api_access(have_api_access)
  revised_extract <- add_to_extract(
    submitted_usa_extract,
    samples = c("us2014a", "us2015a"),
    variables = list("RELATE", "AGE", "SEX", "SEX")
  )
  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(
    revised_extract$samples,
    union(submitted_usa_extract$samples, c("us2014a", "us2015a"))
  )
  expect_equal(
    revised_extract$variables,
    union(submitted_usa_extract$variables, c("RELATE", "AGE", "SEX"))
  )
})


test_that("Can remove from a USA extract", {

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
  expect_equal(
    revised_extract$variables,
    c("YEAR", "RELATE")
  )
})


test_that("Can remove from a CPS extract", {

  revised_extract <- remove_from_extract(
    cps_extract,
    samples = "cps1976_02b",
    variables = c("MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1")
  )

  expect_true(revised_extract$status == "unsubmitted")

  expect_equal(revised_extract$samples, "cps1976_01s")
  expect_equal(
    revised_extract$variables,
    c("YEAR")
  )
})


test_that("Unused revisions do not alter USA extract", {
  expect_identical(usa_extract, add_to_extract(usa_extract))
  expect_identical(usa_extract, remove_from_extract(usa_extract))
  expect_identical(
    usa_extract,
    suppressWarnings(
      add_to_extract(
        usa_extract,
        samples = usa_extract$samples
      )
    )
  )
  expect_identical(
    usa_extract,
    suppressWarnings(
      remove_from_extract(
        usa_extract,
        variables = "not in extract"
      )
    )
  )
})


test_that("Unused revisions do not alter CPS extract", {
  expect_identical(cps_extract, add_to_extract(cps_extract))
  expect_identical(cps_extract, remove_from_extract(cps_extract))
  expect_identical(
    cps_extract,
    suppressWarnings(
      add_to_extract(
        cps_extract,
        samples = cps_extract$samples
      )
    )
  )
  expect_identical(
    cps_extract,
    suppressWarnings(
      remove_from_extract(
        cps_extract,
        variables = "not in extract"
      )
    )
  )
})


test_that("Improper extract revisions throw warnings or errors", {
  expect_warning(
    add_to_extract(usa_extract, samples = "us2017b"),
    regexp = "already included"
  )
  expect_warning(
    remove_from_extract(cps_extract, variables = "RELATE"),
    regexp = "are not included"
  )
  expect_warning(
    remove_from_extract(usa_extract,
                        description = "description",
                        invalid = "invalid"),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed: `description`, `invalid`\nSee ",
      "`add_to_extract\\(\\)`"
    )
  )
  expect_warning(
    add_to_extract(cps_extract,
                   description = "description",
                   invalid = "invalid"),
    paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified: `invalid`"
    )
  )
  expect_silent(
    add_to_extract(cps_extract, description = "description")
  )
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


# > tibble <-> list conversion ----
test_that("tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)
  converted_to_list <- extract_tbl_to_list(
    recent_usa_extracts_tbl,
    validate = FALSE
  )
  converted_to_tbl <- extract_list_to_tbl(recent_usa_extracts_list)
  expect_identical(recent_usa_extracts_list, converted_to_list)
  expect_identical(recent_usa_extracts_tbl, converted_to_tbl)
})


# > Save as / define from JSON ----
test_that("We can export to and import from JSON", {
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(usa_extract, json_tmpfile)
  copy_of_usa_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(usa_extract, copy_of_usa_extract)
})


test_that("We can export to and import from JSON, submitted extract", {
  skip_if_no_api_access(have_api_access)
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(submitted_usa_extract, json_tmpfile)
  copy_of_submitted_usa_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(
    ipumsr:::copy_ipums_extract(submitted_usa_extract),
    copy_of_submitted_usa_extract
  )
})


# > Set IPUMS API key ----
test_that("set_ipums_envvar sets environment variable", {
  skip_if_not_installed("withr")
  current_ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
  withr::defer(Sys.setenv(IPUMS_API_KEY = current_ipums_api_key))
  Sys.setenv(IPUMS_API_KEY = "")
  set_ipums_envvar(IPUMS_API_KEY = "testapikey")
  expect_equal(Sys.getenv("IPUMS_API_KEY"), "testapikey")

  Sys.setenv("IPUMS_DEFAULT_COLLECTION" = "fake-collection")
  expect_error(
    get_default_collection(),
    paste0(
      "The default collection is set to \"fake-collection\", which is not a ",
      "supported IPUMS collection"
    )
  )
})

test_that("set_ipums_envvar sets environment variable and saves to .Renviron", {
  skip_if_not_installed("withr")
  current_ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
  current_home_dir <- Sys.getenv("HOME")
  temp_renviron_file <- file.path(tempdir(), ".Renviron")
  withr::defer(Sys.setenv(HOME = current_home_dir))
  withr::defer(Sys.setenv(IPUMS_API_KEY = current_ipums_api_key))
  withr::defer(file.remove(temp_renviron_file))

  Sys.setenv(IPUMS_API_KEY = "")
  Sys.setenv(HOME = tempdir())
  set_ipums_envvar(IPUMS_API_KEY = "testapikey", save = TRUE)
  expect_equal(Sys.getenv("IPUMS_API_KEY"), "testapikey")
  renviron_lines <- readLines(temp_renviron_file)
  expect_true("IPUMS_API_KEY=\"testapikey\"" %in% renviron_lines)
})


test_that("set_ipums_envvar works with existing .Renviron file", {
  skip_if_not_installed("withr")
  current_ipums_default_collection <- Sys.getenv("IPUMS_DEFAULT_COLLECTION")
  current_home_dir <- Sys.getenv("HOME")
  temp_renviron_file <- file.path(tempdir(), ".Renviron")
  temp_renviron_file_backup <- file.path(tempdir(), ".Renviron_backup")
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
  expect_true(file.exists(temp_renviron_file_backup))
  expect_equal(Sys.getenv("IPUMS_DEFAULT_COLLECTION"), "usa")
  renviron_lines <- readLines(temp_renviron_file)
  expect_true("IPUMS_DEFAULT_COLLECTION=\"usa\"" %in% renviron_lines)

  expect_error(
    set_ipums_envvar(IPUMS_DEFAULT_COLLECTION = "nhgis", save = TRUE),
    "IPUMS_DEFAULT_COLLECTION already exists"
  )

  expect_message(
    set_ipums_envvar(
      IPUMS_DEFAULT_COLLECTION = "nhgis",
      save = TRUE,
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

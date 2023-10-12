# Fixture clean-up -------------------------------------------------------------

# Clean up fixtures after testing
# Files will only be modified on the run that first produces them;
# otherwise, they remain unchanged.
on.exit(
  {
    # Remove all but last API request for wait_for_extract() requests
    # to avoid waiting on subsequent runs
    modify_ready_extract_cassette_files(
      c(
        "ready-usa-extract.yml",
        "ready-cps-extract.yml",
        "ready-nhgis-extract.yml",
        "ready-nhgis-extract-shp.yml"
      )
    )

    # Convert paths in download fixtures to relative
    download_fixtures <- list.files(
      vcr::vcr_test_path("fixtures"),
      pattern = "^download"
    )

    purrr::walk(
      download_fixtures,
      function(file) {
        try(
          convert_paths_in_cassette_file_to_relative(
            file.path(vcr::vcr_test_path("fixtures"), file)
          ),
          silent = TRUE
        )
      }
    )
  },
  add = TRUE,
  after = FALSE
)

# Tests ------------------------------------------------------------------------

# Submit extract ----------------------

test_that("Can submit a USA extract", {
  skip_if_no_api_access(have_api_access)

  usa_extract <- test_usa_extract()

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })

  expect_s3_class(submitted_usa_extract, "usa_extract")
  expect_s3_class(submitted_usa_extract, "ipums_extract")
  expect_equal(submitted_usa_extract$collection, "usa")
  expect_true(submitted_usa_extract$submitted)
  expect_equal(submitted_usa_extract$status, "queued")
  expect_identical(submitted_usa_extract$download_links, EMPTY_NAMED_LIST)

  expect_identical(submitted_usa_extract$samples, usa_extract$samples)

  expect_equal(
    submitted_usa_extract$case_select_who,
    usa_extract$case_select_who
  )
  expect_equal(
    submitted_usa_extract$data_quality_flags,
    usa_extract$data_quality_flags
  )

  usa_vars <- names(usa_extract$variables)

  # Extra preselected vars will be returned as well
  expect_true(
    all(usa_vars %in% names(submitted_usa_extract$variables))
  )

  purrr::walk(
    usa_vars,
    function(var) {
      expect_equal(
        submitted_usa_extract[[var]]$case_selections,
        usa_extract[[var]]$case_selections
      )
      expect_equal(
        submitted_usa_extract[[var]]$case_selection_type,
        usa_extract[[var]]$case_selection_type
      )
      # Some additional attached char names will be returned by server
      expect_true(
        all(
          submitted_usa_extract[[var]]$attached_characteristics %in%
            usa_extract[[var]]$attached_characteristics
        )
      )
      expect_equal(
        submitted_usa_extract[[var]]$data_quality_flags,
        usa_extract[[var]]$data_quality_flags
      )
    }
  )
})

test_that("Can submit a CPS extract", {
  skip_if_no_api_access(have_api_access)

  cps_extract <- test_cps_extract()

  vcr::use_cassette("submitted-cps-extract", {
    submitted_cps_extract <- submit_extract(cps_extract)
  })

  expect_s3_class(submitted_cps_extract, "cps_extract")
  expect_s3_class(submitted_cps_extract, "ipums_extract")
  expect_equal(submitted_cps_extract$collection, "cps")
  expect_true(submitted_cps_extract$submitted)
  expect_equal(submitted_cps_extract$status, "queued")
  expect_identical(submitted_cps_extract$download_links, EMPTY_NAMED_LIST)

  expect_identical(submitted_cps_extract$samples, cps_extract$samples)

  expect_equal(
    submitted_cps_extract$case_select_who,
    cps_extract$case_select_who
  )
  expect_equal(
    submitted_cps_extract$data_quality_flags,
    cps_extract$data_quality_flags
  )

  cps_vars <- names(cps_extract$variables)

  # Extra preselected vars will be returned as well
  expect_true(
    all(cps_vars %in% names(submitted_cps_extract$variables))
  )

  purrr::walk(
    cps_vars,
    function(var) {
      expect_equal(
        submitted_cps_extract[[var]]$case_selections,
        cps_extract[[var]]$case_selections
      )
      expect_equal(
        submitted_cps_extract[[var]]$case_selection_type,
        cps_extract[[var]]$case_selection_type
      )
      # Some additional attached char names will be returned by server
      expect_true(
        all(
          submitted_cps_extract[[var]]$attached_characteristics %in%
            cps_extract[[var]]$attached_characteristics
        )
      )
      expect_equal(
        submitted_cps_extract[[var]]$data_quality_flags,
        cps_extract[[var]]$data_quality_flags
      )
    }
  )
})

test_that("Can submit an NHGIS extract of multiple types", {
  skip_if_no_api_access(have_api_access)

  nhgis_extract <- test_nhgis_extract()

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(nhgis_extract)
  })

  expect_s3_class(submitted_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$collection, "nhgis")
  expect_identical(submitted_nhgis_extract$datasets, nhgis_extract$datasets)
  expect_identical(submitted_nhgis_extract$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(submitted_nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(submitted_nhgis_extract$geographic_extents, c("110", "100"))
  expect_true(submitted_nhgis_extract$submitted)
  expect_equal(submitted_nhgis_extract$status, "queued")
  expect_identical(submitted_nhgis_extract$download_links, EMPTY_NAMED_LIST)
})

test_that("Can submit an NHGIS extract of a single type", {
  skip_if_no_api_access(have_api_access)

  nhgis_extract_shp <- test_nhgis_extract_shp()

  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(nhgis_extract_shp)
  })

  expect_s3_class(
    submitted_nhgis_extract_shp,
    c("nhgis_extract", "ipums_extract")
  )
  expect_equal(submitted_nhgis_extract_shp$collection, "nhgis")
  expect_true(is.null(submitted_nhgis_extract_shp$datasets))
  expect_true(is.null(submitted_nhgis_extract_shp$time_series_table))
  expect_equal(
    submitted_nhgis_extract_shp$shapefiles,
    nhgis_extract_shp$shapefiles
  )
  expect_true(submitted_nhgis_extract_shp$submitted)
  expect_equal(submitted_nhgis_extract_shp$status, "queued")
  expect_identical(submitted_nhgis_extract_shp$download_links, EMPTY_NAMED_LIST)
})

test_that("Can resubmit an extract", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })
  vcr::use_cassette("resubmitted-nhgis-extract", {
    resubmitted_nhgis_extract <- submit_extract(ready_nhgis_extract)
  })

  # Number, download links, etc. won't be same, but core extract will:
  idx <- which(
    !names(resubmitted_nhgis_extract) %in%
      c("download_links", "number", "status")
  )

  expect_s3_class(
    resubmitted_nhgis_extract,
    c("nhgis_extract", "ipums_extract")
  )
  expect_identical(
    resubmitted_nhgis_extract[idx],
    ready_nhgis_extract[idx]
  )
})

test_that("Revisions update status of submitted extract", {
  skip_if_no_api_access(have_api_access)

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


# Download extract ---------------------

test_that("Can download microdata extract with extract object", {
  skip_if_no_api_access(have_api_access)

  download_dir <- file.path(tempdir(), "ipums-api-downloads")

  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })
  vcr::use_cassette("ready-usa-extract", {
    ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  })

  expect_equal(
    names(ready_usa_extract$download_links),
    c("r_command_file", "basic_codebook", "data", "stata_command_file",
      "sas_command_file", "spss_command_file", "ddi_codebook")
  )
  expect_equal(
    names(ready_usa_extract$download_links$data),
    c("url", "bytes", "sha256")
  )

  expect_message(
    vcr::use_cassette("download-usa-extract", {
      ddi_file_path <- download_extract(
        ready_usa_extract,
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    "DDI codebook file saved to"
  )

  ddi_file_path <- file.path(
    vcr::vcr_test_path("fixtures"),
    basename(ddi_file_path)
  )

  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))

  data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
  expect_equal(dim(data), c(910, 10))
})

test_that("Can download NHGIS extract with extract object", {
  skip_if_no_api_access(have_api_access)

  download_dir <- file.path(tempdir(), "ipums-api-downloads")

  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })

  expect_equal(
    names(ready_nhgis_extract$download_links),
    c("codebook_preview", "table_data", "gis_data")
  )
  expect_equal(
    names(ready_nhgis_extract$download_links$table_data),
    c("url", "bytes", "sha256")
  )

  expect_message(
    vcr::use_cassette("download-nhgis-extract", {
      file_paths <- download_extract(
        ready_nhgis_extract,
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    "Data file saved to "
  )

  expect_error(
    download_extract(
      ready_nhgis_extract,
      download_dir = vcr::vcr_test_path("fixtures"),
      overwrite = FALSE
    ),
    "The following files already exist: "
  )

  expect_equal(length(file_paths), 2)
  expect_equal(names(file_paths), c("data", "shape"))

  table_data_file_path <- file.path(
    vcr::vcr_test_path("fixtures"),
    basename(file_paths[1])
  )

  gis_data_file_path <- file.path(
    vcr::vcr_test_path("fixtures"),
    basename(file_paths[2])
  )

  expect_match(table_data_file_path, "_csv\\.zip$")
  expect_match(gis_data_file_path, "_shape\\.zip$")

  expect_true(file.exists(table_data_file_path))
  expect_true(file.exists(gis_data_file_path))
})

test_that("Can download extract with extract id", {
  skip_if_no_api_access(have_api_access)

  download_dir <- file.path(tempdir(), "ipums-api-downloads")

  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })
  vcr::use_cassette("ready-usa-extract", {
    ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  })

  expect_message(
    vcr::use_cassette("download-usa-extract-collection-number", {
      ddi_file_path1 <- download_extract(
        c("usa", ready_usa_extract$number),
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    "DDI codebook file saved to"
  )

  expect_message(
    vcr::use_cassette("download-usa-extract-collection-number", {
      ddi_file_path2 <- download_extract(
        paste0("usa:", ready_usa_extract$number),
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    "DDI codebook file saved to"
  )

  expect_identical(ddi_file_path1, ddi_file_path2)

  ddi_file_path1 <- file.path(
    vcr::vcr_test_path("fixtures"),
    basename(ddi_file_path1)
  )

  expect_match(ddi_file_path1, "\\.xml$")
  expect_true(file.exists(ddi_file_path1))

  data <- read_ipums_micro(ddi_file_path1, verbose = FALSE)
  expect_equal(dim(data), c(910, 10))
})

# TODO: may want to update this fixture to use smaller data to test
# that an extract with a single download file type still works...
test_that("Can download shapefile-only extract", {
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
    ready_nhgis_extract_shp <- wait_for_extract(submitted_nhgis_extract_shp)
  })

  expect_message(
    vcr::use_cassette("download-nhgis-extract-shp", {
      file_paths <- download_extract(
        ready_nhgis_extract_shp,
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    "Shapefile saved to"
  )

  expect_equal(length(file_paths), 1)
  expect_equal(names(file_paths), "shape")

  gis_data_file_path <- file.path(
    vcr::vcr_test_path("fixtures"),
    basename(file_paths)
  )

  expect_match(gis_data_file_path, "_shape\\.zip$")
  expect_true(file.exists(gis_data_file_path))
})

test_that("Download extract errors on incomplete extract", {
  vcr::use_cassette("download-extract-not-ready", {
    expect_error(
      download_extract(submit_extract(test_usa_extract())),
      "not ready to download.+Use `wait_for_extract\\(\\)`"
    )
  })
  # TODO: test behavior for expired and failed extracts
  # This is tougher because get_extract_info() will always update status
  # before attempting download. We test `ipums_api_download_request()` for this,
  # but these errors may trigger errors before making the request.
})

# Read downloaded files ------------------

test_that("Can read downloaded files with ipumsr readers", {
  skip_if_no_api_access(have_api_access)
  skip_if_not_installed("sf")

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })
  vcr::use_cassette("ready-nhgis-extract", {
    number <- wait_for_extract(submitted_nhgis_extract)$number
  })

  table_data_file_path <- list.files(
    vcr::vcr_test_path("fixtures"),
    pattern = paste0(number, "_csv"),
    full.names = TRUE
  )

  gis_data_file_path <- list.files(
    vcr::vcr_test_path("fixtures"),
    pattern = paste0(number, "_shape"),
    full.names = TRUE
  )

  data <- read_nhgis(
    table_data_file_path,
    file_select = contains("blck_grp"),
    verbose = FALSE
  )

  shape_data_sf <- read_ipums_sf(gis_data_file_path)

  expect_equal(nrow(data), 1024)
})

# Submitted extract validation ------------------

test_that("We validate extract before submission", {
  expect_error(
    submit_extract(new_ipums_extract()),
    paste0(
      "`collection` must not contain missing.+",
      "`description` must not contain missing"
    )
  )
  expect_error(
    submit_extract(new_ipums_extract(collection = "usa")),
    "`description` must not contain missing values"
  )
  expect_error(
    submit_extract(
      new_ipums_extract(collection = "usa", description = "Test")
    ),
    "Extract definition must contain values for `samples`"
  )
})

# Revise a submitted extract -------------

test_that("Can add to a submitted extract", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })

  revised_extract <- add_to_extract(
    submitted_usa_extract,
    samples = c("us2014a", "us2015a"),
    variables = list("RELATE", "AGE", "SEX")
  )

  expect_true(revised_extract$status == "unsubmitted")
  expect_equal(
    names(revised_extract$samples),
    union(names(submitted_usa_extract$samples), c("us2014a", "us2015a"))
  )
  expect_equal(
    names(revised_extract$variables),
    union(names(submitted_usa_extract$variables), c("RELATE", "AGE", "SEX"))
  )
})

# Save a submitted extract as JSON -------

test_that("We can export to and import from JSON, submitted extract", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })

  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)

  save_extract_as_json(submitted_usa_extract, json_tmpfile)
  copy_of_submitted_usa_extract <- define_extract_from_json(json_tmpfile)

  expect_identical(
    copy_ipums_extract(submitted_usa_extract),
    copy_of_submitted_usa_extract
  )
})

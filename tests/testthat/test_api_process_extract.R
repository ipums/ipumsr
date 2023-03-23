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

    # Retain multiple requests here to enable test that checks that
    # `download_extract()` with `wait = TRUE` works
    modify_ready_extract_cassette_file(
      "download-nhgis-extract-shp.yml",
      n_requests = 3
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

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })

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

  vcr::use_cassette("submitted-cps-extract", {
    submitted_cps_extract <- submit_extract(test_cps_extract())
  })

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

test_that("Can submit an NHGIS extract of multiple types", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })

  expect_s3_class(submitted_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$collection, "nhgis")
  expect_equal(
    submitted_nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    submitted_nhgis_extract$data_tables,
    list(
      "2014_2018_ACS5a" = c("B01001", "B01002"),
      "2015_2019_ACS5a" = c("B01001", "B01002")
    )
  )
  expect_equal(
    submitted_nhgis_extract$breakdown_values,
    list(
      "2014_2018_ACS5a" = NULL,
      "2015_2019_ACS5a" = NULL
    )
  )
  expect_equal(submitted_nhgis_extract$time_series_tables, "CW3")
  expect_equal(
    submitted_nhgis_extract$geog_levels,
    list(
      "2014_2018_ACS5a" = "nation",
      "2015_2019_ACS5a" = "blck_grp",
      "CW3" = "state"
    )
  )
  expect_equal(submitted_nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(submitted_nhgis_extract$geographic_extents, c("DC", "PA"))
  expect_true(submitted_nhgis_extract$submitted)
  expect_equal(submitted_nhgis_extract$status, "queued")
  expect_identical(
    submitted_nhgis_extract$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})

test_that("Can submit an NHGIS extract of a single type", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(test_nhgis_extract_shp())
  })

  expect_s3_class(
    submitted_nhgis_extract_shp,
    c("nhgis_extract", "ipums_extract")
  )
  expect_equal(submitted_nhgis_extract_shp$collection, "nhgis")
  expect_true(is.null(submitted_nhgis_extract_shp$datasets))
  expect_true(is.null(submitted_nhgis_extract_shp$time_series_table))
  expect_equal(submitted_nhgis_extract_shp$shapefiles, "110_blck_grp_2019_tl2019")
  expect_true(submitted_nhgis_extract_shp$submitted)
  expect_equal(submitted_nhgis_extract_shp$status, "queued")
  expect_identical(
    submitted_nhgis_extract_shp$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
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
    resubmitted_nhgis_extract <- submit_extract(
      c("nhgis", submitted_nhgis_extract$number)
    )
  })

  expect_s3_class(
    resubmitted_nhgis_extract,
    c("nhgis_extract", "ipums_extract")
  )
  # Number, download links, etc. won't be same, but core extract will:
  expect_identical(
    resubmitted_nhgis_extract[1:14],
    ready_nhgis_extract[1:14]
  )
})

# Download extract ---------------------

test_that("Can download a USA extract by supplying extract object", {
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
    vcr::use_cassette("download-usa-extract-ipums-extract", {
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
  expect_equal(nrow(data), 20972)
})

test_that("Can download USA extract with collection and number as vector", {
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
      ddi_file_path <- download_extract(
        c("usa", ready_usa_extract$number),
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
  expect_equal(nrow(data), 20972)
})

test_that("Can download USA extract with collection and number as string", {
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
      ddi_file_path <- download_extract(
        paste0("usa:", ready_usa_extract$number),
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
  expect_equal(nrow(data), 20972)
})

test_that("Can download an NHGIS extract by supplying extract object", {
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

test_that("Can download NHGIS extract with collection/number as vector", {
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

  expect_message(
    vcr::use_cassette("download-nhgis-extract-collection-number", {
      file_paths <- download_extract(
        c("nhgis", ready_nhgis_extract$number),
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

test_that("Can download NHGIS extract with collection/number as string", {
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

  expect_message(
    vcr::use_cassette("download-nhgis-extract-collection-number", {
      file_paths <- download_extract(
        paste0("nhgis:", ready_nhgis_extract$number),
        download_dir = download_dir,
        overwrite = TRUE
      )
    }),
    regexp = "Data file saved to "
  )

  expect_error(
    download_extract(
      ready_nhgis_extract,
      download_dir = vcr::vcr_test_path("fixtures"),
      overwrite = FALSE
    ),
    regexp = "The following files already exist: "
  )

  expect_equal(length(file_paths), 2)
})

# This does not currently test properly, as the necessary fixtures are
# modified to exclude requests prior in test_api_extract_info to running
# Need to consider whether to add a header in either of these files to ensure
# sequencing is done correctly or whether to scrap this test, which essentially
# only tests that we can pass args to wait_for_extract()...
test_that("Can wait for completion during download", {
  skip_if_no_api_access(have_api_access)

  download_dir <- file.path(tempdir(), "ipums-api-downloads")

  if (!dir.exists(download_dir)) {
    dir.create(download_dir)
  }

  on.exit(unlink(download_dir, recursive = TRUE), add = TRUE, after = FALSE)

  vcr::use_cassette("submitted-nhgis-extract-shp-for-download", {
    submitted_nhgis_extract_shp <- submit_extract(test_nhgis_extract_shp())
  })

  expect_message(
    vcr::use_cassette("download-nhgis-extract-shp", {
      file_paths <- download_extract(
        submitted_nhgis_extract_shp,
        wait = TRUE,
        initial_delay_seconds = 1,
        max_delay_seconds = 2,
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

# Read downloaded files ------------------

test_that("Can read downloaded files with ipumsr readers", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

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

  # Can read downloaded files with ipumsr readers:
  data <- read_nhgis(
    table_data_file_path,
    file_select = contains("blck_grp"),
    verbose = FALSE
  )

  shape_data_sf <- read_ipums_sf(gis_data_file_path)

  lifecycle::expect_deprecated(
    shape_data_sp <- read_ipums_sp(gis_data_file_path, verbose = FALSE)@data
  )
  expect_equal(nrow(data), 10190)

  lifecycle::expect_deprecated(
    data_shp_sf <- read_nhgis_sf(
      table_data_file_path,
      gis_data_file_path,
      data_layer = contains("blck_grp"),
      shape_layer = contains("blck_grp"),
      verbose = FALSE
    )
  )

  expect_error(
    read_nhgis_sf(
      table_data_file_path,
      gis_data_file_path,
      verbose = FALSE
    ),
    "`data_layer`"
  )

  expect_error(
    read_nhgis_sf(
      table_data_file_path,
      gis_data_file_path,
      data_layer = contains("blck_grp"),
      shape_layer = contains("fake-layer"),
      verbose = FALSE
    ),
    "`shape_layer`"
  )

  expect_s3_class(data_shp_sf, "sf")
  expect_equal(nrow(data_shp_sf), nrow(data)) # sf keeps unmatched geoms
  expect_equal(
    ncol(data_shp_sf),
    ncol(data) + ncol(shape_data_sf) -
      length(intersect(colnames(data), colnames(shape_data_sf)))
  )

  lifecycle::expect_deprecated(
    data_shp_sp <- read_nhgis_sp(
      table_data_file_path,
      gis_data_file_path,
      data_layer = contains("blck_grp"),
      shape_layer = contains("blck_grp"),
      verbose = FALSE
    )
  )

  expect_error(
    read_nhgis_sp(
      table_data_file_path,
      gis_data_file_path,
      verbose = FALSE
    ),
    "`data_layer`"
  )

  expect_error(
    read_nhgis_sp(
      table_data_file_path,
      gis_data_file_path,
      data_layer = contains("blck_grp"),
      shape_layer = contains("fake-layer"),
      verbose = FALSE
    ),
    "`shape_layer`"
  )

  expect_s4_class(data_shp_sp, "SpatialPolygonsDataFrame")
  expect_equal(nrow(data_shp_sp@data), 450) # sp drops unmatched geoms
  expect_equal(
    ncol(data_shp_sp),
    ncol(data) + ncol(data_shp_sp@data) -
      length(intersect(colnames(data), colnames(data_shp_sp@data)))
  )
})

# Submit extract errors ------------------

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

test_that("We parse API errors on bad requests", {
  bad_extract <- new_ipums_extract("usa", samples = "foo")

  vcr::use_cassette("micro-extract-errors", {
    expect_error(
      ipums_api_json_request(
        "POST",
        collection = "usa",
        path = NULL,
        body = extract_to_request_json(bad_extract),
        api_key = Sys.getenv("IPUMS_API_KEY")
      ),
      "variables"
    )
  })
})

# Revise a submitted extract -------------

test_that("Can add to a submitted USA extract", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(test_usa_extract())
  })

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
    ipumsr:::copy_ipums_extract(submitted_usa_extract),
    copy_of_submitted_usa_extract
  )
})

test_that("We can export to and import from JSON, submitted NHGIS extract", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(test_nhgis_extract())
  })

  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)

  save_extract_as_json(submitted_nhgis_extract, json_tmpfile)
  copy_of_submitted_nhgis_extract <- define_extract_from_json(json_tmpfile)

  expect_identical(
    ipumsr:::copy_ipums_extract(submitted_nhgis_extract),
    copy_of_submitted_nhgis_extract
  )
})

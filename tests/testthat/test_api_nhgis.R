library(dplyr)
library(purrr)

# Setup ------------------------------------------------------------------------

nhgis_extract <- define_extract_nhgis(
  description = "Extract for R client testing",
  dataset = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
  data_tables = c("B01001", "B01002"),
  time_series_table = "CW3",
  geog_levels = list("nation", "blck_grp", "state"),
  geographic_extents = c("110", "Pennsylvania"),
  tst_layout = "time_by_row_layout",
  shapefiles = "110_blck_grp_2019_tl2019",
  data_format = "csv_no_header"
)

nhgis_extract_shp <- define_extract_nhgis(
  shapefiles = "110_blck_grp_2019_tl2019"
)

download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)

if (have_api_access) {

  # Full extract
  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(nhgis_extract)
  })

  submitted_extract_number <- submitted_nhgis_extract$number

  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })

  vcr::use_cassette("resubmitted-nhgis-extract", {
    resubmitted_nhgis_extract <- submit_extract(
      c("nhgis", submitted_extract_number)
    )
  })

  # Shapefile-only extract
  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(nhgis_extract_shp)
  })

  tryCatch({
    vcr::use_cassette("download-nhgis-shp-extract", {
      test_that("Can wait during download", {
        expect_message(
          file_paths <- download_extract(
            submitted_nhgis_extract_shp,
            wait = TRUE,
            initial_delay_seconds = 1,
            max_delay_seconds = 2,
            download_dir = download_dir,
            overwrite = TRUE
          ),
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

        data_shp_sf <- read_ipums_sf(gis_data_file_path)
        expect_s3_class(data_shp_sf, "sf")

        lifecycle::expect_deprecated(
          data_shp_sp <-  read_ipums_sp(gis_data_file_path, verbose = FALSE)
        )
        expect_s4_class(data_shp_sp, "SpatialPolygonsDataFrame")
      })
    })
  },
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  })

  vcr::use_cassette("ready-nhgis-extract-shp", {
    ready_nhgis_extract_shp <- get_extract_info(
      c("nhgis", submitted_nhgis_extract_shp$number)
    )
  })

  # Modify ready-<collection>-extract.yml files to only include the final http
  # request, so that they return the ready-to-download extract immediately on
  # subsequent runs.
  modify_ready_extract_cassette_file("ready-nhgis-extract.yml")

  # Retain last 3 requests for this download casette to ensure we test
  # that download_extract(wait = TRUE) does submit multiple GET requests
  modify_ready_extract_cassette_file(
    "download-nhgis-shp-extract.yml",
    n_requests = 3
  )

  # Recent extracts
  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_extract_info("nhgis")
  })

  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    recent_nhgis_extracts_tbl <- get_extract_info("nhgis", table = TRUE)
  })

}

# Tests ------------------------------------------------------------------------

# > Defining Extracts ----------------------------

test_that("Can define an NHGIS extract", {
  expect_s3_class(nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(
    nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    nhgis_extract$data_tables,
    list("2014_2018_ACS5a" = c("B01001", "B01002"),
         "2015_2019_ACS5a" = c("B01001", "B01002"))
  )
  expect_equal(
    nhgis_extract$geog_levels,
    list("2014_2018_ACS5a" = "nation",
         "2015_2019_ACS5a" = "blck_grp",
         "CW3" = "state")
  )
  expect_equal(
    nhgis_extract$years,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(
    nhgis_extract$breakdown_values,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(nhgis_extract$time_series_tables, "CW3")
  expect_equal(nhgis_extract$tst_layout, "time_by_row_layout")
  expect_equal(nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(nhgis_extract$data_format, "csv_no_header")
  expect_identical(nhgis_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(nhgis_extract$submitted)
  expect_equal(nhgis_extract$number, NA_integer_)
  expect_equal(nhgis_extract$status, "unsubmitted")
  expect_true(is.null(nhgis_extract_shp$datasets))
})

test_that("NHGIS extracts get correct default values", {
  expect_equal(
    nhgis_extract$breakdown_and_data_type_layout,
    "single_file"
  )
  expect_equal(
    define_extract_nhgis(
      time_series_tables = "A00",
      geog_levels = "A1"
    )$tst_layout,
    "time_by_column_layout"
  )
  expect_equal(
    nhgis_extract_shp$breakdown_and_data_type_layout,
    NULL
  )
  expect_equal(
    nhgis_extract_shp$data_format,
    NULL
  )
})

test_that("Extract print coloring works", {

  local_reproducible_output(crayon = TRUE)
  expect_equal(
    format_field_for_printing(
      parent_field = list("Dataset: " = "A"),
      subfields = list(
        "Tables: " = "B",
        "Geog Levels: " = "C"
      ),
      parent_style = extract_field_styler(NHGIS_DS_COLOR, "bold"),
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
      parent_style = extract_field_styler(NHGIS_DS_COLOR, "bold"),
      subfield_style = extract_field_styler("bold"),
      padding = 2
    ),
    "\n\nDataset: A\n  Tables: B\n  Geog Levels: C"
  )
})

test_that("nhgis_extract print method works", {
  expect_output(
    print(nhgis_extract),
    regexp = paste0(
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
      "\n",
      "\nShapefiles: 110_blck_grp_2019_tl2019"
    )
  )
  expect_output(
    print(nhgis_extract_shp),
    regexp = paste0(
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
    regexp = paste0(
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

test_that("nhgis_extract validate method works", {
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis")),
    "`description` must not contain missing values"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = c("A00", "A01"),
        data_tables = NA,
        geog_levels = list(NULL, "b"),
      )
    ),
    paste0(
      "All `datasets` must be associated with a value in `data_tables`.+",
      "All `datasets`, `time_series_tables` must be associated with a value in",
      " `geog_levels`.+",
      "`data_format` must not contain missing values when any `datasets` ",
      "or `time_series_tables` are specified."
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      time_series_tables = "Test",
      geog_levels = NA
    ),
    paste0(
      "`geog_levels` must not contain missing values for any of the ",
      "provided `datasets` or `time_series_tables`."
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      datasets = "Test",
      data_tables = "Test",
      geog_levels = "Test",
      data_format = "Test"
    ),
    "`data_format` must be one of"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = NULL
      )
    ),
    paste0(
      "`description` must not contain missing values"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        geog_levels = "Test"
      )
    ),
    paste0(
      "`geog_levels` must be missing when no `datasets` or ",
      "`time_series_tables` are specified"
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = c("A00", "A01"),
        geog_levels = list(A00 = "a", A01= "b", A01 = "c"),
        data_format = "csv_no_header"
      )
    ),
    paste0(
      "`geog_levels` must not be greater than the number of ",
      "`datasets` \\+ `time_series_tables`.+",
      "`tst_layout` must not contain missing values when any ",
      "`time_series_tables` are specified."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        data_format = "csv_header"
      )
    ),
    "`data_format` must be missing when no `datasets` or `time_series_tables`",
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = NA,
        time_series_tables = c("CW3", NA)
      )
    ),
    "None of `datasets`.+can contain missing values"
  )
  expect_error(
    define_extract_nhgis(
      time_series_tables = "A",
      geog_levels = "A",
      tst_layout = c("time_by_row_layout", "time_by_row_layout")
    ),
    paste0(
      "`tst_layout` must be length 1."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = "A",
        data_tables = c(A = "A"),
        geog_levels = list(A = "A"),
        data_format = "csv_no_header"
      )
    ),
    "`data_tables` must be of type `list`, not `character`"
  )
})

# > Submitting Extracts --------------------------

test_that("Can submit an NHGIS extract of multiple types", {
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$collection, "nhgis")
  expect_equal(
    submitted_nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    submitted_nhgis_extract$data_tables,
    list("2014_2018_ACS5a" = c("B01001", "B01002"),
         "2015_2019_ACS5a" = c("B01001", "B01002"))
  )
  expect_equal(
    submitted_nhgis_extract$breakdown_values,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(submitted_nhgis_extract$time_series_tables, "CW3")
  expect_equal(
    submitted_nhgis_extract$geog_levels,
    list("2014_2018_ACS5a" = "nation",
         "2015_2019_ACS5a" = "blck_grp",
         "CW3" = "state")
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

# > Checking Extracts ----------------------------

test_that("Can check status of an NHGIS extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(submitted_nhgis_extract)
  })
  expect_s3_class(checked_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  vcr::use_cassette("is-nhgis-extract-ready", {
    is_ready <- is_extract_ready(submitted_nhgis_extract)
  })
  expect_true(is_ready)
})

test_that("Can check status of an NHGIS extract with collection and number", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(
      c("nhgis", submitted_nhgis_extract$number)
    )
  })
  expect_s3_class(checked_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(checked_nhgis_extract$status, "completed")
  vcr::use_cassette("is-nhgis-extract-ready", {
    is_ready <- is_extract_ready(c("nhgis", submitted_extract_number))
  })
  expect_true(is_ready)
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(
      paste0("nhgis:", submitted_nhgis_extract$number)
    )
  })
  expect_s3_class(checked_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(checked_nhgis_extract$status, "completed")
  vcr::use_cassette("is-nhgis-extract-ready", {
    is_ready <- is_extract_ready(submitted_nhgis_extract)
  })
  expect_true(is_ready)

  expect_error(
    get_extract_info(
      define_extract_nhgis(
        datasets = "a", data_tables = "B", geog_levels = "C"
      )
    ),
    "Cannot get info for an `ipums_extract` object with missing extract number."
  )
})

test_that("We avoid superflous checks when getting extract status", {
  skip_if_no_api_access(have_api_access)

  # Set api_key to NULL to force authorization errors if any API request
  # is made. Otherwise, we know calls returned with no request.
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

test_that("extract_list_from_json reproduces extract specs", {
  nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))
  expect_identical(
    extract_list_from_json(nhgis_json)[[1]],
    nhgis_extract
  )
})

test_that("Can use default collection when getting extract info", {

  withr::with_envvar(new = c("IPUMS_DEFAULT_COLLECTION" = "nhgis"), {

    extract_id <- standardize_extract_identifier(1)

    expect_equal(extract_id$collection, "nhgis")
    expect_equal(length(extract_id), 2)
    expect_equal(extract_id$number, 1)

    vcr::use_cassette("ready-nhgis-extract", {
      expect_equal(
        wait_for_extract(ready_nhgis_extract$number)$status,
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

# > Downloading -----------------------------------

if (have_api_access) {
  download_nhgis_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-nhgis-extract.yml"
  )

  download_nhgis_extract_cassette_file_shp <- file.path(
    vcr::vcr_test_path("fixtures"), "download-nhgis-shp-extract.yml"
  )

  already_existed <- file.exists(download_nhgis_extract_cassette_file)
  already_existed_shp <- file.exists(download_nhgis_extract_cassette_file_shp)
}

tryCatch(
  vcr::use_cassette("download-nhgis-extract", {
    test_that("Can download an NHGIS extract by supplying extract object", {
      skip_if_no_api_access(have_api_access)

      expect_message(
        file_paths <- download_extract(
          ready_nhgis_extract,
          download_dir = download_dir,
          overwrite = TRUE
        ),
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
      expect_equal(names(file_paths), c("data", "shape"))

      table_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[1])
      )
      # table_data_file_path <- convert_to_relative_path(table_data_file_path)

      gis_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[2])
      )
      # gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

      expect_match(table_data_file_path, "_csv\\.zip$")
      expect_match(gis_data_file_path, "_shape\\.zip$")

      expect_true(file.exists(table_data_file_path))
      expect_true(file.exists(gis_data_file_path))

      data <- read_nhgis(
        table_data_file_path,
        file_select = contains("blck_grp"),
        show_conditions = FALSE
      )

      shape_data_sf <- read_ipums_sf(gis_data_file_path)
      shape_data_sp <- read_ipums_sp(gis_data_file_path, verbose = FALSE)@data

      expect_equal(nrow(data), 10190)

      # TODO: fix read_nhgis_sf so you don't have to supply a shape_layer if
      # there is only 1 shapefile in extract? confusing functionality currently.
      lifecycle::expect_deprecated(
        data_shp_sf <- read_nhgis_sf(
          table_data_file_path,
          gis_data_file_path,
          data_layer = contains("blck_grp"),
          shape_layer = contains("blck_grp"),
          verbose = FALSE
        )
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

      expect_s4_class(data_shp_sp, "SpatialPolygonsDataFrame")
      expect_equal(nrow(data_shp_sp@data), 450) # sp drops unmatched geoms
      expect_equal(
        ncol(data_shp_sp),
        ncol(data) + ncol(data_shp_sp@data) -
          length(intersect(colnames(data), colnames(data_shp_sp@data)))
      )

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
    convert_paths_in_cassette_file_to_relative(download_nhgis_extract_cassette_file)
  }

  if (!already_existed_shp) {
    convert_paths_in_cassette_file_to_relative(download_nhgis_extract_cassette_file_shp)
  }

  download_nhgis_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-nhgis-extract-collection-number.yml"
  )

  already_existed <- file.exists(download_nhgis_extract_cassette_file)
}

tryCatch(
  vcr::use_cassette("download-nhgis-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download NHGIS extract with collection/number as vector", {

      expect_message(
        file_paths <- download_extract(
          c("nhgis", ready_nhgis_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
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

      table_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[1])
      )
      # table_data_file_path <- convert_to_relative_path(table_data_file_path)

      gis_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[2])
      )
      # gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

      expect_match(table_data_file_path, "_csv\\.zip$")
      expect_match(gis_data_file_path, "_shape\\.zip$")

      expect_true(file.exists(table_data_file_path))
      expect_true(file.exists(gis_data_file_path))

      data <- read_nhgis(
        table_data_file_path,
        file_select = contains("state"),
        show_conditions = FALSE
      )

      expect_equal(nrow(data), 153)

      # TODO: fix read_nhgis_sf so you don't have to supply a shape_layer if
      # there is only 1 shapefile in extract? confusing functionality currently.
      data_shp_sf <- read_nhgis_sf(table_data_file_path,
                                   gis_data_file_path,
                                   data_layer = contains("blck_grp"),
                                   shape_layer = contains("blck_grp"),
                                   verbose = FALSE)
      expect_s3_class(data_shp_sf, "sf")

      data_shp_sp <- read_nhgis_sp(table_data_file_path,
                                   gis_data_file_path,
                                   data_layer = contains("blck_grp"),
                                   shape_layer = contains("blck_grp"),
                                   verbose = FALSE)
      expect_s4_class(data_shp_sp, "SpatialPolygonsDataFrame")
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
    convert_paths_in_cassette_file_to_relative(download_nhgis_extract_cassette_file)
  }
}

tryCatch(
  vcr::use_cassette("download-nhgis-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download NHGIS extract with collection/number as string", {

      expect_message(
        file_paths <- download_extract(
          paste0("nhgis:", ready_nhgis_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
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
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)

# > Revising ------------------------------------

test_that("Can add new fields to an NHGIS extract", {

  revised <- add_to_extract(
    nhgis_extract,
    datasets = "New",
    data_tables = c("T1", "T2"),
    time_series_tables = c("CW4", "CW5"),
    geog_levels = list("G1", "G1", "G2"),
    years = "Y1",
    data_format = "csv_header",
    shapefiles = "New"
  )

  expect_equal(revised$datasets, c(nhgis_extract$datasets, "New"))
  expect_equal(
    revised$data_tables,
    c(nhgis_extract$data_tables, list(New = c("T1", "T2")))
  )
  expect_equal(
    revised$geog_levels,
    list(`2014_2018_ACS5a` = "nation",
         `2015_2019_ACS5a` = "blck_grp",
         New = "G1",
         CW3 = "state",
         CW4 = "G1",
         CW5 = "G2")
  )
  expect_equal(
    revised$years,
    c(nhgis_extract$years, list(New = "Y1"))
  )
  expect_equal(
    revised$time_series_tables,
    c(nhgis_extract$time_series_tables, c("CW4", "CW5"))
  )
  expect_equal(revised$data_format, "csv_header")
  expect_equal(revised$shapefiles, c(nhgis_extract$shapefiles, "New"))
})

test_that("Can add subfields to existing parent fields", {

  revised <- add_to_extract(
    nhgis_extract,
    data_tables = c("T1", "T2"),
    time_series_tables = c("CW3", "CW4"),
    geog_levels = list(`2015_2019_ACS5a` = "G1",
                       CW3 = "G1",
                       CW4 = "G2"),
    years = list("Y1", "Y2")
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$data_tables,
    list(`2014_2018_ACS5a` = c("B01001", "B01002", "T1", "T2"),
         `2015_2019_ACS5a` = c("B01001", "B01002", "T1", "T2"))
  )
  expect_equal(revised$time_series_tables, c("CW3", "CW4"))
  expect_equal(
    revised$geog_levels,
    list(`2014_2018_ACS5a` = c("nation"),
         `2015_2019_ACS5a` = c("blck_grp", "G1"),
         CW3 = c("state", "G1"),
         CW4 = "G2")
  )
  expect_equal(
    revised$years,
    list(`2014_2018_ACS5a` = "Y1",
         `2015_2019_ACS5a` = "Y2")
  )

  # Recycling handling for extracts with DS + TST
  expect_equal(
    add_to_extract(
      nhgis_extract,
      time_series_tables = "CW3",
      geog_levels = "C"
    )$geog_levels,
    list("2014_2018_ACS5a" = "nation",
         "2015_2019_ACS5a" = "blck_grp",
         "CW3" = c("state", "C"))
  )

  expect_equal(
    add_to_extract(
      nhgis_extract,
      datasets = "2014_2018_ACS5a",
      time_series_tables = "CW3",
      geog_levels = "C"
    )$geog_levels,
    list("2014_2018_ACS5a" = c("nation", "C"),
         "2015_2019_ACS5a" = "blck_grp",
         "CW3" = c("state", "C"))
  )

})

test_that("Can remove full fields from an NHGIS extract", {

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2015_2019_ACS5a",
    time_series_tables = "CW3",
    geographic_extents = "DC"
  )

  expect_equal(revised$datasets, "2014_2018_ACS5a")

  expect_equal(
    revised$data_tables,
    list(`2014_2018_ACS5a` = c("B01001", "B01002"))
  )
  expect_equal(
    revised$geog_levels,
    list(`2014_2018_ACS5a` = c("nation"))
  )
  expect_equal(
    revised$years,
    list(`2014_2018_ACS5a` = NULL)
  )

  expect_null(revised$time_series_tables)
  expect_null(revised$tst_layout)

  expect_equal(revised$geographic_extents, "PA")

})

test_that("Can remove subfields from an NHGIS extract", {

  revised <- remove_from_extract(
    add_to_extract(
      nhgis_extract,
      geog_levels = list(`2014_2018_ACS5a` = "tract", CW3 = "tract"),
    ),
    data_tables = "B01001",
    geog_levels = list(`2014_2018_ACS5a` = "nation",
                       CW3 = c("state", "blck_grp")),
    geographic_extents = "420"
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$data_tables,
    list(`2014_2018_ACS5a` = "B01002",
         `2015_2019_ACS5a` = "B01002")
  )
  expect_equal(revised$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(
    revised$geog_levels,
    list(`2014_2018_ACS5a` = "tract",
         `2015_2019_ACS5a` = "blck_grp",
         CW3 = "tract")
  )
  expect_equal(revised$geographic_extents, "DC")

})

test_that("Removing parent fields occurs before evaluating subfields", {

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2014_2018_ACS5a",
    data_tables = "B01001"
  )

  expect_equal(revised$datasets, "2015_2019_ACS5a")
  expect_equal(revised$data_tables, list(`2015_2019_ACS5a` = "B01002"))

})

test_that("Revisions do not alter unspecified extract fields", {

  extract1 <- define_extract_nhgis(
    shapefiles = "Test"
  )

  extract2 <- add_to_extract(
    extract1,
    datasets = "Test",
    data_tables = "Test",
    geog_levels = "Test"
  )

  # Test on an extract of multiple types
  expect_identical(nhgis_extract, add_to_extract(nhgis_extract))
  expect_identical(nhgis_extract, remove_from_extract(nhgis_extract))

  # Test on an extract of a single type
  expect_identical(extract1, add_to_extract(extract1))
  expect_identical(extract1, remove_from_extract(extract1))

  expect_null(extract2$time_series_tables)
  expect_null(extract2$tst_layout)

  expect_error(
    add_to_extract(extract1, geog_levels = "Test"),
    "`geog_levels` must be missing when no `datasets` or `time_series_tables`"
  )

})

test_that("Improper extract revisions throw warnings or errors", {
  expect_error(
    remove_from_extract(
      nhgis_extract,
      datasets = "2014_2018_ACS5a",
      data_tables = list("a", "b"),
      geog_levels = list("2014_2018_ACS5a" = "a",
                         "not_in_extract" = "blck_grp"),
      years = list(C = "c")
    ),
    paste0(
      "`data_tables` must not be greater than the number of ",
      "`datasets`.+",
      "All names in `geog_levels` must be present in this extract\'s ",
      "`datasets`, `time_series_tables`.+",
      "All names in `years` must be present in this extract\'s `datasets`"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      datasets = c("New"),
      data_tables = list("A", "B"),
      geog_levels = list(New = "A", not_in_extract = "blck_grp"),
      years = list(C = "c")
    ),
    paste0(
      "`data_tables` must not be greater than the number of ",
      "`datasets`.+",
      "All names in `geog_levels` must be present in this extract\'s ",
      "`datasets`, `time_series_tables`.+",
      "All names in `years` must be present in this extract\'s `datasets`.+"
    )
  )
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      description = "Extract for R client testing",
      bad_field = "not in extract"
    ),
    regexp = paste0(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed: `description`, `bad_field`"
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      geographic_extents = list("A", "B")
    ),
    "`geographic_extents` was provided as a list, but this parameter"
  )
  expect_silent(
    remove_from_extract(
      nhgis_extract,
      datasets = "DS",
      time_series_tables = "TST"
    )
  )
  expect_error(
    remove_from_extract(
      nhgis_extract,
      geog_levels = "nation"
    ),
    paste0(
      "`geog_levels` must not contain missing values"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract_shp,
      geog_levels = "nation"
    ),
    "`geog_levels` must be missing when no `datasets`"
  )
  expect_identical(
    remove_from_extract(
      nhgis_extract_shp,
      geog_levels = "nation"
    ),
    nhgis_extract_shp
  )
  expect_warning(
    add_to_extract(nhgis_extract, vars = "var", number = 45),
    paste0(
      "The following fields were either not found in the ",
      "provided extract or cannot be modified: `vars`, `number`"
    )
  )
  expect_identical(
    suppressWarnings(add_to_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
  expect_identical(
    suppressWarnings(remove_from_extract(nhgis_extract, vars = "var")),
    nhgis_extract
  )
  expect_silent(
    add_to_extract(nhgis_extract, description = "Test")
  )
  expect_error(
    add_to_extract(nhgis_extract, data_format = c("csv_header", "fixed_width")),
    "`data_format` must be length 1"
  )
})

test_that("Can combine extracts", {

  x1 <- define_extract_nhgis(
    description = "Combining",
    shapefiles = "S1"
  )

  x2 <- define_extract_nhgis(
    datasets = c("A", "B"),
    data_tables = list(c("D1", "D2"), "D3"),
    time_series_tables = c("T1", "T2"),
    geog_levels = "G1",
    tst_layout = "time_by_file_layout"
  )

  x3 <- define_extract_nhgis(
    datasets = "C",
    data_tables = c("D2", "D4"),
    geog_levels = c("G2", "G3"),
    years = "Y1",
    shapefiles = "S1",
    data_format = "fixed_width",
  )

  x <- combine_extracts(x1, x2, x3)

  expect_equal(
    x$datasets,
    union(
      union(
        x1$datasets,
        x2$datasets
      ),
      x3$datasets
    )
  )
  expect_equal(
    x$time_series_tables,
    union(
      union(
        x1$time_series_tables,
        x2$time_series_tables
      ),
      x3$time_series_tables
    )
  )
  expect_equal(
    x$shapefiles,
    "S1"
  )

  expect_equal(
    x$data_tables,
    c(x1$data_tables,
      x2$data_tables,
      x3$data_tables)
  )
  expect_equal(
    x$geog_levels,
    c(x1$geog_levels,
      x2$geog_levels,
      x3$geog_levels)[c(x$datasets, x$time_series_tables)]
  )
  expect_equal(
    x$years,
    c(x1$years,
      x2$years,
      x3$years)
  )
  expect_equal(
    x$breakdown_values,
    c(x1$breakdown_values,
      x2$breakdown_values,
      x3$breakdown_values)
  )

  expect_equal(
    x$tst_layout,
    x2$tst_layout
  )
  expect_equal(
    x$data_format,
    x2$data_format
  )
  expect_equal(
    x$description,
    x1$description
  )

  expect_error(
    combine_extracts(
      x1, define_extract_usa("A", "B", "C")
    ),
    "same collection"
  )

})

# > Recent extracts ------------------------------

test_that("Tibble of recent NHGIS extracts has expected structure", {
  skip_if_no_api_access(have_api_access)

  expected_columns <- c("collection", "number", "description", "data_type",
                        "name", "data_tables",
                        "geog_levels", "years",
                        "breakdown_values", "geographic_extents",
                        "tst_layout",
                        "breakdown_and_data_type_layout", "data_format",
                        "submitted", "download_links", "status")

  recent_nhgis_extract_submitted <- recent_nhgis_extracts_tbl[
    which(recent_nhgis_extracts_tbl$number == submitted_extract_number &
            recent_nhgis_extracts_tbl$data_type == "datasets"),
  ]

  row_level_nhgis_tbl <- collapse_nhgis_extract_tbl(recent_nhgis_extracts_tbl)

  row_level_nhgis_tbl_submitted <- row_level_nhgis_tbl[
    which(row_level_nhgis_tbl$number == submitted_extract_number),
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

test_that("Shapefile-only can be converted from tbl to list", {
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

test_that("NHGIS tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)

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

# > JSON export -----------------------------------

test_that("We can export to and import from JSON for NHGIS", {
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(nhgis_extract, json_tmpfile)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(nhgis_extract, copy_of_nhgis_extract)
})

test_that("We can export to and import from JSON, submitted NHGIS extract", {
  skip_if_no_api_access(have_api_access)
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile), add = TRUE, after = FALSE)
  save_extract_as_json(submitted_nhgis_extract, json_tmpfile)
  copy_of_submitted_nhgis_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(
    ipumsr:::copy_ipums_extract(submitted_nhgis_extract),
    copy_of_submitted_nhgis_extract
  )
})

# > Misc ------------------------------------------

test_that("We can get correct API version info for each collection", {
  expect_equal(ipums_api_version("usa"), "beta")
  expect_equal(ipums_api_version("nhgis"), "v1")
  expect_equal(
    ipums_api_version("usa"),
    dplyr::filter(ipums_data_collections(), code_for_api == "usa")$api_support
  )
  expect_equal(
    ipums_api_version("nhgis"),
    dplyr::filter(ipums_data_collections(), code_for_api == "nhgis")$api_support
  )
  expect_error(ipums_api_version("fake collection"), "No API version found")
})

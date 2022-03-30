library(dplyr)
library(purrr)

# Setup ------------------------------------------------------------------------

nhgis_extract <- define_extract_nhgis(
  description = "Extract for R client testing",
  dataset = "2015_2019_ACS5a",
  data_tables = c("B01001", "B01002", "B15003"),
  ds_geog_levels = c("nation", "blck_grp"),
  time_series_table = "CW3",
  ts_geog_levels = "county",
  time_series_table_layout = "time_by_column_layout",
  breakdown_and_data_type_layout = "single_file",
  geographic_extents = c("110", "420"),
  shapefiles = "110_blck_grp_2019_tl2019",
  data_format = "csv_no_header"
)

nhgis_extract_shp <- define_extract_nhgis(
  shapefiles = "110_blck_grp_2019_tl2019"
)

usa_extract <- define_extract_micro(
  collection = "usa",
  samples = "us2017b",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")

usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")

if (have_api_access) {

  # Full extract ---------------------------------------------------------------

  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(nhgis_extract)
  })

  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })

  # Modify ready-nhgis-extract.yml to only includes the final http request, so that
  # it returns the ready-to-download extract immediately on subsequent runs
  ready_nhgis_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "ready-nhgis-extract.yml"
  )

  ready_nhgis_lines <- readLines(ready_nhgis_extract_cassette_file)
  last_request_start_line <- max(which(grepl("^- request:", ready_nhgis_lines)))

  writeLines(
    c(
      ready_nhgis_lines[[1]],
      ready_nhgis_lines[last_request_start_line:length(ready_nhgis_lines)]
    ),
    con = ready_nhgis_extract_cassette_file
  )

  # Shapefile-only extract -----------------------------------------------------

  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(nhgis_extract_shp)
  })

  vcr::use_cassette("ready-nhgis-extract-shp", {
    ready_nhgis_extract_shp <- wait_for_extract(submitted_nhgis_extract_shp)
  })

  # Modify ready-nhgis-extract.yml to only includes the final http request, so that
  # it returns the ready-to-download extract immediately on subsequent runs
  ready_nhgis_extract_cassette_file_shp <- file.path(
    vcr::vcr_test_path("fixtures"), "ready-nhgis-extract-shp.yml"
  )

  ready_nhgis_lines_shp <- readLines(ready_nhgis_extract_cassette_file_shp)
  last_request_start_line_shp <- max(which(grepl("^- request:", ready_nhgis_lines_shp)))

  writeLines(
    c(
      ready_nhgis_lines_shp[[1]],
      ready_nhgis_lines_shp[last_request_start_line_shp:length(ready_nhgis_lines_shp)]
    ),
    con = ready_nhgis_extract_cassette_file_shp
  )

  # USA extract ----------------------------------------------------------------

  # Can add back in when including both togehter in same test script
  # vcr::use_cassette("submitted-usa-extract", {
  #   submitted_usa_extract <- submit_extract(usa_extract)
  # })
  #
  # vcr::use_cassette("ready-usa-extract", {
  #   ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  # })
  #
  # # Modify ready-nhgis-extract.yml to only includes the final http request, so that
  # # it returns the ready-to-download extract immediately on subsequent runs
  # ready_usa_extract_cassette_file <- file.path(
  #   vcr::vcr_test_path("fixtures"), "ready-usa-extract.yml"
  # )
  #
  # ready_usa_lines <- readLines(ready_usa_extract_cassette_file)
  # last_request_start_line <- max(which(grepl("^- request:", ready_usa_lines)))
  #
  # writeLines(
  #   c(
  #     ready_usa_lines[[1]],
  #     ready_usa_lines[last_request_start_line:length(ready_usa_lines)]
  #   ),
  #   con = ready_usa_extract_cassette_file
  # )

  # Recent extracts ------------------------------------------------------------

  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_recent_extracts_info_list("nhgis")
  })

  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    recent_nhgis_extracts_tbl <- get_recent_extracts_info_tbl("nhgis")
  })

}

# Tests ------------------------------------------------------------------------

# Basic Functionality ----------------------------------------------------------

test_that("Can define an NHGIS extract", {
  expect_s3_class(nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(nhgis_extract$dataset, "2015_2019_ACS5a")
  expect_equal(nhgis_extract$data_tables, c("B01001", "B01002", "B15003"))
  expect_equal(nhgis_extract$ds_geog_levels, c("nation", "blck_grp"))
  expect_equal(nhgis_extract$time_series_table, "CW3")
  expect_equal(nhgis_extract$ts_geog_levels, "county")
  expect_equal(nhgis_extract$time_series_table_layout, "time_by_column_layout")
  expect_equal(nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(nhgis_extract$data_format, "csv_no_header")
  expect_identical(nhgis_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(nhgis_extract$submitted)
  expect_equal(nhgis_extract$number, NA_integer_)
  expect_equal(nhgis_extract$status, "unsubmitted")
  expect_true(is.na(nhgis_extract_shp$dataset))
})


test_that("Can submit an NHGIS extract of multiple types", {
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$collection, "nhgis")
  expect_equal(submitted_nhgis_extract$dataset, "2015_2019_ACS5a")
  expect_equal(submitted_nhgis_extract$time_series_table, "CW3")
  expect_equal(submitted_nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_true(submitted_nhgis_extract$submitted)
  expect_equal(submitted_nhgis_extract$status, "submitted")
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
  expect_true(is.na(submitted_nhgis_extract_shp$dataset))
  expect_true(is.na(submitted_nhgis_extract_shp$time_series_table))
  expect_equal(submitted_nhgis_extract_shp$shapefiles, "110_blck_grp_2019_tl2019")
  expect_true(submitted_nhgis_extract_shp$submitted)
  expect_equal(submitted_nhgis_extract_shp$status, "submitted")
  expect_identical(
    submitted_nhgis_extract_shp$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})


test_that("nhgis_extract print method works", {
  expect_output(
    print(nhgis_extract),
    regexp = paste0(
      "Unsubmitted IPUMS NHGIS extract ",
      "\nDescription: Extract for R client testing",
      "\nDataset: 2015_2019_ACS5a",
      "\n  Tables: \\(3 total\\) B01001, B01002, B15003",
      "\n  Geog Levels: \\(2 total\\) nation, blck_grp",
      "\n  Years: ",
      "\n  Breakdowns: ",
      "\nTime Series Tables: CW3",
      "\n  Geog Levels: \\(1 total\\) county",
      "\nShapefiles: \\(1 total\\) 110_blck_grp_2019_tl2019"
    )
  )
  expect_output(
    print(nhgis_extract_shp),
    regexp = paste0(
      "Unsubmitted IPUMS NHGIS extract ",
      "\nDescription: ",
      "\nShapefiles: \\(1 total\\) 110_blck_grp_2019_tl2019"
    )
  )
})

# Should test this on extracts returned from extract tbl output as well
test_that("nhgis_extract validate method works", {
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis")),
    "At least one of `dataset`"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        dataset = "Test")
    ),
    "When a dataset is specified,"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_table = "Test"
      )
    ),
    "When a time series table is specified,"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        dataset = "Test",
        data_tables = "Test",
        ds_geog_levels = "Test",
        data_format = "Test"
      )
    ),
    "`data_format` must be one of"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = NULL,
        shapefiles = "Test"
      )
    ),
    paste0(
      "The following elements of an nhgis_extract must not contain ",
      "missing values: desc"
    )
  )
})

# Checking Extracts ------------------------------------------------------------

test_that("Can check status of an NHGIS extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(submitted_nhgis_extract)
  })
  expect_s3_class(checked_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$status, "submitted")
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
    is_ready <- is_extract_ready(submitted_nhgis_extract)
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
})


test_that("Tibble of recent NHGIS extracts has expected structure", {
  skip_if_no_api_access(have_api_access)
  expected_columns <- c("collection", "description", "dataset",
                        "data_tables", "ds_geog_levels", "years",
                        "breakdown_values", "time_series_table",
                        "ts_geog_levels", "shapefiles", "data_format",
                        "breakdown_and_data_type_layout",
                        "time_series_table_layout", "geographic_extents",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_nhgis_extracts_tbl), expected_columns)
  expect_equal(nrow(recent_nhgis_extracts_tbl), 10)
  expect_equal(
    recent_nhgis_extracts_tbl[2,]$dataset,
    submitted_nhgis_extract$dataset
  )
  expect_equal(
    unlist(recent_nhgis_extracts_tbl[2,]$data_tables),
    submitted_nhgis_extract$data_tables
  )
  expect_equal(
    recent_nhgis_extracts_tbl[2,]$time_series_table,
    submitted_nhgis_extract$time_series_table
  )
})


test_that("Can limit number of recent extracts to get info on", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-nhgis-extracts-tbl-two", {
    two_recent_nhgis_extracts <- get_recent_extracts_info_tbl("nhgis", 2)
  })
  expect_equal(nrow(two_recent_nhgis_extracts), 2)
})

test_that("extract_list_from_json reproduces extract specs", {
  expect_s3_class(nhgis_json, c("nhgis_json", "ipums_json"))
  expect_s3_class(usa_json, c("usa_json", "ipums_json"))
  expect_identical(
    extract_list_from_json(nhgis_json)[[1]],
    nhgis_extract
  )
  expect_identical(
    extract_list_from_json(usa_json)[[1]],
    usa_extract
  )
})


# Misc -------------------------------------------------------------------------

# Included as reminder to update handling of these fields when API is updated
test_that("NHGIS API v1 has missing fields but are recovered when submitting", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-nhgis-extract-info", {
    checked_nhgis_extract <- get_extract_info(submitted_nhgis_extract)
  })

  expect_warning(
    get_extract_info(submitted_nhgis_extract),
    "The current version"
  )

  expect_true(is_empty(checked_nhgis_extract$shapefiles))
  expect_false(is_empty(submitted_nhgis_extract$shapefiles))

  expect_true(is.na(checked_nhgis_extract$breakdown_and_data_type_layout))
  expect_false(is.na(submitted_nhgis_extract$breakdown_and_data_type_layout))

  expect_true(is.na(checked_nhgis_extract$time_series_table_layout))
  expect_false(is.na(submitted_nhgis_extract$time_series_table_layout))

  expect_true(is_empty(checked_nhgis_extract$geographic_extents))
  expect_false(is_empty(submitted_nhgis_extract$geographic_extents))
})

test_that("We can get correct API version info for each collection", {
  expect_equal(ipums_api_version("usa"), "beta")
  expect_equal(ipums_api_version("nhgis"), "v1")
  expect_equal(
    ipums_api_version("usa"),
    dplyr::filter(ipums_collection_versions(), collection == "usa")$version
  )
  expect_equal(
    ipums_api_version("nhgis"),
    dplyr::filter(ipums_collection_versions(), collection == "nhgis")$version
  )
  expect_error(ipums_api_version("fake collection"), "No API version found")
})

# Downloading ------------------------------------------------------------------


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

download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)

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

      table_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[1])
      )
      table_data_file_path <- convert_to_relative_path(table_data_file_path)

      gis_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[2])
      )
      gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

      expect_match(table_data_file_path, "_csv\\.zip$")
      expect_match(gis_data_file_path, "_shape\\.zip$")

      expect_true(file.exists(table_data_file_path))
      expect_true(file.exists(gis_data_file_path))

      data <- read_nhgis(table_data_file_path,
                         data_layer = contains("county"),
                         verbose = FALSE)
      expect_equal(nrow(data), 3143)

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

tryCatch(
  vcr::use_cassette("download-nhgis-shp-extract", {
    test_that("Can download an NHGIS shp extract by supplying extract object", {
      skip_if_no_api_access(have_api_access)

      expect_message(
        file_paths <- download_extract(
          ready_nhgis_extract_shp,
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "GIS file saved to "
      )

      expect_error(
        download_extract(
          ready_nhgis_extract_shp,
          download_dir = vcr::vcr_test_path("fixtures"),
          overwrite = FALSE
        ),
        regexp = "The following files already exist: "
      )

      expect_equal(length(file_paths), 1)

      gis_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths)
      )
      gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

      expect_match(gis_data_file_path, "_shape\\.zip$")

      expect_true(file.exists(gis_data_file_path))

      data_shp_sf <- read_ipums_sf(gis_data_file_path, verbose = FALSE)
      expect_s3_class(data_shp_sf, "sf")

      data_shp_sp <-  read_ipums_sp(gis_data_file_path, verbose = FALSE)
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
      table_data_file_path <- convert_to_relative_path(table_data_file_path)

      gis_data_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(file_paths[2])
      )
      gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

      expect_match(table_data_file_path, "_csv\\.zip$")
      expect_match(gis_data_file_path, "_shape\\.zip$")

      expect_true(file.exists(table_data_file_path))
      expect_true(file.exists(gis_data_file_path))

      data <- read_nhgis(table_data_file_path,
                         data_layer = contains("county"),
                         verbose = FALSE)
      expect_equal(nrow(data), 3143)

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

# # # Revising ---------------------------------------------------------------------
# # test_that("Can revise an extract", {
# #   revised_extract <- revise_extract_micro(
# #     usa_extract,
# #     samples_to_add = "us2014a",
# #     vars_to_add = "RELATE"
# #   )
# #   expect_true(revised_extract$status == "unsubmitted")
# #   expect_equal(
# #     revised_extract$description,
# #     paste0("Revision of (", usa_extract$description, ")")
# #   )
# #   expect_equal(
# #     revised_extract$samples,
# #     c(usa_extract$samples, "us2014a")
# #   )
# #   expect_equal(
# #     revised_extract$variables,
# #     c(usa_extract$variables, "RELATE")
# #   )
# # })
#
# # test_that("Can revise a submitted extract", {
# #   skip_if_no_api_access(have_api_access)
# #   revised_extract <- revise_extract_micro(
# #     submitted_usa_extract,
# #     samples_to_add = "us2014a",
# #     vars_to_add = "RELATE"
# #   )
# #   expect_true(revised_extract$status == "unsubmitted")
# #   expect_equal(
# #     revised_extract$description,
# #     paste0("Revision of (", submitted_usa_extract$description, ")")
# #   )
# #   expect_equal(
# #     revised_extract$samples,
# #     c(submitted_usa_extract$samples, "us2014a")
# #   )
# #   expect_equal(
# #     revised_extract$variables,
# #     c(submitted_usa_extract$variables, "RELATE")
# #   )
# # })
#
# # test_that("We warn user when their revisions don't make sense", {
# #   expect_warning(
# #     revise_extract_micro(usa_extract, samples_to_add = "us2017b"),
# #     regexp = "already included"
# #   )
# #   expect_warning(
# #     revise_extract_micro(usa_extract, vars_to_remove = "RELATE"),
# #     regexp = "are not included"
# #   )
# # })
#
# test_that("NHGIS tbl to list and list to tbl conversion works", {
#   skip_if_no_api_access(have_api_access)
#   converted_to_list <- extract_tbl_to_list(
#     recent_nhgis_extracts_tbl,
#     validate = FALSE
#   )
#   converted_to_tbl <- extract_list_to_tbl(recent_nhgis_extracts_list)
#   expect_identical(recent_nhgis_extracts_list, converted_to_list)
#   expect_identical(recent_nhgis_extracts_tbl, converted_to_tbl)
#   # expect_error(
#   #   extract_list_to_tbl(
#   #     list(submitted_nhgis_extract,
#   #          submitted_usa_extract)
#   #   ),
#   #   "All extracts in `extract_list` must belong to same collection"
#   # )
# })
#
# test_that("We can export to and import from JSON for NHGIS", {
#   json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
#   on.exit(unlink(json_tmpfile))
#   save_extract_as_json(nhgis_extract, json_tmpfile)
#   copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile, "nhgis")
#   expect_identical(nhgis_extract, copy_of_nhgis_extract)
#   expect_error(
#     define_extract_from_json(json_tmpfile, "usa"),
#     ".+Did you specify the correct collection for this extract?"
#   )
# })
#
# test_that("We can export to and import from JSON, submitted NHGIS extract", {
#   skip_if_no_api_access(have_api_access)
#   json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
#   on.exit(unlink(json_tmpfile))
#   save_extract_as_json(submitted_nhgis_extract, json_tmpfile)
#   copy_of_submitted_nhgis_extract <- define_extract_from_json(
#     json_tmpfile,
#     "nhgis"
#   )
#   expect_identical(
#     ipumsr:::copy_ipums_extract(submitted_nhgis_extract),
#     copy_of_submitted_nhgis_extract
#   )
#   expect_error(
#     define_extract_from_json(json_tmpfile, "usa"),
#     ".+Did you specify the correct collection for this extract?"
#   )
# })
#

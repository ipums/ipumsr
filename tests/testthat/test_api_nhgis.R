library(dplyr)
library(purrr)

# Setup ------------------------------------------------------------------------

nhgis_extract <- define_extract_nhgis(
  description = "Extract for R client testing",
  dataset = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
  ds_tables = c("B01001", "B01002"),
  ds_geog_levels = list("nation", "blck_grp"),
  geographic_extents = c("110", "420"),
  # breakdown_and_data_type_layout = "single_file",
  time_series_table = "CW3",
  tst_geog_levels = "state",
  tst_layout = "time_by_row_layout",
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

  submitted_extract_number <- submitted_nhgis_extract$number

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
  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })
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
  expect_equal(
    nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    nhgis_extract$ds_tables,
    list("2014_2018_ACS5a" = c("B01001", "B01002"),
         "2015_2019_ACS5a" = c("B01001", "B01002"))
  )
  expect_equal(
    nhgis_extract$ds_geog_levels,
    list("2014_2018_ACS5a" = "nation",
         "2015_2019_ACS5a" = "blck_grp")
  )
  expect_equal(
    nhgis_extract$ds_years,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(
    nhgis_extract$ds_breakdown_values,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(nhgis_extract$time_series_tables, "CW3")
  expect_equal(nhgis_extract$tst_geog_levels, list("CW3" = "state"))
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
      tst_geog_levels = "A1"
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


test_that("Can submit an NHGIS extract of multiple types", {
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_nhgis_extract, c("nhgis_extract", "ipums_extract"))
  expect_equal(submitted_nhgis_extract$collection, "nhgis")
  expect_equal(
    submitted_nhgis_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a")
  )
  expect_equal(
    submitted_nhgis_extract$ds_tables,
    list("2014_2018_ACS5a" = c("B01001", "B01002"),
         "2015_2019_ACS5a" = c("B01001", "B01002"))
  )
  expect_equal(
    submitted_nhgis_extract$ds_breakdown_values,
    list("2014_2018_ACS5a" = NULL,
         "2015_2019_ACS5a" = NULL)
  )
  expect_equal(submitted_nhgis_extract$time_series_tables, "CW3")
  expect_equal(submitted_nhgis_extract$tst_geog_levels, list("CW3" = "state"))
  expect_equal(submitted_nhgis_extract$shapefiles, "110_blck_grp_2019_tl2019")
  expect_equal(submitted_nhgis_extract$geographic_extents, c("110", "420"))
  expect_true(submitted_nhgis_extract$submitted)
  expect_equal(submitted_nhgis_extract$status, "submitted") # Currently fails because API does not return status upon submission
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
      "\n",
      "\nDataset: 2014_2018_ACS5a",
      "\n  Tables: B01001, B01002",
      "\n  Geog Levels: nation",
      "\n  Extents: 110, 420",
      "\n",
      "\nDataset: 2015_2019_ACS5a",
      "\n  Tables: B01001, B01002",
      "\n  Geog Levels: blck_grp",
      "\n  Extents: 110, 420",
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
        ds_tables = "DT",
        ds_geog_levels = "DG",
        ds_years = "Y1",
        ds_breakdown_values = "B1"
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
      "\n  Breakdowns: B1",
      "\n"
    )
  )
})

# Will need a lot more testing here after list conversion?
test_that("nhgis_extract validate method works", {
  expect_identical(validate_ipums_extract(nhgis_extract), nhgis_extract)
  expect_identical(validate_ipums_extract(nhgis_extract_shp), nhgis_extract_shp)
  expect_error(
    validate_ipums_extract(new_ipums_extract("nhgis")),
    "An nhgis_extract must contain at least one of `datasets`"
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      datasets = "Test"
    ),
    paste0(
      "An nhgis_extract that contains datasets must also contain values for: ",
      "`ds_tables`, `ds_geog_levels`."
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      time_series_tables = "Test",
      tst_geog_levels = NA
    ),
    paste0(
      "An nhgis_extract that contains time series tables must also contain ",
      "values for: `tst_geog_levels`"
    )
  )
  expect_error(
    define_extract_nhgis(
      description = "",
      datasets = "Test",
      ds_tables = "Test",
      ds_geog_levels = "Test",
      data_format = "Test"
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
      "An nhgis_extract must contain values for: `description`."
    )
  )
  expect_warning(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        ds_geog_levels = "Test"
      )
    ),
    paste0(
      "The following parameters are not relevant for an nhgis_extract that ",
      "does not include any datasets: `ds_geog_levels`."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        time_series_tables = c("A00", "A01"),
        tst_geog_levels = list("a", "b", "c"),
        data_format = "csv_no_header",
        tst_layout = "time_by_row_layout"
      )
    ),
    regexp = "The number of selections provided in `tst_geog_levels` \\(3\\)"
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        datasets = c("A00", "A01"),
        ds_tables = NA,
        ds_geog_levels = list(NULL, "b"),
      )
    ),
    paste0(
      "An nhgis_extract that contains datasets must also contain values for: ",
      "`ds_tables`, `ds_geog_levels`, `data_format`."
    )
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = "",
        shapefiles = "Test",
        data_format = "Test"
      )
    ),
    "`data_format` must be one of ",
  )
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        datasets = NA,
        time_series_tables = c("CW3", NA)
      )
    ),
    paste0(
      "An nhgis_extract cannot include missing values in `datasets`, ",
      "`time_series_tables`."
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
                         data_layer = contains("blck_grp"),
                         verbose = FALSE)
      expect_equal(nrow(data), 10190)

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
                         data_layer = contains("state"),
                         verbose = FALSE)
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

# # # Revising ---------------------------------------------------------------------

test_that("Can add full datasets/time series tables to an NHGIS extract", {
  revised_extract <- add_to_extract(
    nhgis_extract,
    datasets = "ds1",
    ds_tables = "dt1",
    ds_geog_levels = c("g1", "g2"),
    time_series_tables = c("tst1", "tst2"),
    tst_geog_levels = list("g1", "g2")
  )

  expect_equal(
    revised_extract$datasets,
    c("2014_2018_ACS5a", "2015_2019_ACS5a", "ds1")
  )
  expect_equal(revised_extract$ds_tables$ds1, "dt1")
  expect_equal(revised_extract$ds_geog_levels$ds1, c("g1", "g2"))
  expect_null(revised_extract$ds_years$ds1)
  expect_null(revised_extract$ds_breakdown_values$ds1)
  expect_equal(
    revised_extract$ds_tables[["2014_2018_ACS5a"]],
    c("B01001", "B01002")
  )
  expect_equal(revised_extract$time_series_tables, c("CW3", "tst1", "tst2"))
  expect_equal(revised_extract$tst_geog_levels$tst1, "g1")
  expect_equal(revised_extract$tst_geog_levels$tst2, "g2")

  expect_equal(revised_extract$data_format, nhgis_extract$data_format)
  expect_equal(revised_extract$shapefiles, nhgis_extract$shapefiles)
})

test_that("Can add subfields to all fields in an NHGIS extract", {
  revised_extract <- add_to_extract(
    nhgis_extract,
    ds_tables = c("B01001", "B01003"),
    ds_years = 1990:1995,
    tst_geog_levels = "county",
    shapefiles = "shp1",
    data_format = "csv_header"
  )

  expect_equal(revised_extract$datasets, nhgis_extract$datasets)
  expect_equal(
    unname(revised_extract$ds_tables),
    rep(list(c("B01001", "B01002", "B01003")), 2)
  )
  expect_equal(
    unname(revised_extract$ds_years),
    rep(list(1990:1995), 2)
  )
  expect_equal(revised_extract$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(
    revised_extract$tst_geog_levels$CW3,
    c("state", "county")
  )
  expect_equal(
    revised_extract$shapefiles,
    c("110_blck_grp_2019_tl2019", "shp1")
  )
  expect_equal(
    revised_extract$data_format,
    "csv_header"
  )
})

test_that("Can add subfields to specific fields in an NHGIS extract", {
  revised_extract <- add_to_extract(
    nhgis_extract,
    datasets = "2015_2019_ACS5a",
    ds_tables = c("B01001", "B01003"),
    ds_years = 1990:1995,
    time_series_tables = "CW3",
    tst_geog_levels = "county",
    shapefiles = "shp1"
  )

  expect_equal(revised_extract$datasets, nhgis_extract$datasets)
  expect_equal(
    unname(revised_extract$ds_tables),
    list(c("B01001", "B01002"), c("B01001", "B01002", "B01003"))
  )
  expect_equal(
    unname(revised_extract$ds_years),
    list(NULL, 1990:1995)
  )
  expect_equal(revised_extract$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(
    revised_extract$tst_geog_levels$CW3,
    c("state", "county")
  )
  expect_equal(
    revised_extract$shapefiles,
    c("110_blck_grp_2019_tl2019", "shp1")
  )
})

test_that("Can add to particular fields by index if left unspecified", {
  revised_extract <- add_to_extract(
    nhgis_extract,
    ds_tables = list("B01004", "B01003"),
    ds_years = list(NULL, 1990:1992),
    tst_geog_levels = "county"
  )

  expect_equal(
    unname(revised_extract$ds_tables),
    list(c("B01001", "B01002", "B01004"), c("B01001", "B01002", "B01003"))
  )
  expect_equal(
    unname(revised_extract$ds_years),
    list(NULL, 1990:1992)
  )
  expect_equal(
    unname(revised_extract$tst_geog_levels),
    list(c("state", "county"))
  )
})

test_that("Can remove full datasets/time series tables from an NHGIS extract", {
  revised_extract <- remove_from_extract(
    nhgis_extract,
    datasets = "2014_2018_ACS5a",
    time_series_tables = "CW3"
  )

  expect_null(revised_extract$ds_tables[["2014_2018_ACS5a"]])
  expect_null(revised_extract$ds_geog_levels[["2014_2018_ACS5a"]])
  expect_null(revised_extract$ds_years[["2014_2018_ACS5a"]])
  expect_null(revised_extract$ds_breakdown_values[["2014_2018_ACS5a"]])

  expect_equal(revised_extract$datasets, "2015_2019_ACS5a")
  expect_equal(
    revised_extract$ds_tables[["2015_2019_ACS5a"]],
    nhgis_extract$ds_tables[["2015_2019_ACS5a"]]
  )
  expect_equal(
    revised_extract$ds_geog_levels[["2015_2019_ACS5a"]],
    nhgis_extract$ds_geog_levels[["2015_2019_ACS5a"]]
  )

  expect_null(revised_extract$time_series_tables)
  expect_null(revised_extract$tst_geog_levels)
  expect_null(revised_extract$tst_layout)
})

test_that("Can remove subfields from all ds/tsts in an NHGIS extract", {
  revised_extract <- remove_from_extract(
    nhgis_extract,
    ds_tables = "B01001",
    ds_years = "Fake year",
    tst_geog_levels = "state",
    validate = FALSE
  )

  expect_equal(revised_extract$datasets, nhgis_extract$datasets)
  expect_equal(
    unname(revised_extract$ds_tables),
    list("B01002", "B01002")
  )
  expect_equal(
    revised_extract$ds_years,
    nhgis_extract$ds_years
  )
  expect_equal(
    revised_extract$time_series_tables,
    nhgis_extract$time_series_tables
  )
  expect_null(revised_extract$tst_geog_levels$CW3)
})

test_that("Can remove subfields from specific ds/tst in NHGIS extract", {
  revised_extract <- remove_from_extract(
    nhgis_extract,
    datasets = "2014_2018_ACS5a",
    ds_tables = "B01001",
    time_series_tables = "CW3",
    tst_geog_levels = "state",
    validate = FALSE
  )

  expect_equal(revised_extract$datasets, nhgis_extract$datasets)
  expect_equal(
    unname(revised_extract$ds_tables),
    list("B01002", c("B01001", "B01002"))
  )
  expect_equal(revised_extract$time_series_tables, nhgis_extract$time_series_tables)
  expect_null(revised_extract$tst_geog_levels$CW3)
})

test_that("Can remove from particular fields by index if left unspecified", {
  revised_extract <- remove_from_extract(
    nhgis_extract,
    ds_tables = list("B01001", "B01002")
  )

  expect_equal(revised_extract$datasets, nhgis_extract$datasets)
  expect_equal(
    unname(revised_extract$ds_tables),
    list("B01002", "B01001")
  )
  expect_equal(revised_extract$time_series_tables, nhgis_extract$time_series_tables)
})

test_that("Can add new fields and modify existing fields in same call", {
  revised_extract <- add_to_extract(
    nhgis_extract,
    datasets = c("NEWDS", "2015_2019_ACS5a"),
    ds_tables = "DT1",
    ds_geog_levels = list("G1", "G2"),
    time_series_tables = "CW5",
    tst_geog_levels = "nation",
    tst_layout = "time_by_column_layout",
    geographic_extents = "550"
  )

  expect_equal(revised_extract$datasets, c(nhgis_extract$datasets, "NEWDS"))
  expect_equal(
    unname(revised_extract$ds_tables),
    list(c("B01001", "B01002"), c("B01001", "B01002", "DT1"), "DT1")
  )
  expect_equal(
    unname(revised_extract$ds_geog_levels),
    list("nation", c("blck_grp", "G2"), "G1")
  )

  expect_equal(revised_extract$time_series_tables, c("CW3", "CW5"))
  expect_equal(
    revised_extract$tst_geog_levels,
    list(CW3 = "state", CW5 = "nation")
  )

  expect_equal(
    revised_extract$tst_layout,
    "time_by_column_layout"
  )

  expect_equal(
    revised_extract$geographic_extents,
    c(nhgis_extract$geographic_extents, "550")
  )
})

test_that("Incompatible extract revisions return same extract", {
  expect_identical(
    suppressWarnings(
      remove_from_extract(nhgis_extract, time_series_tables = "FAKE")
    ),
    nhgis_extract
  )
  expect_identical(
    suppressWarnings(
      remove_from_extract(nhgis_extract, datasets = "FAKE")
    ),
    nhgis_extract
  )
  expect_identical(
    suppressWarnings(
      remove_from_extract(
        nhgis_extract,
        datasets = "FAKE",
        ds_tables = "FAKE",
        shapefiles = "FAKE"
      )
    ),
    nhgis_extract
  )
  expect_identical(
    add_to_extract(nhgis_extract, datasets = "2014_2018_ACS5a"),
    nhgis_extract
  )
  expect_identical(
    add_to_extract(nhgis_extract),
    nhgis_extract
  )
  expect_identical(
    remove_from_extract(nhgis_extract),
    nhgis_extract
  )
})

test_that("Bad extract revisions throw correct warnings and errors", {
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      time_series_tables = "TST"
    ),
    "Some time series tables \\(\"TST\"\\) could not be modified because"
  )
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      datasets = "TST"
    ),
    "Some datasets \\(\"TST\"\\) could not be modified because"
  )
  expect_error(
    remove_from_extract(
      nhgis_extract,
      ds_geog_levels = "nation"
    ),
    paste0(
      "An nhgis_extract that contains datasets must also contain values for: ",
      "`ds_geog_levels`"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      datasets = "A",
      ds_tables = list("A", "B")
    ),
    paste0(
      "The number of selections provided in `ds_tables` \\(2\\) does not ",
      "match the number of datasets to be modified \\(1\\). To recycle"
    )
  )
  expect_error(
    add_to_extract(
      nhgis_extract,
      ds_tables = list("A", "B", "C")
    ),
    paste0(
      "The number of selections provided in `ds_tables` \\(3\\) does not ",
      "match the number of datasets to be modified \\(2\\). To recycle"
    )
  )
  expect_error(
    remove_from_extract(
      nhgis_extract,
      tst_geog_levels = list("A", "B")
    ),
    paste0(
      "The number of selections provided in `tst_geog_levels` \\(2\\) does not ",
      "match the number of time series tables to be modified \\(1\\). To recyc"
    )
  )
})

test_that("Tibble of recent NHGIS extracts has expected structure", {
  skip_if_no_api_access(have_api_access)

  expected_columns <- c("collection", "number", "description", "data_type",
                        "name", "ds_tables",
                        "ds_geog_levels",  "tst_geog_levels", "ds_years",
                        "ds_breakdown_values", "geographic_extents",
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
    row_level_nhgis_tbl_submitted$ds_tables[[1]],
    unname(ready_nhgis_extract$ds_tables)
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$ds_geog_levels[[1]],
    unname(ready_nhgis_extract$ds_geog_levels)
  )
  # When NULL, values are not recycled in the tbl format:
  expect_equal(
    row_level_nhgis_tbl_submitted$ds_years[[1]],
    unlist(unname(ready_nhgis_extract$ds_years))
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$ds_breakdown_values[[1]],
    unname(ready_nhgis_extract$ds_breakdown_values)
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$time_series_tables[[1]],
    ready_nhgis_extract$time_series_tables
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$tst_geog_levels[[1]],
    unname(ready_nhgis_extract$tst_geog_levels)
  )
  expect_equal(
    row_level_nhgis_tbl_submitted$shapefiles[[1]],
    ready_nhgis_extract$shapefiles
  )
})


test_that("Can limit number of recent extracts to get info on", {
  skip_if_no_api_access(have_api_access)

  vcr::use_cassette("recent-nhgis-extracts-tbl-two", {
    two_recent_nhgis_extracts <- get_recent_extracts_info_tbl("nhgis", 2)
  })

  expect_equal(length(unique(two_recent_nhgis_extracts$number)), 2)
  expect_equal(nrow(collapse_nhgis_extract_tbl(two_recent_nhgis_extracts)), 2)
})

test_that("Shapefile-only can be converted from tbl to list", {
  vcr::use_cassette("recent-nhgis-extracts-tbl-one", {
    nhgis_extract_tbl_shp <- get_recent_extracts_info_tbl("nhgis", 1)
  })

  expect_identical(
    extract_tbl_to_list(nhgis_extract_tbl_shp)[[1]],
    ready_nhgis_extract_shp
  )
})

test_that("NHGIS tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)
  converted_to_list <- extract_tbl_to_list(
    recent_nhgis_extracts_tbl,
    validate = FALSE
  )
  converted_to_tbl <- extract_list_to_tbl(recent_nhgis_extracts_list)
  expect_identical(
    recent_nhgis_extracts_list[[1]],
    converted_to_list[[1]]
  )
  expect_identical(recent_nhgis_extracts_tbl, converted_to_tbl)
  expect_error(
    extract_list_to_tbl(
      list(submitted_nhgis_extract,
           submitted_usa_extract)
    ),
    "All extracts in `extract_list` must belong to same collection"
  )
})

test_that("We can export to and import from JSON for NHGIS", {
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(nhgis_extract, json_tmpfile)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile, "nhgis")
  expect_identical(nhgis_extract, copy_of_nhgis_extract)
  expect_error(
    define_extract_from_json(json_tmpfile, "usa"),
    ".+Did you specify the correct collection for this extract?"
  )
})

test_that("We can export to and import from JSON, submitted NHGIS extract", {
  skip_if_no_api_access(have_api_access)
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(submitted_nhgis_extract, json_tmpfile)
  copy_of_submitted_nhgis_extract <- define_extract_from_json(
    json_tmpfile,
    "nhgis"
  )
  expect_identical(
    ipumsr:::copy_ipums_extract(submitted_nhgis_extract),
    copy_of_submitted_nhgis_extract
  )
  expect_error(
    define_extract_from_json(json_tmpfile, "usa"),
    ".+Did you specify the correct collection for this extract?"
  )
})


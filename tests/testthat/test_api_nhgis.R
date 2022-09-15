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

usa_extract <- define_extract_usa(
  samples = "us2017b",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

nhgis_json <- new_ipums_json(extract_to_request_json(nhgis_extract), "nhgis")
usa_json <- new_ipums_json(extract_to_request_json(usa_extract), "usa")

if (have_api_access) {

  # Full extract
  vcr::use_cassette("submitted-nhgis-extract", {
    submitted_nhgis_extract <- submit_extract(nhgis_extract)
  })

  submitted_extract_number <- submitted_nhgis_extract$number

  vcr::use_cassette("ready-nhgis-extract", {
    ready_nhgis_extract <- wait_for_extract(submitted_nhgis_extract)
  })

  # Shapefile-only extract
  vcr::use_cassette("submitted-nhgis-extract-shp", {
    submitted_nhgis_extract_shp <- submit_extract(nhgis_extract_shp)
  })

  vcr::use_cassette("ready-nhgis-extract-shp", {
    ready_nhgis_extract_shp <- wait_for_extract(submitted_nhgis_extract_shp)
  })

  # Modify ready-<collection>-extract.yml files to only include the final http
  # request, so that they return the ready-to-download extract immediately on
  # subsequent runs.
  modify_ready_extract_cassette_file("ready-nhgis-extract.yml")
  modify_ready_extract_cassette_file("ready-nhgis-extract-shp.yml")

  # USA extract
  # TODO: move tests that use this to test_api.R
  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })

  # Recent extracts
  vcr::use_cassette("recent-nhgis-extracts-list", {
    recent_nhgis_extracts_list <- get_recent_extracts_info_list("nhgis")
  })

  vcr::use_cassette("recent-nhgis-extracts-tbl", {
    recent_nhgis_extracts_tbl <- get_recent_extracts_info_tbl("nhgis")
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

test_that("Extract print coloring works", {
  local_reproducible_output(crayon = TRUE)
  expect_equal(
    format_dataset_for_printing("A", "B", "C"),
    paste0(
      "\n\n\033[34m\033[1mDataset: ",
      "\033[22m\033[39mA\n  \033[1mTables: ",
      "\033[22mB\n  \033[1mGeog Levels: \033[22mC"
    )
  )
  expect_equal(
    format_tst_for_printing("A", "B"),
    paste0(
      "\n\n\033[32m\033[1mTime Series Table: ",
      "\033[22m\033[39mA\n  \033[1mGeog Levels: \033[22mB"
    )
  )
  local_reproducible_output(crayon = FALSE)
  expect_equal(
    format_dataset_for_printing("A", "B", "C"),
    "\n\nDataset: A\n  Tables: B\n  Geog Levels: C"
  )
  expect_equal(
    format_tst_for_printing("A", "B"),
    "\n\nTime Series Table: A\n  Geog Levels: B"
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
      "\nGeographic extents: 110, 420",
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
      "\n  Breakdowns: B1"
    )
  )
})

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
  expect_error(
    validate_ipums_extract(
      new_ipums_extract(
        "nhgis",
        description = c("a", "b"),
        data_format = c("a", "b"),
        breakdown_and_data_type_layout = c("a", "b"),
        tst_layout = c("a", "b"),
        shapefiles = "shp"
      )
    ),
    paste0(
      "The following fields of an `nhgis_extract` must be of length 1: ",
      "`description`, `breakdown_and_data_type_layout`, ",
      "`tst_layout`, `data_format`."
    )
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

# > Checking Extracts ----------------------------

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
      # gis_data_file_path <- convert_to_relative_path(gis_data_file_path)

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

# > Revising ------------------------------------

test_that("Can add new fields to an NHGIS extract", {

  revised <- add_to_extract(
    nhgis_extract,
    datasets = "New",
    ds_tables = c("T1", "T2"),
    ds_geog_levels = "G1",
    ds_years = "Y1",
    time_series_tables = c("CW4", "CW5"),
    tst_geog_levels = list("G1", "G2"),
    data_format = "csv_header",
    shapefiles = "New"
  )

  expect_equal(revised$datasets, c(nhgis_extract$datasets, "New"))
  expect_equal(
    revised$ds_tables,
    c(nhgis_extract$ds_tables, list(New = c("T1", "T2")))
  )
  expect_equal(
    revised$ds_geog_levels,
    c(nhgis_extract$ds_geog_levels, list(New = "G1"))
  )
  expect_equal(
    revised$ds_years,
    c(nhgis_extract$ds_years, list(New = "Y1"))
  )
  expect_equal(
    revised$time_series_tables,
    c(nhgis_extract$time_series_tables, c("CW4", "CW5"))
  )
  expect_equal(
    revised$tst_geog_levels,
    c(nhgis_extract$tst_geog_levels, list(CW4 = "G1", CW5 = "G2"))
  )
  expect_equal(revised$data_format, "csv_header")
  expect_equal(revised$shapefiles, c(nhgis_extract$shapefiles, "New"))
})

test_that("Can add subfields to existing parent fields", {

  revised <- add_to_extract(
    nhgis_extract,
    ds_tables = c("T1", "T2"),
    ds_geog_levels = list(`2015_2019_ACS5a` = "G1"),
    ds_years = list("Y1", "Y2"),
    time_series_tables = c("CW3", "CW4"),
    tst_geog_levels = list("G1", "G2")
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$ds_tables,
    list(`2014_2018_ACS5a` = c("B01001", "B01002", "T1", "T2"),
         `2015_2019_ACS5a` = c("B01001", "B01002", "T1", "T2"))
  )
  expect_equal(
    revised$ds_geog_levels,
    list(`2014_2018_ACS5a` = c("nation"),
         `2015_2019_ACS5a` = c("blck_grp", "G1"))
  )
  expect_equal(
    revised$ds_years,
    list(`2014_2018_ACS5a` = "Y1",
         `2015_2019_ACS5a` = "Y2")
  )

  expect_equal(revised$time_series_tables, c("CW3", "CW4"))
  expect_equal(
    revised$tst_geog_levels,
    list(CW3 = c("state", "G1"),
         CW4 = "G2")
  )
})

test_that("Can remove full fields from an NHGIS extract", {

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2015_2019_ACS5a",
    time_series_tables = "CW3",
    geographic_extents = "110"
  )

  expect_equal(revised$datasets, "2014_2018_ACS5a")

  expect_equal(
    revised$ds_tables,
    list(`2014_2018_ACS5a` = c("B01001", "B01002"))
  )
  expect_equal(
    revised$ds_geog_levels,
    list(`2014_2018_ACS5a` = c("nation"))
  )
  expect_equal(
    revised$ds_years,
    list(`2014_2018_ACS5a` = NULL)
  )

  expect_null(revised$time_series_tables)
  expect_null(revised$tst_geog_levels)
  expect_null(revised$tst_layout)

  expect_equal(revised$geographic_extents, "420")

})

test_that("Can remove subfields from an NHGIS extract", {

  revised <- remove_from_extract(
    add_to_extract(
      nhgis_extract,
      ds_geog_levels = list(`2014_2018_ACS5a` = "tract"),
      tst_geog_levels = "tract"
    ),
    ds_tables = "B01001",
    ds_geog_levels = list(`2014_2018_ACS5a` = "nation"),
    tst_geog_levels = c("state", "blck_grp"),
    geographic_extents = "420"
  )

  expect_equal(revised$datasets, nhgis_extract$datasets)
  expect_equal(
    revised$ds_tables,
    list(`2014_2018_ACS5a` = "B01002",
         `2015_2019_ACS5a` = "B01002")
  )
  expect_equal(
    revised$ds_geog_levels,
    list(`2014_2018_ACS5a` = "tract",
         `2015_2019_ACS5a` = "blck_grp")
  )

  expect_equal(revised$time_series_tables, nhgis_extract$time_series_tables)
  expect_equal(revised$tst_geog_levels, list(CW3 = "tract"))

  expect_equal(revised$geographic_extents, "110")

})

test_that("Removing parent fields occurs before evaluating subfields", {

  revised <- remove_from_extract(
    nhgis_extract,
    datasets = "2014_2018_ACS5a",
    ds_tables = "B01001"
  )

  expect_equal(revised$datasets, "2015_2019_ACS5a")
  expect_equal(revised$ds_tables, list(`2015_2019_ACS5a` = "B01002"))

})

test_that("Revisions do not alter unspecified extract fields", {

  extract1 <- define_extract_nhgis(
    shapefiles = "Test"
  )

  extract2 <- add_to_extract(
    extract1,
    datasets = "Test",
    ds_tables = "Test",
    ds_geog_levels = "Test"
  )

  extract3 <- suppressWarnings(
    add_to_extract(
      extract2,
      tst_geog_levels = "Test"
    )
  )

  # Test on an extract of multiple types
  expect_identical(nhgis_extract, add_to_extract(nhgis_extract))
  expect_identical(nhgis_extract, remove_from_extract(nhgis_extract))

  # Test on an extract of a single type
  expect_identical(extract1, add_to_extract(extract1))
  expect_identical(extract1, remove_from_extract(extract1))

  expect_null(extract2$time_series_tables)
  expect_null(extract2$tst_geog_levels)
  expect_null(extract2$tst_layout)

  expect_warning(
    add_to_extract(
      extract2,
      tst_geog_levels = "Test"
    ),
    "The following parameters are not relevant for an nhgis_extract that"
  )

  expect_null(extract3$time_series_tables)
  expect_equal(extract3$tst_geog_levels, "Test")

})

test_that("Improper extract revisions throw warnings or errors", {
  expect_warning(
    add_to_extract(nhgis_extract, ds_tables = list("A")),
    paste0(
      "The number of values in `ds_tables` \\(1\\) does not match the number of ",
      "datasets in this extract \\(2\\)"
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      datasets = c("New"),
      ds_tables = list("A", "B"),
      ds_geog_levels = "A"
    ),
    paste0(
      "The number of values in `ds_tables` \\(2\\) does not match the number of ",
      "datasets to be modified \\(1\\)"
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      tst_geog_levels = list(not_in_extract = "blck_grp")
    ),
    paste0(
      "The specification for `tst_geog_levels` references time_series_tables ",
      "that do not exist in this extract \\(\"not_in_extract\"\\). These values ",
      "will be ignored."
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      tst_geog_levels = c(not_in_extract = "blck_grp")
    ),
    paste0(
      "Ignoring names in the specification for `tst_geog_levels`. To apply ",
      "values to time_series_tables by name, ensure values are stored in a ",
      "list, not a vector."
    )
  )
  expect_warning(
    add_to_extract(
      nhgis_extract,
      ds_geog_levels = list("blck_grp", `2014_2018_ACS5a` = "nation")
    ),
    paste0(
      "The specification for `ds_geog_levels` references datasets that do not ",
      "exist in this extract \\(\"\"\\). These values will be ignored."
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
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      time_series_tables = "TST"
    ),
    paste0(
      "Some time_series_tables \\(\"TST\"\\) could not be removed because",
      " they were not found in this extract's time_series_tables \\(\"CW3\"\\)."
    )
  )
  expect_warning(
    remove_from_extract(
      nhgis_extract,
      datasets = "DS"
    ),
    paste0(
      "Some datasets \\(\"DS\"\\) could not be removed because",
      " they were not found in this extract's datasets ",
      "\\(\"2014_2018_ACS5a\", \"2015_2019_ACS5a\"\\)."
    )
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
  expect_warning(
    add_to_extract(nhgis_extract, data_format = c("csv_header", "fixed_width")),
    "Multiple values passed to `data_format`, which must be length 1"
  )
})

# > Recent extracts ------------------------------

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

# > JSON export -----------------------------------

test_that("We can export to and import from JSON for NHGIS", {
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(nhgis_extract, json_tmpfile)
  copy_of_nhgis_extract <- define_extract_from_json(json_tmpfile)
  expect_identical(nhgis_extract, copy_of_nhgis_extract)
})

test_that("We can export to and import from JSON, submitted NHGIS extract", {
  skip_if_no_api_access(have_api_access)
  json_tmpfile <- file.path(tempdir(), "nhgis_extract.json")
  on.exit(unlink(json_tmpfile))
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

# > Metadata --------------------------------------

test_that("We can get summary metadata for all data types", {

  ds_meta <- get_nhgis_metadata("datasets")
  dt_meta <- get_nhgis_metadata("data_tables")
  tst_meta <- get_nhgis_metadata("time_series_tables")
  shp_meta <- get_nhgis_metadata("shapefiles")

  expect_true(tibble::is_tibble(ds_meta))
  expect_equal(ncol(ds_meta), 4)
  expect_equal(
    colnames(ds_meta),
    c("name", "group", "description", "sequence")
  )

  expect_true(tibble::is_tibble(dt_meta))
  expect_equal(ncol(dt_meta), 5)
  expect_equal(
    colnames(dt_meta),
    c("dataset", "name", "nhgis_code", "description", "sequence")
  )

  expect_true(tibble::is_tibble(tst_meta))
  expect_equal(ncol(tst_meta), 6)
  expect_equal(
    colnames(tst_meta),
    c("name", "description", "geographic_integration",
      "sequence", "years", "geog_levels")
  )

  expect_true(tibble::is_tibble(shp_meta))
  expect_equal(ncol(shp_meta), 6)
  expect_equal(
    colnames(shp_meta),
    c("name", "year", "geographic_level",
      "extent", "basis", "sequence")
  )

})

test_that("We can filter summary metadata", {
  dt_meta <- get_nhgis_metadata(
    "data_tables",
    keywords = c("sex", "age", "race", "hispanic")
  )

  expect_true(all(grepl("[Ss]ex", dt_meta$description)))
  expect_true(all(grepl("[Aa]ge", dt_meta$description )))
  expect_true(all(grepl("[Rr]ace", dt_meta$description)))
  expect_true(all(grepl("[Hh]ispanic", dt_meta$description)))

  dt_meta <- get_nhgis_metadata(
    "data_tables",
    keywords = c("sex", "age", "race", "hispanic"),
    match_all = FALSE
  )

  expect_true(
    all(grepl("[Ss]ex|[Aa]ge|[Rr]ace|[Hh]ispanic", dt_meta$description))
  )

  expect_warning(
    get_nhgis_metadata("shapefiles", keywords = "state"),
    paste0(
      "`keywords` only implemented when `type` is one of \"datasets\", ",
      "\"data_tables\", or \"time_series_tables\""
    )
  )
  expect_warning(
    get_nhgis_metadata(dataset = "1990_STF1", keywords = "state"),
    paste0(
      "`keywords` only implemented when `type` is one of \"datasets\", ",
      "\"data_tables\", or \"time_series_tables\""
    )
  )
})

test_that("We can get metadata for specific data sources", {

  dataset <- "2010_SF1a"
  table <- "P8"
  ds_meta <- get_nhgis_metadata(dataset = dataset)
  dt_meta <- get_nhgis_metadata(dataset = dataset, ds_table = table)

  tst <- "CM0"
  tst_meta <- get_nhgis_metadata(time_series_table = tst)

  expect_equal(length(ds_meta), 10)
  expect_equal(
    names(ds_meta),
    c("name", "nhgis_id", "group", "description", "sequence",
      "has_multiple_data_types", "data_tables", "geog_levels",
      "geographic_instances", "breakdowns")
  )
  expect_equal(ds_meta$name, dataset)
  expect_true(
    all(
      tibble::is_tibble(ds_meta$data_tables),
      tibble::is_tibble(ds_meta$geog_levels),
      tibble::is_tibble(ds_meta$geographic_instances),
      tibble::is_tibble(ds_meta$breakdowns)
    )
  )

  expect_equal(length(tst_meta), 7)
  expect_equal(
    names(tst_meta),
    c("name", "description", "geographic_integration", "sequence",
      "time_series", "years", "geog_levels")
  )
  expect_equal(tst_meta$name, tst)
  expect_true(
    all(
      tibble::is_tibble(tst_meta$time_series),
      tibble::is_tibble(tst_meta$years),
      tibble::is_tibble(tst_meta$geog_levels)
    )
  )

  expect_equal(length(dt_meta), 6)
  expect_equal(
    names(dt_meta),
    c("name", "description", "universe", "nhgis_code",
      "sequence", "variables")
  )
  expect_equal(dt_meta$name, table)
  expect_true(tibble::is_tibble(dt_meta$variables))

})

test_that("We throw errors on bad metadata requests", {

  expect_error(
    get_nhgis_metadata(),
    "At least one of `type`, `dataset`, or `time_series_table`"
  )
  expect_error(
    get_nhgis_metadata(
      type = c("shapefiles", "time_series_tables")
    ),
    "Can only retrieve metadata for a single value of `type` at a time"
  )
  expect_error(
    get_nhgis_metadata(type = "Bad type"),
    "`type` must be one of"
  )
  expect_error(
    get_nhgis_metadata(dataset = "bad-dataset"),
    "Couldn't find Dataset"
  )
  expect_error(
    get_nhgis_metadata(dataset = "1980_STF1", ds_table = "bad-table"),
    "Couldn't find DataTable"
  )
  expect_error(
    get_nhgis_metadata(time_series_table = "bad-tst"),
    "Couldn't find TimeSeriesTable"
  )
  expect_error(
    get_nhgis_metadata(dataset = "1980_STF1", ds_table = "bad table"),
    "bad/illegal format or missing URL"
  )
  expect_error(
    get_nhgis_metadata(dataset = "1980_STF1", time_series_table = "CW3"),
    "Only one of `type`, `dataset`, or `time_series_table` may be specified"
  )
  expect_error(
    get_nhgis_metadata(ds_table = "P8"),
    "`ds_table` must be specified with a corresponding `dataset`"
  )
  expect_error(
    get_nhgis_metadata(time_series_table = "CW3", ds_table = "a"),
    "`ds_table` must be specified with a corresponding `dataset`"
  )

})






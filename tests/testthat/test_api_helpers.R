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

  current_ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
  current_home_dir <- Sys.getenv("HOME")
  temp_renviron_file <- file.path(tempdir(), ".Renviron")

  # Reset envvars to original state upon test completion
  withr::defer(Sys.setenv(HOME = current_home_dir))
  withr::defer(Sys.setenv(IPUMS_API_KEY = current_ipums_api_key))
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


# Misc ------------------------------------------

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

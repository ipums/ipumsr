# Sample extracts for use in api unit testing
test_usa_extract <- function() {
  define_extract_usa(
    samples = "us2017b",
    variables = "YEAR",
    description = "Test extract",
    data_format = "fixed_width"
  )
}

test_cps_extract <- function() {
  define_extract_cps(
    samples = c("cps1976_01s", "cps1976_02b"),
    variables = c("YEAR", "MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1"),
    description = "Compare age-sex-race breakdowns 1976",
    data_format = "fixed_width"
  )
}

test_nhgis_extract <- function() {
  define_extract_nhgis(
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
}

test_nhgis_extract_shp <- function() {
  define_extract_nhgis(
    shapefiles = "110_blck_grp_2019_tl2019"
  )
}

#' Remove intermediate API requests from API unit test fixtures
#'
#' @description
#' Removes all but the final request from an existing API test fixture found in
#' tests/fixtures. This is useful for processes that may make multiple
#' API requests (e.g. `wait_for_extract()`). Modifying the output fixture
#' ensures that we do not "wait" for completion when using the fixture in the
#' future, as the request will make it look as if the extract has already
#' completed. This greatly speeds up testing time.
#'
#' If more than the final API request are required for proper testing of certain
#' features, you can retain multiple requests with the `n_requests` argument.
#'
#' @param cassette_file_name Name of the test fixture .yml file to be modified
#' @param n_requests Number of requests to retain in the modified file.
#'   By default, includes only the last API request. However, in some cases
#'   it may be beneficial to save multiple requests in a single fixture (for
#'   instance, if testing `wait_for_extract()` functionality).
#'
#' @noRd
modify_ready_extract_cassette_file <- function(cassette_file_name,
                                               n_requests = 1) {
  ready_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), cassette_file_name
  )

  ready_lines <- readLines(ready_extract_cassette_file)
  request_lines <- which(grepl("^- request:", ready_lines))

  start_line <- request_lines[length(request_lines) - n_requests + 1]

  writeLines(
    c(
      ready_lines[[1]],
      ready_lines[start_line:length(ready_lines)]
    ),
    con = ready_extract_cassette_file
  )
}

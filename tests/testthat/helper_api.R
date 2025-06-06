# Sample extracts for use in api unit testing
test_usa_extract <- function() {
  define_extract_micro(
    collection = "usa",
    description = "Test USA extract",
    samples = samp_spec("us2017b"),
    variables = list(
      var_spec(
        "RACE",
        case_selections = c("801", "802"),
        case_selection_type = "detailed",
        preselected = FALSE
      ),
      "YEAR"
    ),
    data_format = "fixed_width",
    case_select_who = "households"
  )
}

test_usa_extract_household_only <- function() {
  define_extract_micro(
    collection = "usa",
    description = "Test USA household-only extract",
    samples = samp_spec("us2017b"),
    variables = "STATEFIP",
    data_structure = "household_only"
  )
}

test_cps_extract <- function() {
  define_extract_micro(
    collection = "cps",
    description = "Compare age-sex-race breakdowns 1976",
    samples = c("cps2018_03s", "cps2019_03s"),
    variables = list(
      var_spec(
        "AGE",
        attached_characteristics = "head",
        data_quality_flags = FALSE
      ),
      var_spec(
        "SEX",
        case_selections = "2",
        attached_characteristics = c("mother", "father")
      ),
      var_spec(
        "RACE",
        case_selections = c("810", "811", "812"),
        case_selection_type = "general"
      )
    ),
    data_format = "fixed_width",
    data_structure = "hierarchical",
    case_select_who = "individuals",
    data_quality_flags = TRUE
  )
}

test_ipumsi_extract <- function() {
  define_extract_micro(
    collection = "ipumsi",
    description = "Test IPUMSI extract",
    samples = c("mx2015a", "cl2017a"),
    variables = list(
      var_spec("AGE", case_selections = "010"),
      var_spec("SEX", attached_characteristics = "father"),
      "EDATTAIN"
    ),
    data_format = "csv"
  )
}

test_atus_extract <- function() {
  define_extract_micro(
    collection = "atus",
    description = "Test ATUS extract",
    samples = c("at2020", "at2021"),
    variables = list(
      "STATEFIP",
      var_spec(
        "AGE",
        attached_characteristics = c("mother", "father", "spouse"),
        data_quality_flags = TRUE
      ),
      "SEX",
      "DIFFANY",
      "RELATER"
    ),
    time_use_variables = list(
      "ACT_PCARE",
      tu_var_spec("my_time_use_var", owner = "example@example.com")
    ),
    sample_members = "include_household_members",
    data_structure = "hierarchical"
  )
}

# In order to test that we can request user-created time use variables, this
# extract request includes a real time use variable created by Derek Burk
# (burkx031@umn.edu). Using vcr fixtures, tests that use this extract should
# be runnable by anyone, but if the fixtures aren't available, those tests will
# fail.
test_atus_extract_submittable <- function() {
  define_extract_micro(
    collection = "atus",
    description = "Test ATUS extract",
    samples = c("at2020", "at2021"),
    variables = list(
      "STATEFIP",
      var_spec(
        "AGE",
        attached_characteristics = c("mother", "father", "spouse"),
        data_quality_flags = TRUE
      ),
      "SEX",
      "DIFFANY",
      "RELATER"
    ),
    time_use_variables = list(
      "ACT_PCARE",
      tu_var_spec("screentime", owner = "robe2037@umn.edu")
    ),
    sample_members = c("include_household_members", "include_non_respondents"),
    data_structure = "hierarchical"
  )
}


test_nhis_extract <- function() {
  define_extract_micro(
    collection = "nhis",
    description = "Test NHIS extract",
    samples = c("ih2010", "ih2011"),
    variables = list(
      "LIVINGQTR",
      "AGE",
      "SEX",
      var_spec("BMI", attached_characteristics = c("mother", "father")),
      "IRCAUSE",
      "IRMONTH"
    ),
    rectangular_on = "I"
  )
}

test_meps_extract <- function() {
  define_extract_micro(
    collection = "meps",
    description = "Test MEPS extract",
    samples = c("mp2005", "mp2006"),
    variables = list(
      "REGMEPSRD",
      "AGERD",
      "MARSTATRD",
      "ASTHPINQUICKRD",
      var_spec("INCCHLD", data_quality_flags = TRUE)
    ),
    rectangular_on = "R"
  )
}

test_nhgis_extract <- function() {
  define_extract_agg(
    "nhgis",
    description = "Extract for R client testing",
    datasets = list(
      ds_spec("2014_2018_ACS5a", c("B01001", "B01002"), "nation"),
      ds_spec("2015_2019_ACS5a", c("B01001", "B01002"), "blck_grp")
    ),
    time_series_tables = tst_spec("CW3", "state", "1990"),
    geographic_extents = c("110", "100"),
    tst_layout = "time_by_row_layout",
    shapefiles = "110_blck_grp_2019_tl2019",
    data_format = "csv_no_header"
  )
}

test_nhgis_extract_shp <- function() {
  define_extract_agg("nhgis", shapefiles = "110_blck_grp_2019_tl2019")
}

test_ihgis_extract <- function() {
  define_extract_agg(
    "ihgis",
    "Extract for R client testing",
    datasets = ds_spec(
      "AL2001pop",
      c("AL2001pop.ADF", "AL2001pop.ADG"),
      tabulation_geographies = c("AL2001pop.g0", "AL2001pop.g1")
    )
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

  if (file.exists(ready_extract_cassette_file)) {
    ready_lines <- readLines(ready_extract_cassette_file)
    request_lines <- which(grepl("^- request:", ready_lines))

    if (length(request_lines) > n_requests) {
      message(paste0("Modifying ", cassette_file_name, "..."))

      start_line <- request_lines[length(request_lines) - n_requests + 1]

      writeLines(
        c(
          ready_lines[[1]],
          ready_lines[start_line:length(ready_lines)]
        ),
        con = ready_extract_cassette_file
      )
    }
  }
}

modify_ready_extract_cassette_files <- function(cassette_file_names,
                                                n_requests = 1) {
  purrr::walk(
    cassette_file_names,
    ~ modify_ready_extract_cassette_file(.x, n_requests = n_requests)
  )
}

#' Convert all download file paths in a cassette file to be relative to the
#' working directory
#'
#' @noRd
convert_paths_in_cassette_file_to_relative <- function(cassette_path) {
  lines <- readLines(cassette_path)
  file_path_lines <- which(fostr_detect(lines, "file: yes")) + 1
  orig_paths <- purrr::map_chr(
    lines[file_path_lines],
    ~ strsplit(.x, "string: ")[[1]][[2]]
  )
  converted_paths <- purrr::map_chr(orig_paths, convert_to_relative_path)
  for (i in seq_along(orig_paths)) {
    lines <- gsub(orig_paths[[i]], converted_paths[[i]], lines, fixed = TRUE)
  }
  writeLines(lines, con = cassette_path)
}

#' Convert an absolute path to be relative to the working directory
#'
#' This is only used in unit tests at the moment. #TODO: move to utils?
#'
#' @noRd
convert_to_relative_path <- function(path) {
  path_components <- strsplit(path, "/|\\\\", perl = TRUE)[[1]]
  wd_components <- strsplit(getwd(), "/|\\\\", perl = TRUE)[[1]]
  if (identical(path_components, wd_components)) {
    rlang::abort("Supplied path cannot be the path to the working directory")
  }
  path_length <- length(path_components)
  wd_length <- length(wd_components)
  min_length <- min(path_length, wd_length)
  path_components_min_length <- path_components[1:min_length]
  wd_components_min_length <- wd_components[1:min_length]
  components_match <- path_components_min_length == wd_components_min_length
  if (!any(components_match)) {
    rlang::abort("Supplied path must be an absolute path")
  }
  on_same_branch <- all(components_match)
  if (on_same_branch) {
    if (path_length > wd_length) {
      return(
        do.call(file.path, as.list(utils::tail(path_components, -min_length)))
      )
    } else {
      return(
        do.call(file.path, as.list(rep("..", times = wd_length - path_length)))
      )
    }
  }
  split_point <- min(which(!components_match))
  path_relative_to_split <- path_components[split_point:length(path_components)]
  wd_relative_to_split <- wd_components[split_point:length(wd_components)]
  wd_levels_below_split <- length(wd_relative_to_split)
  do.call(
    file.path,
    c(
      as.list(rep("..", times = wd_levels_below_split)),
      as.list(path_relative_to_split)
    )
  )
}

have_api_access <- function() {
  api_access <- TRUE
  vcr_dir <- vcr::vcr_test_path("fixtures")

  if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) {
    if (!dir.exists(vcr_dir) || length(dir(vcr_dir)) == 0) {
      # If there are no mock files nor API token, can't run API tests
      api_access <- FALSE
    }
  }

  api_access
}

skip_if_no_api_access <- function() {
  if (!have_api_access()) {
    return(testthat::skip("no API access and no saved API responses"))
  }
}

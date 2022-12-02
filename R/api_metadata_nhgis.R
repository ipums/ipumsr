
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Get NHGIS Metadata
#'
#' @description
#' Retrieve information about available NHGIS datasets, data tables, time
#' series tables, and shapefiles.
#'
#' To retrieve summary metadata for all available data sources of a particular
#' type, use the `type` argument. To retrieve detailed metadata for a
#' single data source, use the `dataset`, `data_table`, or `time_series_table`
#' arguments.
#'
#' See the **metadata availability** section below for more information on the
#' metadata provided for each data type.
#'
#' @section Metadata availability:
#' The following sections summarize the metadata fields provided for each data
#' type. Summary metadata include a subset of the fields provided for individual
#' data sources.
#'
#' The names of the arguments that can be passed to `...` for each `type` are
#' indicated with an asterisk.
#'
#' ## Datasets:
#'
#' - ***`name`:** The unique identifier for the dataset. This is the code that is
#'   used to refer to the dataset when interacting with the NHGIS extract API.
#' - ***`group:`** The group of datasets to which the dataset belongs.
#'   For instance, 5 separate datasets are part of the
#'   `"2015 American Community Survey"` group.
#' - ***`description:`** A short description of the dataset.
#' - ***`sequence:`** Order in which the dataset will appear in the metadata API
#'   and extracts.
#' - **`has_multiple_data_types:`** Logical value indicating whether multiple
#'   data types exist for this dataset. For example, ACS
#'   datasets include both estimates and margins of error.
#' - **`data_tables:`** A [`tibble`][tibble::tbl_df-class] containing names,
#'   codes, and descriptions for all data tables available for the dataset.
#'   See **data tables** section below.
#' - **`geog_levels:`** A [`tibble`][tibble::tbl_df-class] containing names,
#'   descriptions, and extent information for the geographic levels available
#'   for the dataset. The `has_geog_extent_selection` field contains logical
#'   values indicating whether extent selection is allowed (and required) for
#'   the associated geographic level. See `geographic_instances` below.
#' - **`breakdowns:`** A [`tibble`][tibble::tbl_df-class] containing names,
#'   types, descriptions, and breakdown values for all breakdowns available
#'   for the dataset.
#' - **`years:`** A vector of years for which the dataset is available. This
#'   field is only present if a dataset is available for multiple years. Note
#'   that ACS datasets are not considered to be available for multiple years.
#' - **`geographic_instances:`** A [`tibble`][tibble::tbl_df-class] containing
#'   names and descriptions for all valid geographic extents for the
#'   dataset. This field is only present if at least one of the dataset's
#'   `geog_levels` allows geographic extent selection.
#'
#' ## Data tables:
#'
#' - ***`name`:** The unique identifier for the data table. This is the code that
#'   is used to refer to the data table when interacting with the NHGIS extract
#'   API.
#' - ***`dataset`:** The dataset with which the data table is associated.
#' - ***`description`:** A short description of the data table.
#' - **`universe`:** The statistical population measured by this data table
#'   (e.g. persons, families, occupied housing units, etc.)
#' - ***`nhgis_code`:** The code identifying the data table in the extract.
#'   Variables in the extract data will include column names prefixed with this
#'   code.
#' - ***`sequence`:** Order in which the data table will appear in the metadata
#'   API and extracts.
#' - **`variables`:** A [`tibble`][tibble::tbl_df-class] containing variable
#'   descriptions and codes for the data table
#'
#' ## Time series tables:
#'
#' - ***`name`:** The unique identifier for the time series table. This is the
#'   code that is used to refer to the time series table when interacting with
#'   the NHGIS extract API.
#' - ***`description`:** A short description of the time series table.
#' - ***`geographic_integration`:** The method by which the time series table
#'   aligns geographic units across time. `"Nominal"` integration indicates
#'   that geographic units are aligned by name (disregarding changes in unit
#'   boundaries). `"Standardized"` integration indicates that data from multiple
#'   time points are standardized to the indicated year's census units. For
#'   more information, click
#'   [here](https://www.nhgis.org/time-series-tables#geographic-integration).
#' - ***`sequence`:** Order in which the time series table will appear in the
#'   metadata API and extracts.
#' - **`time_series`:** A [`tibble`][tibble::tbl_df-class] containing names
#'   and descriptions for the individual time series available for the
#'   time series table.
#' - ***`years`:** A [`tibble`][tibble::tbl_df-class] containing
#'   information on the available data years for the
#'   time series table.
#' - ***`geog_levels`:** A [`tibble`][tibble::tbl_df-class] containing names
#'   and descriptions for the geographic levels available
#'   for the time series table.
#'
#' ## Shapefiles:
#'
#' - ***`name`:** The unique identifier for the shapefile. This is the
#'   code that is used to refer to the shapefile when interacting with
#'   the NHGIS extract API.
#' - ***`year`:** The survey year in which the shapefile's represented areas
#'   were used for tabulations, which may be different than the vintage of the
#'   represented areas. For more information, click
#'   [here](https://www.nhgis.org/gis-files#years).
#' - ***`geographic_level`:** The geographic level of the shapefile.
#' - ***`extent`:** The geographic extent covered by the shapefile.
#' - ***`basis`:** The derivation source of the shapefile.
#' - ***`sequence`:** Order in which the shapefile will appear in the
#'   metadata API and extracts.
#'
#' @section Notes:
#' If `type` is not specified, then a `dataset`, `time_series_table`,
#' or both a `dataset` and a `data_table` must be provided. Metadata is only
#' provided for a single `type` or data source at a time.
#'
#' The expressions in `...` are interpreted as regular expressions, which
#' allows for partial matching. This is
#' typically the desired behavior when searching for keywords in data source
#' descriptions, but an exact match may be useful for certain fields,
#' like data table names. Any terms can be wrapped in `"^"` and `"$"`
#' to enforce an exact match. For instance,
#' `get_nhgis_metadata("data_tables", name = "^NT1$")` will match tables whose
#' name is `"NT1"` exactly, rather than all tables that have `"NT1"` somewhere
#' in their name.
#'
#' ## Data table summary metadata
#' The IPUMS NHGIS API does not currently provide updated summary metadata
#' for data tables by default. Therefore, the metadata provided for data tables
#' may be incomplete if new datasets have recently been added.
#'
#' `get_nhgis_metadata("data_tables")` will automatically produce
#' a warning if it detects any datasets whose tables are not included in the
#' data table summary metadata. The metadata for the data tables
#' associated with these datasets can be included by setting
#' `update_tables = TRUE`. Note that updating these metadata will result in an
#' API request for each missing dataset (the IPUMS API is rate limited to 100
#' API requests per minute).
#'
#' The updated data will automatically
#' be cached across R sessions in the directory given by
#' `rappdirs::user_cache_dir("ipumsr")`. If you get unexpected results from
#' `get_nhgis_metadata("data_tables")`, consider clearing the cache directory.
#'
#'
#' @inheritParams submit_extract
#' @param type One of `"datasets"`, `"data_tables"`,
#'   `"time_series_tables"`, or `"shapefiles"` indicating the type of summary
#'   metadata to retrieve.
#' @param dataset Name of an individual dataset for which to retrieve metadata.
#' @param data_table Name of an individual data table for which to retrieve
#'   metadata. If provided, an associated `dataset` must also be specified.
#' @param time_series_table Name of an individual time series table for which
#'   to retrieve metadata.
#' @param ... Optional set of arguments used to filter the requested
#'   summary metadata.
#'
#'   Each argument should be named and consist of a
#'   character vector. Each string in the vector is interpreted as
#'   a regular expression that is matched to the values in the metadata column
#'   with the same name as the argument name. Only metadata records
#'   whose values match the provided expressions will be included in the output.
#'
#'   Only used when `type` is provided. See **metadata availability** section
#'   below.
#' @param match_all If `TRUE`, only metadata records that match *all* of the
#'   expressions provided in `...` will be included in the output. If `FALSE`,
#'   metadata records that match *any* of the expressions will be included.
#'   Defaults to `TRUE`.
#' @param match_case If `TRUE`, use case-sensitive matching when interpreting
#'   the expressions provided in `...`. Defaults to `FALSE`.
#' @param update_tables If `TRUE` and `type = "data_tables"`, update the
#'   provided data table summary metadata to include records for tables
#'   associated with recently-released datasets that are not yet included by
#'   default. The updated metadata will be cached for future use.
#'
#'   By default, updates the table metadata only if fewer than 10 datasets'
#'   tables are missing from the default metadata.
#'
#'   If the current data table summary metadata are already up to date, this
#'   does nothing. See **notes** section below.
#' @return If `type` is provided, a [`tibble`][tibble::tbl_df-class] of
#'   summary metadata for all data sources of the provided `type`.
#'   Otherwise, a named list of metadata for the specified `dataset`,
#'   `data_table`, or `time_series_table`.
#' @export
#'
#' @md
#'
#' @examples
#' # Get summary metadata for all available sources of a given data type
#' get_nhgis_metadata("datasets")
#'
#' # Filter summary metadata to records that meet specific criteria.
#' # This returns records whose description includes both "Sex" and "Age" and
#' # whose years include 1990 and 2000:
#' get_nhgis_metadata(
#'   "time_series_tables",
#'   description = c("Sex", "Age"),
#'   years = c(1990, 2000)
#' )
#'
#' # Alternatively, return records whose description includes
#' # either "Sex" or "Age"
#' get_nhgis_metadata(
#'   "time_series_tables",
#'   description = c("Sex", "Age"),
#'   match_all = FALSE
#' )
#'
#' # Get metadata for single data source
#' get_nhgis_metadata(dataset = "1990_STF1")
#' get_nhgis_metadata(data_table = "NP1", dataset = "1990_STF1")
get_nhgis_metadata <- function(type = NULL,
                               dataset = NULL,
                               data_table = NULL,
                               time_series_table = NULL,
                               ...,
                               match_all = TRUE,
                               match_case = FALSE,
                               update_tables = NULL,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

  summary_req <- !is.null(type)
  ds_req <- !is.null(dataset) && (type %||% "") != "data_tables"
  dt_req <- !is.null(data_table)
  tst_req <- !is.null(time_series_table)

  if (sum(summary_req, ds_req, tst_req) > 1) {
    rlang::abort(
      paste0(
        "Only one of `type`, `dataset`, or `time_series_table` may be ",
        "specified at a time."
      )
    )
  }

  if (dt_req && !ds_req) {
    rlang::abort(
      "`data_table` must be specified with a corresponding `dataset`."
    )
  }

  if (!any(summary_req, ds_req, tst_req)) {
    rlang::abort(
      "One of `type`, `dataset`, or `time_series_table` must be specified."
    )
  }

  is_too_long <- purrr::map_lgl(
    list(type, dataset, data_table, time_series_table),
    ~length(.x) > 1
  )

  if (any(is_too_long)) {
    rlang::abort(
      paste0(
        "Can only retrieve metadata for one `",
        paste0(
          c("type", "dataset", "data_table", "time_series_table")[is_too_long],
          collapse = "`, `"
        ),
        "` at a time."
      )
    )
  }

  if (summary_req) {

    metadata <- get_nhgis_summary_metadata(
      type = type,
      api_key = api_key,
      update_tables = update_tables
    )

    metadata <- filter_multi_col(
      metadata,
      dataset = dataset, # valid filter for "data_tables", but not in `...`
      ...,
      match_all = match_all,
      match_case = match_case
    )

  } else {

    api_url <- metadata_request_url(
      .base_url = nhgis_api_metadata_url(),
      datasets = dataset,
      data_tables = data_table,
      time_series_tables = time_series_table
    )

    metadata <- request_metadata(api_url, api_key)

  }

  metadata

}

# Internal functions -----------------------------------------------------------

#' Get NHGIS summary metadata
#'
#' Helper to retrieve summary metadata for all NHGIS datasets, data tables,
#' time series tables, or shapefiles. Handles data table caching and updating.
#'
#' @inheritParams get_nhgis_metadata
#'
#' @return Tibble of summary metadata for the requested `type`
#'
#' @noRd
get_nhgis_summary_metadata <- function(type,
                                       update_tables = NULL,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {

  if (!type %in% c("datasets", "data_tables",
                   "time_series_tables", "shapefiles")) {
    rlang::abort(
      paste0(
        "`type` must be one of \"datasets\", ",
        "\"data_tables\", \"time_series_tables\", or \"shapefiles\""
      )
    )
  }

  if (type == "data_tables") {

    metadata <- tryCatch(
      load_cached_data(pattern = type),
      error = function(cnd) table_metadata
    )

    metadata <- check_table_metadata(
      metadata,
      update = update_tables,
      api_key = api_key,
      check_pkg_metadata = TRUE
    )

  } else {

    metadata <- request_metadata(
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        type
      ),
      api_key
    )

  }

  metadata

}

#' Check whether data table summary metadata is up to date with latest NHGIS
#' datasets
#'
#' @param metadata Default table metadata as output by `get_nhgis_metadata("data_tables")`
#' @param update Logical indicating whether out-of-date metadata should
#'   be updated.
#' @param api_key API key associated with the user's
#' @param check_pkg_metadata Logical indicating whether to override cached
#'   table metadata with the value of `table_metadata` currently stored
#'   in the package internally. If the package `table_metadata` is more current
#'   than the cached table metadata, `table_metadata` will be used to determine
#'   whether any updates are needed to the provided data table summary metadata.
#'   Otherwise, the cached version will be used.
#'
#'   This is designed to support unit tests of table updating functionality.
#'   In general, this argument should be set to `TRUE` in user-facing cases.
#'
#' @return If `update = FALSE` or if no updates are needed, returns `metadata`.
#'   Otherwise, returns updated data table summary metadata produced by
#'   `update_table_metadata()`
#'
#' @noRd
check_table_metadata <- function(metadata,
                                 update = NULL,
                                 api_key = Sys.getenv("IPUMS_API_KEY"),
                                 check_pkg_metadata = TRUE) {

  datasets <- get_nhgis_summary_metadata(
    type = "datasets",
    api_key = api_key
  )

  missing_datasets <- setdiff(datasets$name, metadata$dataset)

  if (check_pkg_metadata) {
    missing_datasets_from_pkg <- setdiff(datasets$name, table_metadata$dataset)

    # If these differ, the cached metadata version is behind the pkg version,
    # so we will use the pkg version
    if (length(missing_datasets_from_pkg) < length(missing_datasets)) {
      missing_datasets <- missing_datasets_from_pkg
      metadata <- table_metadata
    }
  }

  n_missing <- length(missing_datasets)

  # Automatically update for just a few datasets
  if (n_missing < 10 && rlang::is_null(update)) {
    update <- TRUE
  }

  max_print <- 15

  if (n_missing > max_print) {
    trunc_text <- "... (truncated)"
    missing_datasets <- missing_datasets[1:max_print]
  } else {
    trunc_text <- NULL
  }

  if (n_missing > 0) {
    if (update) {

      metadata <- update_table_metadata(
        metadata,
        datasets = missing_datasets,
        api_key = api_key,
        quiet = FALSE
      )

      metadata <- dplyr::arrange(
        metadata,
        match(.data[["dataset"]], datasets$name)
      )

    } else {
      rlang::warn(
        c(
          paste0(
            "Tables for the following recently released datasets are not ",
            "yet included in the default summary metadata: "
          ),
          set_names(c(missing_datasets, trunc_text), "*"),
          "",
          "i" = paste0(
            "To add table metadata for these datasets and cache for ",
            "future use, set `update_tables = TRUE`."
          )
        )
      )
    }
  }



  metadata

}

#' Update internal data table summary metadata with tables for recently
#' released datasets
#'
#' @param metadata The existing table metadata. Typically should correspond to
#'   the internal data source `table_metadata`, which stores data table
#'   metadata for currently available datasets.
#' @param datasets The datasets whose tables should be added to the existing
#'   data table metadata
#' @param api_key API Key associated with the user's IPUMS account.
#' @param quiet Logical indicating whether user should be alerted when updating
#' tables. Not currently used
#'
#' @return If no updates are needed, returns `metadata`. Otherwise, returns
#'   a tbl of the same format as `metadata` with additional records.
#'
#' @noRd
update_table_metadata <- function(metadata,
                                  datasets,
                                  api_key = Sys.getenv("IPUMS_API_KEY"),
                                  quiet = FALSE) {

  # Important to avoid API request limit?
  # 100 requests per minute limit
  sleep <- length(datasets) > 50

  if (!quiet) {
    rlang::inform(
      c("Adding metadata for the following recently released datasets:",
        set_names(datasets, "*"),
        "i" = "Updated metadata will be cached for future use."
      )
    )
  }

  new_tables <- purrr::map_dfr(
    datasets,
    ~{
      ds_metadata <- dplyr::mutate(
        get_nhgis_metadata(dataset = .x, api_key = api_key)$data_tables,
        dataset = .x,
        .after = name
      )

      if (sleep) Sys.sleep(1)

      ds_metadata
    }
  )

  metadata <- dplyr::bind_rows(metadata, new_tables)

  metadata <- cache_data(
    data = metadata,
    file_name = "nhgis_summary_metadata_data_tables.rds",
    overwrite = TRUE
  )

  metadata

}

#' Generate a URL to submit to the NHGIS metadata API
#'
#' @param .base_url Base url to which arguments passed to `...` will be
#'   appended.
#' @param ... Arbitrary number of arguments. Arguments will be appended
#'   to the `.base_url` in the format `argument_name/argument_value`. Arguments
#'   without names will be appended with `/` padding
#'
#' @return A character with the URL corresponding to the metadata API request
#'   for the provided values of `...`
#'
#' @noRd
metadata_request_url <- function(.base_url, ...) {

  dots <- purrr::compact(rlang::list2(...))
  fields <- names(dots)

  url_split <- strsplit(.base_url, split = "/")[[1]]
  collection <- url_split[length(url_split)]

  args <- c(.base_url, rbind(fields, unlist(dots)))
  args <- args[which(args != "")]

  url <- paste(args, collapse = "/")

  api_url <- httr::modify_url(
    url = url,
    query = paste0("version=", ipums_api_version(collection))
  )

  api_url

}

#' Submit a request to the IPUMS metadata API and parse response
#'
#' @param request_url URL for the request
#' @param api_key API key
#'
#' @return Either a tibble/data.frame or list object with the metadata for the
#'   provided URL
#'
#' @noRd
request_metadata <- function(request_url,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  tryCatch({
    res <- httr::GET(
      url = request_url,
      httr::user_agent(
        paste0(
          "https://github.com/ipums/ipumsr ",
          as.character(utils::packageVersion("ipumsr"))
        )
      ),
      httr::content_type_json(),
      add_user_auth_header(api_key)
    )

    content <- jsonlite::fromJSON(
      suppressMessages(httr::content(res, "text")),
      simplifyVector = TRUE
    )},
    error = function(cond) {
      rlang::abort(
        paste0(
          "Unable to submit metadata request. Received the following error:",
          "\n\n",
          cond,
          "\nThe metadata value you requested likely produced an invalid ",
          "request URL."
        )
      )
    }
  )

  if (httr::http_error(res)) {
    error_details <- content$detail %||% content$error
    rlang::abort(
      paste0(
        "Received status code ", res$status_code,
        " with the following info: ", error_details
      )
    )
  }

  metadata <- nested_df_to_tbl(content)

  metadata

}

#' Identify list elements that match provided regular expressions
#'
#' For each element in a list, identify whether that list element's values match
#' a set of provided regular expressions.
#'
#' @param l List to filter
#' @param match_vals Vector of regular expressions to match to elements in list
#'   `l`
#' @param match_all If `TRUE`, each element of `l` must match *all* expressions
#'   provided in `match_vals` to return `TRUE`. If `FALSE` each element of `l`
#'   must match *at least one* of the expressions provided in `match_vals` to
#'   return `TRUE`. Defaults to `TRUE`
#' @param match_case Logical indicating whether to perform case-sensitive
#'   (`TRUE`) or case-insensitive matching (`FALSE`). Defaults to `FALSE`.
#'
#' @return A logical vector of the same length as `l` indicating which list
#'   elements match the provided regular expressions.
#'
#' @noRd
filter_list <- function(l, match_vals, match_all = FALSE, match_case = FALSE) {

  if (!match_case) {
    l <- purrr::map(l, tolower)
    match_vals <- tolower(match_vals)
  }

  if (match_all) {
    filt <- function(...) all(...)
  } else {
    filt <- function(...) any(...)
  }

  i <- purrr::map_lgl(
    l,
    function(x) filt(
      purrr::map_lgl(
        match_vals,
        function(y) any(grepl(y, x))
      )
    )
  )

  i

}

#' Filter tibble with column-specific regular expressions
#'
#' Filter the records in a tibble/data.frame to those that match a set of
#' regular expressions, which may differ across columns.
#'
#' @param .data Tibble/data.frame to filter
#' @param ... Character vectors to use when filtering `.data`.
#'   Argument names should correspond to column names found in `.data`. Values
#'   represent regular expressions that will be matched to the values found
#'   in the column of `.data` corresponding to the given argument name.
#' @param match_all If `TRUE`, each row in `.data` must match *all* expressions
#'   provided in `...` to be included in the output. If `FALSE` each row in
#'   `.data` must match *at least one* of the expressions provided in `...` to
#'   be included. Defaults to `TRUE`
#' @param match_case Logical indicating whether to perform case-sensitive
#'   (`TRUE`) or case-insensitive matching (`FALSE`). Defaults to `FALSE`.
#'
#' @return tibble/data.frame. Rows are a subset of the input `.data`, while
#' columns remain unmodified.
#'
#' @noRd
filter_multi_col <- function(.data, ..., match_all = TRUE, match_case = FALSE) {

  dots <- purrr::compact(rlang::list2(...))

  missing_names <- setdiff(names(dots), colnames(.data))
  dots <- dots[names(dots) %in% colnames(.data)]

  if (length(missing_names) > 0) {
    rlang::warn(
      c(
        "Ignoring unrecognized metadata variables:",
        set_names(paste0("`", missing_names, "`"), "*")
      )
    )
  }

  if (length(dots) == 0) {
    return(.data)
  }

  matches <- purrr::map(
    names(dots),
    ~filter_list(
      .data[[.x]],
      match_vals = dots[[.x]],
      match_all = match_all,
      match_case = match_case
    )
  )

  if (match_all) {
    matches <- purrr::reduce(matches, `&`)
  } else {
    matches <- purrr::reduce(matches, `|`)
  }

  .data[matches, ]

}

nhgis_api_metadata_url <- function() "https://api.ipums.org/metadata/nhgis"

#' Cache metadata for cross-session use
#'
#' Used when handling data table summary metadata, which is not kept up to date
#' automatically by the metadata API. This caches updated table metadata
#' so users do not have to request table metadata for new datasets each time
#' they call `get_nhgis_metadata("data_tables")`.
#'
#' Caches in the directory given by `rappdirs::user_cache_dir("ipumsr")`.
#' Cached data persists across R sessions.
#'
#' @param data Data object to be cached as .rds file
#' @param file_name File name for the cached data
#' @param cache_dir Directory in which to cache data
#' @param overwrite Whether to overwrite an existing .rds file in the cache
#'   of the same name.
#'
#' @return Returns `data` invisibly
#'
#' @noRd
cache_data <- function(data,
                       file_name,
                       cache_dir = ipumsr_cache_dir(),
                       overwrite = FALSE) {

  if (!grepl(".rds$", file_name)) {
    file_name <- paste0(file_name, ".rds")
  }

  fp <- file.path(cache_dir, file_name)

  dir_exists <- file.exists(cache_dir)
  fp_exists <- file.exists(fp)

  if (!dir_exists) {
    dir.create(cache_dir, recursive = TRUE)
  }

  if (fp_exists && !overwrite) {
    # TODO: remove + force overwrite?
    rlang::warn("Unable to cache data: file exists and `overwrite = FALSE`.")
    return(invisible(data))
  }

  readr::write_rds(data, fp)

  invisible(data)

}


#' Load data from cache
#'
#' @param cache_dir Directory where data file is cached
#' @param pattern Character of regex pattern to match to the files in
#'   `cache_dir` to determine which to open. Must uniquely identify a file in
#'   `cache_dir`
#' @param ... Additional arguments passed to `list.files`
#'
#' @return The data stored in the .rds file identified by `pattern`
#'
#' @noRd
load_cached_data <- function(cache_dir = ipumsr_cache_dir(),
                             pattern = NULL,
                             ...) {
  readr::read_rds(
    list.files(cache_dir, pattern = pattern, full.names = TRUE, ...)
  )
}

#' Get information on cache directory for ipumsr files
#'
#' Allows for easier access to cache directory and contents when saving
#' files to user system. Currently used for caching updated data table
#' metadata for NHGIS.
#'
#' @param list If `TRUE`, list files within the cache directory.
#'   If `FALSE`, return the path to the cache directory itself. Ignored if
#'   `clear = TRUE`, which will always return the directory path.
#' @param clear If `TRUE`, recursively remove contents of the cache directory.
#' @param ... Additional arguments passed to `list.files`
#'
#' @return If `list = FALSE`, the cache directory path.
#'   If `list = TRUE`, a vector of file paths to all files in the directory.
#'
#' @noRd
ipumsr_cache_dir <- function(list = FALSE, clear = FALSE, ...) {

  cache_dir <- rappdirs::user_cache_dir("ipumsr")

  if (clear) {
    unlink(list.files(cache_dir, full.names = TRUE, ...), recursive = TRUE)
  } else if (list) {
    return(list.files(cache_dir, ...))
  }

  cache_dir

}

#' Convert all data.frames in a nested list/data.frame into tibbles
#'
#' Single-dataset metadata breakdowns field returns a
#' data.frame that includes a column of data.frames. We want to convert
#' all levels of such a hierarchy to tibbles for consistent formatting.
#' @noRd
nested_df_to_tbl <- function(l) {

  if (is.data.frame(l) || tibble::is_tibble(l)) {
    l <- dplyr::mutate(
      tibble::as_tibble(l),
      dplyr::across(
        dplyr::everything(),
        ~if (rlang::is_list(.x)) {
          purrr::map(.x, nested_df_to_tbl)
        } else {
          .x
        }
      )
    )
  } else if (rlang::is_list(l)) {
    l <- purrr::map(l, nested_df_to_tbl)
  }

  l

}

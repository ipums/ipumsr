
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
#' type, use the `data_type` argument. To retrieve detailed metadata for a
#' single data source, use the `dataset`, `ds_table`, or `time_series_table`
#' arguments.
#'
#' @details
#' ## General use
#' If `data_type` is not specified, then either `dataset`, `time_series_table`,
#' or both `dataset` and `ds_table`, must be provided. Metadata is only
#' provided for a single `data_type` or data source at a time.
#'
#' @details
#' ## Filtering
#' The names of the expressions provided in `...` should be found in the
#' column names of the summary metadata for each data type. The
#' following summarizes the columns available for each data type:
#'
#' * Datasets: `name`, `group`, `description`, `sequence`
#' * Data tables: `dataset`, `name`, `nhgis_code`, `description`, `sequence`
#' * Time series tables: `name`, `description`, `geographic_integration`,
#'   `sequence`, `years`, `geog_levels`
#' * Shapefiles: `name`, `year`, `geographic_level`, `extent`, `basis`,
#'   `sequence`
#'
#' The expressions in `...` are interpreted as regular expressions. This is
#' typically the desired behavior when searching for keywords in data source
#' descriptions, but a precise match may be useful for unique fields,
#' like dataset or data table names. Any terms can be wrapped in `"^"` and `"$"`
#' to enforce an exact match. For instance,
#' `get_nhgis_metadata("data_tables", name = "^NT1$")` will match tables whose
#' name is `"NT1"` exactly, rather than all tables that have `"NT1"` somewhere
#' in their name.
#'
#' @details
#' ## Data table metadata
#' The IPUMS NHGIS API does not currently provide updated summary metadata
#' for data tables by default. Therefore, the metadata provided for data tables
#' may be incomplete if new datasets have recently been added.
#'
#' `get_nhgis_metadata("data_tables")` will automatically produce
#' a warning if it detects any datasets whose tables are not included in the
#' data table summary metadata. The metadata for the data tables
#' associated with these datasets can be included by setting
#' `update_tables = TRUE`. Note that updating these metadata will result in an
#' API request for each missing dataset.
#'
#' The updated data will automatically
#' be cached across R sessions in the directory given by
#' `rappdirs::user_cache_dir("ipumsr")`. If you get unexpected results from
#' `get_nhgis_metadata("data_tables")`, consider clearing the cache directory.
#'
#' @inheritParams submit_extract
#' @param data_type One of "datasets", "data_tables",
#'   "time_series_tables", or "shapefiles" indicating the type of summary
#'   metadata to retrieve.
#' @param dataset Name of the dataset for which to retrieve metadata.
#' @param ds_table Name of the data table for which to retrieve metadata.
#'   If provided, an associated `dataset` must also be specified.
#' @param time_series_table Name of the time series table for which to retrieve
#'   metadata.
#' @param match_all If `TRUE`, only metadata records that match *all* of the
#'   expressions provided in `...` will be included in the output. If `FALSE`,
#'   metadata records that match *any* of the expressions will be included.
#'   Defaults to `TRUE`.
#' @param match_case If `TRUE` use case-sensitive matching when interpreting
#'   the expressions provided in `...`. Defaults to `FALSE`.
#' @param update_tables If `TRUE` and `data_type = "data_tables"`, update the
#'   provided data table metadata to include any recently released
#'   datasets that are not yet included in the default summary metadata and
#'   cache the updated data for future use. If the current data table summary
#'   metadata are already up to date, this does nothing. See details.
#' @param ... Optional set of character vectors used to filter the requested
#'   summary metadata. These values are interpreted as
#'   regular expressions that are matched to the values in the metadata column
#'   whose name matches the given argument name. Only metadata records
#'   whose values match the provided expressions will be included in the output.
#'   Only used when `data_type` is provided. See examples.
#'
#' @return If `data_type` is provided, a [`tibble`][tibble::tbl_df-class] of
#'   summary metadata for all data sources of the provided `data_type`.
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
#' get_nhgis_metadata(ds_table = "NP1", dataset = "1990_STF1")
get_nhgis_metadata <- function(data_type = NULL,
                               dataset = NULL,
                               ds_table = NULL,
                               time_series_table = NULL,
                               match_all = TRUE,
                               match_case = FALSE,
                               update_tables = FALSE,
                               api_key = Sys.getenv("IPUMS_API_KEY"),
                               ...) {

  summary_req <- !is.null(data_type)
  ds_req <- !is.null(dataset) && (data_type %||% "") != "data_tables"
  dt_req <- !is.null(ds_table)
  tst_req <- !is.null(time_series_table)

  if (sum(summary_req, ds_req, tst_req) > 1) {
    stop(
      "Only one of `data_type`, `dataset`, or `time_series_table` may be ",
      "specified at a time.",
      call. = FALSE
    )
  }

  if (dt_req && !ds_req) {
    stop(
      "`ds_table` must be specified with a corresponding `dataset`.",
      call. = FALSE
    )
  }

  if (!any(summary_req, ds_req, tst_req)) {
    stop(
      "One of `data_type`, `dataset`, or `time_series_table` must be ",
      "specified.",
      call. = FALSE
    )
  }

  is_too_long <- purrr::map_lgl(
    list(data_type, dataset, ds_table, time_series_table),
    ~length(.x) > 1
  )

  if (any(is_too_long)) {
    stop(
      "Can only retrieve metadata for one `",
      paste0(
        c("data_type", "dataset", "ds_table", "time_series_table")[is_too_long],
        collapse = "`, `"
      ),
      "` at a time.",
      call. = FALSE
    )
  }

  if (summary_req) {

    metadata <- get_nhgis_summary_metadata(
      data_type = data_type,
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
      data_tables = ds_table,
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
#' @return Tibble of summary metadata for the requested `data_type`
#'
#' @noRd
get_nhgis_summary_metadata <- function(data_type,
                                       update_tables = FALSE,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {

  if (!data_type %in% c("datasets", "data_tables",
                        "time_series_tables", "shapefiles")) {
    stop(
      "`data_type` must be one of \"datasets\", ",
      "\"data_tables\", \"time_series_tables\", or \"shapefiles\"",
      call. = FALSE
    )
  }

  if (data_type == "data_tables") {

    metadata <- tryCatch(
      load_cached_data(pattern = data_type),
      error = function(cnd) table_metadata
    )

    metadata <- check_table_metadata(
      metadata,
      update = update_tables,
      api_key = api_key
    )

  } else {

    metadata <- request_metadata(
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        data_type
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
#' @param api_key API key associated with the user's account.
#'
#' @return If `update = FALSE` or if no updates are needed, returns `metadata`.
#'   Otherwise, returns updated data table summary metadata produced by
#'   `update_table_metadata()`
#'
#' @noRd
check_table_metadata <- function(metadata,
                                 update = FALSE,
                                 api_key = Sys.getenv("IPUMS_API_KEY")) {

  datasets <- get_nhgis_summary_metadata(
    data_type = "datasets",
    api_key = api_key
  )

  missing_datasets <- setdiff(datasets$name, metadata$dataset)

  n_missing <- length(missing_datasets)

  # Automatically update for just a few datasets?
  # if (n_missing < 5) {
  #   update <- TRUE
  #   quiet <- TRUE
  # } else {
  #   quiet <- FALSE
  # }

  max_print <- 15

  if (n_missing > max_print) {
    trunc_text <- " [... output truncated]"
    missing_datasets <- missing_datasets[1:max_print]
  } else {
    trunc_text <- ""
  }

  if (n_missing > 0) {
    if (update) {
      metadata <- update_table_metadata(
        metadata,
        datasets = missing_datasets,
        api_key = api_key
        # quiet = quiet
      )
    } else {
      warning(
        "The IPUMS NHGIS API does not yet provide data table metadata for ",
        "the following recently released datasets: \n\n\"",
        paste0(missing_datasets, collapse = "\"\n\""),
        "\"", trunc_text,
        "\n\nTo add table metadata for these datasets and cache for ",
        "future use, set `update_tables = TRUE`.",
        call. = FALSE
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

  if (!quiet) {
    message(
      "Adding metadata for the following datasets: ",
      "\n\n\"", paste0(datasets, collapse = "\"\n\""), "\""
    )
  }

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
      stop(
        "Unable to submit metadata request. Received the following error:\n\n",
        cond,
        "\nThe metadata value you requested likely produced an invalid ",
        "request URL.",
        call. = FALSE
      )
    }
  )

  if (httr::http_error(res)) {
    error_details <- content$detail %||% content$error
    stop(
      "Received status code ", res$status_code,
      " with the following info: ", error_details
      , call. = FALSE
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
    warning(
      "Ignoring unrecognized metadata variables: `",
      paste0(missing_names, collapse = "`, `"), "`",
      call. = FALSE
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
    # Remove + force overwrite?
    warning(
      "Unable to cache data: file exists and `overwrite = FALSE`.",
      call. = FALSE
    )
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

ipumsr_cache_dir <- function() rappdirs::user_cache_dir("ipumsr")

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

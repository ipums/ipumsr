
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
#' Metadata is only provided for a single `data_type` or data source at a  time.
#'
#' If `data_type` is not specified, then either `dataset`, `dataset` and
#' `ds_table`, or `time_series_table` must be provided.
#'
#' The expressions provided in `...` must match the column names present in the
#' summary metadata for each data type. The following summarizes the columns
#' available for each data type:
#'
#' * Datasets: `name`, `group`, `description`, `sequence`
#' * Data tables: `dataset`, `name`, `nhgis_code`, `description`, `sequence`
#' * Time series tables: `name`, `description`, `geographic_integration`,
#'   `sequence`, `years`, `geog_levels`
#' * Shapefiles: `name`, `year`, `geographic_level`, `extent`, `basis`,
#'   `sequence`
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
#' @param ... Optional set of character vectors used to filter the requested
#'   summary metadata. These values are interpreted as
#'   regular expressions that are matched to the values in the metadata column
#'   whose name matches that provided in the argument. Only metadata records
#'   whose values match the provided expressions will be included in the output.
#'   Only used when `data_type` is provided. See examples.
#' @param match_all If `TRUE`, only metadata records that match *all* of the
#'   expressions provided in `...` will be included in the output. If `FALSE`,
#'   metadata records that match *any* of the expressions will be included.
#'   Defaults to `TRUE`.
#' @param match_case If `TRUE`, use case-sensitive matching when interpreting
#'   the expressions provided in `...`. Defaults to `FALSE`.
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
#'
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
get_nhgis_metadata <- function(data_type = NULL,
                               dataset = NULL,
                               ds_table = NULL,
                               time_series_table = NULL,
                               ...,
                               match_all = TRUE,
                               match_case = FALSE,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

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

    meta <- get_nhgis_summary_metadata(
      data_type = data_type,
      api_key = api_key
    )

    meta <- filter_multi_col(
      meta,
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

    meta <- metadata_request(api_url, api_key)

  }

  meta

}

# Internal functions -----------------------------------------------------------

#' Get NHGIS summary metadata
#'
#' Helper to retrieve summary metadata for all NHGIS datasets, data tables,
#' time series tables, or shapefiles
#'
#' @param data_type One of "datasets", "data_tables", "time_series_tables", or
#'   "shapefiles" indicating the type of summary metadata to retrieve.
#' @param api_key User's API key
#'
#' @return Tibble of summary metadata for the requested `data_type`
#'
#' @noRd
get_nhgis_summary_metadata <- function(data_type,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {

  data_types <- c("datasets", "data_tables", "time_series_tables", "shapefiles")

  if (!data_type %in% data_types) {
    stop(
      "`data_type` must be one of \"datasets\", \"data_tables\", ",
      "\"time_series_tables\", or \"shapefiles\"",
      call. = FALSE
    )
  }

  # data_tables not provided directly by API, so return pre-loaded metadata
  if (data_type == "data_tables") {

    rlang::warn(
      paste0(
        "Table metadata may not include recently released datasets.\n",
        "To get table metadata for these datasets, provide the dataset ",
        "name to the `dataset` argument of `get_nhgis_metadata()`."
      ),
      .frequency = "once",
      .frequency_id = "dt_meta_warn"
    )

    return(table_metadata)

  }

  api_url <- switch(
    data_type,
    "datasets" = {
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        datasets = ""
      )
    },
    "time_series_tables" = {
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        time_series_tables = ""
      )
    },
    "shapefiles" = {
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        shapefiles = ""
      )
    }
  )

  meta <- metadata_request(api_url, api_key)

  meta

}

#' Generate a URL to submit to the NHGIS metadata API
#'
#' @param .base_url Base url to which arguments passed to `...` will be
#'   appended.
#' @param ... Arbitrary number of named arguments. Arguments will be appended
#'   to the `.base_url` in the format `argument_name/argument_value`
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

  url <- paste(c(.base_url, rbind(fields, unlist(dots))), collapse = "/")
  url <- gsub("/$", "", url)

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
metadata_request <- function(request_url,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  tryCatch(
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
    ),
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

  metadata <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = TRUE
  )

  if (httr::http_error(res)) {
    error_details <- parse_400_error(res)
    stop(error_details, call. = FALSE)
  }

  metadata_type <- class(metadata)

  if (metadata_type == "list") {
    metadata <- purrr::map(
      metadata,
      ~if (is.data.frame(.x)) {
        tibble::as_tibble(.x)
      } else {
        .x
      }
    )
  } else if (metadata_type == "data.frame") {
    metadata <- tibble::as_tibble(metadata)
  }

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

  dots <- dots[names(dots) %in% colnames(.data)]

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

nhgis_api_metadata_url <- function() {
  "https://api.ipums.org/metadata/nhgis"
}

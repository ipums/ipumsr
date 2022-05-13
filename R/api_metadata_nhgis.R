
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' View NHGIS metadata
#'
#' @description
#' NHGIS metadata is provided for individual datasets, summary tables, and
#' time series tables as well as in summary form for all available datasets,
#' time series tables, and shapefiles.
#'
#' Use the \code{type} argument to obtain summary metadata for all datasets,
#' time series tables, or shapefiles.
#'
#' Use the \code{dataset}, \code{ds_table}, or \code{time_series_table}
#' arguments to get detailed metadata for a specific dataset, summary table,
#' or time series table, respectively. Only one of \code{type}, \code{dataset},
#' or \code{time_series_table} may be provided at a time.
#'
#' For more information on the NHGIS metadata API, click
#' \href{https://developer.ipums.org/docs/workflows/explore_metadata/nhgis/}{here}
#'
#' @inheritParams submit_extract
#' @param type If provided, one of \code{"datasets"},
#'   \code{"time_series_tables"}, or \code{"shapefiles"} indicating the desired
#'   summary metadata.
#' @param dataset Character indicating the name of the dataset for
#'   which to obtain metadata. Required if \code{ds_table} is specified.
#' @param ds_table Character indicating the name of the summary
#'   table for which to obtain metadata. The summary table must be present in
#'   the provided \code{dataset}.
#' @param time_series_table Character indicating the name of the
#'   time series table for which to obtain metadata.
#'
#' @return If obtaining summary metadata, a \code{tibble} containing metadata
#'   for the specified data type. If obtaining metadata for a single dataset,
#'   summary table, or time series table, a named list containing metadata for
#'   that data source.
#' @export
#'
#' @family ipums_api_nhgis
#'
#' @examples
#' # Metadata for all datasets, time series tables, or shapefiles
#' get_nhgis_metadata("datasets")
#' get_nhgis_metadata("time_series_tables")
#' get_nhgis_metadata("shapefiles")
#'
#' # Metadata for a particular data source
#' get_nhgis_metadata(dataset = "1980_STF1")
#' get_nhgis_metadata(dataset = "1980_STF1", ds_table = "NT1A")
#' get_nhgis_metadata(time_series_table = "CW3")
get_nhgis_metadata <- function(type = NULL,
                               dataset = NULL,
                               ds_table = NULL,
                               time_series_table = NULL,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

  if (sum(!is.null(type), !is.null(dataset), !is.null(time_series_table)) > 1) {
    stop(
      "Only one of `type`, `dataset`, or `time_series_table` may be specified ",
      "at a time.",
      call. = FALSE
    )
  }

  is_too_long <- purrr::map_lgl(
    list(type, dataset, ds_table, time_series_table),
    ~length(.x) > 1
  )

  if (any(is_too_long)) {
    stop(
      "Can only retrieve metadata for a single value of `",
      paste0(
        c("type", "dataset", "ds_table", "time_series_table")[is_too_long],
        collapse = "`, `"
      ),
      "` at a time.",
      call. = FALSE
    )
  }

  if (all(is.null(type),
          is.null(dataset),
          is.null(time_series_table))) {
    if (!is.null(ds_table)) {
      stop(
        "If a `ds_table` is specified, a `dataset` must also be specified.",
        call. = FALSE
      )
    } else {
      stop(
        "At least one of `type`, `dataset`, or `time_series_table`",
        " must be specified.",
        call. = FALSE
      )
    }
  }

  shapefile <- NULL

  if (!is.null(type)) {

    if (!type %in% c("datasets", "time_series_tables", "shapefiles")) {
      stop(
        "`type` must be one of \"datasets\", \"time_series_tables\", or ",
        "\"shapefiles\"",
        call. = FALSE
      )
    }

    if (type == "datasets") {
      dataset <- ""
    } else if (type == "time_series_tables") {
      time_series_table <- ""
    } else if (type == "shapefiles") {
      shapefile <- ""
    }

  }

  metadata_url <- metadata_request_url(
    base_url = nhgis_api_metadata_url(),
    datasets = dataset,
    data_tables = ds_table,
    time_series_tables = time_series_table,
    shapefiles = shapefile
  )

  metadata <- metadata_request(metadata_url, api_key)

  metadata

}

# Internal functions -----------------------------------------------------------

#' Generate a URL to submit to the NHGIS metadata API
#'
#' @param base_url Base url to which arguments passed to \code{...} will be
#'   appended.
#' @param ... Arbitrary number of named arguments. Arguments will be appended
#'   to the \code{base_url} in the format \{argument_name\}/\{argument_value\}
#'
#' @return A character with the URL corresponding to the metadata API request
#'   for the provided values of \code{...}
#'
#' @noRd
metadata_request_url <- function(base_url, ...) {

  dots <- rlang::list2(...)

  dots <- purrr::compact(dots)
  fields <- names(dots)

  sep <- ifelse(dots[[1]] == "", "", "/")

  url <- paste(
    base_url,
    paste(fields, dots[fields], sep = sep, collapse = "/"),
    sep = "/"
  )

  api_url <- httr::modify_url(
    url = url,
    query = paste0("version=", ipums_api_version("nhgis"))
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

  res <- httr::GET(
    url = request_url,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  metadata <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = TRUE
  )

  if (httr::http_error(res)) {
    stop(
      "Extract submission failed for ", request_url, ".\n",
      "Status: ", metadata$status$code, "\n",
      "Details: ", metadata$detail,
      call. = FALSE
    )
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

nhgis_api_metadata_url <- function() {
  "https://api.ipums.org/metadata/nhgis"
}

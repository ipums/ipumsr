
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
#' @param type If provided, one of \code{"datasets"}, \code{"data_tables"},
#'   \code{"time_series_tables"}, or \code{"shapefiles"} indicating the desired
#'   summary metadata.
#' @param dataset Character indicating the name of the dataset for
#'   which to obtain metadata. Required if \code{ds_table} is specified.
#' @param ds_table Character indicating the name of the summary
#'   table for which to obtain metadata. The summary table must be present in
#'   the provided \code{dataset}.
#' @param time_series_table Character indicating the name of the
#'   time series table for which to obtain metadata.
#' @param keywords Optional vector of strings used to subset requested summary
#'   metadata. Only records for those entities whose description field contains
#'   the provided keywords will be included in the output. Keywords are
#'   interpreted as regular expressions and are case-insensitive.
#' @param match_all If TRUE, metadata records whose description contains
#'   all of the provided `keywords` will be included in the output. If FALSE,
#'   any metadata records whose description contains at least one of the
#'   provided `keywords` will be included in the output.
#'
#' @return If obtaining summary metadata, a \code{\link[tibble]{tibble}}
#'   containing metadata for the specified data type. If obtaining metadata
#'   for a single dataset, summary table, or time series table, a named list
#'   containing metadata for that data source.
#'
#' @export
#'
#' @family ipums_api_nhgis
#'
#' @examples
#' get_nhgis_metadata("datasets")
#' get_nhgis_metadata("time_series_tables")
#' get_nhgis_metadata("shapefiles")
#'
#' get_nhgis_metadata("data_tables", keywords = c("Sex", "Age", "Race"))
#'
#' # Metadata for a particular data source
#' get_nhgis_metadata(dataset = "1980_STF1")
#' get_nhgis_metadata(dataset = "1980_STF1", ds_table = "NT1A")
#' get_nhgis_metadata(time_series_table = "CW3")
get_nhgis_metadata <- function(type = NULL,
                               dataset = NULL,
                               ds_table = NULL,
                               time_series_table = NULL,
                               keywords = NULL,
                               match_all = TRUE,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

  summary_req <- !is.null(type)
  ds_req <- !is.null(dataset)
  dt_req <- !is.null(ds_table)
  tst_req <- !is.null(time_series_table)

  shapefile <- NULL

  if (sum(summary_req, ds_req, tst_req) > 1) {
    stop(
      "Only one of `type`, `dataset`, or `time_series_table` may be specified ",
      "at a time.",
      call. = FALSE
    )
  }

  if (!ds_req && dt_req) {
    stop(
      "`ds_table` must be specified with a corresponding `dataset`.",
      call. = FALSE
    )
  }

  if (all(!summary_req, !ds_req, !tst_req)) {
    stop(
      "At least one of `type`, `dataset`, or `time_series_table` must be ",
      "specified.",
      call. = FALSE
    )
  }

  # Can reconsider this restriction if we decide to repackage
  # metadata formatting. Not hard to make 2 requests.
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

  if (summary_req) {

    # Should "data_tables" be renamed to "ds_tables"?
    if (!type %in% c("datasets", "data_tables", "time_series_tables", "shapefiles")) {
      stop(
        "`type` must be one of \"datasets\", \"data_tables\", \"time_series_tables\", or ",
        "\"shapefiles\". \nTo get metadata for a single dataset, summary table, ",
        "or time series table, use the corresponding argument.",
        call. = FALSE
      )
    }

    if (type == "datasets") {
      metadata <- metadata_request(
        metadata_request_url(
          .base_url = nhgis_api_metadata_url(),
          datasets = ""
        ),
        api_key
      )
    } else if (type == "time_series_tables") {
      metadata <- metadata_request(
        metadata_request_url(
          .base_url = nhgis_api_metadata_url(),
          time_series_tables = ""
        ),
        api_key
      )
    } else if (type == "shapefiles") {
      metadata <- metadata_request(
        metadata_request_url(
          .base_url = nhgis_api_metadata_url(),
          shapefiles = ""
        ),
        api_key
      )
    } else if (type == "data_tables") {
      rlang::warn(
        paste0(
          "Table metadata may not include recently released datasets. ",
          "To see table metadata for these datasets, provide the dataset ",
          "name to `get_nhgis_metadata()`"),
        .frequency = "once",
        .frequency_id = "dt_meta"
      )
      metadata <- table_metadata
    }

  } else {

    metadata <- metadata_request(
      metadata_request_url(
        .base_url = nhgis_api_metadata_url(),
        datasets = dataset,
        data_tables = ds_table,
        time_series_tables = time_series_table,
        shapefiles = shapefile
      ),
      api_key
    )

  }

  if (!is.null(keywords)) {
    if(!summary_req || type == "shapefiles") {
      warning(
        "`keywords` only implemented when `type` is one of \"datasets\", ",
        "\"data_tables\", or \"time_series_tables\"",
        call. = FALSE
      )
    } else {
      metadata <- metadata[
        str_detect_multi(
          metadata$description,
          patterns = keywords,
          match_all = match_all
        ),
      ]
    }
  }

  metadata

}

# Internal functions -----------------------------------------------------------

#' Generate a URL to submit to the NHGIS metadata API
#'
#' @param .base_url Base url to which arguments passed to \code{...} will be
#'   appended.
#' @param collection Collection for which to request metadata. Used to include
#'   correct API version in the output request URL
#' @param ... Arbitrary number of named arguments. Arguments will be appended
#'   to the \code{.base_url} in the format \{argument_name\}/\{argument_value\}
#'
#' @return A character with the URL corresponding to the metadata API request
#'   for the provided values of \code{...}
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

nhgis_api_metadata_url <- function() {
  "https://api.ipums.org/metadata/nhgis"
}

# Really is more of a utility
str_detect_multi <- function(x, patterns, match_all = TRUE, match_case = FALSE) {

  if (!match_case) {
    x <- tolower(x)
    patterns <- tolower(patterns)
  }

  matches <- purrr::map(patterns, ~grepl(.x, x, perl = TRUE))

  if (match_all) {
    matches <- purrr::reduce(matches, `&`)
  } else {
    matches <- purrr::reduce(matches, `|`)
  }

  matches

}

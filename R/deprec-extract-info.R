# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Get information on recent extracts
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Functionality for accessing recent extract information has been moved to
#' [`get_extract_history()`].
#'
#' Please use that function instead.
#'
#' @keywords internal
#'
#' @export
get_recent_extracts_info_list <- function(collection = NULL,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "get_recent_extracts_info_list()",
    "get_extract_history()"
  )

  get_extract_history(
    collection = collection,
    how_many = how_many,
    api_key = api_key
  )
}

#' @rdname get_recent_extracts_info_list
#' @export
get_recent_extracts_info_tbl <- function(collection = NULL,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "get_recent_extracts_info_tbl()",
    details = "Please use `get_extract_history()` with `table = TRUE` instead."
  )

  get_extract_history(
    collection = collection,
    how_many = how_many,
    table = TRUE,
    api_key = api_key
  )
}

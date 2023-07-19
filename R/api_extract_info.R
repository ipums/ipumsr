# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Retrieve the definition and latest status of an extract request
#'
#' @description
#' Retrieve the latest status of an extract request.
#'
#' `get_last_extract_info()` is a convenience function to retrieve the most
#' recent extract for a given collection.
#'
#' To browse definitions of your previously submitted extract requests, see
#' [get_extract_history()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract One of:
#'   * An [`ipums_extract`][ipums_extract-class] object
#'   * The data collection and extract number formatted as a string of the
#'     form `"collection:number"` or as a vector of the form
#'     `c("collection", number)`
#'   * An extract number to be associated with your default IPUMS
#'     collection. See [set_ipums_default_collection()]
#'
#'   For a list of codes used to refer to each collection, see
#'   [ipums_data_collections()].
#' @inheritParams submit_extract
#'
#' @return
#' An [`ipums_extract`][ipums_extract-class] object.
#'
#' @seealso
#' [get_extract_history()] to browse past extract definitions
#'
#' [wait_for_extract()] to wait for an extract to finish processing.
#'
#' [download_extract()] to download an extract's data files.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Get latest info for the request associated with a given `ipums_extract`
#' # object:
#' updated_extract <- get_extract_info(submitted_extract)
#'
#' updated_extract$status
#'
#' # Or specify the extract collection and number:
#' get_extract_info("usa:1")
#' get_extract_info(c("usa", 1))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("nhgis")
#' get_extract_info(1)
#'
#' # To get the most recent extract (for instance, if you have forgotten its
#' # extract number), use `get_last_extract_info()`
#' get_last_extract_info("nhgis")
#' }
get_extract_info <- function(extract,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)

  if (is_empty(extract$number) || is.na(extract$number)) {
    rlang::abort(
      c(
        paste0(
          "Cannot get info for an `ipums_extract` object with missing ",
          "extract number."
        ),
        "i" = "Use `submit_extract()` to submit this extract request."
      )
    )
  }

  url <- api_request_url(
    collection = extract$collection,
    path = extract_request_path(extract$number)
  )

  response <- ipums_api_extracts_request(
    "GET",
    collection = extract$collection,
    url = url,
    api_key = api_key
  )

  extract_list_from_json(response)[[1]]
}

#' Browse definitions of previously submitted extract requests
#'
#' @description
#' Retrieve definitions of an arbitrary number of previously submitted extract
#' requests for a given IPUMS collection, starting from the most recent
#' extract request.
#'
#' To check the status of a particular extract request, use
#' [get_extract_info()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @inheritParams submit_extract
#' @param collection Character string of the IPUMS collection for which to
#'   retrieve extract history. Defaults to the current default
#'   collection, if it exists. See [set_ipums_default_collection()].
#'
#'   For a list of codes used to refer to each collection, see
#'   [ipums_data_collections()].
#' @param how_many The number of extract requests for which to retrieve
#'   information. Defaults to the 10 most recent extracts.
#' @param delay Number of seconds to delay between
#'   successive API requests, if multiple requests are needed to retrieve all
#'   records.
#'
#'   A delay is highly unlikely to be necessary and is
#'   intended only as a fallback in the event that you cannot retrieve your
#'   extract history without exceeding the API rate limit.
#'
#' @return
#' A list of [`ipums_extract`][ipums_extract-class] objects
#'
#' @seealso
#' [get_extract_info()] to get the current status of a specific extract request.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get information for most recent extract requests.
#' # By default gets the most recent 10 extracts
#' get_extract_history("usa")
#'
#' # Return only the most recent 3 extract definitions
#' get_extract_history("cps", how_many = 3)
#'
#' # To get the most recent extract (for instance, if you have forgotten its
#' # extract number), use `get_last_extract_info()`
#' get_last_extract_info("nhgis")
#' }
#'
#' # To browse your extract history by particular criteria, you can
#' # loop through the extract objects. We'll create a sample list of 2 extracts:
#' extract1 <- define_extract_usa(
#'   description = "2013 ACS",
#'   samples = "us2013a",
#'   variables = var_spec(
#'     "SEX",
#'     case_selections = "2",
#'     data_quality_flags = TRUE
#'   )
#' )
#'
#' extract2 <- define_extract_usa(
#'   description = "2014 ACS",
#'   samples = "us2014a",
#'   variables = list(
#'     var_spec("RACE"),
#'     var_spec(
#'       "SEX",
#'       case_selections = "1",
#'       data_quality_flags = FALSE
#'     )
#'   )
#' )
#'
#' extracts <- list(extract1, extract2)
#'
#' # `purrr::keep()`` is particularly useful for filtering:
#' purrr::keep(extracts, ~ "RACE" %in% names(.x$variables))
#'
#' purrr::keep(extracts, ~ grepl("2014 ACS", .x$description))
#'
#' # You can also filter on variable-specific criteria
#' purrr::keep(extracts, ~ isTRUE(.x$variables[["SEX"]]$data_quality_flags))
#'
#' # To filter based on all variables in an extract, you'll need to
#' # create a nested loop. For instance, to find all extracts that have
#' # any variables with data_quality_flags:
#' purrr::keep(
#'   extracts,
#'   function(extract) {
#'     any(purrr::map_lgl(
#'       names(extract$variables),
#'       function(var) isTRUE(extract$variables[[var]]$data_quality_flags)
#'     ))
#'   }
#' )
#'
#' # To peruse your extract history without filtering, `purrr::map()` is more
#' # useful
#' purrr::map(extracts, ~ names(.x$variables))
#'
#' purrr::map(extracts, ~ names(.x$samples))
#'
#' purrr::map(extracts, ~ .x$variables[["RACE"]]$case_selections)
#'
#' # Once you have identified a past extract, you can easily download or
#' # resubmit it
#' \dontrun{
#' extracts <- get_extract_history("nhgis")
#'
#' extract <- purrr::keep(
#'   extracts,
#'   ~ "CW3" %in% names(.x$time_series_tables)
#' )
#'
#' download_extract(extract[[1]])
#' }
get_extract_history <- function(collection = NULL,
                                how_many = 10,
                                delay = 0,
                                api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (how_many <= 0) {
    rlang::abort("Must request a positive number of records.")
  }

  collection <- collection %||% get_default_collection()
  page_limit <- api_page_size_limit("extracts")

  url <- api_request_url(
    collection = collection,
    path = extract_request_path(),
    queries = list(pageNumber = 1, pageSize = min(how_many, page_limit))
  )

  # Determine number of pages required to get the requested
  # records. Needed to avoid getting full extract history for requests
  # that require multiple pages but have finite `how_many`
  max_pages <- ceiling(how_many / page_limit)

  responses <- ipums_api_paged_request(
    url,
    max_pages = max_pages,
    delay = delay,
    api_key = api_key
  )

  extracts <- purrr::map(
    responses,
    function(res) {
      extract_list_from_json(
        new_ipums_json(
          httr::content(res, "text"),
          collection = collection
        )
      )
    }
  )

  # Extracts will be in a list chunked by page, but we want a single-depth list
  extracts <- purrr::flatten(extracts)

  # Multi-page requests will always get a number of records that is a multiple
  # of the page size. If a non-multiple `how_many` was requested, we need to
  # truncate the output.
  if (length(extracts) > how_many) {
    extracts <- extracts[seq_len(how_many)]
  }

  if (length(extracts) == 0) {
    rlang::abort("No past extracts were found for this collection.")
  }

  extracts
}

#' @inheritParams get_extract_history
#'
#' @rdname get_extract_info
#' @export
get_last_extract_info <- function(collection = NULL,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- get_extract_history(
    collection = collection %||% get_default_collection(),
    how_many = 1,
    api_key = api_key
  )

  extract[[1]]
}

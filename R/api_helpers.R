
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' List IPUMS data collections
#'
#' List IPUMS data collections with corresponding codes used by the IPUMS
#' extract API. Note that some data collections do not yet have API support.
#'
#' Currently, \code{ipumsr} supports extract definitions for the following
#' collections:
#'
#' \itemize{
#'   \item{IPUMS NHGIS}
#'   \item{IPUMS USA}
#' }
#'
#' For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @family ipums_api
#' @return A \code{\link[tibble]{tbl_df}} with two columns containing the
#'   full collection name and the corresponding code used by the IPUMS API.
#'
#' @examples
#' # Print a tibble of all IPUMS data collections:
#' ipums_data_collections()
#'
#' @export
ipums_data_collections <- function() {
  tibble::tribble(
    ~collection_name, ~code_for_api,
    "IPUMS USA", "usa",
    "IPUMS CPS", "cps",
    "IPUMS International", "ipumsi",
    "IPUMS NHGIS", "nhgis",
    "IPUMS AHTUS", "ahtus",
    "IPUMS MTUS", "mtus",
    "IPUMS ATUS", "atus",
    "IPUMS DHS", "dhs",
    "IPUMS Higher Ed", "highered",
    "IPUMS MEPS", "meps",
    "IPUMS NHIS", "nhis",
    "IPUMS PMA", "pma"
  )
}

# Internal functions -----------------------------------------------------------

#' Convert all download file paths in a cassette file to be relative to the
#' working directory
#'
#' @noRd
convert_paths_in_cassette_file_to_relative <- function(cassette_path) {
  lines <- readLines(cassette_path)
  file_path_lines <- which(fostr_detect(lines, "file: yes")) + 1
  orig_paths <- purrr::map_chr(
    lines[file_path_lines],
    ~strsplit(.x, "string: ")[[1]][[2]]
  )
  converted_paths <- purrr::map_chr(orig_paths, convert_to_relative_path)
  for (i in seq_along(orig_paths)) {
    lines <- gsub(orig_paths[[i]], converted_paths[[i]], lines, fixed = TRUE)
  }
  writeLines(lines, con = cassette_path)
}

skip_if_no_api_access <- function(have_api_access) {
  if (!have_api_access) {
    return(testthat::skip("no API access and no saved API responses"))
  }
}

#' Convert an absolute path to be relative to the working directory
#'
#' This is only used in unit tests at the moment
#' @noRd
convert_to_relative_path <- function(path) {
  path_components <- strsplit(path, "/|\\\\", perl = TRUE)[[1]]
  wd_components <- strsplit(getwd(), "/|\\\\", perl = TRUE)[[1]]
  if (identical(path_components, wd_components)) {
    stop(
      "Supplied path cannot be the path to the working directory",
      call. = FALSE
    )
  }
  path_length <- length(path_components)
  wd_length <- length(wd_components)
  min_length <- min(path_length, wd_length)
  path_components_min_length <- path_components[1:min_length]
  wd_components_min_length <- wd_components[1:min_length]
  components_match <- path_components_min_length == wd_components_min_length
  if (!any(components_match)) {
    stop("Supplied path must be an absolute path", call. = FALSE)
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

add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
}

api_base_url <- function() {
  api_url <- Sys.getenv("IPUMS_API_URL")
  if (api_url == "") return("https://api.ipums.org/extracts/")
  api_url
}

api_extracts_path <- function() {
  basename(api_base_url())
}

ipums_api_version <- function(collection) {

  versions <- ipums_collection_versions()

  if(!collection %in% versions$collection) {
    stop(
      paste0(
        "No API version found for collection `", collection, "`\n",
        "IPUMS API is currently available for the following collections: ",
        paste0(versions$collection, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  api_version <- Sys.getenv("IPUMS_API_VERSION")

  if (api_version == "") {
    versions[[which(versions$collection == collection), "version"]]
  } else {
    api_version
  }

}

ipums_collection_versions <- function() {
  tibble::tribble(
    ~collection, ~version,
    "usa", "beta",
    "nhgis", "v1"
  )
}

EMPTY_NAMED_LIST <- setNames(list(), character(0))

standardize_extract_identifier <- function(extract) {
  if (inherits(extract, "ipums_extract")) return(extract)
  if (length(extract) == 1) {
    extract <- fostr_split(extract, ":")[[1]]
  }

  if (length(extract) != 2) {
    stop(
      paste0(
        "Expected extract to be either an `ipums_extract` object, a ",
        "length-two vector where the first element is an IPUMS collection ",
        "and the second is an extract number, or a single string with a ':' ",
        "separating the collection from the extract number."
      ),
      call. = FALSE
    )
  }
  collection <- extract[[1]]
  number <- suppressWarnings(as.numeric(extract[[2]]))
  if (is.na(number)) {
    stop("Expected extract number to be a number", call. = FALSE)
  }

  list(collection = collection, number = number)
}

# Should be moved to utils?
`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

#' Helper function to form, submit, and receive responses of requests expecting
#'   a JSON response.
#'
#' @param verb "GET" or "POST"
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Defaults to FALSE, which creates a body-less request.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to httr::modify_url.
#'
#' @return If the request returns a JSON response, this function returns a
#'   length-one character vector containing the response from the API
#'   formatted as JSON. Otherwise, the function throws an error.
#'
#' @noRd
ipums_api_json_request <- function(verb,
                                   collection,
                                   path,
                                   body = FALSE,
                                   queries = NULL,
                                   api_key = Sys.getenv("IPUMS_API_KEY")) {

  queries_is_null_or_named_list <- is.null(queries) ||
    is.list(queries) && !is.null(names(queries)) && !any(names(queries) == "")

  if (!queries_is_null_or_named_list) {
    stop("`queries` argument must be NULL or a named list")
  }

  api_url <- httr::modify_url(
    api_base_url(),
    path = path,
    query = c(
      list(collection = collection, version = ipums_api_version(collection)),
      queries
    )
  )

  res <- httr::VERB(
    verb = verb,
    url = api_url,
    body = body,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(utils::packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  if (httr::http_status(res)$category != "Success") {
    if (httr::status_code(res) == 400) {
      tryCatch(
        error_details <- parse_400_error(res),
        error = function(cond) {
          stop(
            "Received error from server (status code 400), but could not ",
            "parse response for more details."
          )
        }
      )
      stop(error_details, call. = FALSE)
    } else if (httr::status_code(res) == 404) {
      if (fostr_detect(path, "^extracts/\\d+$")) {
        extract_number <- as.numeric(fostr_split(path, "/")[[1]][[2]])
        most_recent_extract <- get_recent_extracts_info_list(
          collection,
          how_many = 1
        )
        most_recent_extract_number <- most_recent_extract[[1]]$number
        if (extract_number > most_recent_extract_number) {
          coll <- format_collection_for_printing(collection)
          stop(coll, " extract number ", extract_number, " does not exist; ",
               "most recent extract number is ", most_recent_extract_number,
               call. = FALSE)
        }
      }
      stop("URL not found", call. = FALSE)
    } else if (httr::status_code(res) %in% 500:599) {
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    } else { # other non-success codes, e.g. 300s
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    }
  }

  if (httr::http_type(res) != "application/json") {
    stop("Extract API did not return json", call. = FALSE)
  }

  new_ipums_json(
    httr::content(res, "text"),
    collection = collection
  )

}

parse_400_error <- function(res) {
  response_content <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = FALSE
  )
  response_detail <- response_content$detail
  response_detail <- unlist(response_detail)
  error_message <- paste0(
    "Received status code 400, with the following details:\n\n",
    paste0(response_detail, collapse = "\n\n")
  )
  return(error_message)
}

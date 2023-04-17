# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Retrieve the definition and latest status of an extract request
#'
#' @description
#' Retrieve the latest extract status of an extract request.
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
#'   Extract numbers do not need to be zero-padded. That is, use `1`, not
#'   `"0001"`.
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
#' [submit_extract()] and [download_extract()] to
#'   process and manage an extract request.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' [add_to_extract()], [remove_from_extract()] and [combine_extracts()] to
#'   revise an extract definition.
#'
#' [set_ipums_default_collection()] to set a default collection.
#'
#' [extract_tbl_to_list()] and [extract_list_to_tbl()] to manipulate information
#' about recent extract requests.
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
#' get_extract_info(submitted_extract)
#'
#' # Or specify the extract collection and number:
#' get_extract_info("usa:1")
#' get_extract_info(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("nhgis")
#' get_extract_info(1)
#' }
get_extract_info <- function(extract,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)

  if (inherits(extract, "ipums_extract")) {
    extract <- validate_ipums_extract(extract)
  }

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

  extract_list_from_json(response, validate = TRUE)[[1]]
}

#' Browse definitions of previously-submitted extract requests
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
#' @param how_many The number of extract requests for which to retrieve
#'   information. Defaults to the 10 most recent extracts.
#' @param as_table If `TRUE`, summarize extract history in a
#'   [`tibble`][tibble::tbl_df-class]. Otherwise, provide extract history as a
#'   list of [`ipums_extract`][ipums_extract-class] objects.
#'   Defaults to `FALSE`.
#' @param delay Number of seconds to delay between
#'   successive API requests, if multiple requests are needed to retrieve all
#'   records.
#'
#'   A delay is highly unlikely to be necessary and is
#'   intended only as a fallback in the event that you cannot retrieve your
#'   extract history without exceeding the API rate limit.
#'
#' @return
#' Either a list of [`ipums_extract`][ipums_extract-class] objects or
#' a [`tibble`][tibble::tbl_df-class] containing the extract definitions,
#' depending on the value of `as_table`.
#'
#' @seealso
#' [get_extract_info()] to get the current status of a specific extract request.
#'
#' [add_to_extract()], [remove_from_extract()] and [combine_extracts()] to
#'   revise an extract definition.
#'
#' [extract_tbl_to_list()] and [extract_list_to_tbl()] to manipulate information
#' about recent extract requests.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get information for most recent extract requests
#' get_extract_history("usa")
#'
#' # Adjust the number to return and output format
#' get_extract_history("cps", how_many = 3, as_table = TRUE)
#'
#' # Setting `how_many` to `Inf` will retrieve your entire history.
#' # You may want to set `as_table = TRUE` to consolidate output depending on
#' # the size of your extract history.
#' get_extract_history("nhgis", how_many = Inf, as_table = TRUE)
#'
#' # To get the most recent extract (for instance, if you have forgotten its
#' # extract number), use `get_last_extract_info()`
#' get_last_extract_info("nhgis")
#' }
get_extract_history <- function(collection = NULL,
                                how_many = 10,
                                as_table = FALSE,
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

  if (as_table) {
    extracts <- extract_list_to_tbl(extracts)
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
    as_table = FALSE,
    api_key = api_key
  )

  extract[[1]]
}

#' Convert recent extract definitions from tibble to list format
#'
#' @description
#' Convert a [`tibble`][tibble::tbl_df-class] of extract definition
#' specifications to a list of [`ipums_extract`][ipums_extract-class] objects
#' or vice versa.
#'
#' Use [`get_extract_history()`] to obtain definitions of recently-submitted
#' extracts.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract_tbl A [`tibble`][tibble::tbl_df-class] (or
#'   [`data.frame`][base::data.frame()]) containing the specifications for one
#'   or more [`ipums_extract`][ipums_extract-class] objects.
#'
#'   Use [get_extract_history()] with `as_table = TRUE` to produce such an object.
#' @param validate Logical value indicating whether to
#'   check that each of the output `ipums_extract`
#'   objects contains a valid and complete extract
#'   definition. Defaults to `TRUE`.
#'
#' @return For [extract_tbl_to_list()], a list of length equal to the number of
#'   extracts represented in `extract_tbl`.
#'
#'   For [extract_list_to_tbl()], a [`tibble`][tibble::tbl_df-class]
#'   representing the specifications for each of the extract requests
#'   represented in `extract_list`. Each column corresponds to an extract field.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_extract_history("usa", as_table = TRUE)
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
#'
#' income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]
#'
#' # Convert tibble of extracts to list of extracts
#' income_extracts <- extract_tbl_to_list(income_extracts)
#'
#' # Now it's easier to operate on those elements as extract objects:
#' revised_income_extract <- add_to_extract(
#'   income_extracts[[1]],
#'   samples = "us2018a"
#' )
#'
#' submitted_revised_income_extract <- submit_extract(revised_income_extract)
#'
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_extract_history("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) {
#'     print(extract$number)
#'   }
#' }
#'
#' # Convert list of extracts to tibble of extracts to view in a tabular format
#' extract_list_to_tbl(list_of_last_10_extracts)
#' }
extract_tbl_to_list <- function(extract_tbl, validate = TRUE) {
  collection <- unique(extract_tbl$collection)

  if (length(collection) > 1) {
    rlang::abort(
      "All extracts in `extract_tbl` must belong to same collection."
    )
  }

  if (length(collection) == 0) {
    rlang::abort("Cannot convert empty `extract_tbl` to list")
  }

  expected_names <- get_extract_tbl_fields(
    new_ipums_extract(collection = collection)
  )

  unexpected_names <- setdiff(names(extract_tbl), expected_names)

  if (length(unexpected_names) > 0) {
    rlang::abort(
      c(
        "Unexpected names in `extract_tbl`: ",
        paste0("\"", unexpected_names, "\"", collapse = ", ")
      )
    )
  }

  if (collection == "nhgis") {
    # This is about the jankiest code I've ever written. We should deprecate this
    # functionality to be honest.
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      rlang::abort(
        paste0(
          "The `tidyr` package must be installed to convert NHGIS extracts ",
          "from tbl to list format."
        )
      )
    }

    ds <- extract_tbl %>%
      dplyr::filter(.data$data_type == "datasets")

    if (nrow(ds) > 0) {
      ds <- ds %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "datasets" = list(
            new_dataset(
              .data$name,
              .data$data_tables,
              .data$geog_levels,
              .data$years,
              .data$breakdown_values
            )
          )
        )
    }

    tst <- extract_tbl %>%
      dplyr::filter(.data$data_type == "time_series_tables")

    if (nrow(tst) > 0) {
      tst <- tst %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "time_series_tables" = list(
            new_tst(.data$name, .data$geog_levels, .data$years)
          )
        )
    }

    shp <- extract_tbl %>%
      dplyr::filter(.data$data_type == "shapefiles") %>%
      dplyr::rename("shapefiles" = "name")

    extract_tbl <- dplyr::bind_rows(ds, tst, shp) %>%
      dplyr::select(-c("data_type", "name", "data_tables", "geog_levels", "years", "breakdown_values")) %>%
      dplyr::group_by(.data$number) %>%
      dplyr::mutate(
        "datasets" = tryCatch(
          purrr::map(list(.data$datasets), purrr::compact),
          error = function(cnd) list(NULL)
        ),
        "time_series_tables" = tryCatch(
          purrr::map(list(.data$time_series_tables), purrr::compact),
          error = function(cnd) list(NULL)
        )
      ) %>%
      tidyr::fill("shapefiles", .direction = "downup") %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(.data$number))

    var_sort <- c(
      "collection", "number",
      "description", "datasets", "time_series_tables",
      "geographic_extents", "shapefiles",
      "breakdown_and_data_type_layout",
      "tst_layout", "data_format",
      "submitted", "download_links", "status"
    )

    extract_tbl <- extract_tbl %>% dplyr::select(dplyr::any_of(var_sort))
    numbers <- extract_tbl$number

    extract_tbl <- purrr::map(
      extract_tbl[, setdiff(var_sort, "number")],
      function(c) {
        purrr::map(
          c,
          ~ if ((is_empty(.x) && !is_named(.x)) || is_na(.x)) {
            NULL
          } else {
            .x
          }
        )
      }
    )

    extract_tbl <- c(extract_tbl, list(number = numbers))
  }

  extract_list <- purrr::pmap(extract_tbl, new_ipums_extract)

  if (validate) {
    extract_list <- purrr::walk(extract_list, validate_ipums_extract)
  }

  extract_list
}

#' @rdname extract_tbl_to_list
#'
#' @param extract_list A list of [`ipums_extract`][ipums_extract-class] objects
#'   or a single `ipums_extract` object.
#'
#' @export
extract_list_to_tbl <- function(extract_list) {
  if ("ipums_extract" %in% class(extract_list)) {
    extract_list <- list(extract_list)
  }

  extract_types <- unique(
    purrr::map_chr(
      extract_list,
      function(x) x$collection
    )
  )

  if (length(extract_types) != 1) {
    rlang::abort(
      "All extracts in `extract_list` must belong to same collection."
    )
  }

  purrr::map_dfr(extract_list, extract_to_tbl)
}

# Non-exported functions ---------------------------------------------------

#' Convert a single extract to a tibble
#'
#' @description
#' S3 generic to allow for collection-specific method dispatch when converting
#' extract objects to tibble format. Collection-specific functionality is
#' needed because NHGIS extracts return a tibble whose extracts are spread
#' across multiple rows. However, we cannot perform dispatch on a list of
#' extract objects directly, as is done in `extract_list_to_tbl`.
#'
#' @param x An `ipums_extract` object
#'
#' @return A tibble representing the specifications for the extract `x`.
#'   These can be combined to form a larger tibble for multiple recent extracts.
#'
#' @noRd
extract_to_tbl <- function(x) {
  UseMethod("extract_to_tbl")
}

#' @export
extract_to_tbl.micro_extract <- function(x) {
  if (is.character(x$samples)) x$samples <- list(x$samples)
  if (is.character(x$variables)) x$variables <- list(x$variables)
  x$download_links <- list(x$download_links)

  unclassed_extract <- unclass(x)

  do.call(tibble::tibble, unclassed_extract)
}

#' @export
extract_to_tbl.nhgis_extract <- function(x) {
  base_vars <- tibble::tibble(
    collection = x$collection,
    description = x$description %||% NA_character_,
    data_format = x$data_format %||% NA_character_,
    breakdown_and_data_type_layout = x$breakdown_and_data_type_layout %||%
      NA_character_,
    tst_layout = x$tst_layout %||% NA_character_,
    geographic_extents = list(x$geographic_extents) %||% list(NULL),
    submitted = x$submitted,
    download_links = list(x$download_links),
    number = x$number,
    status = x$status
  )

  ds <- purrr::map_dfr(
    x$datasets,
    ~ tibble::tibble(
      name = .x$name,
      data_type = "datasets",
      data_tables = list(.x$data_tables),
      geog_levels = list(.x$geog_levels),
      years = list(.x$years),
      breakdown_values = list(.x$breakdown_values)
    )
  )

  tst <- purrr::map_dfr(
    x$time_series_tables,
    ~ tibble::tibble(
      name = .x$name,
      data_type = "time_series_tables",
      data_tables = list(NULL),
      geog_levels = list(.x$geog_levels),
      years = list(.x$years),
      breakdown_values = list(NULL)
    )
  )

  shp <- tibble::tibble(
    name = x$shapefiles,
    data_type = "shapefiles",
    data_tables = list(NULL),
    geog_levels = list(NULL),
    years = list(NULL),
    breakdown_values = list(NULL),
  )

  tbl <- dplyr::bind_rows(ds, tst, shp) %>%
    dplyr::mutate(number = x$number) %>%
    dplyr::full_join(base_vars, by = "number")

  var_order <- c(
    "collection", "number", "description", "data_type",
    "name", "data_tables", "geog_levels",
    "years", "breakdown_values", "geographic_extents",
    "tst_layout", "breakdown_and_data_type_layout",
    "data_format", "submitted", "download_links", "status"
  )

  tbl[, var_order]
}

#' This is currently used only to catch unexpected names in
#' `extract_tbl_to_list()`. However, unexpected names vary across
#' collections, so we use an S3 generic.
#'
#' @noRd
get_extract_tbl_fields <- function(x) {
  UseMethod("get_extract_tbl_fields")
}

#' @export
get_extract_tbl_fields.nhgis_extract <- function(x) {
  c(
    "collection", "description", "datasets", "data_tables", "geog_levels",
    "years", "breakdown_values", "geographic_extents",
    "breakdown_and_data_type_layout", "time_series_tables",
    "tst_layout", "shapefiles", "data_format",
    "submitted", "download_links", "number", "status",
    "name", "data_type" # Used in long-format NHGIS tbl structure
  )
}

#' @export
get_extract_tbl_fields.micro_extract <- function(x) {
  c(
    "collection", "description", "samples", "variables",
    "data_format", "data_structure", "rectangular_on",
    "submitted", "download_links", "number", "status"
  )
}

#' @export
get_extract_tbl_fields.ipums_extract <- function(x) {
  c(
    "collection", "description", "submitted",
    "download_links", "number", "status"
  )
}

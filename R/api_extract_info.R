# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Get information about submitted extract requests
#'
#' @description
#' Retrieve the definitions of your previously submitted extract requests.
#'
#' Use `get_extract_info()` to retrieve the definition and current status of a
#' specific extract request.
#'
#' Use `get_recent_extracts_info()` to retrieve the information for the most
#' recent extract requests for a particular collection.
#'
#' `get_last_extract_info()` is a convenience function to retrieve the
#' most recent extract request for a given collection.
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
#' For `get_extract_info()` and `get_last_extract_info()`, an
#' [`ipums_extract`][ipums_extract-class] object.
#'
#' For `get_recent_extracts_info()`, a list of
#' [`ipums_extract`][ipums_extract-class] objects (if `table = FALSE`) or
#' a [`tibble`][tibble::tbl_df-class] (if `table = TRUE`).
#'
#' @seealso
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
#' # Get information for multiple recent requests by specifying a collection
#' # with no number:
#' get_recent_extracts_info("usa")
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("nhgis")
#' get_extract_info(1)
#'
#' # Or omit the number to get information about recent extracts
#' get_recent_extracts_info(how_many = 3, table = TRUE)
#' get_last_extract_info()
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

  response <- ipums_api_json_request(
    "GET",
    collection = tolower(extract$collection),
    path = paste0("extracts/", extract$number),
    queries = NULL,
    api_key = api_key
  )

  extract_list_from_json(response, validate = TRUE)[[1]]
}

#' @param collection Character string of the IPUMS collection for which to
#'   retrieve recent extract requests.
#'
#'   If `NULL`, uses the current default
#'   collection, if it exists. See [set_ipums_default_collection()].
#' @param how_many The number of recent extract requests for which to retrieve
#'   information.
#' @param table Logical value indicating whether to return recent extract
#'   information as a [`tibble`][tibble::tbl_df-class] (`TRUE`) or a list of
#'   [`ipums_extract`][ipums_extract-class] objects (`FALSE`). Defaults to
#'   `FALSE`.
#'
#' @rdname get_extract_info
#' @export
get_recent_extracts_info <- function(collection = NULL,
                                     how_many = 10,
                                     table = FALSE,
                                     api_key = Sys.getenv("IPUMS_API_KEY")) {
  response <- ipums_api_json_request(
    "GET",
    collection = collection %||% get_default_collection(),
    path = "extracts/",
    queries = list(pageNumber = 1, pageSize = how_many),
    api_key = api_key
  )

  recent_extracts <- extract_list_from_json(response)

  if (table) {
    recent_extracts <- extract_list_to_tbl(recent_extracts)
  }

  recent_extracts
}

#' @rdname get_extract_info
#' @export
get_last_extract_info <- function(collection = NULL,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  collection <- collection %||% get_default_collection()

  check_api_support(collection)

  get_recent_extracts_info(collection, how_many = 1, api_key = api_key)[[1]]
}

#' Convert recent extract definitions from tibble to list format
#'
#' @description
#' Convert a [`tibble`][tibble::tbl_df-class] of extract definition
#' specifications to a list of [`ipums_extract`][ipums_extract-class] objects
#' or vice versa.
#'
#' Use [`get_recent_extracts_info()`] to obtain definitions of recently-submitted
#' extracts.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract_tbl A [`tibble`][tibble::tbl_df-class] (or
#'   [`data.frame`][base::data.frame()]) containing the specifications for one
#'   or more [`ipums_extract`][ipums_extract-class] objects.
#'
#'   Use [get_recent_extracts_info()] with `table = TRUE` to produce such an object.
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
#' tbl_of_last_10_extracts <- get_recent_extracts_info("usa", table = TRUE)
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
#' list_of_last_10_extracts <- get_recent_extracts_info("usa")
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
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      rlang::abort(
        paste0(
          "The `tidyr` package must be installed to convert NHGIS extracts ",
          "from tbl to list format."
        )
      )
    }

    # NHGIS extract tbls are not one-row-per-extract by default,
    # but need to be for conversion using new_ipums_extract()
    extract_tbl <- collapse_nhgis_extract_tbl(extract_tbl)
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
extract_to_tbl.usa_extract <- function(x) {
  if (is.character(x$samples)) x$samples <- list(x$samples)
  if (is.character(x$variables)) x$variables <- list(x$variables)
  x$download_links <- list(x$download_links)

  unclassed_extract <- unclass(x)

  do.call(tibble::tibble, unclassed_extract)
}

#' @export
extract_to_tbl.cps_extract <- function(x) {
  if (is.character(x$samples)) x$samples <- list(x$samples)
  if (is.character(x$variables)) x$variables <- list(x$variables)
  x$download_links <- list(x$download_links)

  unclassed_extract <- unclass(x)

  do.call(tibble::tibble, unclassed_extract)
}

#' @export
extract_to_tbl.nhgis_extract <- function(x) {
  base_vars <- list(
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

  ds <- c(
    list(
      name = unlist(x$datasets) %||% NA_character_,
      data_tables = unname(x$data_tables[x$datasets]),
      geog_levels = unname(x$geog_levels[x$datasets]),
      years = if (is_empty(x$years)) {
        list(NULL)
      } else {
        unname(purrr::map(x$years, ~.x)[x$datasets])
      },
      breakdown_values = if (is_empty(x$breakdown_values)) {
        list(NULL)
      } else {
        unname(purrr::map(x$breakdown_values, ~.x)[x$datasets])
      }
    ),
    base_vars
  )

  ts <- c(
    list(
      name = unlist(x$time_series_tables) %||% NA_character_,
      data_tables = list(NULL),
      geog_levels = unname(x$geog_levels[x$time_series_tables]),
      years = if (is_empty(x$years)) {
        list(NULL)
      } else {
        unname(purrr::map(x$years, ~.x)[x$time_series_tables])
      },
      breakdown_values = list(NULL)
    ),
    base_vars
  )

  shp <- c(
    list(
      name = unlist(x$shapefiles) %||% NA_character_,
      data_tables = list(NULL),
      geog_levels = list(NULL),
      years = list(NULL),
      breakdown_values = list(NULL)
    ),
    base_vars
  )

  tbl1 <- do.call(tibble::tibble, ds)
  tbl1$data_type <- "datasets"

  tbl2 <- do.call(tibble::tibble, ts)
  tbl2$data_type <- "time_series_tables"

  tbl3 <- do.call(tibble::tibble, shp)
  tbl3$data_type <- "shapefiles"

  tbl <- dplyr::bind_rows(tbl1, tbl2, tbl3)
  tbl <- tbl[!is.na(tbl$name), ]

  var_order <- c(
    "collection", "number", "description", "data_type",
    "name", "data_tables", "geog_levels",
    "years", "breakdown_values", "geographic_extents",
    "tst_layout", "breakdown_and_data_type_layout",
    "data_format", "submitted", "download_links", "status"
  )

  tbl[, var_order]
}

#' Flatten a long-format tibble of NHGIS extract specifications
#'
#' Converts tibble where each extract is spread out across multiple rows to a
#' tibble where each row represents a specific extract. This enables the use of
#' a standard conversion method from an extract tibble to list across microdata
#' and NHGIS, even though NHGIS extract tibbles are delivered in a different
#' layout (each row is not a single extract).
#'
#' @param extract_tbl Tibble of NHGIS extract specifications as provided by
#'   `get_recent_extracts_info("nhgis", table = TRUE)`
#'
#' @return A tibble where each row represents a single NHGIS extract. Fields
#'   with multiple values are collapsed as list-columns.
#'
#' @noRd
collapse_nhgis_extract_tbl <- function(extract_tbl) {
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    rlang::abort(
      paste0(
        "Package `tidyr` must be installed to convert NHGIS extracts from tbl ",
        "to list format."
      )
    )
  }

  stopifnot(unique(extract_tbl$collection) == "nhgis")

  extract_tbl <- extract_tbl %>%
    # Subfields that are specific to datasets or time series tables
    # need to be rolled up while grouped by data type
    dplyr::group_by(
      .data$number,
      .data$data_type
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c("data_tables", "breakdown_values"),
        ~ ifelse(is_null(unlist(.x)), .x, list(.x))
      )
    ) %>%
    # Subfields that apply to both datasets and time series tables
    # need to be rolled up across datasets and tsts, but not shapefiles
    dplyr::group_by(
      .data$number,
      is_ds_or_tst = .data$data_type %in% c("datasets", "time_series_tables")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c("geog_levels", "years"),
        ~ ifelse(is_null(unlist(.x)), .x, list(.x))
      )
    ) %>%
    dplyr::group_by(.data$number) %>%
    dplyr::select(-"is_ds_or_tst") %>%
    tidyr::pivot_wider(
      names_from = "data_type",
      values_from = "name",
      values_fn = list
    ) %>%
    tidyr::fill(dplyr::everything(), .direction = "updown") %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  # Join to ensure all extract parameters are present (if the extract table
  # does not include at least one each of datasets, time_series_tables,
  # and shapefiles)
  join_df <- tibble::tibble(
    shapefiles = list(NULL),
    time_series_tables = list(NULL),
    datasets = list(NULL)
  )

  # Attach names to child fields
  extract_tbl <- extract_tbl %>%
    dplyr::left_join(
      join_df,
      by = intersect(colnames(extract_tbl), colnames(join_df))
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c("data_tables", "breakdown_values"),
        function(d) {
          purrr::map2(
            d,
            .data$datasets,
            ~ recycle_extract_subfield(.x, .y)
          )
        }
      ),
      dplyr::across(
        c("geog_levels", "years"),
        function(d) {
          purrr::map2(
            d,
            purrr::map2(.data$datasets, .data$time_series_tables, c),
            ~ recycle_extract_subfield(.x, .y)
          )
        }
      ),
    ) %>%
    # For consistency of output after conversion to list, define_extract_nhgis()
    # and tbl to list conversion should align on all fields
    dplyr::mutate(
      dplyr::across(
        c("data_format", "breakdown_and_data_type_layout", "tst_layout"),
        ~ ifelse(is.na(.x), list(NULL), .x)
      )
    )

  # Reorder
  var_sort <- c(
    "collection", "number",
    "description", "datasets", "data_tables", "time_series_tables",
    "geog_levels", "years", "breakdown_values",
    "geographic_extents", "shapefiles",
    "breakdown_and_data_type_layout",
    "tst_layout", "data_format",
    "submitted", "download_links", "status"
  )

  extract_tbl <- extract_tbl[, var_sort]

  extract_tbl
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
get_extract_tbl_fields.usa_extract <- function(x) {
  c(
    "collection", "description", "samples", "variables",
    "data_format", "data_structure", "rectangular_on",
    "submitted", "download_links", "number", "status"
  )
}

#' @export
get_extract_tbl_fields.cps_extract <- function(x) {
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

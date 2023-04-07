# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' List available data sources from IPUMS NHGIS
#'
#' @description
#' Retrieve information about available NHGIS data sources, including
#' [datasets](https://www.nhgis.org/overview-nhgis-datasets),
#' data tables (summary tables),
#' [time series tables](https://www.nhgis.org/time-series-tables),
#' and [shapefiles](https://www.nhgis.org/gis-files) (GIS files).
#'
#' To retrieve summary metadata for all available data sources of a particular
#' type, use the `type` argument. To retrieve detailed metadata for a
#' single data source, use the `dataset`, `data_table`, or `time_series_table`
#' arguments. See the *metadata availability* section below for information on
#' the metadata provided for each data type.
#'
#' For general information, see the NHGIS
#' [data source overview](https://www.nhgis.org/data-availability) and the
#' [FAQ](https://www.nhgis.org/frequently-asked-questions-faq).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @section Metadata availability:
#' The following sections summarize the metadata fields provided for each data
#' type. Summary metadata include a subset of the fields provided for individual
#' data sources.
#'
#' ## Datasets:
#'
#' - **`name`:** The unique identifier for the dataset. This is the code that is
#'   used to refer to the dataset when interacting with the IPUMS API.
#' - **`group:`** The group of datasets to which the dataset belongs.
#'   For instance, 5 separate datasets are part of the
#'   `"2015 American Community Survey"` group.
#' - **`description:`** A short description of the dataset.
#' - **`sequence:`** Order in which the dataset will appear in the metadata API
#'   and extracts.
#' - **`has_multiple_data_types:`** Logical value indicating whether multiple
#'   data types exist for this dataset. For example, ACS
#'   datasets include both estimates and margins of error.
#' - **`data_tables:`** A [`tibble`][tibble::tbl_df-class] containing names,
#'   codes, and descriptions for all data tables available for the dataset.
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
#' - **`name`:** The unique identifier for the data table within its dataset.
#'   This is the code that is used to refer to the data table when interacting
#'   with the IPUMS API.
#' - **`description`:** A short description of the data table.
#' - **`universe`:** The statistical population measured by this data table
#'   (e.g. persons, families, occupied housing units, etc.)
#' - **`nhgis_code`:** The code identifying the data table in the extract.
#'   Variables in the extract data will include column names prefixed with this
#'   code.
#' - **`sequence`:** Order in which the data table will appear in the metadata
#'   API and extracts.
#' - **`dataset_name`:** Name of the dataset to which this data table belongs.
#' - **`variables`:** A [`tibble`][tibble::tbl_df-class] containing variable
#'   descriptions and codes for the data table
#'
#' ## Time series tables:
#'
#' - **`name`:** The unique identifier for the time series table. This is the
#'   code that is used to refer to the time series table when interacting with
#'   the IPUMS API.
#' - **`description`:** A short description of the time series table.
#' - **`geographic_integration`:** The method by which the time series table
#'   aligns geographic units across time. `"Nominal"` integration indicates
#'   that geographic units are aligned by name (disregarding changes in unit
#'   boundaries). `"Standardized"` integration indicates that data from multiple
#'   time points are standardized to the indicated year's census units. For
#'   more information, click
#'   [here](https://www.nhgis.org/time-series-tables#geographic-integration).
#' - **`sequence`:** Order in which the time series table will appear in the
#'   metadata API and extracts.
#' - **`time_series`:** A [`tibble`][tibble::tbl_df-class] containing names
#'   and descriptions for the individual time series available for the
#'   time series table.
#' - **`years`:** A [`tibble`][tibble::tbl_df-class] containing
#'   information on the available data years for the
#'   time series table.
#' - **`geog_levels`:** A [`tibble`][tibble::tbl_df-class] containing names
#'   and descriptions for the geographic levels available
#'   for the time series table.
#'
#' ## Shapefiles:
#'
#' - **`name`:** The unique identifier for the shapefile. This is the
#'   code that is used to refer to the shapefile when interacting with
#'   the IPUMS API.
#' - **`year`:** The survey year in which the shapefile's represented areas
#'   were used for tabulations, which may be different than the vintage of the
#'   represented areas. For more information, click
#'   [here](https://www.nhgis.org/gis-files#years).
#' - **`geographic_level`:** The geographic level of the shapefile.
#' - **`extent`:** The geographic extent covered by the shapefile.
#' - **`basis`:** The derivation source of the shapefile.
#' - **`sequence`:** Order in which the shapefile will appear in the
#'   metadata API and extracts.
#'
#' @inheritParams submit_extract
#' @param type One of `"datasets"`, `"time_series_tables"`, or `"shapefiles"`
#'   indicating the type of summary metadata to retrieve. Leave `NULL` if
#'   requesting metadata for a single `dataset`, `data_table`, or
#'   `time_series_table`.
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
#'   See examples.
#'
#'   Only used when `type` is provided.
#' @param match_all If `TRUE`, only metadata records that match all of the
#'   expressions provided in `...` will be included in the output. If `FALSE`,
#'   metadata records that match any of the expressions will be included.
#'   Defaults to `TRUE`.
#' @param match_case If `TRUE`, use case-sensitive matching when interpreting
#'   the expressions provided in `...`. Defaults to `FALSE`.
#' @param n_records For summary metadata, the number of records to retrieve,
#'   starting from the first record. If `NULL`, obtains all records for the
#'   given `type`. Maximum allowed value is 2500.
#'
#'   Only used if `type` is provided.
#'
#' @return If `type` is provided, a [`tibble`][tibble::tbl_df-class] of
#'   summary metadata for all data sources of the provided `type`.
#'   Otherwise, a named list of metadata for the specified `dataset`,
#'   `data_table`, or `time_series_table`.
#'
#' @seealso
#' [define_extract_nhgis()] to create an IPUMS NHGIS extract definition.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
get_nhgis_metadata <- function(type = NULL,
                               dataset = NULL,
                               data_table = NULL,
                               time_series_table = NULL,
                               ...,
                               match_all = TRUE,
                               match_case = FALSE,
                               n_records = NULL,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {
  summary_req <- !is.null(type)
  ds_req <- !is.null(dataset)
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
    ~ length(.x) > 1
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
      type,
      n_records = n_records,
      api_key = api_key
    )

    metadata <- filter_df_cols(
      metadata,
      ...,
      match_all = match_all,
      match_case = match_case
    )
  } else {
    metadata <- ipums_api_metadata_request(
      url = metadata_request_url(
        collection = "nhgis",
        path = list(
          datasets = dataset,
          data_tables = data_table,
          time_series_tables = time_series_table
        )
      ),
      api_key = api_key
    )

    metadata <- nested_df_to_tbl(metadata)
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
#' @param get_all_records Logical indicating whether all records should be
#'   retrieved by iterating through the paginated API response. This is only
#'   needed to enable unit testing of page iteration without producing very
#'   large cassette files. Users can trigger iterative behavior by leaving
#'   `n_records = NULL` in `get_nhgis_metadata()`.
#'
#' @return Tibble of summary metadata for the requested `type`
#'
#' @noRd
get_nhgis_summary_metadata <- function(type,
                                       n_records,
                                       api_key = Sys.getenv("IPUMS_API_KEY"),
                                       get_all_records = is_null(n_records)) {
  valid_types <- c(
    "datasets",
    "data_tables",
    "time_series_tables",
    "shapefiles"
  )

  if (!type %in% valid_types) {
    rlang::abort(
      paste0(
        "`type` must be one of \"datasets\", \"data_tables\", ",
        "\"time_series_tables\", or \"shapefiles\""
      )
    )
  }

  metadata <- ipums_api_metadata_request(
    url = metadata_request_url(
      collection = "nhgis",
      path = list(type),
      queries = list(pageNumber = 1, pageSize = n_records %||% 2500)
    ),
    api_key = api_key
  )

  out <- list(metadata)

  # get_all_records used to enable dev to trigger looping in tests, but is
  # not exposed to the user and is instead triggered by `n_records = NULL`
  if (get_all_records) {
    while (!is.null(metadata$links$nextPage)) {
      metadata <- ipums_api_metadata_request(
        url = metadata$links$nextPage,
        api_key = api_key
      )

      out <- c(out, list(metadata))
    }
  }

  metadata <- purrr::map_dfr(out, ~ .x$data)

  nested_df_to_tbl(metadata)
}

#' Submit a request to the IPUMS metadata API and parse response
#'
#' @param url URL for the request
#' @param api_key API key
#'
#' @return Either a tibble/data.frame or list object with the metadata for the
#'   provided URL
#'
#' @noRd
ipums_api_metadata_request <- function(url,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {
  tryCatch(
    {
      res <- httr::GET(
        url = url,
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
      )
    },
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

  content
}

#' Generate a URL to submit to the NHGIS metadata API
#'
#' @param collection IPUMS collection for the request
#' @param path List of components that should be added to the path of the
#'   request URL. Elements will be collapsed in order and separated with `/` to
#'   form the path of the URL.
#'
#'   Unnamed elements will be inserted as-is. Named elements will be constructed
#'   in the format `name/value`. However, named elements with value `NULL` will
#'   have both name and value omitted from the path. Therefore, optional
#'   elements can be provided with a name if they evaluate to `NULL` when not
#'   used.
#' @param queries Queries to add to the request URL. For metadata endpoints,
#'   this typically corresponds to pageSize and pageNumber.
#'
#' @return A character the constructed request URL
#'
#' @noRd
metadata_request_url <- function(collection, path, queries = NULL) {
  queries_is_null_or_named_list <- is.null(queries) ||
    is.list(queries) && !is.null(names(queries)) && !any(names(queries) == "")

  if (!queries_is_null_or_named_list) {
    rlang::abort("`queries` argument must be `NULL` or a named list")
  }

  api_url <- httr::modify_url(
    api_base_url(),
    path = metadata_request_path(collection, path),
    query = c(
      list(version = ipums_api_version()),
      queries
    )
  )

  api_url
}

# Helper to construct path for `metadata_request_url`
metadata_request_path <- function(collection, path) {
  path <- purrr::compact(path)
  fields <- names(path)

  args <- c("metadata", collection, rbind(fields, unlist(path)))

  # Avoids extra `/` for unnamed args in `...`
  args <- args[which(args != "")]

  paste(args, collapse = "/")
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
    function(x) {
      filt(
        purrr::map_lgl(
          match_vals,
          function(y) any(grepl(y, x))
        )
      )
    }
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
#'   columns remain unmodified.
#'
#' @noRd
filter_df_cols <- function(.data, ..., match_all = TRUE, match_case = FALSE) {
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
    ~ filter_list(
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

#' Convert all data.frames in a nested list/data.frame into tibbles
#'
#' Single-dataset metadata breakdowns field returns a
#' data.frame that includes a column of data.frames. We want to convert
#' all levels of such a hierarchy to tibbles for consistent formatting.
#'
#' @noRd
nested_df_to_tbl <- function(l) {
  if (is.data.frame(l) || tibble::is_tibble(l)) {
    l <- dplyr::mutate(
      tibble::as_tibble(l),
      dplyr::across(
        dplyr::everything(),
        ~ if (rlang::is_list(.x)) {
          purrr::map(.x, nested_df_to_tbl)
        } else {
          .x
        }
      )
    )

    # Convert from API's camelCase to snake_case for consistency with
    # ipums_extract objects
    colnames(l) <- to_snake_case(colnames(l))
  } else if (rlang::is_list(l)) {
    l <- purrr::map(l, nested_df_to_tbl)
    names(l) <- to_snake_case(names(l))
  }

  l
}

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
#' argument. See the *metadata availability* section below for information on
#' the metadata provided for each data type.
#'
#' For general information, see the NHGIS
#' [data source overview](https://www.nhgis.org/data-availability) and the
#' [FAQ](https://www.nhgis.org/frequently-asked-questions-faq).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")` and
#' NHGIS extract definitions in `vignette("ipums-api-nhgis")`.
#'
#' @section Metadata availability:
#' The following sections summarize the metadata fields provided for each data
#' type. Summary metadata include a subset of the fields provided for individual
#' data sources.
#'
#' ## Datasets:
#'
#' - **`name`:** The unique identifier for the dataset. This is the value that
#'   is used to refer to the dataset when interacting with the IPUMS API.
#' - **`group`:** The group of datasets to which the dataset belongs.
#'   For instance, 5 separate datasets are part of the
#'   `"2015 American Community Survey"` group.
#' - **`description`:** A short description of the dataset.
#' - **`sequence`:** Order in which the dataset will appear in the metadata API
#'   and extracts.
#' - **`has_multiple_data_types`:** Logical value indicating whether multiple
#'   data types exist for this dataset. For example, ACS
#'   datasets include both estimates and margins of error.
#' - **`data_tables`:** A [`tibble`][tibble::tbl_df-class] containing names,
#'   codes, and descriptions for all data tables available for the dataset.
#' - **`geog_levels`:** A [`tibble`][tibble::tbl_df-class] containing names,
#'   descriptions, and extent information for the geographic levels available
#'   for the dataset. The `has_geog_extent_selection` field contains logical
#'   values indicating whether extent selection is allowed (and required) for
#'   the associated geographic level. See `geographic_instances` below.
#' - **`breakdowns`:** A [`tibble`][tibble::tbl_df-class] containing names,
#'   types, descriptions, and breakdown values for all breakdowns available
#'   for the dataset.
#' - **`years`:** A vector of years for which the dataset is available. This
#'   field is only present if a dataset is available for multiple years. Note
#'   that ACS datasets are not considered to be available for multiple years.
#' - **`geographic_instances`:** A [`tibble`][tibble::tbl_df-class] containing
#'   names and descriptions for all valid geographic extents for the
#'   dataset. This field is only present if at least one of the dataset's
#'   `geog_levels` allows geographic extent selection.
#'
#' ## Data tables:
#'
#' - **`name`:** The unique identifier for the data table within its dataset.
#'   This is the value that is used to refer to the data table when interacting
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
#' - **`n_variables`:** Number of variables included in this data table.
#' - **`variables`:** A [`tibble`][tibble::tbl_df-class] containing variable
#'   descriptions and codes for the variables included in the data table
#'
#' ## Time series tables:
#'
#' - **`name`:** The unique identifier for the time series table. This is the
#'   value that is used to refer to the time series table when interacting with
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
#'   value that is used to refer to the shapefile when interacting with
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
#' @param type One of `"datasets"`, `"data_tables"`, `"time_series_tables"`,
#'   or `"shapefiles"` indicating the type of summary metadata to retrieve.
#'   Leave `NULL` if requesting metadata for a single `dataset`, `data_table`,
#'   or `time_series_table`.
#' @param dataset Name of an individual dataset for which to retrieve metadata.
#' @param data_table Name of an individual data table for which to retrieve
#'   metadata. If provided, an associated `dataset` must also be specified.
#' @param time_series_table Name of an individual time series table for which
#'   to retrieve metadata.
#' @param delay Number of seconds to delay between
#'   successive API requests, if multiple requests are needed to retrieve all
#'   records.
#'
#'   A delay is highly unlikely to be necessary and is intended only as a
#'   fallback in the event that you cannot retrieve all metadata records without
#'   exceeding the API rate limit.
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
#' library(dplyr)
#'
#' # Get summary metadata for all available sources of a given data type
#' get_metadata_nhgis("datasets")
#'
#' # Filter to identify data sources of interest by their metadata values
#' all_tsts <- get_metadata_nhgis("time_series_tables")
#'
#' tsts <- all_tsts %>%
#'   filter(
#'     grepl("Children", description),
#'     grepl("Families", description),
#'     geographic_integration == "Standardized to 2010"
#'   )
#'
#' tsts$name
#'
#' # Get detailed metadata for a single source with its associated argument:
#' cs5_meta <- get_metadata_nhgis(time_series_table = "CS5")
#' cs5_meta$geog_levels
#'
#' # Use the available values when defining an NHGIS extract request
#' define_extract_nhgis(
#'   time_series_tables = tst_spec("CS5", geog_levels = "state")
#' )
#'
#' # Detailed metadata is also provided for datasets and data tables
#' get_metadata_nhgis(dataset = "1990_STF1")
#' get_metadata_nhgis(data_table = "NP1", dataset = "1990_STF1")
#'
#' # Iterate over data sources to retrieve detailed metadata for several
#' # records. For instance, to get variable metadata for a set of data tables:
#' tables <- c("NP1", "NP2", "NP10")
#'
#' var_meta <- purrr::map(
#'   tables,
#'   function(dt) {
#'     dt_meta <- get_metadata_nhgis(dataset = "1990_STF1", data_table = dt)
#'
#'     # This ensures you avoid hitting rate limit for large numbers of tables
#'     Sys.sleep(1)
#'
#'     dt_meta$variables
#'   }
#' )
#' }
get_metadata_nhgis <- function(type = NULL,
                               dataset = NULL,
                               data_table = NULL,
                               time_series_table = NULL,
                               delay = 0,
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
    valid_types <- c(
      "datasets", "data_tables", "time_series_tables", "shapefiles"
    )

    if (!type %in% valid_types) {
      rlang::abort(
        paste0(
          "`type` must be one of \"datasets\", \"data_tables\", ",
          "\"time_series_tables\", or \"shapefiles\""
        )
      )
    }

    metadata <- get_summary_metadata(
      collection = "nhgis",
      type,
      delay = delay,
      api_key = api_key
    )
  } else {
    metadata <- get_detailed_metadata(
      collection = "nhgis",
      datasets = dataset,
      data_tables = data_table,
      time_series_tables = time_series_table,
      api_key = api_key
    )
  }

  metadata
}

#' List available samples for IPUMS microdata collections
#'
#' @description
#' Retrieve sample IDs and descriptions for IPUMS microdata collections.
#'
#' Currently supported microdata collections are:
#'   - IPUMS USA (`"usa"`)
#'   - IPUMS CPS (`"cps"`)
#'   - IPUMS International (`"ipumsi"`)
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @inheritParams get_metadata_nhgis
#' @param collection Character string of the IPUMS collection for which to
#'   retrieve sample IDs. Defaults to the current default collection,
#'   if it exists. See [set_ipums_default_collection()].
#'
#'   For a list of codes used to refer to each collection, see
#'   [ipums_data_collections()].
#' @param delay Number of seconds to delay between
#'   successive API requests, if multiple requests are needed to retrieve all
#'   records.
#'
#'   A delay is highly unlikely to be necessary and is intended only as a
#'   fallback in the event that you cannot retrieve all sample IDs without
#'   exceeding the API rate limit.
#'
#' @return A [`tibble`][tibble::tbl_df-class] containing sample IDs and
#'   descriptions for the indicated collection.
#'
#' @seealso
#' [`define_extract_*()`][define_extract-micro] to create an IPUMS microdata
#'   extract definition.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_sample_info("usa")
#' get_sample_info("cps")
#' get_sample_info("ipumsi")
#' }
get_sample_info <- function(collection = NULL,
                            delay = 0,
                            api_key = Sys.getenv("IPUMS_API_KEY")) {
  collection <- collection %||% get_default_collection()

  metadata <- get_summary_metadata(
    collection = collection,
    type = "samples",
    delay = delay,
    api_key = api_key
  )

  metadata
}

# Internal functions -----------------------------------------------------------

#' Get summary metadata
#'
#' @description
#' Helper to retrieve summary metadata for all records of a given metadata type.
#' This is in contrast to "detailed" metadata, which provides information on a
#' particular data source (e.g. for a single NHGIS dataset).
#'
#' For NHGIS, summary metadata is available for datasets, time series tables,
#' and shapefiles.
#'
#' For microdata, summary metadata is currently only available for samples.
#'
#' @inheritParams get_sample_info
#' @inheritParams get_metadata_nhgis
#'
#' @return Tibble of summary metadata for the requested `type`
#'
#' @noRd
get_summary_metadata <- function(collection,
                                 type,
                                 delay = 0,
                                 api_key = Sys.getenv("IPUMS_API_KEY")) {
  url <- api_request_url(
    collection = collection,
    path = metadata_request_path(collection, type),
    queries = list(pageNumber = 1, pageSize = api_page_size_limit("metadata"))
  )

  responses <- ipums_api_paged_request(
    url = url,
    max_pages = Inf,
    delay = delay,
    api_key = api_key
  )

  metadata <- purrr::map_dfr(
    responses,
    function(res) {
      content <- jsonlite::fromJSON(
        httr::content(res, "text"),
        simplifyVector = TRUE
      )

      content$data
    }
  )

  # Recursively convert all metadata data.frames to tibbles and all
  # camelCase names to snake_case
  convert_metadata(metadata)
}

#' Get detailed metadata for a particular data source
#'
#' @inheritParams get_sample_info
#' @inheritParams get_metadata_nhgis
#' @param ... Arbitrary number of named and/or unnamed arguments to be passed
#'   to `metadata_request_path()`. This constructs the URL for the metadata
#'   request. Named arguments will have their names placed before their
#'   corresponding values in the output URL. Unnamed arguments will be added
#'   to the URL in the order they are provided. All arguments and values
#'   will be separated by slashes.
#'
#'   Arguments whose value is `NULL` will not be included. This syntax
#'   allows you to input all *possible* endpoint parameters while safely
#'   leaving them out of the resulting URL if not provided.
#'
#' @return List of metadata for the requested data source
#'
#' @noRd
get_detailed_metadata <- function(collection,
                                  ...,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  url <- api_request_url(
    collection = collection,
    path = metadata_request_path(collection = collection, ...)
  )

  response <- ipums_api_request(
    "GET",
    url = url,
    body = FALSE,
    api_key = api_key,
    httr::content_type_json()
  )

  metadata <- jsonlite::fromJSON(
    httr::content(response, "text"),
    simplifyVector = TRUE
  )

  # Recursively convert all metadata data.frames to tibbles and all
  # camelCase names to snake_case
  convert_metadata(metadata)
}

#' Convert metadata provided by API to appropriate data structures and
#' naming conventions used in ipumsr
#'
#' @description
#' By default the metadata responses will contain tabular data
#' structures as `data.frames` and with the API's camelCase naming conventions.
#'
#' This recursively converts all data.frames (which may be nested in other
#' data.frame and list objects) to tibbles and converts camelCase field names
#' to snake_case throughout.
#'
#' @noRd
convert_metadata <- function(metadata) {
  if (is.data.frame(metadata) || tibble::is_tibble(metadata)) {
    # If the metadata is tabular, ensure all columns only contain
    # tibbles, not data.frames
    metadata <- dplyr::mutate(
      tibble::as_tibble(metadata),
      dplyr::across(
        dplyr::everything(),
        ~ if (rlang::is_list(.x)) {
          purrr::map(.x, convert_metadata)
        } else {
          .x
        }
      )
    )

    # Convert from API's camelCase to snake_case for consistency with
    # ipums_extract objects
    colnames(metadata) <- to_snake_case(colnames(metadata))
  } else if (rlang::is_list(metadata)) {
    # If metadata is in a list, ensure each list element is cleaned
    # appropriately. Tabular structures within the list will be recursively
    # processed.
    metadata <- purrr::map(metadata, convert_metadata)
    names(metadata) <- to_snake_case(names(metadata))
  }

  # If not tabular or list, return the cleaned metadata
  metadata
}

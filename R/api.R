
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Define an IPUMS extract object
#'
#' @description
#' Specify the parameters for a new IPUMS extract request object to be
#' submitted via the IPUMS extract API.
#'
#' @section Supported collections:
#' Currently, ipumsr supports extract definitions for the following
#' collections:
#'
#' - **IPUMS NHGIS**: [define_extract_nhgis()]
#' - **IPUMS USA**: [define_extract_usa()]
#' - **IPUMS CPS**: [define_extract_cps()]
#'
#' @section Value:
#' These functions produce an [`ipums_extract`][ipums_extract-class] object
#' with a subclass based on the collection corresponding to the extract. For
#' instance, an IPUMS NHGIS extract created with
#' `define_extract_nhgis()` will return an object of classes
#' `nhgis_extract` and `ipums_extract`. These objects are compatible
#' with the rest of the API functionality provided by ipumsr.
#'
#' For an overview of ipumsr API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @family ipums_api
#'
#' @name define_extract
NULL

# > `ipums_extract` class ----

#' `ipums_extract` class
#'
#' @description
#' The `ipums_extract` class provides a data structure for storing the
#' definition and status of a submitted or unsubmitted IPUMS data extract
#' for the purpose of interacting with the IPUMS extract API.
#'
#' `ipums_extract` is a superclass encompassing all of the collection-specific
#' extract classes.
#'
#' All objects with class `ipums_extract` will also have a collection-specific
#' subclass (e.g. `usa_extract`, `cps_extract`) to accommodate
#' collection-specific differences in extract options and content. All
#' collection-specific subclasses share the similarities described below.
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @section Properties:
#' Objects of class `ipums_extract` have:
#' * A `class` attribute of the form
#'   `c("{collection}_extract", "ipums_extract")`
#'   (e.g. `c("cps_extract", "ipums_extract")`)
#' * A base type of `"list"`
#' * A `names` attribute that is a character vector the same length as the
#'   underlying list
#'
#' @section Creating an extract:
#' * Create an `ipums_extract` object from scratch with the appropriate
#'   [define_extract] function. These functions take the form
#'   `define_extract_{collection}` (e.g. [define_extract_usa()])
#' * Use [get_extract_info()] to retrieve a previously-submitted
#'   extract. Previous extract definitions can be
#'   viewed with [get_recent_extracts_info]
#' * Create an `ipums_extract` object from a JSON-formatted definition with
#'   [define_extract_from_json()]
#'
#' @section Submitting an extract:
#' * Use [submit_extract()] to submit an extract for processing through the
#'   IPUMS extract API.
#' * [wait_for_extract()] will periodically check the status of a submitted
#'   extract until it is ready to download.
#' * Use [is_extract_ready()] to manually check whether a submitted extract
#'   is ready to download
#'
#' @section Downloading an extract:
#' * Download the data contained in a completed extract with
#'   [download_extract()]
#'
#' @section Revising an extract:
#' * Modify values in an existing extract defiition with [add_to_extract()] and
#' [remove_from_extract()]
#' * Combine definitions from multiple extracts into a single extract with
#' [combine_extracts()]
#'
#' @section Saving an extract:
#' * Save an extract to a JSON-formatted file with [save_extract_as_json()]
#'
#' @name ipums_extract-class
NULL

# > Define extract ----

#' Define an IPUMS USA extract request
#'
#' Define an IPUMS USA extract request to be submitted via the IPUMS microdata
#' extract API. For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   [sample ID values](https://usa.ipums.org/usa-action/samples/sample_ids).
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file (one of
#'   `"fixed_width"`, `"csv"`, `"stata"`, `"spss"`, or `"sas9"`).
#' @param data_structure Currently, this must be `"rectangular"`, which is also
#'   the default. In the future, the API will also support `"hierarchical"`
#'   extracts.
#' @param rectangular_on Currently, this must be `"P"`, indicating that the
#'   extract will be rectangularized on person records. In the future, the API
#'   will also support household-only extracts (`rectangular_on = "H"`).
#'
#' @family ipums_api
#'
#' @return An object of class [`usa_extract`][ipums_extract-class]
#'   containing the extract definition.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' @export
define_extract_usa <- function(description,
                               samples,
                               variables,
                               data_format = c("fixed_width", "csv", "stata",
                                               "spss", "sas9"),
                               data_structure = "rectangular",
                               rectangular_on = "P") {

  data_format <- match.arg(data_format)

  # Remove these once we allow for hierarchical and rectangular on H extracts
  if (data_structure != "rectangular") {
    rlang::abort(
      paste0(
        "Currently, the `data_structure` argument must be equal to ",
        "\"rectangular\"; in the future, the API will also support ",
        "\"hierarchical\" extracts."
      )
    )
  }

  # Note when hierarchical extracts are supported, default rectangular_on
  # should be `NULL` as rectangular_on must be missing for hierarchical extracts
  if (rectangular_on != "P") {
    rlang::abort(
      paste0(
        "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
        "the future, the API will also support `rectangular_on = \"H\"."
      ),
    )
  }

  extract <- new_ipums_extract(
    collection = "usa",
    description = description,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format,
    samples = samples,
    variables = variables
  )

  extract <- validate_ipums_extract(extract)

  extract
}

#' Define an IPUMS CPS extract request
#'
#' Define an IPUMS CPS extract request to be submitted via the IPUMS microdata
#' extract API. For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @inheritParams define_extract_usa
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   [sample ID values](https://cps.ipums.org/cps-action/samples/sample_ids).
#'
#' @family ipums_api
#'
#' @return An object of class [`cps_extract`][ipums_extract-class]
#'   containing the extract definition.
#'
#' @examples
#' my_extract <- define_extract_cps("Example", "cps2020_03s", "YEAR")
#'
#' @export
define_extract_cps <- function(description,
                               samples,
                               variables,
                               data_format = c("fixed_width", "csv", "stata",
                                               "spss", "sas9"),
                               data_structure = "rectangular",
                               rectangular_on = "P") {

  data_format <- match.arg(data_format)

  # Remove these once we allow for hierarchical and rectangular on H extracts
  if (data_structure != "rectangular") {
    rlang::abort(
      paste0(
        "Currently, the `data_structure` argument must be equal to ",
        "\"rectangular\"; in the future, the API will also support ",
        "\"hierarchical\" extracts."
      )
    )
  }

  # Note when hierarchical extracts are supported, default rectangular_on
  # should be `NULL` as rectangular_on must be missing for hierarchical extracts
  if (rectangular_on != "P") {
    rlang::abort(
      paste0(
        "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
        "the future, the API will also support `rectangular_on = \"H\"."
      ),
    )
  }

  extract <- new_ipums_extract(
    collection = "cps",
    description = description,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format,
    samples = samples,
    variables = variables
  )

  extract <- validate_ipums_extract(extract)

  extract
}

#' Define an NHGIS extract request
#'
#' @description
#' Define an extract request object to be submitted via the IPUMS NHGIS
#' extract API.
#'
#' For more information on NHGIS data, click
#' [here](https://www.nhgis.org/data-availability).
#'
#' For the NHGIS FAQ, click
#' [here](https://www.nhgis.org/frequently-asked-questions-faq)
#'
#' For information on the IPUMS NHGIS Extract API, click
#' [here](https://developer.ipums.org/docs/workflows/create_extracts/nhgis_data/)
#'
#' Note that some of the terminology used in the API has been altered
#' for use in the ipumsr package.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table.
#'
#' The following arguments are applied to an extract's `datasets`:
#' * `data_tables`
#' * `years`
#' * `breakdown_values`
#'
#' The following arguments are applied to an extract's `datasets` and
#' `time_series_tables`:
#' * `geog_levels`
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' - If values are passed as a **vector**, they will be
#'   applied to *all* of the datasets and/or time series tables in the extract
#'   definition.
#' - If values are passed as a **named list** or **named vector**, they
#'   will be matched to the datasets and/or time series tables by name. Names
#'   in the provided list should correspond to datasets or time series tables
#'   that exist in the extract definition.
#' - If values are passed as an **unnamed list**, they will be
#'   matched by index to the datasets and/or time series tables in the extract.
#'   That is, the first element will be associated with the first dataset in the
#'   extract definition, the second element with the second dataset in the
#'   extract, etc.
#'     - `geog_levels` applies to both datasets and time series tables.
#'       If an extract contains both datasets and time series tables,
#'       the provided `geog_levels` will be matched first to the extract's
#'       datasets, then to the extract's time series tables.
#'
#' When handling extracts with multiple datasets and/or time series tables,
#' it is safest to explicitly match the subfield values with datasets and
#' time series tables by using the named list syntax.
#'
#' Alternatively, extract definitions for individual datasets and
#' time series tables can be combined using [combine_extracts()].
#'
#' For more information on valid values for these arguments, see
#' [get_nhgis_metadata()].
#'
#' @param description Description of the extract.
#' @param datasets Character vector of
#'   [datasets](https://www.nhgis.org/overview-nhgis-datasets) to include in
#'   the extract.
#' @param data_tables Summary tables to retrieve from the extract's
#'   `datasets`.
#'
#'   Can be provided as a vector or list; see details for syntax options.
#'
#'   Required if an extract includes any `datasets`.
#' @param time_series_tables Character vector of
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to include in the extract.
#' @param geog_levels Geographic levels (for example, `"county"` or `"state"`)
#'   at which to obtain data for the extract's `datasets` or
#'   `time_series_tables`.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   Required if an extract includes any `datasets` or `time_series_tables`.
#' @param years Years for which to obtain the data contained in the extract's
#'   `datasets`. Use `"*"` to select all available years for a given dataset.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   See [get_nhgis_metadata()] to determine if a dataset allows year selection.
#' @param breakdown_values [breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to apply to the requested `data_tables`.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   If more than one breakdown value is requested,
#'   `breakdown_and_data_type_layout` must also be specified.
#' @param geographic_extents Character vector of geographic extents to use for
#'   all of the extract's `datasets`. Use `"*"` to select all available extents.
#'
#'   Required when the extract includes any `geog_levels` that require extent
#'   selection. See [get_nhgis_metadata()] to determine if a geographic level
#'   requires extent selection. (Currently, NHGIS supports extent selection only
#'   for blocks and block groups.)
#' @param shapefiles Character vector of
#'   [shapefiles](https://www.nhgis.org/gis-files) to include in the extract.
#' @param breakdown_and_data_type_layout The desired layout
#'   of any `datasets` that have multiple data types or breakdown values.
#'
#'   * `"single_file"` keeps all data types and breakdown values in one file.
#'   * `"separate_files"` splits each data type or breakdown value into its
#'   own file.
#'
#'   Defaults to `"single_file"`.
#'
#'   Required if any `datasets` included in the extract consist of
#'   multiple data types (for instance, estimates and margins of error) or have
#'   multiple breakdown values specified. See [get_nhgis_metadata()] to
#'   determine whether a requested dataset has multiple data types.
#' @param tst_layout The desired layout of all `time_series_tables` included in
#'   the extract. One of:
#'
#'   * `"time_by_column_layout"` (wide format): rows correspond to geographic
#'     units, columns correspond to different times in the time series
#'   * `"time_by_row_layout"` (long format): rows correspond to a single
#'     geographic unit at a single point in time
#'   * `"time_by_file_layout"`: data for different times are provided in
#'     separate files
#'
#'   Defaults to `"time_by_column_layout`.
#'
#'   Required when an extract includes any `time_series_tables`.
#' @param data_format The desired format of the extract data file. One of:
#'
#'   * `"csv_no_header"` includes only a minimal header in the first row.
#'   * `"csv_header"` includes a second, more descriptive header row.
#'   * `"fixed_width"` provides data in a fixed width format.
#'
#'   Required when an extract includes any `datasets` or `time_series_tables`.
#'
#' @family ipums_api
#'
#' @return An object of class [`nhgis_extract`][ipums_extract-class]
#'   containing the extract definition.
#'
#' @examples
#' define_extract_nhgis(
#'   description = "Test NHGIS extract",
#'   datasets = "1990_STF3",
#'   data_tables = "NP57",
#'   geog_levels = c("county", "tract"),
#' )
#'
#' # For extracts with multiple datasets or time series tables, subfield
#' # arguments are recycled to all datasets:
#' define_extract_nhgis(
#'   description = "Extract with multiple datasets",
#'   datasets = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
#'   data_tables = "B01001",
#'   geog_levels = c("state", "county")
#' )
#'
#' # To attach specific subfield values to each dataset or time series table,
#' # pass a list to the subfield argument.
#' # With an unnamed list, items are matched by index:
#' define_extract_nhgis(
#'   description = "Extract with multiple time series tables",
#'   time_series_tables = c("CW3", "CW5"),
#'   geog_levels = list("state", "county")
#' )
#'
#' # With a named list, items are matched by name:
#' define_extract_nhgis(
#'   description = "Extract with multiple time series tables",
#'   time_series_tables = c("CW3", "CW5"),
#'   geog_levels = list(CW5 = "county", CW3 = "state")
#' )
#'
#' # For extracts with datasets and time series tables,
#' # geog_levels are matched to both sources:
#' define_extract_nhgis(
#'   description = "Extract with datasets and time series tables",
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2"),
#'   time_series_tables = "CL6",
#'   geog_levels = "state"
#' )
#'
#' define_extract_nhgis(
#'   description = "Extract with datasets and time series tables",
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2"),
#'   time_series_tables = "CL6",
#'   geog_levels = list("1990_STF1" = "county", CL6 = "state")
#' )
#'
#' @export
define_extract_nhgis <- function(description = "",
                                 datasets = NULL,
                                 data_tables = NULL,
                                 time_series_tables = NULL,
                                 geog_levels = NULL,
                                 years = NULL,
                                 breakdown_values = NULL,
                                 geographic_extents = NULL,
                                 shapefiles = NULL,
                                 breakdown_and_data_type_layout = NULL,
                                 tst_layout = NULL,
                                 data_format = NULL) {

  n_datasets <- length(datasets)
  n_tsts <- length(time_series_tables)

  if (n_datasets > 0) {
    data_format <- data_format %||% "csv_no_header"
    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      "single_file"
  }

  if (n_tsts > 0) {
    data_format <- data_format %||% "csv_no_header"
    tst_layout <- tst_layout %||%
      "time_by_column_layout"
  }

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be applied to all datasets.",
      call. = FALSE
    )
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = unlist(datasets),
    data_tables = recycle_extract_subfield(
      data_tables,
      datasets
    ),
    time_series_tables = unlist(time_series_tables),
    geog_levels = recycle_extract_subfield(
      geog_levels,
      c(datasets, time_series_tables)
    ),
    years = recycle_extract_subfield(
      years,
      datasets
    ),
    breakdown_values = recycle_extract_subfield(
      breakdown_values,
      datasets
    ),
    geographic_extents = geog_extent_lookup(
      unlist(geographic_extents),
      state_geog_lookup$abbs
    ),
    shapefiles = unlist(shapefiles),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    tst_layout = tst_layout,
    data_format = data_format
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Define an IPUMS extract from a JSON-formatted definition
#'
#' Create an [`ipums_extract`][ipums_extract-class] object based on an extract
#' definition formatted as JSON. For an overview of ipumsr microdata API
#' functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param extract_json Path to a file containing the JSON definition or a
#'   JSON string.
#'
#' @family ipums_api
#' @return An [`ipums_extract`][ipums_extract-class] object.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path)
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
define_extract_from_json <- function(extract_json) {

  collection <- jsonlite::fromJSON(extract_json)$collection

  if (is.null(collection)) {
    stop(
      "Could not determine the collection associated with this extract ",
      "definition. Ensure that the JSON file includes an element containing ",
      "the IPUMS collection associated with the extract. ",
      "(For example, `\"collection\": \"usa\"`)",
      call. = FALSE
    )
  }

  extract_json <- new_ipums_json(extract_json, collection)
  json_api_version <- api_version_from_json(extract_json)

  list_of_extracts <- extract_list_from_json(
    extract_json,
    validate = TRUE
  )

  if (length(list_of_extracts) != 1) {
    stop(
      paste0(
        "`extract_json` should contain the definition of one and only one ",
        "extract"
      ),
      call. = FALSE
    )
  }

  if (is.null(json_api_version)) {
    warning(
      "Could not determine the API version corresponding to the provided ",
      "extract definition. ipumsr is currently configured to submit extract ",
      "requests using API version ", ipums_api_version(collection), ".",
      call. = FALSE
    )
  } else if (ipums_api_version(collection) != json_api_version){
    warning(
      "The extract provided in `extract_json` was made using API version ",
      json_api_version, ". ipumsr is currently configured to submit extract ",
      "requests using API version ", ipums_api_version(collection), ".",
      call. = FALSE
    )
  }

  list_of_extracts[[1]]

}

# > Save extract as json ----

#' Save an [`ipums_extract`][ipums_extract-class] to disk as JSON
#'
#' Save an [`ipums_extract`][ipums_extract-class] to a JSON-formatted file.
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @inheritParams add_to_extract
#' @param file File path to which to write the JSON-formatted extract
#'   definition.
#'
#' @details Note that this function only saves out the properties of an extract
#'   that are required to submit a new extract request, namely, the description,
#'   data structure, data format, samples, variables, and collection.
#' @family ipums_api
#'
#' @return Invisibly returns the file path where the extract definition was
#'   written.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path)
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
save_extract_as_json <- function(extract, file) {
  extract_as_json <- extract_to_request_json(
    extract,
    include_endpoint_info = TRUE
  )
  writeLines(jsonlite::prettify(extract_as_json), con = file)
  invisible(file)
}

#' Submit an extract request via the IPUMS API
#'
#' Given an [`ipums_extract`][ipums_extract-class] object, submit an extract
#' request via the IPUMS API, and return a modified copy of the extract object
#' with the newly-assigned extract number. For an overview of ipumsr microdata API
#' functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param extract If submitting a new extract request, an
#'   [`ipums_extract`][ipums_extract-class] object.
#'
#'   If resubmitting an old request, one of:
#'   * The data collection and extract number formatted as a single string of the
#'     form `"collection:number"`
#'   * The data collection and extract number formatted as a vector of the form
#'     `c("collection", number)`
#'   * The extract number for the collection specified by the
#'     `IPUMS_DEFAULT_COLLECTION` environment variable. See
#'     [set_ipums_default_collection()]
#' @param api_key API key associated with your user account. Defaults to the
#'   value of environment variable `"IPUMS_API_KEY"`.
#'
#' @family ipums_api
#' @return An [`ipums_extract`][ipums_extract-class] object containing the
#'   extract definition and newly-assigned extract number of the submitted
#'   extract.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' # `submit_extract()` returns an ipums_extract object updated to include the
#' # extract number, so it is often useful to name the return object:
#' submitted_extract <- submit_extract(my_extract)
#'
#' # If you didn't capture the return object of submit_extract for your most
#' # recent extract, you can recover that information with:
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # View the extract number
#' submitted_extract$number
#'
#' # Check if submitted extract is ready
#' is_extract_ready(submitted_extract) # returns TRUE or FALSE
#'
#' # Or have R check periodically until the extract is ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
#'
#' @export
submit_extract <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  if (!inherits(extract, "ipums_extract")) {
    extract <- get_extract_info(extract)
  } else {
    extract <- validate_ipums_extract(extract)
  }

  response <- ipums_api_json_request(
    "POST",
    collection = extract$collection,
    path = NULL,
    body = extract_to_request_json(extract),
  )

  # extract_list_from_json() always returns a list of extracts, but in
  # this case there is always only one, so pluck it out
  extract <- extract_list_from_json(response)[[1]]

  # For NHGIS extracts, API does not currently return a status upon submission.
  # extract_list_from_json() will incorrectly populate this with "unsubmitted".
  if (extract$status == "unsubmitted") extract$status <- "queued"

  message(
    sprintf(
      "Successfully submitted %s extract number %d",
      format_collection_for_printing(extract$collection),
      extract$number
    )
  )

  invisible(extract)

}

#' Get information about a submitted extract
#'
#' @description
#' Get information about previously submitted extracts via the IPUMS API.
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param extract One of:
#'
#' * An [`ipums_extract`][ipums_extract-class] object
#' * An IPUMS collection with API support
#' * The data collection and extract number formatted as a single string of the
#'   form `"collection:number"`
#' * The data collection and extract number formatted as a vector of the form
#'   `c("collection", number)`
#' * The extract number for the collection specified by the
#'   `IPUMS_DEFAULT_COLLECTION` environment variable. See
#'   [set_ipums_default_collection()]
#'
#' The extract number does not need to be zero-padded (e.g., use `"usa:1"`
#' or `c("usa", "1")`, not `"usa:00001"` or `c("usa", "00001")`).
#' See below for examples of each form.
#'
#' For a list of codes used to refer to each collection, see
#' [ipums_data_collections()].
#' @param how_many If the provided `extract` is an IPUMS collection, the number
#'   of recent extracts for which to retrieve information.
#'   Defaults to 10 extracts.
#' @param table If the provided `extract` is an IPUMS collection, a logical
#'   indicating whether to return recent extract information as a
#'   [`tibble`][tibble::tbl_df-class] (`TRUE`) or a list of
#'   [`ipums_extract`][ipums_extract-class] objects (`FALSE`). Defaults to
#'   `FALSE`.
#' @inheritParams submit_extract
#'
#' @family ipums_api
#' @return An [`ipums_extract`][ipums_extract-class] object.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Get info by supplying an ipums_extract object:
#' get_extract_info(submitted_extract)
#'
#' # Get info by supplying the data collection and extract number, as a string:
#' get_extract_info("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # Get info by supplying the data collection and extract number, as a vector:
#' get_extract_info(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' get_extract_info(1)
#'
#' # The default collection will automatically be used even if no extract
#' # number is supplied:
#' get_extract_info(how_many = 3, table = TRUE)
#' }
#'
#' @export
get_extract_info <- function(extract = NULL,
                             how_many = NULL,
                             table = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(
    extract %||% get_default_collection(),
    collection_ok = TRUE
  )

  if (inherits(extract, "ipums_extract")) {
    extract <- validate_ipums_extract(extract)
    is_collection <- FALSE

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
  } else {
    is_collection <- is.na(extract$number)
  }

  # If a collection, get recent extracts. Otherwise get a single extract.
  if (is_collection) {
    queries <- list(limit = how_many %||% 10)
    path <- NULL
  } else {
    queries <- NULL
    path <- paste0(api_extracts_path(), "/", extract$number)
  }

  response <- ipums_api_json_request(
    "GET",
    collection = tolower(extract$collection),
    path = path,
    queries = queries,
    api_key = api_key
  )

  extract_info <- extract_list_from_json(response)

  if (table) {
    extract_info <- extract_list_to_tbl(extract_info)
  } else if (!is_collection) {
    # Collection request will always return a list, but a single extract should
    # return an `ipums_extract` object
    extract_info <- extract_info[[1]]
  }

  extract_info

}

#' Wait for extract to finish
#'
#' Wait for an extract to finish by periodically checking its status via the
#' IPUMS API and returning when the extract is ready to download. For an
#' overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @inheritParams define_extract_usa
#' @inheritParams download_extract
#' @inheritParams get_extract_info
#' @inheritParams submit_extract
#' @param extract One of:
#'
#' * An [`ipums_extract`][ipums_extract-class] object
#' * The data collection and extract number formatted as a single string of the
#'   form `"collection:number"`
#' * The data collection and extract number formatted as a vector of the form
#'   `c("collection", number)`
#' * The extract number for the collection specified by the
#'   `IPUMS_DEFAULT_COLLECTION` environment variable. See
#'   [set_ipums_default_collection()]
#'
#' The extract number does not need to be zero-padded (e.g., use `"usa:1"`
#' or `c("usa", "1")`, not `"usa:00001"` or `c("usa", "00001")`).
#' See below for examples of each form.
#'
#' For a list of codes used to refer to each collection, see
#' [ipums_data_collections()].
#' @param initial_delay_seconds How many seconds to wait before first status
#'   check.
#' @param max_delay_seconds Maximum seconds to wait between status checks. The
#'   function doubles the wait time after each check, but will cap the wait
#'   time at this maximum value (300 seconds, or 5 minutes, by default).
#' @param timeout_seconds Maximum total number of seconds to continue waiting
#'   for the extract before throwing an error. Defaults to 10,800 seconds (three
#'   hours).
#' @param verbose If `TRUE`, the default, messages will be printed at the
#'   beginning of each wait interval with the current wait time, each time the
#'   status of the extract is checked, and when the extract is ready to
#'   download. Setting this argument to `FALSE` will silence these
#'   messages.
#'
#' @family ipums_api
#' @return An [`ipums_extract`][ipums_extract-class] object containing the
#'   extract definition and the URLs from which to download extract files.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Wait for extract by supplying ipums_extract object:
#' downloadable_extract <- wait_for_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' downloadable_extract <- wait_for_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' downloadable_extract <- wait_for_extract(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' downloadable_extract <- wait_for_extract(1)
#' }
#'
#' @export
wait_for_extract <- function(extract,
                             initial_delay_seconds = 0,
                             max_delay_seconds = 300,
                             timeout_seconds = 10800,
                             verbose = TRUE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  stopifnot(is.numeric(initial_delay_seconds))
  stopifnot(is.numeric(max_delay_seconds))
  stopifnot(is.null(timeout_seconds) || is.numeric(timeout_seconds))

  extract <- standardize_extract_identifier(extract)

  current_delay <- initial_delay_seconds
  is_timed_out <- FALSE
  is_finished <- FALSE
  is_error_state <- FALSE
  err_message <- "Unknown Error"

  wait_start <- Sys.time()

  while (!is_timed_out && !is_finished && !is_error_state) {
    if (current_delay > 0) {
      if (verbose) {
        message(paste("Waiting", current_delay, "seconds..."))
      }

      Sys.sleep(current_delay)
    }

    if (verbose) {
      message("Checking extract status...")
    }

    extract <- get_extract_info(extract, api_key = api_key)

    is_downloadable <- is_extract_ready(extract)

    is_failed <- !(is_downloadable ||
                     extract$status %in% c("queued", "started", "produced"))
    is_timed_out <- !is.null(timeout_seconds) &&
      as.numeric(Sys.time() - wait_start, units = "secs") > timeout_seconds

    if (is_downloadable) {
      if (verbose) {
        message(
          paste0(
            format_collection_for_printing(extract$collection),
            " extract ", extract$number, " ready to download"
          )
        )
      }
      is_finished <- TRUE
    } else if (is_failed) {
      err_message <- paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number,
        " has finished, but is not in a downloadable state, likely ",
        "because the extract files have been removed from IPUMS servers. Try ",
        "resubmitting the extract with `submit_extract()`."
      )
      is_error_state <- TRUE
    } else if (is_timed_out) {
      err_message <- "Max timeout elapsed"
    }

    if (current_delay == 0) {
      current_delay <- 10
    } else {
      current_delay <- min(c(current_delay * 2, max_delay_seconds))
    }
  }

  if (!is_finished) {
    stop(err_message)
  }

  extract
}

#' Check if a submitted extract is ready to download
#'
#' @description
#' This function uses the IPUMS API to check whether the given extract is ready
#' to download, returning `TRUE` for extracts that are ready and `FALSE` for
#' those that are not.
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' This function checks the `"download_links"` element of the supplied extract
#' to determine whether the extract files are available to download.
#'
#' The "status" of a submitted extract is one of `"queued"`, `"started"`,
#' `"produced"`, `"canceled"`, `"failed"`, or `"completed"`. Only completed
#' extracts can be ready to download, but not all completed extracts are ready
#' to download, because extract files are subject to removal from the IPUMS
#' servers 72 hours after they first become available. Completed extracts older
#' than 72 hours will still have a "completed" status, but will return `FALSE`
#' from `is_extract_ready()`, because the extract files are no longer available.
#'
#' @inheritParams wait_for_extract
#'
#' @family ipums_api
#' @return A logical vector of length one.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Check if extract is ready by supplying an ipums_extract object:
#' is_extract_ready(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' is_extract_ready("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' is_extract_ready(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' is_extract_ready(1)
#' }
#'
#' @export
is_extract_ready <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  # First check if extract object already contains download info...
  if (inherits(extract, "ipums_extract") &&
      extract_is_completed_and_has_links(extract)) {
    return(TRUE)
  }

  # ... if it doesn't contain download info, make sure we have the latest
  # status by fetching it via the API and checking again
  extract <- get_extract_info(extract, api_key = api_key)

  extract_is_completed_and_has_links(extract)

}

#' Download an IPUMS data extract
#'
#' @description
#' Download an IPUMS data extract via the IPUMS API.
#'
#' For an overview of ipumsr
#' microdata API functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' For NHGIS extracts, data files and GIS files (shapefiles) will be saved in
#' separate zip archives. `download_extract()` will return a character vector
#' including the file paths to all downloaded files.
#'
#' For microdata extracts, only the file path to the downloaded .xml DDI file
#' will be returned, as it is sufficient for loading the data provided in the
#' associated .gz data file.
#'
#' @inheritParams wait_for_extract
#' @inheritParams define_extract_usa
#' @inheritParams submit_extract
#' @param download_dir In what folder should the downloaded files be saved?
#'   Defaults to current working directory.
#' @param overwrite Logical indicating whether to overwrite files that already
#'   exist. Defaults to `FALSE`.
#'
#' @family ipums_api
#' @return Invisibly returns the path(s) to the files required to read the data
#'   requested in the extract.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Download extract by supplying an ipums_extract object:
#' path_to_ddi_file <- download_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' path_to_ddi_file <- download_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' path_to_ddi_file <- download_extract(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' download_extract(1)
#' }
#'
#' @export
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  # Make sure we get latest extract status, but also make sure we don't check
  # the status twice
  is_ipums_extract_object <- inherits(extract, "ipums_extract")

  if (is_ipums_extract_object && extract_is_completed_and_has_links(extract)) {
    is_downloadable <- TRUE
  } else {
    extract <- get_extract_info(extract, api_key = api_key)
    is_downloadable <- extract_is_completed_and_has_links(extract)
  }

  if (!is_downloadable) {
    stop(
      paste0(
        format_collection_for_printing(extract$collection), " extract number ",
        extract$number, " is not ready to download"
      ),
      call. = FALSE
    )
  }

  download_dir <- normalizePath(download_dir, winslash = "/", mustWork = FALSE)
  download_dir_doesnt_exist <- !dir.exists(download_dir)

  if (download_dir_doesnt_exist) {
    stop("The directory ", download_dir, " does not exist.", call. = FALSE)
  }

  ipums_extract_specific_download(extract, download_dir, overwrite, api_key)

}

#' Add values to an existing IPUMS extract
#'
#' @description
#' Add or replace values in an existing `ipums_extract` object.
#' This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To add to an **NHGIS** extract, click [here][add_to_extract.nhgis_extract]
#' - To add to a **USA** extract, click [here][add_to_extract.usa_extract]
#' - To add to a **CPS** extract, click [here][add_to_extract.cps_extract]
#'
#' In general, for a given collection, the arguments to `add_to_extract()`
#' are identical to those used when defining an extract for that collection. For
#' more information about defining an extract, click [here][define_extract].
#'
#' To remove existing values from an extract, see [remove_from_extract()].
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   add to the extract. Argument names should correspond to the available
#'   arguments in the extract definition function for the class of the extract
#'   specified in `extract`.
#'
#' @family ipums_api
#'
#' @return An object of the same class as `extract` containing the modified
#'   extract definition
#'
#' @export
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
}

#' Add values to an existing IPUMS USA extract
#'
#' @description
#' Add new values or replace existing values in an IPUMS USA extract. All
#' fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS USA extract, see
#' [`remove_from_extract()`][remove_from_extract.usa_extract].
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_usa
#' @inheritParams add_to_extract
#' @param samples Character vector of samples to add to the extract.
#'   Values should correspond to [USA sample ID values]
#'   (https://usa.ipums.org/usa-action/samples/sample_ids)
#' @param variables Character vector of variables to add to the extract.
#' @param ... Ignored
#'
#' @family ipums_api
#' @return A modified `usa_extract` object
#'
#' @export
add_to_extract.usa_extract <- function(extract,
                                       description = NULL,
                                       samples = NULL,
                                       variables = NULL,
                                       data_format = NULL,
                                       ...) {

  add_to_extract_micro(
    extract,
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    ...
  )

}

#' Add values to an existing IPUMS CPS extract
#'
#' @description
#' Add new values or replace existing values in an IPUMS CPS extract. All
#' fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS CPS extract, see
#' [`remove_from_extract()`][remove_from_extract.cps_extract].
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_cps
#' @inheritParams add_to_extract
#' @param samples Character vector of samples to add to the extract.
#'   Values should correspond to [CPS sample ID values]
#'   (https://cps.ipums.org/cps-action/samples/sample_ids)
#' @param variables Character vector of variables to add to the extract.
#' @param ... Ignored
#'
#' @family ipums_api
#' @return A modified `cps_extract` object
#'
#' @export
add_to_extract.cps_extract <- function(extract,
                                       description = NULL,
                                       samples = NULL,
                                       variables = NULL,
                                       data_format = NULL,
                                       ...) {

  add_to_extract_micro(
    extract,
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    ...
  )

}

#' Add values to an existing IPUMS NHGIS extract
#'
#' @description
#' Add new values or replace existing values in an IPUMS NHGIS extract. All
#' fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS NHGIS extract, see
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract].
#'
#' In general, adding to an extract follows the same syntax conventions as used
#' in [`define_extract_nhgis()`][define_extract_nhgis]. See details for more
#' information on how values passed to dataset and time series table subfields
#' are interpreted.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table.
#'
#' The following arguments are applied to an extract's `datasets`:
#' * `data_tables`
#' * `years`
#' * `breakdown_values`
#'
#' The following arguments are applied to an extract's `datasets` and
#' `time_series_tables`:
#' * `geog_levels`
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' - If values are passed as a **vector**, they will be
#'   applied to all of the `datasets` and/or `time_series_tables`
#'   included in the call to `add_to_extract()`. If no `datasets` or
#'   `time_series_tables` are specified, the values will be applied to all
#'   `datasets` and/or `time_series_tables` that exist in the extract.
#' - If values are passed as an **unnamed list**, they will be
#'   matched to the `datasets` and/or `time_series_table` included in the call
#'   to `add_to_extract()` by index. For instance, the first element will be
#'   associated with the first value provided to the `datasets` argument,
#'   the second element with the second value in `datasets`, etc. If no
#'   `datasets` or `time_series_tables` are specified, the values will be
#'   matched to all `datasets` and/or `time_series_tables` that exist in the
#'   extract.
#'     - `geog_levels` applies to both `datasets` and `time_series_tables`.
#'       If values are passed to both the `datasets` and `time_series_tables`
#'       arguments, the provided `geog_levels` will be matched first to the
#'       provided `datasets`, then to the provided `time_series_tables`.
#' - If values are passed as a **named list** or **named vector**, they
#'   will be matched to the `datasets` and/or `time_series_tables` by name.
#'   Names can correspond to any `datasets` or `time_series_tables` that exist
#'   in the extract definition, regardless of whether they are currently being
#'   added to the extract or already exist in the extract.
#'
#' For extract fields that take a single value, `add_to_extract()` will
#' replace the existing value with the new value provided for that field.
#' It is not necessary to first remove this value using
#' `remove_from_extract`.
#'
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams add_to_extract
#' @param datasets Character vector of
#'   [datasets][https://www.nhgis.org/overview-nhgis-datasets] to add to
#'   the extract. Any specified `datasets` that already exist in the extract
#'   will be modified by the values provided to dataset subfields.
#'   See details.
#' @param data_tables Summary tables to retrieve from the specified
#'   `datasets`. If no `datasets` are specified, values apply to the `datasets`
#'   that already exist in the extract.
#'
#'   Can be provided as a vector or list; see details for syntax options.
#'
#'   Required if any new `datasets` are specified.
#' @param time_series_tables Character vector of
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to add to the extract. Any specified `time_series_tables` that already
#'   exist in the extract will be modified by the values provided to
#'   time series table subfields. See details.
#' @param geog_levels Geographic levels (for example, `"county"` or `"state"`)
#'   to add for the specified `datasets` or `time_series_tables`. If no
#'   `datasets` or `time_series_tables` are specified, values apply to the
#'   `datasets` and `time_series_tables` that already exist in the extract.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   Required if any `datasets` or `time_series_tables` are specified.
#' @param years Years to add for the specified `datasets`. If no `datasets` are
#'   specified, values apply to the `datasets` that already exist in the
#'   extract. Use `"*"` to select all available years for a given dataset.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   See [get_nhgis_metadata()] to determine if a dataset allows year selection.
#' @param breakdown_values [breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to add for the specified `datasets`. If no `datasets` are specified, values
#'   apply to the `datasets` that already exist in the extract.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#'
#'   If more than one breakdown value is requested,
#'   `breakdown_and_data_type_layout` must also be specified.
#' @param geographic_extents Character vector of geographic extents to use for
#'   all of the extract's `datasets`. Use `"*"` to select all available extents.
#'
#'   Required when the extract includes any `geog_levels` that require extent
#'   selection. See [get_nhgis_metadata()] to determine if a geographic level
#'   requires extent selection. (Currently, NHGIS supports extent selection only
#'   for blocks and block groups.)
#' @param shapefiles Character vector of
#'   [shapefiles](https://www.nhgis.org/gis-files) to add to the extract.
#' @param ... Ignored
#'
#' @return A modified `nhgis_extract` object
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2"),
#'   geog_levels = "county"
#' )
#'
#' # Modify existing datasets in extract:
#' extract <- add_to_extract(
#'   extract,
#'   data_tables = "NP3"
#' )
#'
#' # Add new dataset. New subfields will only be attached to datasets specified
#' # in the datasets argument:
#' extract <- add_to_extract(
#'   extract,
#'   datasets = "1980_STF1",
#'   data_tables = "NT1A",
#'   geog_levels = c("county", "state")
#' )
#'
#' # Modify existing datasets.
#' # Vectors recycle to all datasets, lists match by index or name.
#' add_to_extract(
#'   extract,
#'   data_tables = list("1990_STF1" = "NP4", "1980_STF1" = "NT1B"),
#'   geog_levels = "nation"
#' )
#'
#' @export
add_to_extract.nhgis_extract <- function(extract,
                                         description = NULL,
                                         datasets = NULL,
                                         data_tables = NULL,
                                         time_series_tables = NULL,
                                         geog_levels = NULL,
                                         years = NULL,
                                         breakdown_values = NULL,
                                         geographic_extents = NULL,
                                         shapefiles = NULL,
                                         breakdown_and_data_type_layout = NULL,
                                         tst_layout = NULL,
                                         data_format = NULL,
                                         ...) {

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be applied to all datasets.",
      call. = FALSE
    )
  }

  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    warning(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified: `",
      paste0(names(dots), collapse = "`, `"), "`",
      call. = FALSE
    )
  }

  # We will only recycle the child fields for the newly-specified parent fields
  # but want all of the parent fields in the final extract.
  all_ds <- union(extract$datasets, unlist(datasets))
  all_tst <- union(extract$time_series_tables, unlist(time_series_tables))

  new_ds <- datasets %||% all_ds
  new_ds_tst <- c(datasets, time_series_tables) %||% c(all_ds, all_tst)

  # Set defaults for extracts that may not already have them included
  if (!is.null(all_ds)) {
    data_format <- data_format %||%
      extract$data_format %||%
      "csv_no_header"

    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      extract$breakdown_and_data_type_layout %||%
      "single_file"
  }

  if (!is.null(all_tst)) {
    data_format <- data_format %||%
      extract$data_format %||%
      "csv_no_header"

    tst_layout <- tst_layout %||%
      extract$tst_layout %||%
      "time_by_column_layout"
  }

  # Construct extract, recycling new child fields to new parent fields and
  # combining with existing parent/child fields
  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description %||% extract$description,
    datasets = all_ds,
    data_tables = reduce_list_by_name(
      c(
        extract$data_tables,
        recycle_extract_subfield(data_tables, new_ds)
      ),
      name_order = all_ds
    ),
    time_series_tables = all_tst,
    geog_levels = reduce_list_by_name(
      c(
        extract$geog_levels,
        recycle_extract_subfield(geog_levels, new_ds_tst)
      ),
      name_order = c(all_ds, all_tst)
    ),
    years = reduce_list_by_name(
      c(
        extract$years,
        recycle_extract_subfield(years, new_ds)
      ),
      name_order = all_ds
    ),
    breakdown_values = reduce_list_by_name(
      c(
        extract$breakdown_values,
        recycle_extract_subfield(breakdown_values, new_ds)
      ),
      name_order = all_ds
    ),
    geographic_extents = geog_extent_lookup(
      union(extract$geographic_extents, unlist(geographic_extents)),
      state_geog_lookup$abbs
    ),
    shapefiles = union(extract$shapefiles, unlist(shapefiles)),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    tst_layout = tst_layout,
    data_format = data_format
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Remove values from an existing IPUMS extract
#'
#' @description
#' Remove values for specific fields in an existing `ipums_extract`
#' object. This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To remove from an **NHGIS** extract, click
#'   [here][remove_from_extract.nhgis_extract]
#' - To remove from a **USA** extract, click '
#'   [here][remove_from_extract.usa_extract]
#' - To remove from a **CPS** extract, click
#'   [here][remove_from_extract.cps_extract]
#'
#' In general, for a given collection, the arguments to
#' `remove_from_extract()` are identical to those used when defining an
#' extract for that collection. For more about defining an extract, click
#' [here][define_extract].
#'
#' To add new values to an extract, see [add_to_extract()].
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   remove from the extract. The available arguments correspond to the
#'   available arguments in the extract definition function for the class of the
#'   extract specified in `extract`.
#'
#' @family ipums_api
#'
#' @return An object of the same class as `extract` containing the modified
#'   extract definition
#'
#' @export
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' Remove values from an existing IPUMS USA extract
#'
#' @description
#' Remove existing values from an IPUMS USA extract. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS USA extract, see
#' [`add_to_extract()`][add_to_extract.usa_extract].
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_usa
#' @inheritParams remove_from_extract
#' @param samples Character vector of samples to remove from the extract.
#' @param variables Character vector of variables to remove from the extract.
#' @param ... Ignored
#'
#' @family ipums_api
#' @return A modified `usa_extract` object
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = "YEAR"
#' )
#'
#' revised_usa_extract <- remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a"
#' )
#'
#' revised_usa_extract
#'
#' @export
remove_from_extract.usa_extract <- function(extract,
                                            samples = NULL,
                                            variables = NULL,
                                            ...) {

  remove_from_extract_micro(
    extract = extract,
    samples = samples,
    variables = variables,
    ...
  )

}

#' Remove values from an existing IPUMS CPS extract
#'
#' @description
#' Remove existing values from an IPUMS CPS extract. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS CPS extract, see
#' [`add_to_extract()`][add_to_extract.cps_extract].
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_cps
#' @inheritParams remove_from_extract
#' @param samples Character vector of samples to remove from the extract.
#' @param variables Character vector of variables to remove from the extract.
#' @param ... Ignored
#'
#' @family ipums_api
#' @return A modified `cps_extract` object
#'
#' @examples
#' cps_extract <- define_extract_cps(
#'   description = "CPS example",
#'   samples = c("cps2019_03s", "cps2020_03s"),
#'   variables = "YEAR"
#' )
#'
#' revised_cps_extract <- remove_from_extract(
#'   cps_extract,
#'   samples = "cps2020_03s"
#' )
#'
#' revised_cps_extract
#'
#' @export
remove_from_extract.cps_extract <- function(extract,
                                            samples = NULL,
                                            variables = NULL,
                                            ...) {

  remove_from_extract_micro(
    extract = extract,
    samples = samples,
    variables = variables,
    ...
  )

}

#' Remove values from an existing NHGIS extract
#'
#' @description
#' Remove existing values from an IPUMS NHGIS extract. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS NHGIS extract, see
#' [`add_to_extract()`][add_to_extract.nhgis_extract]. When replacing values,
#' it is best to first add new values using
#' `add_to_extract()` before removing values with
#' `remove_from_extract()`. This limits the possibility of producing a
#' temporarily invalid extract specification.
#'
#' In general, removing from an extract follows the same syntax conventions as
#' used in [`define_extract_nhgis()`][define_extract_nhgis]. See details for
#' more information on how values passed to dataset and time series table
#' subfields are interpreted.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table.
#'
#' The following arguments are applied to an extract's `datasets`:
#' * `data_tables`
#' * `years`
#' * `breakdown_values`
#'
#' The following arguments are applied to an extract's `datasets` and
#' `time_series_tables`:
#' * `geog_levels`
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' - If values are passed as a **vector**, they will be
#'   removed from all of the `datasets` and/or `time_series_tables`
#'   that exist in the extract.
#' - If values are passed as an **unnamed list**, they will be
#'   matched by index to the `datasets` and/or `time_series_table` that
#'   exist in the extract and removed from those `datasets` and/or
#'   `time_series_tables`. For instance, the first element will be associated
#'   with the first dataset in the extract, the second element with the second
#'   dataset, etc.
#'     - `geog_levels` applies to both `datasets` and `time_series_tables`.
#'       When matching by index, the provided `geog_levels` will be matched
#'       first to the extract's `datasets`, then to the extract's
#'       `time_series_tables`.
#' - If values are passed as a **named list** or **named vector**, they
#'   will be matched to (and removed from) the `datasets` and/or
#'   `time_series_tables` by name.
#'
#' Importantly, subfields are modified *after* the removal of any `datasets`
#' and `time_series_tables` specified in `datasets` or
#' `time_series_tables` arguments. This can cause confusion if providing
#' unnamed lists to subfield arguments, as the index position of each dataset
#' and/or time series table in the extract may shift after removing the
#' provided `datasets` and `time_series_tables`. It is safest to use the named
#' list syntax for subfields to avoid this ambiguity.
#'
#' Any extract fields that are rendered irrelevant after modifying the extract
#' will be automatically removed. (For instance, if all `time_series_tables`
#' are removed from an extract, `tst_layout` will also be
#' removed.) Thus, it is not necessary to explicitly remove these values. To
#' replace the existing values for these fields, see
#' [`add_to_extract()`][add_to_extract.nhgis_extract]
#'
#' Note that it is possible to produce invalid extracts using
#' `remove_from_extract()` (for instance, an extract that includes a
#' time series table without associated geographic levels). This can occur if
#' you intend to replace the existing values for a required extract field.
#' If the goal is not to remove values but to replace them,
#' it is best to first use `add_to_extract()`,
#' then `remove_from_extract()`, as this will avoid the possibility
#' of temporarily producing an invalid extract.
#'
#' If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams remove_from_extract
#' @param datasets Character vector of
#'   [datasets][https://www.nhgis.org/overview-nhgis-datasets] to remove from
#'   the extract. All dataset subfields associated with the specified
#'   `datasets` will also be removed. See details.
#' @param data_tables Summary tables to remove from the specified `datasets`.
#'
#'   Can be provided as a vector or list; see details for syntax options.
#' @param time_series_tables Character vector of
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to remove from the extract. All time series table subfields associated
#'   with the specified `time_series_tables` will also be removed. See details.
#' @param geog_levels Geographic levels to remove from the specified
#'   `datasets` and `time_series_tables`. If no
#'   `datasets` or `time_series_tables` are specified, values apply to the
#'   `datasets` and `time_series_tables` that already exist in the extract.
#'
#'   Can be provided as a vector or list; see details for syntax options.
#' @param years Years to remove from the specified `datasets`. If no
#'   `datasets` are specified, values apply to the `datasets` that already
#'   exist in the extract.
#'
#'   Can be provided as a vector or list; see details for syntax options.
#' @param breakdown_values [breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to remove from the specified `datasets`. If no `datasets` are specified,
#'   values apply to the `datasets` that already exist in the extract.
#'
#'   Can be provided as a vector or a list; see details for syntax options.
#' @param geographic_extents Geographic extents to remove from the extract.
#' @param shapefiles Character vector of
#'   [shapefiles](https://www.nhgis.org/gis-files) to remove from the extract.
#' @param ... Ignored
#'
#' @return A modified `nhgis_extract` object
#'
#' @examples
#'
#' library(dplyr)
#'
#' extract <- define_extract_nhgis(
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2", "NP3"),
#'   geog_levels = "county",
#' )
#'
#' # Remove tables from existing datasets in extract:
#' remove_from_extract(
#'   extract,
#'   data_tables = "NP3"
#' )
#'
#' extract2 <- define_extract_nhgis(
#'   time_series_tables = c("CW3", "CW5"),
#'   geog_levels = c("state", "county")
#' )
#'
#' # Remove an entire time series table:
#' remove_from_extract(
#'   extract2,
#'   time_series_tables = "CW3"
#' )
#'
#' # Use a list to remove subfield values from specific datasets or time
#' # series tables.
#' remove_from_extract(
#'   extract2,
#'   geog_levels = list(CW5 = "state", CW3 = "county")
#' )
#'
#' # Replace values by using `add_to_extract()` first:
#' extract2 %>%
#'   add_to_extract(
#'     geog_levels = list(CW3 = "tract")
#'   ) %>%
#'   remove_from_extract(
#'     geog_levels = list(CW3 = c("county", "state"))
#'   )
#'
#' @export
remove_from_extract.nhgis_extract <- function(extract,
                                              datasets = NULL,
                                              data_tables = NULL,
                                              time_series_tables = NULL,
                                              shapefiles = NULL,
                                              geog_levels = NULL,
                                              years = NULL,
                                              breakdown_values = NULL,
                                              geographic_extents = NULL,
                                              ...) {

  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    warning(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed: `",
      paste0(names(dots), collapse = "`, `"), "`\n",
      "See `add_to_extract()` to replace existing values in applicable extract ",
      "fields.",
      call. = FALSE
    )
  }

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be removed from all datasets.",
      call. = FALSE
    )
  }

  if (any(!datasets %in% extract$datasets)) {
    rlang::warn(
      paste0(
        "Some `datasets` (\"",
        paste0(setdiff(datasets, extract$datasets), collapse = "\", \""),
        "\") could not be removed because they were not found among this ",
        "extract\'s `datasets`"
      )
    )
  }

  if (any(!time_series_tables %in% extract$time_series_tables)) {
    rlang::warn(
      paste0(
        "Some `time_series_tables` (\"",
        paste0(
          setdiff(time_series_tables, extract$time_series_tables),
          collapse = "\", \""
        ),
        "\") could not be removed because they were not found among this ",
        "extract\'s `time_series_tables`"
      )
    )
  }

  new_ds <- setdiff_null(extract$datasets, datasets)
  new_tst <- setdiff_null(extract$time_series_tables, time_series_tables)

  # If removal results in extract with no ds/tst, remove irrelevant values
  if (is_null(new_ds)) {
    breakdown_and_data_type_layout <- NULL
    geographic_extents <- NULL
  } else {
    breakdown_and_data_type_layout <- extract$breakdown_and_data_type_layout
    geographic_extents <- setdiff_null(
      extract$geographic_extents,
      geog_extent_lookup(
        unlist(geographic_extents),
        state_geog_lookup$abbs
      )
    )
  }

  if (is_null(new_tst)) {
    tst_layout <- NULL
  } else {
    tst_layout <- extract$tst_layout
  }

  if (is_null(new_ds) && is_null(new_tst)) {
    data_format <- NULL
  } else {
    data_format <- extract$data_format
  }

  ds_args <- list(
    data_tables = data_tables,
    years = years,
    breakdown_values = breakdown_values
  )

  ds_tst_args <- list(
    geog_levels = geog_levels
  )

  ds_args <- purrr::set_names(
    purrr::map(
      names(ds_args),
      ~if (is_null(extract[[.x]])) {
        ds_args[.x] <- NULL
      } else {
        ds_args[[.x]] <- reduce_list_by_name(
          c(extract[[.x]][new_ds],
            recycle_extract_subfield(ds_args[[.x]], new_ds)),
          setdiff_null
        )
      }
    ),
    names(ds_args)
  )

  ds_tst_args <- purrr::set_names(
    purrr::map(
      names(ds_tst_args),
      ~if (is_null(extract[[.x]])) {
        ds_tst_args[.x] <- NULL
      } else {
        ds_tst_args[[.x]] <- reduce_list_by_name(
          c(extract[[.x]][c(new_ds, new_tst)],
            recycle_extract_subfield(ds_tst_args[[.x]], c(new_ds, new_tst))),
          setdiff_null
        )
      }
    ),
    names(ds_tst_args)
  )

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = extract$description,
    datasets = new_ds,
    data_tables = ds_args$data_tables,
    time_series_tables = new_tst,
    geog_levels = ds_tst_args$geog_levels,
    years = ds_args$years,
    breakdown_values = ds_args$breakdown_values,
    geographic_extents = geographic_extents,
    shapefiles = setdiff_null(extract$shapefiles, unlist(shapefiles)),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    tst_layout = tst_layout,
    data_format = data_format
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Combine IPUMS extracts into a single extract definition
#'
#' Creates a single extract that includes all of the specifications included in
#' a set of `ipums_extract` objects.
#'
#' @details
#' Values that exist in more than one of the provided extracts will be
#' de-duplicated such that only one entry for the duplicated specification is
#' included in the final extract.
#'
#' Some extract fields can only take a single value. In the event that the
#' extracts being combined each have different values for such a field, the
#' value found in the first extract provided to `...` is retained in the final
#' extract.
#'
#' To add or remove values in extract fields, see [add_to_extract] or
#' [remove_from_extract].
#'
#' @param ... Arbitrary number of `ipums_extract` objects to be combined. All
#'   extracts must belong to the same collection.
#'
#' @return An `ipums_extract` object of the same collection as the extracts
#'   provided in `...` containing the combined extract definition.
#'
#' @export
#'
#' @examples
#' ext1 <- define_extract_nhgis(
#'   datasets = "2011_2015_ACS5a",
#'   data_tables = "B00001",
#'   geog_levels = "county"
#' )
#'
#' ext2 <- define_extract_nhgis(
#'   datasets = c("2011_2015_ACS5a", "2012_2016_ACS5a"),
#'   data_tables = c("B00002", "B01001"),
#'   geog_levels = "county"
#' )
#'
#' combine_extracts(ext1, ext2)
combine_extracts <- function(...) {
  UseMethod("combine_extracts")
}

#' @export
combine_extracts.usa_extract <- function(...) {

  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~.x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    # Warnings are not as relevant when combining extracts.
    # TODO: we can also remove warnings from add to extract?
    ~suppressWarnings(
      add_to_extract(
        .x,
        description = .x$description,
        samples = .y$samples,
        variables = .y$variables,
        data_format = .x$data_format %||% .y$data_format,
        data_structure = .x$data_structure %||% .y$data_structure,
        rectangular_on = .x$rectangular_on %||% .y$rectangular_on
      )
    )
  )

  extract

}

#' @export
combine_extracts.cps_extract <- function(...) {

  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~.x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    # Warnings are not as relevant when combining extracts.
    # We can also remove warnings from add to extract...
    ~suppressWarnings(
      add_to_extract(
        .x,
        description = .x$description,
        samples = .y$samples,
        variables = .y$variables,
        data_format = .x$data_format %||% .y$data_format,
        data_structure = .x$data_structure %||% .y$data_structure,
        rectangular_on = .x$rectangular_on %||% .y$rectangular_on
      )
    )
  )

  extract

}

#' @export
combine_extracts.nhgis_extract <- function(...) {

  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~.x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    ~add_to_extract(
      .x,
      description = .x$description,
      datasets = .y$datasets,
      data_tables = .y$data_tables,
      time_series_tables = .y$time_series_tables,
      geog_levels = .y$geog_levels,
      years = .y$years,
      breakdown_values = .y$breakdown_values,
      geographic_extents = .y$geographic_extents,
      breakdown_and_data_type_layout =
        .x$breakdown_and_data_type_layout %||%
        .y$breakdown_and_data_type_layout,
      tst_layout = .x$tst_layout %||% .y$tst_layout,
      shapefiles = .y$shapefiles,
      data_format = .x$data_format %||% .y$data_format
    )
  )

  extract

}

# > Get info on recent extracts ----

#' Get information on recent extracts
#'
#' @description
#' Get information on recent extracts for a given IPUMS collection
#' via the IPUMS API.
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @details
#' `get_last_extract_info` is a convenience function that is
#' similar to `get_recent_extracts_info_list(..., how_many = 1)`,
#' but returns an [`ipums_extract`][ipums_extract-class] object rather than
#' a list of `ipums_extract` objects.
#'
#' @param collection The code for an IPUMS data collection.
#'
#'   Defaults to the value of the `IPUMS_DEFAULT_COLLECTION` environment
#'   variable. To set a default collection, use
#'   [set_ipums_default_collection()].
#'
#'   For a list of the codes used to refer to the data collections,
#'   use [ipums_data_collections()].
#' @param how_many Number of recent extracts for which to retrieve information.
#'   Defaults to 10 extracts.
#' @inheritParams submit_extract
#'
#' @family ipums_api
#' @return For `get_recent_extracts_info_list()`, a list of extract objects.
#'
#'   For `get_recent_extracts_info_tbl()`, a [`tibble`][tibble::tbl_df-class]
#'   representing extract specifications. Each column corresponds to an extract
#'   field.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # If you have a default collection set, it will be used by default:
#' set_ipums_default_collection("usa")
#' list_of_last_10_extracts <- get_recent_extracts_info_list()
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
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
#' }
#'
#' # get_last_extract_info() can be used for convenience in the extract
#' # submission workflow as shown below:
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submit_extract(my_extract)
#'
#' # Oops, forgot to capture the return object from submit_extract.
#' # Grab it with:
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # View the extract number
#' submitted_extract$number
#'
#' # Check if submitted extract is ready
#' is_extract_ready(submitted_extract) # returns TRUE or FALSE
#'
#' # Or have R check periodically until the extract is ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
#' @name get_recent_extracts_info
NULL

#' @rdname get_recent_extracts_info
#' @export
get_recent_extracts_info_list <- function(collection = NULL,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {

  collection <- collection %||% get_default_collection()

  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = NULL,
    queries = list(limit = how_many),
    api_key = api_key
  )

  extract_list_from_json(response)

}

#' @rdname get_recent_extracts_info
#' @export
get_recent_extracts_info_tbl <- function(collection = NULL,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {

  collection <- collection %||% get_default_collection()

  extract_list <- get_recent_extracts_info_list(
    collection,
    how_many,
    api_key
  )

  extract_list_to_tbl(extract_list)

}

#' @param collection Character string of the IPUMS collection for which to
#'   retrieve the most recent extract request.
#'
#' @rdname get_extract_info
#' @export
get_last_extract_info <- function(collection = NULL,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {

  collection <- collection %||% get_default_collection()

  ipums_api_version(collection)

  get_extract_info(collection, how_many = 1, api_key = api_key)[[1]]

}

#' Convert a tibble of extract definitions to a list
#'
#' @description
#' Convert a [`tibble`][tibble::tbl_df-class] (or
#' [`data.frame`][base::data.frame()]) of extract definitions, such as that
#' returned by [get_recent_extracts_info_tbl()], to a list of
#' [`ipums_extract`][ipums_extract-class] objects.
#'
#' For an overview of ipumsr microdata API
#' functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param extract_tbl A [`tibble`][tibble::tbl_df-class] (or
#'   [`data.frame`][base::data.frame()]) that contains
#'   the specifications for one or more [`ipums_extract`][ipums_extract-class]
#'   objects.
#' @param validate Logical value indicating whether to
#'   check that each row of `extract_tbl` contains a valid and complete extract
#'   definition. Defaults to `TRUE`.
#'
#' @family ipums_api
#' @return A list of length equal to the number of extracts represented in
#'   `extract_tbl`. Unique extracts can be identified by their extract
#'   number, which is contained in the `number` column of `extract_tbl`.
#'
#' @examples
#' \dontrun{
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
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
#' }
#'
#' @export
extract_tbl_to_list <- function(extract_tbl, validate = TRUE) {

  collection <- unique(extract_tbl$collection)

  if (length(collection) > 1) {
    stop(
      "All extracts in `extract_tbl` must belong to same collection.",
      call. = FALSE
    )
  }

  expected_names <- get_extract_tbl_fields(
    new_ipums_extract(collection = collection)
  )

  unexpected_names <- setdiff(names(extract_tbl), expected_names)

  if (length(unexpected_names) > 0) {
    stop(
      "Unexpected names in `extract_tbl`: ",
      paste0('"', unexpected_names, '"', collapse = ", "),
      call. = FALSE
    )
  }

  if (collection == "nhgis") {

    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop(
        "Package \"tidyr\" must be installed to convert NHGIS extracts from ",
        "tbl to list format.",
        call. = FALSE
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

#' Convert a list of extract definitions to a tibble
#'
#' Convert a list of [`ipums_extract`][ipums_extract-class] objects to a
#' [`tibble`][tibble::tbl_df-class] containing the specifications for
#' those extracts. For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param extract_list A list of [`ipums_extract`][ipums_extract-class] objects
#'   or a single `ipums_extract` object.
#'
#' @family ipums_api
#' @return A [`tibble`][tibble::tbl_df-class] representing the specifications
#'   for each of the extracts represented in `extract_list`. Each column
#'   corresponds to an extract field.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Convert list of extracts to tibble of extracts to view in a tabular format
#' extract_list_to_tbl(list_of_last_10_extracts)
#' }
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

  if(length(extract_types) != 1) {
    stop(
      "All extracts in `extract_list` must belong to same collection.",
      call. = FALSE
    )
  }

  purrr::map_dfr(extract_list, extract_to_tbl)

}

#' List IPUMS data collections
#'
#' List IPUMS data collections with corresponding codes used by the IPUMS API.
#' Note that some data collections do not yet have API support.
#'
#' Currently, ipumsr supports extract definitions for the following collections:
#'
#' * IPUMS USA
#' * IPUMS CPS
#' * IPUMS NHGIS
#'
#' For an overview of ipumsr API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @family ipums_api
#' @return A [`tibble`][tibble::tbl_df-class] with three columns containing the
#'   full collection name, the corresponding code used by the IPUMS API, and the
#'   status of API support for the collection.
#'
#' @export
ipums_data_collections <- function() {
  tibble::tribble(
    ~collection_name, ~code_for_api, ~api_support,
    "IPUMS USA", "usa", "beta",
    "IPUMS CPS", "cps", "beta",
    "IPUMS International", "ipumsi", "none",
    "IPUMS NHGIS", "nhgis", "v1",
    "IPUMS AHTUS", "ahtus", "none",
    "IPUMS MTUS", "mtus", "none",
    "IPUMS ATUS", "atus", "none",
    "IPUMS DHS", "dhs", "none",
    "IPUMS Higher Ed", "highered", "none",
    "IPUMS MEPS", "meps", "none",
    "IPUMS NHIS", "nhis", "none",
    "IPUMS PMA", "pma", "none"
  )
}

# > Set IPUMS API key ----

#' Set your IPUMS API key
#'
#' @description
#' Set your IPUMS API key as the value associated with the `IPUMS_API_KEY`
#' environment variable.
#'
#' The key can be stored for the duration of your session or for future
#' sessions. If saved for future sessions, it is added to the `.Renviron`
#' file in your home directory. If you choose to save your key to `.Renviron`,
#' this function will create a backup copy of the file before modifying.
#'
#' This function is modeled after the `census_api_key()` function
#' from [tidycensus](https://walker-data.com/tidycensus/).
#'
#' @param api_key API key associated with your user account.
#' @param save If `TRUE`, save the key for use in future
#'   sessions by adding it to the `.Renviron` file in your home directory.
#'   Defaults to `FALSE`.
#' @param overwrite If `TRUE`, overwrite any existing value of
#'   `IPUMS_API_KEY` in the `.Renviron` file with the provided `api_key`.
#'   Defaults to `FALSE`.
#' @param unset if `TRUE`, remove the existing value of `IPUMS_API_KEY`
#'   from the environment and the `.Renviron` file in your home directory.
#'
#' @return The value of `api_key`, invisibly.
#'
#' @family ipums_api
#'
#' @export
set_ipums_api_key <- function(api_key,
                              save = FALSE,
                              overwrite = FALSE,
                              unset = FALSE) {

  if (unset) {
    api_key <- unset_ipums_envvar("IPUMS_API_KEY")
  } else {
    api_key <- set_ipums_envvar(
      IPUMS_API_KEY = api_key,
      save = save,
      overwrite = overwrite
    )
  }

  invisible(api_key)

}

#' Set your default IPUMS collection
#'
#' @description
#' Set the default IPUMS collection as the value associated with the
#' `IPUMS_DEFAULT_COLLECTION` environment variable. If this environment variable
#' exists, IPUMS API functions that require a collection specification will use
#' the value of `IPUMS_DEFAULT_COLLECTION`, unless another collection is
#' indicated.
#'
#' The default collection can be stored for the duration of your session or
#' for future sessions. If saved for future sessions, it is added to the
#' `.Renviron` file in your home directory. If you choose to save your key
#' to `.Renviron`, this function will create a backup copy of the file before
#' modifying.
#'
#' This function is modeled after the `census_api_key()` function
#' from [tidycensus](https://walker-data.com/tidycensus/).
#'
#' @param collection Character string of the collection to set as your
#'   default collection. The collection must currently be supported
#'   by the IPUMS API.
#' @param save If `TRUE`, save the key for use in future
#'   sessions by adding it to the `.Renviron` file in your home directory.
#'   Defaults to `FALSE`.
#' @param overwrite If `TRUE`, overwrite any existing value of
#'   `IPUMS_API_KEY` in the `.Renviron` file with the provided `api_key`.
#'   Defaults to `FALSE`.
#' @param unset if `TRUE`, remove the existing value of
#'  `IPUMS_DEFAULT_COLLECTION` from the environment and the `.Renviron` file in
#'  your home directory.
#'
#' @return The value of `collection`, invisibly.
#'
#' @family ipums_api
#'
#' @export
#'
#' @examples
#' set_ipums_default_collection("nhgis")
#'
#' \dontrun{
#' # Extract info will now be retrieved for the default collection:
#' get_last_extract_info()
#' is_extract_ready(1)
#' get_extract_info(1)
#'
#' # Equivalent to:
#' get_extract_info("nhgis:1")
#' get_extract_info(c("nhgis", 1))
#'
#' # Other collections can be specified explicitly:
#' is_extract_ready("usa:2")
#'
#' # This does not alter the default collection, though:
#' get_last_extract_info()
#' }
#'
#' # Remove the variable from the environment and .Renviron, if saved
#' set_ipums_default_collection(unset = TRUE)
set_ipums_default_collection <- function(collection = NULL,
                                         save = FALSE,
                                         overwrite = FALSE,
                                         unset = FALSE) {

  if (unset) {
    collection <- unset_ipums_envvar("IPUMS_DEFAULT_COLLECTION")
  } else {
    collection <- tolower(collection)

    # Error if collection is not currently available for API
    ipums_api_version(collection)

    collection <- set_ipums_envvar(
      IPUMS_DEFAULT_COLLECTION = collection,
      save = save,
      overwrite = overwrite
    )
  }

  invisible(collection)

}

# Non-exported functions ---------------------------------------------------

#' Create a new extract object
#'
#' Creates an object inheriting from class `ipums_extract` for use in
#' interacting with the IPUMS extract API.
#'
#' @param collection Character indicating the IPUMS collection for this extract.
#'   See [ipums_data_collections()].
#' @param description Description of the extract.
#' @param submitted Logical indicating whether this extract has been submitted
#'   to the IPUMS extract API. See [submit_extract()].
#' @param download_links List of paths to the data included in the API response
#'   after submitting an extract.
#' @param number Number of the extract returned by the extract API on
#'   submission.
#' @param status Character indicating the current status of the extract as
#'   returned by the extract API. If the extract has not been submitted, the
#'   status is set to "unsubmitted".
#' @param ... Additional arguments used in creating extract definitions for
#'   specific data collections.
#'
#' @return An object inheriting from class `ipums_extract` containing the
#'   extract definition. Each collection produces an object of its own class.
#'
#' @noRd
new_ipums_extract <- function(collection = NA_character_,
                              description = NA_character_,
                              submitted = FALSE,
                              download_links = EMPTY_NAMED_LIST,
                              number = NA_integer_,
                              status = "unsubmitted",
                              ...) {

  out <- list(
    collection = collection,
    description = description,
    ...,
    submitted = submitted,
    download_links = download_links,
    number = number,
    status = status
  )

  structure(
    out,
    class = c(paste0(collection, "_extract"), "ipums_extract")
  )

}

#' Create a new IPUMS JSON object
#'
#' Creates a classed JSON object to allow for collection-specific method
#' dispatch when reading extract definitions from JSON.
#'
#' @param json A JSON-formatted extract definition
#' @param collection The collection of the extract represented in `json`
#'
#' @return JSON-formatted text with superclass `ipums_json` and a subclass
#'  of `{collection}_json`
#'
#' @noRd
new_ipums_json <- function(json, collection) {
  structure(
    json,
    class = c(paste0(collection, "_json"), "ipums_json", class(json))
  )
}

standardize_extract_identifier <- function(extract, collection_ok = FALSE) {

  if (inherits(extract, "ipums_extract")) {
    return(extract)
  }

  # If extract is length 1, must be a "collection:number" id
  # or a single number to be paired with the default collection
  if (length(extract) == 1) {

    if (fostr_detect(extract, ":")) {

      extract <- fostr_split(extract, ":")[[1]]

    } else {

      # Only use default collection if `extract` can be coerced to numeric.
      extract_as_num <- suppressWarnings(
        as.numeric(fostr_replace(extract, "L$", "")) # Handle int specification
      )
      extract_is_chr <- is.na(extract_as_num) || length(extract_as_num) == 0

      # If not coercible to numeric, we are dealing with a collection
      if (extract_is_chr) {
        if (!collection_ok) {
          rlang::abort(
            c(
              "Invalid `extract` argument. Expected `extract` to be one of:",
              "*" = "An `ipums_extract` object",
              "*" = "A string of the form \"collection:number\"",
              "*" = "A vector of the form `c(collection, number)`",
              "*" = paste0(
                "An integer indicating the extract number for the collection ",
                "specified by IPUMS_DEFAULT_COLLECTION. ",
                "See `?set_ipums_default_collection()`."
              )
            )
          )
        } else {
          ipums_api_version(extract)
          return(list(collection = extract, number = NA))
        }
      }

      extract <- c(get_default_collection(), extract)

    }

  }

  # At this point, should be in `c(collection, number)` format
  if (length(extract) > 1) {
    collection <- extract[[1]]
    number <- suppressWarnings(
      as.numeric(fostr_replace(extract[[2]], "L$", ""))
    )
  }

  if (length(extract) != 2 || is.na(number)) {
    rlang::abort(
      c(
        "Invalid `extract` argument. Expected `extract` to be one of:",
        "*" = "An `ipums_extract` object",
        "*" = "A string of the form \"collection:number\"",
        "*" = "A vector of the form `c(collection, number)`",
        "*" = paste0(
          "An integer indicating the extract number for the collection ",
          "specified by IPUMS_DEFAULT_COLLECTION. ",
          "See `?set_ipums_default_collection()`."
        )
      )
    )
  }

  ipums_api_version(collection)

  if (number != round(number)) {
    rlang::abort(
      paste0("Unable to interpret extract number ", number, " as integer.")
    )
  }

  list(collection = collection, number = number)

}

get_default_collection <- function() {

  collection <- Sys.getenv("IPUMS_DEFAULT_COLLECTION")

  versions <- dplyr::filter(
    ipums_data_collections(),
    .data$api_support != "none"
  )

  if(!collection %in% versions$code_for_api) {

    if (collection == "") {
      rlang::abort(
        c(
          "No default collection set.",
          "i" = paste0(
            "Please specify a collection or use ",
            "`set_ipums_default_collection()` to add a default collection."
          )
        )
      )
    } else {
      rlang::abort(
        c(
          paste0(
            "The default collection is set to \"", collection,
            "\", which is not a supported IPUMS collection."
          ),
          "i" = paste0(
            "The IPUMS API currently supports the following collections: \"",
            paste0(versions$code_for_api, collapse = "\", \""), "\""
          ),
          "i" = paste0(
            "Please specify a collection or use ",
            "`set_ipums_default_collection()` to update your default ",
            "collection."
          )
        )
      )
    }

  }

  collection

}

# Do we actually need to back this up? If we're *sure* we're not touching any
# of the variables already in the file, why worry so much about backing up?
set_ipums_envvar <- function(...,
                             save = FALSE,
                             overwrite = FALSE) {

  dots <- rlang::list2(...)

  stopifnot(length(dots) == 1 && is_named(dots))

  var_name <- names(dots)
  var_value <- dots[[var_name]]

  if (save) {

    home_dir <- Sys.getenv("HOME")
    renviron_file <- file.path(home_dir, ".Renviron")
    new_envvar <- paste0(var_name, "=\"", var_value, "\"")

    if (!file.exists(renviron_file)) {

      file.create(renviron_file)
      writeLines(new_envvar, con = renviron_file)

    } else {

      backup_file <- file.path(home_dir, ".Renviron_backup")

      backed_up_renviron <- file.copy(
        renviron_file,
        to = backup_file,
        overwrite = TRUE
      )

      if (!backed_up_renviron) {
        rlang::warn("Failed to back up .Renviron.")
      } else {
        message(
          "Existing .Renviron file copied to ", backup_file,
          " for backup purposes."
        )
      }

      renviron_lines <- readLines(renviron_file)
      var_match <- paste0("^(\\s?)+", var_name)

      if (isTRUE(overwrite)) {

        renviron_lines[fostr_detect(renviron_lines, var_match)] <- new_envvar
        writeLines(renviron_lines, con = renviron_file)

      } else {

        if (any(fostr_detect(renviron_lines, var_match))) {
          stop(
            var_name, " already exists in .Renviron. To overwrite it, set ",
            "`overwrite = TRUE`.",
            call. = FALSE
          )
        }

        writeLines(c(renviron_lines, new_envvar), con = renviron_file)

      }

    }

    Sys.setenv(...)

    message(
      "The environment variable ", var_name,
      " has been set and saved for future sessions."
    )

  } else {

    Sys.setenv(...)

    message(
      "The environment variable ", var_name,
      " has been set. To save it for future sessions, ",
      "set `save = TRUE`."
    )

  }

  invisible(var_value)

}

unset_ipums_envvar <- function(var_name) {

  home_dir <- Sys.getenv("HOME")
  renviron_file <- file.path(home_dir, ".Renviron")

  if (!file.exists(renviron_file)) {
    rlang::warn("No .Renviron file to update.")
  } else {

    backup_file <- file.path(home_dir, ".Renviron_backup")

    backed_up_renviron <- file.copy(
      renviron_file,
      to = backup_file,
      overwrite = TRUE
    )

    if (!backed_up_renviron) {
      rlang::warn("Failed to back up .Renviron.")
    } else {
      message(
        "Existing .Renviron file copied to ", backup_file,
        " for backup purposes."
      )
    }

    renviron_lines <- readLines(renviron_file)
    var_match <- paste0("^(\\s?)+", var_name)

    renviron_lines <- renviron_lines[!fostr_detect(renviron_lines, var_match)]
    writeLines(renviron_lines, con = renviron_file)

  }

  message(
    "Unsetting environment variable ", var_name, " and removing from .Renviron."
  )

  Sys.unsetenv(var_name)

  invisible("")

}

#' Validate the structure of an IPUMS extract object
#'
#' @description
#' Ensures that the structure of an extract object is consistent with what is
#' required by the IPUMS extract API.
#'
#' The checks are primarily handled by wrapper function
#' `validate_extract_field()` and its helper functions. See the documentation
#' for these functions for more details on their arguments and implementation.
#'
#' @param x Object inheriting from class `ipums_extract`
#'
#' @return The input extract object `x`, invisibly
#'
#' @noRd
validate_ipums_extract <- function(x) {
  UseMethod("validate_ipums_extract")
}

#' @export
validate_ipums_extract.nhgis_extract <- function(x) {

  # Call base .ipums_extract method
  NextMethod(x)

  includes_ds <- !is.null(x$datasets) && !is_na(x$datasets)
  includes_tst <- !is.null(x$time_series_tables) && !is_na(x$time_series_tables)
  includes_shp <- !is.null(x$shapefiles) && !is_na(x$shapefiles)

  if (!any(includes_ds, includes_tst, includes_shp)) {
    rlang::abort(
      paste0(
        "An `nhgis_extract` must contain at least one of `datasets`, ",
        "`time_series_tables`, or `shapefiles`."
      )
    )
  }

  if (any(is.na(x$datasets)) ||
      any(is.na(x$time_series_tables)) ||
      any(is.na(x$shapefiles))) {
    rlang::abort(
      paste0(
        "None of `datasets`, `time_series_tables`, or `shapefiles` ",
        "can contain missing values."
      )
    )
  }

  if (length(intersect(x$datasets, x$time_series_tables)) > 0) {
    rlang::abort(
      "No `datasets` and `time_series_tables` can share the same names."
    )
  }

  # Specify the validation requirements for each extract field
  extract_field_spec <-  list(
    list(
      field = "datasets",
      type = "character"
    ),
    list(
      field = "data_tables",
      required = includes_ds,
      type = "list",
      parent_field = "datasets"
    ),
    list(
      field = "geog_levels",
      required = includes_ds || includes_tst,
      type = "list",
      parent_field = c("datasets", "time_series_tables")
    ),
    list(
      field = "years",
      type = "list",
      parent_field = "datasets"
    ),
    list(
      field = "breakdown_values",
      type = "list",
      parent_field = "datasets"
    ),
    list(
      field = "geographic_extents",
      allowed = includes_ds,
      must_be_missing_msg = " when no `datasets` are specified"
    ),
    list(
      field = "breakdown_and_data_type_layout",
      allowed = includes_ds,
      choices = c("single_file", "separate_files"),
      match_length = 1,
      must_be_missing_msg = " when no `datasets` are specified"
    ),
    list(
      field = "time_series_tables",
      type = "character"
    ),
    list(
      field = "tst_layout",
      required = includes_tst,
      allowed = includes_tst,
      choices = c("time_by_row_layout", "time_by_column_layout",
                  "time_by_file_layout"),
      match_length = 1,
      must_be_missing_msg = " when no `time_series_tables` are specified",
      must_be_present_msg = " when any `time_series_tables` are specified"
    ),
    list(
      field = "shapefiles",
      type = "character"
    ),
    list(
      field = "data_format",
      required = includes_ds || includes_tst,
      allowed = includes_ds || includes_tst,
      choices = c("csv_header", "csv_no_header", "fixed_width"),
      match_length = 1,
      must_be_missing_msg =
        " when no `datasets` or `time_series_tables` are specified",
      must_be_present_msg =
        " when any `datasets` or `time_series_tables` are specified"
    )
  )

  # Validate based on each argument's validation specifications
  # Collect errors and display together.
  extract_issues <- purrr::map(
    extract_field_spec,
    ~tryCatch(
      rlang::exec("validate_extract_field", !!!.x, extract = x),
      error = function(cnd) {
        conditionMessage(cnd)
      }
    )
  )

  extract_issues <- unlist(purrr::compact(extract_issues))

  if (length(extract_issues) > 0) {
    rlang::abort(
      c(
        "Invalid `nhgis_extract` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)

}

#' @export
validate_ipums_extract.usa_extract <- function(x) {

  NextMethod(x)

  extract_field_spec <- list(
    list(
      field = "data_structure",
      required = TRUE,
      choices = c("rectangular", "hierarchical"),
      match_length = 1,
      type = "character"
    ),
    list(
      field = "rectangular_on",
      required = isTRUE(x$data_structure == "rectangular"),
      allowed = isTRUE(x$data_structure == "rectangular"),
      choices = c("P", "H"),
      must_be_present_msg = " when `data_structure == \"rectangular\"`",
      must_be_missing_msg = " when `data_structure != \"rectangular\"`",
      match_length = 1,
      type = "character"
    ),
    list(
      field = "data_format",
      required = TRUE,
      choices = c("fixed_width", "csv", "stata", "spss", "sas9"),
      match_length = 1,
      type = "character"
    ),
    list(
      field = "samples",
      required = TRUE,
      type = "character"
    ),
    list(
      field = "variables",
      required = TRUE,
      type = "character"
    )
  )

  extract_issues <- purrr::map(
    extract_field_spec,
    ~tryCatch(
      rlang::exec("validate_extract_field", !!!.x, extract = x),
      error = function(cnd) {
        conditionMessage(cnd)
      }
    )
  )

  extract_issues <- unlist(purrr::compact(extract_issues))

  if (length(extract_issues) > 0) {
    rlang::abort(
      c(
        "Invalid `usa_extract` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)

}

#' @export
validate_ipums_extract.cps_extract <- function(x) {

  NextMethod(x)

  extract_field_spec <- list(
    list(
      field = "data_structure",
      required = TRUE,
      choices = c("rectangular", "hierarchical"),
      match_length = 1,
      type = "character"
    ),
    list(
      field = "rectangular_on",
      required = isTRUE(x$data_structure == "rectangular"),
      allowed = isTRUE(x$data_structure == "rectangular"),
      choices = c("P", "H"),
      must_be_present_msg = " when `data_structure == \"rectangular\"`",
      must_be_missing_msg = " when `data_structure != \"rectangular\"`",
      match_length = 1,
      type = "character"
    ),
    list(
      field = "data_format",
      required = TRUE,
      choices = c("fixed_width", "csv", "stata", "spss", "sas9"),
      match_length = 1,
      type = "character"
    ),
    list(
      field = "samples",
      required = TRUE,
      type = "character"
    ),
    list(
      field = "variables",
      required = TRUE,
      type = "character"
    )
  )

  extract_issues <- purrr::map(
    extract_field_spec,
    ~tryCatch(
      rlang::exec("validate_extract_field", !!!.x, extract = x),
      error = function(cnd) {
        conditionMessage(cnd)
      }
    )
  )

  extract_issues <- unlist(purrr::compact(extract_issues))

  if (length(extract_issues) > 0) {
    rlang::abort(
      c(
        "Invalid `cps_extract` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)

}

#' @export
validate_ipums_extract.ipums_extract <- function(x) {

  # Throw error if no API for collection
  ipums_api_version(x$collection)

  extract_field_spec <- list(
    list(
      field = "collection",
      required = TRUE,
      type = "character",
      match_length = 1
    ),
    list(
      field = "description",
      required = TRUE,
      match_length = 1,
      type = "character"
    ),
    list(
      field = "status",
      choices = c("unsubmitted", "queued", "started", "produced",
                  "canceled", "failed", "completed"),
      match_length = 1
    ),
    list(
      field = "download_links",
      type = "list"
    ),
    list(
      field = "number",
      match_length = 1,
      type = c("character", "integer", "double")
    )
  )

  # Validate based on each argument's validation specifications
  # Collect errors and display together.
  extract_issues <- purrr::map(
    extract_field_spec,
    ~tryCatch(
      rlang::exec("validate_extract_field", !!!.x, extract = x),
      error = function(cnd) {
        conditionMessage(cnd)
      }
    )
  )

  extract_issues <- unlist(purrr::compact(extract_issues))

  if (length(extract_issues) > 0) {
    rlang::abort(
      c(
        "Invalid `ipums_extract` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  x

}

#' Validate the structure of a single field in an IPUMS extract
#'
#' @description
#' Check whether an extract field has the structure expected based on the
#' values for several input arguments.
#'
#' `validate_extract_field()` is a wrapper for several helper functions:
#'   * `validate_subfield_names()` checks whether a child field of
#'     another extract field (e.g. `data_tables` nests within `datasets` in NHGIS
#'     extracts) has names that match the parent field values.
#'   * `validate_exists()` checks whether an extract field is missing and
#'     required or provided and not allowed.
#'   * `validate_choices()` checks whether the values provided in an extract
#'     field are in a vector of accepted choices.
#'   * `validate_length()` checks whether the correct number of values is
#'     provided for an extract field.
#'   * `validate_type()` checks whether an extract field is of the correct
#'     data type.
#'
#' Any arguments that are left `NULL` are not used for validation.
#'
#' @details
#' This function provides a consistent syntax for the specification of
#' extract requirements at the level of a single extract field. If an extract
#' collection requires validation checks that cannot be encapsulated in a single
#' field, they can be included in that extract collection's validation method.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_extract_field <- function(extract,
                                   field,
                                   required = FALSE,
                                   allowed = TRUE,
                                   choices = NULL,
                                   type = NULL,
                                   match_length = NULL,
                                   must_be_present_msg = NULL,
                                   must_be_missing_msg = NULL,
                                   parent_field = NULL) {

  if (!is_null(parent_field)) {

    # Child field is only allowed when its parent field is provided.
    allowed <- any(purrr::map_lgl(
      parent_field,
      ~!is_null(extract[[.x]])
    ))

    # Child field messages default to use parent_field for convenience
    must_be_present_msg <- must_be_present_msg %||%
      paste0(
        " for any of the provided `",
        paste0(parent_field, collapse = "` or `"),
        "`"
      )

    must_be_missing_msg <- must_be_missing_msg %||%
      paste0(
        " when no `",
        paste0(parent_field, collapse = "` or `"),
        "` are specified"
      )

  } else {
    parent_field_msg <- NULL
  }

  if (required) {
    allowed <- TRUE
  }

  # Check that field has correct names if names are required
  # (e.g. for nested fields)
  validate_subfield_names(
    extract,
    field,
    parent_field = parent_field,
    required = required
  )

  # Check that the correct number of field values are provided
  validate_length(
    extract,
    field,
    match_length = match_length %||% parent_field
  )

  # Check that field exists if required or is missing if not allowed
  validate_exists(
    extract,
    field,
    required = required,
    allowed = allowed,
    must_be_present_msg = must_be_present_msg,
    must_be_missing_msg = must_be_missing_msg
  )

  # Check that field value is in the list of accepted choices
  validate_choices(
    extract,
    field,
    choices = choices
  )

  # Check that field is of the correct data type
  validate_type(
    extract,
    field,
    type = type
  )

  invisible(NULL)

}

#' Check whether an extract field exists or has missing values
#'
#' Throws an error if an extract field is required and missing, or if an
#' extract field is not allowed to be in an extract and is present
#' (for instance, if a certain field must be missing if another field is
#' specified).
#'
#' @param extract An object inheriting from `ipums_extract`.
#' @param field Character of length 1 containing the name of the extract field
#'   to check for validity.
#' @param required Logical indicating whether this field is required to be
#'   present in the extract.
#' @param allowed Logical indicating whether this field is allowed to be
#'   present in the extract.
#' @param must_be_present_msg Message to append to the default error message
#'   when `field` is required but does not exist in the extract. This can be
#'   used to provide context about when the `field` must be present in the
#'   extract. The default output error message is
#'   `{field} must not contain missing values`
#' @param must_be_missing_msg Message to append to the default error message
#'   when `field` is not allowed to be in the extract but is still provided.
#'   This can be used to provide context about when the `field` must be missing
#'   from the extract. The default output error message is
#'   `{field} must be missing`
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_exists <- function(extract,
                            field,
                            required = FALSE,
                            allowed = TRUE,
                            must_be_present_msg = NULL,
                            must_be_missing_msg = NULL) {

  values <- extract[[field]]

  if (is_null(required) && is_null(allowed)) {
    return(invisible(NULL))
  }

  is_missing <- is_empty(values) || is_na(values) || length(values) == 0
  has_missing_values <- any(purrr::map_lgl(values, is.null)) ||
    any(is.na(unlist(values)))

  if (!allowed && !is_missing) {
    rlang::abort(
      paste0("`", field, "` must be missing", must_be_missing_msg, ".")
    )
  }

  if (required && (has_missing_values || is_missing)) {
    rlang::abort(
      paste0("`", field,
             "` must not contain missing values", must_be_present_msg, ".")
    )
  }

  invisible(NULL)
}

#' Check whether an extract field is of the correct length
#'
#' Throws an error if an extract field is not of a given length or is not the
#' same length as another extract field.
#'
#' @param extract An object inheriting from `ipums_extract`.
#' @param field Character of length 1 containing the name of the extract field
#'   to check for validity.
#' @param match_length Numeric or character value indicating the number
#'   of values expected in the given `field`. If a character value is provided,
#'   it should reference the name of another extract field. The length of the
#'   given `field` is then expected to be the same length as the field provided
#'   to `match_length`.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_length <- function(extract,
                            field,
                            match_length = NULL) {

  values <- extract[[field]]

  if (is_character(match_length)) {

    match_field <- match_length

    match_length <- sum(
      purrr::map_dbl(
        match_length,
        ~length(extract[[.x]]))
    )

    if (length(values) > match_length) {
      length_msg <- paste0(
        "The number of values in `", field,
        "` must not be greater than the number of `",
        paste0(match_field, collapse = "` + `"), "`"
      )
    } else if (length(values) < match_length) {
      length_msg <- paste0(
        "The number of values in `", field,
        "` must not be less than the number of `",
        paste0(match_field, collapse = "` + `"), "`"
      )
    }

  } else {
    length_msg <- paste0("`", field, "` must be length ", match_length, ".")
  }

  if (is_null(values) || length(values) == 0 ||
      is_null(match_length) || match_length == 0) {
    return(invisible(NULL))
  }

  if (!is.null(match_length) && length(values) != match_length) {
    rlang::abort(length_msg)
  }

  invisible(NULL)

}

#' Check whether an extract field's values are in a selection of choices
#'
#' Throws an error if any values in an extract field are not found in a set
#' of accepted choices.
#'
#' @param extract An object inheriting from `ipums_extract`.
#' @param field Character of length 1 containing the name of the extract field
#'   to check for validity.
#' @param choices Character vector of acceptable values for the given `field`.
#'   Any values found in `field` must be included in `choices`.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_choices <- function(extract, field, choices = NULL) {

  values <- extract[[field]]

  if (is_null(choices) || is_null(values)) {
    return(invisible(NULL))
  }

  if (!all(values %in% choices)) {
    rlang::abort(paste0(
      "`", field, "` must be one of \"",
      paste0(choices, collapse = "\", \""), "\""
    ))
  }

  invisible(NULL)

}

#' Check whether an extract field is of the correct type
#'
#' Throws an error if an extract field is not of a given type. Uses `typeOf()`
#' to determine the value to compare to `type`.
#'
#' @param extract An object inheriting from `ipums_extract`.
#' @param field Character of length 1 containing the name of the extract field
#'   to check for validity.
#' @param type Character vector of acceptable types for the given `field`.
#'   Values should correspond to the expected output when `typeOf()` is
#'   applied to the extract field.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_type <- function(extract, field, type = NULL) {

  values <- extract[[field]]

  if (is_null(type) || is_null(values)) {
    return(invisible(NULL))
  }

  if (!typeof(values) %in% type) {
    rlang::abort(
      paste0(
        "`", field, "` must be of type `", paste0(type, collapse = "` or `"),
        "`, not `", typeof(values), "`."
      )
    )
  }

  invisible(NULL)

}

#' Check whether an extract subfield is consistent with its parent field
#'
#' An extract field that is a child of a given parent field must be a list
#' whose names are identical to the values provided in that field's parent
#' field. The list must also be the same length as the parent field.
#'
#' @param extract An object inheriting from `ipums_extract`.
#' @param field Character of length 1 containing the name of the extract field
#'   to check for validity.
#' @param parent_field Character indicating the name of the extract field that
#'   serves as the parent field to `field`. The values in `field` are expected
#'   to be lists with the same length and names as the extract's `parent_field`.
#' @param required Logical indicating whether this field is required to be
#'   present in the extract.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_subfield_names <- function(extract,
                                    field,
                                    parent_field = NULL,
                                    required = FALSE) {

  values <- extract[[field]]

  parent_values <- unlist(purrr::map(
    parent_field,
    ~extract[[.x]]
  ))

  parent_field <- paste0("`", paste0(parent_field, collapse = "`, `"), "`")

  if (is_null(parent_values) || is_null(values)) {
    return(invisible(NULL))
  }

  non_na_names <- names(values)[!is.na(names(values))]

  if (!all(non_na_names %in% parent_values)) {
    rlang::abort(
      paste0(
        "All names in `", field, "` must be present in this extract's ",
        parent_field, "."
      )
    )
  }

  if (!all(parent_values %in% names(values))) {
    rlang::abort(
      paste0(
        "All ", parent_field, " must be associated with a value in `",
        field, "`."
      )
    )
  }

  invisible(NULL)
}

#' @export
print.ipums_extract <- function(x, ...) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE)
  )

  cat(to_cat)

  invisible(x)

}

#' @export
print.usa_extract <- function(x, ...) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: "),
    "\n"
  )

  cat(to_cat)

  invisible(x)

}

#' @export
print.cps_extract <- function(x, ...) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: "),
    "\n"
  )

  cat(to_cat)

  invisible(x)

}

#' @export
print.nhgis_extract <- function(x, ...) {

  style_ds <- extract_field_styler(NHGIS_DS_COLOR, "bold")

  ds_to_cat <- purrr::map(
    seq_along(x$datasets),
    ~format_field_for_printing(
      parent_field = list("Dataset: " = x$datasets[[.x]]),
      subfields = list(
        "Tables: " = x$data_tables[x$datasets][[.x]],
        "Geog Levels: " = x$geog_levels[x$datasets][[.x]],
        "Years: " = x$years[x$datasets][[.x]],
        "Breakdowns: " = x$breakdown_values[x$datasets][[.x]]
      ),
      parent_style = style_ds,
      subfield_style = extract_field_styler("bold")
    )
  )

  ds_to_cat <- c(
    ds_to_cat,
    format_field_for_printing(
      parent_field = list("Geographic extents: " = x$geographic_extents),
      parent_style = style_ds
    )
  )

  tst_to_cat <- purrr::map(
    seq_along(x$time_series_tables),
    ~format_field_for_printing(
      parent_field = list("Time Series Table: " = x$time_series_tables[[.x]]),
      subfields = list(
        "Geog Levels: " = x$geog_levels[x$time_series_tables][[.x]]
      ),
      parent_style = extract_field_styler(NHGIS_TST_COLOR, "bold"),
      subfield_style = extract_field_styler("bold")
    )
  )

  shp_to_cat <- format_field_for_printing(
    parent_field = list("Shapefiles: " = x$shapefiles),
    parent_style = extract_field_styler(NHGIS_SHP_COLOR, "bold")
  )

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    paste0(ds_to_cat, collapse = ""),
    paste0(tst_to_cat, collapse = ""),
    shp_to_cat,
    "\n"
  )

  cat(to_cat)

  invisible(x)

}

#' @importFrom rlang .data
#' @noRd
format_collection_for_printing <- function(collection) {
  collection_info <- dplyr::filter(
    ipums_data_collections(),
    .data$code_for_api == collection
  )

  if (nrow(collection_info) == 0) {
    return(UNKNOWN_DATA_COLLECTION_LABEL)
  }

  collection_info$collection_name
}

#' Format an extract field for printing
#'
#' Format an extract field, along with associated subfields (if applicable),
#' which will be indented and displayed beneath the entry for the associated
#' parent field.
#'
#' @param parent_field Named list of length 1. Names serve as text that will
#'   be displayed before the list values
#' @param subfields Named list of arbitrary length. Names serve as text that
#'   will be displayed before the corresponding values in that list element.
#' @param parent_style Optional `crayon` style function used to style the name
#'   of the `parent_field`.
#' @param subfield_style Optional `crayon` style function used to style the
#'   name(s) of the `subfields`.
#' @param padding Number of line breaks to include between each parent field.
#'
#' @return Formatted text for printing
#'
#' @noRd
format_field_for_printing <- function(parent_field = NULL,
                                      subfields = NULL,
                                      parent_style = NULL,
                                      subfield_style = NULL,
                                      padding = 2) {

  stopifnot(length(parent_field) == 1)

  parent_val <- parent_field[[1]]
  parent_name <- names(parent_field)

  if (is.null(parent_val)) {
    return(NULL)
  }

  style_field <- parent_style %||% extract_field_styler("reset")
  style_subfield <- subfield_style %||% extract_field_styler("reset")

  output <- paste0(
    paste0(rep("\n", padding), collapse = ""),
    print_truncated_vector(
      parent_val,
      style_field(parent_name),
      FALSE
    )
  )

  if (!is.null(subfields)) {
    purrr::map(
      names(subfields),
      ~if (!is.null(subfields[[.x]])) {
        output <<- paste0(
          output,
          "\n  ",
          print_truncated_vector(subfields[[.x]], style_subfield(.x), FALSE)
        )
      }
    )
  }

  output

}

#' Create a `crayon` style function if `crayon` is installed
#'
#' @param ... Values passed to [crayon::combine_styles()]
#'
#' @return A `crayon` style function
#'
#' @noRd
extract_field_styler <- function(...) {
  if (rlang::is_installed("crayon")) {
    style <- crayon::combine_styles(...)
  } else {
    style <- function(x) x
  }

  style
}

NHGIS_DS_COLOR <- "blue"
NHGIS_TST_COLOR <- "green"
NHGIS_SHP_COLOR <- "yellow"

UNKNOWN_DATA_COLLECTION_LABEL <- "Unknown data collection"

print_truncated_vector <- function(x, label = NULL, include_length = TRUE) {

  max_width <- min(getOption("width"), 80)
  max_width <- max(max_width, 20) # don't allow width less than 20

  full_list <- paste0(x, collapse = ", ")

  untruncated <- ifelse(
    include_length,
    paste0(label, "(", length(x), " total) ", full_list),
    paste0(label, full_list)
  )

  if (rlang::is_installed("crayon")) {
    count_chr <- function(x) crayon::col_nchar(x)
  } else {
    count_chr <- function(x) nchar(x)
  }

  if (count_chr(untruncated) > max_width) {
    return(paste0(substr(untruncated, 1, max_width - 3), "..."))
  }

  untruncated

  # will_be_truncated <- length(x) > truncate_at
  # x <- head(x, truncate_at)
  # out <- paste0(x, collapse = ", ")
  # if (will_be_truncated) {
  #   return(paste0(out, "..."))
  # }
  # out

}

#' Convert an `ipums_extract` object to a JSON string for API submission
#'
#' @param extract An `ipums_extract` object
#' @param include_endpoint_info Logical indicating whether to include
#'   collection and API version information in the JSON object
#'
#' @return A JSON string containing the formatted extract definition
#'
#' @noRd
extract_to_request_json <- function(extract, include_endpoint_info) {
  UseMethod("extract_to_request_json")
}

#' @export
extract_to_request_json.nhgis_extract <- function(extract,
                                                  include_endpoint_info = FALSE) {

  # if (is.null(extract$description) || is.na(extract$description)) {
  #   extract$description <- ""
  # }

  extract$years <- purrr::map(
    extract$years,
    ~if(!is.null(.x)) as.character(.x)
  )

  if(!is.null(extract$geographic_extents)) {
    extract$geographic_extents <- as.character(extract$geographic_extents)
  }

  request_list <- list(
    datasets = format_nhgis_field_for_json(
      datasets = extract$datasets,
      data_tables = extract$data_tables[extract$datasets],
      geog_levels = extract$geog_levels[extract$datasets],
      years = extract$years[extract$datasets],
      breakdown_values = extract$breakdown_values[extract$datasets]
    ),
    time_series_tables = format_nhgis_field_for_json(
      time_series_tables = extract$time_series_tables,
      geog_levels = extract$geog_levels[extract$time_series_tables]
    ),
    shapefiles = extract$shapefiles,
    data_format = jsonlite::unbox(extract$data_format),
    description = jsonlite::unbox(extract$description),
    breakdown_and_data_type_layout = jsonlite::unbox(
      extract$breakdown_and_data_type_layout
    ),
    time_series_table_layout = jsonlite::unbox(
      extract$tst_layout
    ),
    geographic_extents = geog_extent_lookup(
      extract$geographic_extents,
      state_geog_lookup$codes
    )
  )

  request_list <- purrr::keep(
    request_list,
    ~!(any(is.na(.x)) || is_empty(.x))
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = jsonlite::unbox(extract$collection),
      api_version = jsonlite::unbox(ipums_api_version(extract$collection))
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list)

}

geog_extent_lookup <- function(states) {
  purrr::map_chr(
    states,
    ~if (.x %in% state_extent_codes$description) {
      state_extent_codes[.x == state_extent_codes$description, ]$name
    } else {
      .x
    }
  )
}

# tst_year_lookup <- function() {
#   list(
#     "125" = 2008:2012,
#     "195" = 2015:2019
#   )
# }

#' @export
extract_to_request_json.usa_extract <- function(extract,
                                                include_endpoint_info = FALSE) {

  if (is.null(extract$description) || is.na(extract$description)) {
    extract$description <- ""
  }

  if (is.null(extract$data_format) || is.na(extract$data_format)) {
    extract$data_format <- ""
  }

  request_list <- list(
    description = extract$description,
    data_structure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    data_format = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = extract$collection,
      api_version = ipums_api_version(extract$collection)
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}

#' @export
extract_to_request_json.cps_extract <- function(extract,
                                                include_endpoint_info = FALSE) {

  if (is.null(extract$description) || is.na(extract$description)) {
    extract$description <- ""
  }

  if (is.null(extract$data_format) || is.na(extract$data_format)) {
    extract$data_format <- ""
  }

  request_list <- list(
    description = extract$description,
    data_structure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    data_format = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = extract$collection,
      api_version = ipums_api_version(extract$collection)
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}

#' @export
extract_to_request_json.ipums_extract <- function(extract) {

  if (is_na(extract$description)) {
    extract$description <- ""
  }

  request_list <- list(
    description = extract$description
  )

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}


format_samples_for_json <- function(samples) {
  if (length(samples) == 1 && is.na(samples)) {
    return(EMPTY_NAMED_LIST)
  }
  sample_spec <- purrr::map(seq_along(samples), ~ EMPTY_NAMED_LIST)
  purrr::set_names(sample_spec, samples)
}

format_variables_for_json <- function(variables) {
  if (length(variables) == 1 && is.na(variables)) {
    return(EMPTY_NAMED_LIST)
  }
  var_spec <- purrr::map(seq_along(variables), ~ EMPTY_NAMED_LIST)
  purrr::set_names(var_spec, variables)
}

format_data_structure_for_json <- function(data_structure, rectangular_on) {
  if (is.null(data_structure) || is.na(data_structure)) {
    return(EMPTY_NAMED_LIST)
  } else if (data_structure == "rectangular") {
    return(list(rectangular = list(on = rectangular_on)))
  } else if (data_structure == "hierarchical") {
    return(list(hierarchical = EMPTY_NAMED_LIST))
  } else {
    return(EMPTY_NAMED_LIST)
  }
}

format_nhgis_field_for_json <- function(...) {

  dots <- rlang::list2(...)

  if (all(is.na(dots[[1]])) || is_empty(dots[[1]])) {
    return(NULL)
  }

  supfields <- purrr::map(purrr::compact(dots[[1]]), c)
  n_supfields <- length(supfields)

  subfields <- dots[2:length(dots)]

  subfields_grp <- purrr::map(
    1:n_supfields,
    ~purrr::map(subfields, .x)
  )

  subfields_formatted <- setNames(
    purrr::map(subfields_grp, purrr::compact),
    supfields
  )

  subfields_formatted

}

#' Writes the given url to file_path. Returns the file path of the
#' downloaded data. Raises an error if the request is not successful.
#'
#' @importFrom utils packageVersion
#'
#' @noRd
ipums_api_download_request <- function(url,
                                       file_path,
                                       overwrite,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {


  file_already_exists <- file.exists(file_path)

  if (file.exists(file_path) && !overwrite) {
    stop("File ", file_path, " already exists. If you want to overwrite, set ",
         "`overwrite` to TRUE.", call. = FALSE)
  }

  response <- httr::GET(
    url,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(utils::packageVersion("ipumsr"))
      )
    ),
    add_user_auth_header(api_key),
    httr::write_disk(file_path, overwrite = TRUE)
  )

  if (httr::http_status(response)$category != "Success") {
    stop(paste("Could not save", url, "to", file_path))
  }

  return(file_path)
}

#' Collection-specific extract download
#'
#' S3 generic implementation to allow for collection-specific method dispatch
#' for extract downloading. This need arises because of differences
#' in the types of files and file formats provided upon download for different
#' collections.
#'
#' @noRd
ipums_extract_specific_download <- function(extract,
                                            download_dir,
                                            overwrite,
                                            api_key) {
  UseMethod("ipums_extract_specific_download")
}

#' @export
ipums_extract_specific_download.usa_extract <- function(extract,
                                                        download_dir,
                                                        overwrite,
                                                        api_key) {

  ddi_url <- extract$download_links$ddi_codebook$url
  data_url <- extract$download_links$data$url

  ddi_file_path <- normalizePath(
    file.path(download_dir, basename(ddi_url)),
    winslash = "/",
    mustWork = FALSE
  )

  data_file_path <- normalizePath(
    file.path(download_dir, basename(data_url)),
    winslash = "/",
    mustWork = FALSE
  )

  ipums_api_download_request(ddi_url, ddi_file_path, overwrite, api_key)
  ipums_api_download_request(data_url, data_file_path, overwrite, api_key)

  message(
    paste0("DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
           "to ", data_file_path)
  )

  invisible(ddi_file_path)

}

#' @export
ipums_extract_specific_download.cps_extract <- function(extract,
                                                        download_dir,
                                                        overwrite,
                                                        api_key) {

  ddi_url <- extract$download_links$ddi_codebook$url
  data_url <- extract$download_links$data$url

  ddi_file_path <- normalizePath(
    file.path(download_dir, basename(ddi_url)),
    winslash = "/",
    mustWork = FALSE
  )

  data_file_path <- normalizePath(
    file.path(download_dir, basename(data_url)),
    winslash = "/",
    mustWork = FALSE
  )

  ipums_api_download_request(ddi_url, ddi_file_path, overwrite, api_key)
  ipums_api_download_request(data_url, data_file_path, overwrite, api_key)

  message(
    paste0("DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
           "to ", data_file_path)
  )

  invisible(ddi_file_path)

}

#' @export
ipums_extract_specific_download.nhgis_extract <- function(extract,
                                                          download_dir,
                                                          overwrite,
                                                          api_key) {

  table_url <- extract$download_links$table_data
  gis_url <- extract$download_links$gis_data

  urls <- purrr::compact(
    list(
      table_url,
      gis_url
    )
  )

  file_paths <- purrr::map_chr(
    urls,
    ~normalizePath(
      file.path(download_dir, basename(.x)),
      winslash = "/",
      mustWork = FALSE
    )
  )

  file_names <- basename(file_paths)

  existing_files <- file_names[purrr::map_lgl(file_paths, file.exists)]

  # Currently, if any of the files to be downloaded (table or gis) exist,
  # no files are downloaded.
  if (length(existing_files) > 0 && !overwrite) {
    stop(
      "The following files already exist: ",
      paste0(existing_files, collapse = ", "),
      "\nIf you want to overwrite, set `overwrite` to TRUE.",
      call. = FALSE
    )
  }

  file_paths <- purrr::map2_chr(
    urls,
    file_paths,
    ~ipums_api_download_request(.x, .y, overwrite, api_key)
  )

  if (!is.null(table_url) && !is.null(gis_url)) {
    message(
      paste0(
        "Data file saved to ", file_paths[1],
        "\nGIS file saved to ", file_paths[2]
      )
    )
  } else if (!is.null(table_url)) {
    message(
      paste0("Data file saved to ", file_paths)
    )
  } else if (!is.null(gis_url)) {
    message(
      paste0("GIS file saved to ", file_paths)
    )
  }

  invisible(file_paths)

}

#' Helper function to form, submit, and receive responses of requests expecting
#'   a JSON response.
#'
#' @param verb `"GET"` or `"POST"`
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Defaults to `FALSE`, which creates a body-less request.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to [httr::modify_url].
#'
#' @return If the request returns a JSON response, this function returns a
#'   length-one character vector containing the response from the API
#'   formatted as JSON. Otherwise, the function throws an error.
#'
#' @importFrom utils packageVersion
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


#' Convert JSON containing extract specifications to an extract object
#'
#' @param extract_json JSON containing the extract specification as returned
#'   by the extract API
#' @param validate Logical indicating whether the created extract object should
#'   be validated using `validate_ipums_extract`
#'
#' @return An object inheriting from class `ipums_extract` containing the
#'   extract definition. Each collection produces an object of its own class.
#'
#' @noRd
extract_list_from_json <- function(extract_json, validate) {
  UseMethod("extract_list_from_json")
}

#' @export
extract_list_from_json.nhgis_json <- function(extract_json, validate = FALSE) {

  list_of_extract_info <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {

      no_datasets <- is.null(x$datasets)
      no_tsts <- is.null(x$time_series_tables)

      out <- new_ipums_extract(
        collection = "nhgis",
        description = x$description,
        datasets = names(x$datasets),
        data_tables = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$data_tables))
        },
        time_series_tables = names(x$time_series_tables),
        geog_levels = if (no_datasets && no_tsts) {
          NULL
        } else {
          purrr::map(c(x$datasets, x$time_series_tables),
                     ~unlist(.x$geog_levels))
        },
        years = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$years))
        },
        breakdown_values = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$breakdown_values))
        },
        geographic_extents = geog_extent_lookup(
          unlist(x$geographic_extents),
          state_geog_lookup$abbs
        ),
        shapefiles = unlist(x$shapefiles),
        breakdown_and_data_type_layout = x$breakdown_and_data_type_layout,
        tst_layout = x$time_series_table_layout,
        data_format = x$data_format,
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = x$download_links %||% EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = x$status %||% "unsubmitted"
      )

      if (validate) validate_ipums_extract(out)

      out
    }
  )
}

#' @export
extract_list_from_json.usa_json <- function(extract_json, validate = FALSE) {

  list_of_extract_info <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {
      out <- new_ipums_extract(
        collection = "usa",
        description = x$description,
        data_structure = names(x$data_structure),
        rectangular_on = ifelse(
          names(x$data_structure) == "rectangular",
          x$data_structure$rectangular$on,
          NA_character_
        ),
        data_format = x$data_format,
        samples = names(x$samples),
        variables = names(x$variables),
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = if ("download_links" %in% names(x)) {
          x$download_links
        } else EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = ifelse("status" %in% names(x), x$status, "unsubmitted")
      )
      if (validate) validate_ipums_extract(out)
      out
    }
  )
}


#' @export
extract_list_from_json.cps_json <- function(extract_json, validate = FALSE) {

  list_of_extract_info <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {
      out <- new_ipums_extract(
        collection = "cps",
        description = x$description,
        data_structure = names(x$data_structure),
        rectangular_on = ifelse(
          names(x$data_structure) == "rectangular",
          x$data_structure$rectangular$on,
          NA_character_
        ),
        data_format = x$data_format,
        samples = names(x$samples),
        variables = names(x$variables),
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = if ("download_links" %in% names(x)) {
          x$download_links
        } else EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = ifelse("status" %in% names(x), x$status, "unsubmitted")
      )
      if (validate) validate_ipums_extract(out)
      out
    }
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
    "Received status code ", res$status_code,
    " with the following details:\n\n",
    paste0(response_detail, collapse = "\n\n")
  )
  return(error_message)
}

#' Check if an extract is ready for download
#'
#' @param extract An `ipums_extract` object
#'
#' @return Logical indicating whether extract is ready for download
#'
#' @noRd
extract_is_completed_and_has_links <- function(extract) {
  UseMethod("extract_is_completed_and_has_links")
}

#' @export
extract_is_completed_and_has_links.usa_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return (is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddi_codebook") &&
    has_url(download_links, "data")
}

#' @export
extract_is_completed_and_has_links.cps_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return (is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddi_codebook") &&
    has_url(download_links, "data")
}

#' @export
extract_is_completed_and_has_links.nhgis_extract <- function(extract) {

  status <- extract$status
  download_links <- extract$download_links

  # TODO: Check if there needs to be more logic here for NHGIS.
  status == "completed" && length(download_links) > 0

}

add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
}

#' Helper taking advantage of the fact that USA and CPS work the same way for
#' now
#'
#' @noRd
add_to_extract_micro <- function(extract,
                                 description = NULL,
                                 samples = NULL,
                                 variables = NULL,
                                 data_format = NULL,
                                 data_structure = NULL,
                                 rectangular_on = NULL,
                                 ...) {

  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    warning(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified: `",
      paste0(names(dots), collapse = "`, `"), "`",
      call. = FALSE
    )
  }

  # Remove these once we allow for hierarchical and rectangular on H extracts
  if (!is_null(data_structure) && data_structure != "rectangular") {
    rlang::abort(
      paste0(
        "Currently, the `data_structure` argument must be equal to ",
        "\"rectangular\"; in the future, the API will also support ",
        "\"hierarchical\" extracts."
      )
    )
  }

  if (!is_null(rectangular_on) && rectangular_on != "P") {
    rlang::abort(
      paste0(
        "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
        "the future, the API will also support `rectangular_on = \"H\"."
      ),
    )
  }

  if (any(samples %in% extract$samples)) {
    rlang::warn(
      paste0(
        "The following samples are already included in the ",
        "supplied extract definition, and thus will not be added: \"",
        paste0(intersect(samples, extract$samples), collapse = "\", \""),
        "\""
      )
    )
  }

  if (any(variables %in% extract$variables)) {
    rlang::warn(
      paste0(
        "The following variables are already included in the ",
        "supplied extract definition, and thus will not be added: \"",
        paste0(intersect(variables, extract$variables), collapse = "\", \""),
        "\""
      )
    )
  }

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = description %||% extract$description,
    data_structure = data_structure %||% extract$data_structure,
    rectangular_on = rectangular_on %||% extract$rectangular_on,
    data_format = data_format %||% extract$data_format,
    samples = union(extract$samples, unlist(samples)),
    variables = union(extract$variables, unlist(variables))
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Helper taking advantage of the fact that USA and CPS work the same way for
#' now
#'
#' @noRd
remove_from_extract_micro <- function(extract,
                                      samples = NULL,
                                      variables = NULL,
                                      ...) {

  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    warning(
      "The following fields were either not found in the provided extract ",
      "or cannot be removed: `",
      paste0(names(dots), collapse = "`, `"), "`\n",
      "See `add_to_extract()` to replace existing values in applicable extract ",
      "fields.",
      call. = FALSE
    )
  }

  if (any(!samples %in% extract$samples)) {
    rlang::warn(
      paste0(
        "The following samples are not included in the ",
        "supplied extract definition, and thus will not be removed: \"",
        paste0(setdiff(samples, extract$samples), collapse = "\", \""),
        "\""
      )
    )
  }

  if (any(!variables %in% extract$variables)) {
    rlang::warn(
      paste0(
        "The following variables are not included in the ",
        "supplied extract definition, and thus will not be removed: \"",
        paste0(setdiff(variables, extract$variables), collapse = "\", \""),
        "\""
      )
    )
  }

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = extract$description,
    data_structure = extract$data_structure,
    rectangular_on = extract$rectangular_on,
    data_format = extract$data_format,
    samples = setdiff_null(extract$samples, unlist(samples)),
    variables = setdiff_null(extract$variables, unlist(variables))
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Combine values in a named list by common names
#'
#' For named lists that have multiple entries with identical names, combine the
#' values in these list entries into a single entry with the common name.
#'
#' @param l Named list
#' @param f Function used to combine the elements of l that have common names.
#'   The function should take two arguments and be compatible with
#'   [purrr::reduce()]. Defaults to `union()`.
#' @param name_order Vector of names by which to order the resulting output
#'   list. If names exist in the output list that are not found in `name_order`,
#'   they will be placed at the end of the output list.
#'
#' @return Named list with a single entry for each unique name found in `l`
#'
#' @noRd
reduce_list_by_name <- function(l, f = ~union(.x, .y), name_order = NULL) {

  if (is_empty(l)) {
    return(NULL)
  }

  if (!any(have_name(l))) {
    return(l)
  }

  labs <- unique(names(l))

  l <- purrr::map(
    labs,
    ~purrr::reduce(l[.x == names(l)], f)
  )

  l <- setNames(l, labs)

  if (!is_null(name_order)) {
    missing_idx <- which(!names(l) %in% name_order)
    l <- c(l[name_order], l[missing_idx])
  }

  l

}

#' Calculate set difference with the empty set represented as `NULL`
#'
#' Convenience function to allow for easier removal of values from extracts
#' whose extract fields can contain `NULL` values.
#'
#' @param x,y Vectors to use to calculate set difference
#'
#' @return Same output as `setdiff`, except that empty set return values
#'   are `NULL` rather than length-0 vectors.
#'
#' @noRd
setdiff_null <- function(x, y) {

  v <- setdiff(x, y)

  if (length(v) == 0) {
    v <- NULL
  }

  v

}

copy_ipums_extract <- function(extract) {

  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract

}

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
      years = list(NULL),
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

  var_order <- c("collection", "number", "description", "data_type",
                 "name", "data_tables", "geog_levels",
                 "years", "breakdown_values",  "geographic_extents",
                 "tst_layout", "breakdown_and_data_type_layout",
                 "data_format", "submitted", "download_links", "status")

  tbl[, var_order]

}

#' Flatten a long-format tibble of NHGIS extract specifications
#'
#' Converts tibble where each extract is spread out across multiple rows to a
#' tibble where each row represents a specific extract. This enables the use of
#' a standard conversion method from an extract tibble to list across microdata
#' and NHGIS, even though NHGIS extract tibbles are delivered in a different
#' layout.
#'
#' @param extract_tbl Tibble of NHGIS extract specifications as provided by
#'   `get_recent_extracts_info_tbl("nhgis")`
#'
#' @return A tibble where each row represents a single NHGIS extract. Fields
#'   with multiple values are collapsed as list-columns.
#'
#' @noRd
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>%
collapse_nhgis_extract_tbl <- function(extract_tbl) {

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop(
      "Package \"tidyr\" must be installed to convert NHGIS extracts from tbl ",
      "to list format.",
      call. = FALSE
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
        c("data_tables", "years", "breakdown_values"),
        ~ifelse(is_null(unlist(.x)), .x, list(.x))
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
        "geog_levels",
        ~ifelse(is_null(unlist(.x)), .x, list(.x))
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
        c("data_tables", "years", "breakdown_values"),
        function(d) purrr::map2(
          d,
          .data$datasets,
          ~recycle_extract_subfield(.x, .y)
        )
      ),
      dplyr::across(
        "geog_levels",
        function(d) purrr::map2(
          d,
          purrr::map2(.data$datasets, .data$time_series_tables, c),
          ~recycle_extract_subfield(.x, .y)
        )
      ),
    ) %>%
    # For consistency of output after conversion to list, define_extract_nhgis()
    # and tbl to list conversion should align on all fields
    dplyr::mutate(
      dplyr::across(
        c("data_format", "breakdown_and_data_type_layout", "tst_layout"),
        ~ifelse(is.na(.x), list(NULL), .x)
      )
    )

  # Reorder
  var_sort <- c("collection", "number",
                "description", "datasets", "data_tables", "time_series_tables",
                "geog_levels", "years", "breakdown_values",
                "geographic_extents", "shapefiles",
                "breakdown_and_data_type_layout",
                "tst_layout", "data_format",
                "submitted", "download_links", "status")

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

api_base_url <- function() {
  api_url <- Sys.getenv("IPUMS_API_URL")
  if (api_url == "") return("https://api.ipums.org/extracts/")
  api_url
}

api_extracts_path <- function() {
  basename(api_base_url())
}

#' Get the current API verison for a specified IPUMS collection
#'
#' @param collection IPUMS collection
#'
#' @importFrom rlang .data
#' @noRd
ipums_api_version <- function(collection) {

  versions <- dplyr::filter(
    ipums_data_collections(),
    .data$api_support != "none"
  )

  if(!collection %in% versions$code_for_api) {
    rlang::abort(
      c(
        paste0("No API version found for collection \"", collection, "\""),
        "i" = paste0(
          "The IPUMS API currently supports the following collections: \"",
          paste0(versions$code_for_api, collapse = "\", \""), "\""
        )
      )
    )
  }

  api_version <- Sys.getenv("IPUMS_API_VERSION")

  if (api_version == "") {
    versions[[which(versions$code_for_api == collection), "api_support"]]
  } else {
    api_version
  }

}

api_version_from_json <- function(extract_json) {
  extract <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )
  extract$api_version
}

EMPTY_NAMED_LIST <- setNames(list(), character(0))

#' Convert an absolute path to be relative to the working directory
#'
#' This is only used in unit tests at the moment
#'
#' @importFrom utils tail
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

#' Recycle values to a named list
#'
#' For use in recycling values for nested extract fields.
#'
#' @details
#' This function supports the syntax currently used in `define_extract_nhgis()`
#' for nested extract fields (e.g. each values of `datasets` must be associated
#' with at least one value of `data_tables`). Users can input values for
#' child fields in one of three ways:
#'   * If `l` is a vector, creates a list whose elements each consist of
#'     that vector and whose names correspond to the values of `names`.
#'   * If `l` is an unnamed list, attaches `names` to `l` in index
#'     order. In the case of a length mismatch between `l` and `names`,
#'     unmatched names receive a value of `NULL` and values with unmatched
#'     names receive a name of `NA`
#'   * If `l` is a named list, returns `l` ordered by `names`.
#'     If multiple elements of the same name are found, they
#'     are collapsed into a single entry with the union of their associated
#'     values. Unmatched names in `names` receive a value of
#'     `NULL`. If `l` includes elements with names not found in `names`, these
#'     elements are included in the output list after the values associated with
#'     the names provided in `names`.
#'
#' @param l List or vector to recycle. Typically corresponds to user-input list
#'   in subfields in extract functions.
#' @param names Character vector of names for the entries in the output list.
#'
#' @return A list of recycled values. See details.
#'
#' @noRd
recycle_extract_subfield <- function(l, names) {

  if (is_null(names)) {
    return(l)
    # return(NULL)
  }

  if (any(have_name(l))) {
    l <- as.list(l)
  }

  # EMPTY_NAMED_LIST is special case?
  if (is_list(l) && is_empty(l) && is_named(l)) {
    l <- rep(list(l), length(names))
  } else if (!is_list(l)) { # But otherwise we don't want to recycle lists
    l <- rep(list(l), length(names))
  }

  if (any(have_name(l))) {
    # If list is named, consolidate any entries with duplicate names
    if (any(duplicated(names(l)))) {
      labs <- unique(names(l))
      l <- purrr::map(labs, ~unlist(l[.x == names(l)], use.names = FALSE))
      names(l) <- labs
    }
  } else {
    # If list is unnamed, attach names in index order
    names(l) <- names[1:length(l)]
  }

  # Add NULL entries for any names that are not present in l
  null_list <- setNames(
    rep(list(NULL), length(names)),
    names
  )

  l_sub <- setNames(
    purrr::map(
      names,
      ~union(null_list[[.x]], l[[.x]])
    ),
    names
  )

  l <- c(l_sub, l[!names(l) %in% names])

  l

}

# Should be moved to utils?
`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

#' Convert geog extent values from names to codes and vice versa
#'
#' Allows users to specify geographic extents (currently states) by providing
#' full names, abbreviations, or NHGIS codes. These are converted to codes
#' before submitting to the API and converted to abbreviations for use in R.
#'
#' @param values Geog extent values to match to their associated abbreviations
#' @param lookup_key Named vector whose names match `values` and whose values
#'   represent the associated name to convert those values to
#'
#' @return A vector of length `values` with the recoded values
#'
#' @noRd
geog_extent_lookup <- function(values, lookup_key) {

  values_lower <- tolower(values)

  # not_in_key <- !values_lower %in% names(lookup_key)
  #
  # if (any(not_in_key)) {
  #   warning(
  #     "Values `", paste0(values[not_in_key], collapse = "`, `"),
  #     "` were not found.",
  #     call. = FALSE
  #   )
  # }

  if (length(values_lower) == 0) {
    return(NULL)
  }

  recoded <- toupper(dplyr::recode(values_lower, !!!lookup_key))

  recoded

}

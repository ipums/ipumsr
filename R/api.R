
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
#' Currently, \code{ipumsr} supports extract definitions for the following
#' collections:
#'
#' \itemize{
#'   \item{IPUMS NHGIS}
#'   \item{IPUMS USA}
#' }
#'
#' For information on defining NHGIS extracts, see
#' \code{\link{define_extract_nhgis}()}
#'
#' For information defining extracts for IPUMS microdata collections, see
#' \code{\link{define_extract_micro}()}
#'
#' @section Value:
#' These functions produce an object inheriting from class \code{ipums_extract}
#' with a subclass based on the collection corresponding to the extract. For
#' instance, an IPUMS NHGIS extract created with
#' \code{\link{define_extract_nhgis}} will return an object of classes
#' \code{nhgis_extract} and \code{ipums_extract}. These objects are compatible
#' with the rest of the API functionality provided by \code{ipumsr}.
#'
#' For an overview of \code{ipumsr} API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @family ipums_api
#'
#' @name define_extract
NULL

# > `ipums_extract` class ----

#' `ipums_extract` class
#'
#' @md
#' @description
#' The `ipums_extract` class provides a data structure for storing the
#' definition and status of a submitted or unsubmitted IPUMS data extract,
#' for the purpose of interacting with the IPUMS extract API.
#'
#' It is a superclass encompassing all of the collection-specific extract
#' classes.
#'
#' All objects with class `ipums_extract` will also have a collection-specific
#' subclass (e.g. `usa_extract`, `cps_extract`) to accommodate
#' collection-specific differences in extract options and contents, but all
#' these subclasses share similarities as described below.
#'
#' For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @section Properties of `ipums_extract`:
#'
#' Objects of class `ipums_extract` have:
#' * A `class` attribute of the form `c("<collection>_extract", "ipums_extract")`
#'   (e.g. `c("cps_extract", "ipums_extract")`).
#' * A base type of `"list"`.
#' * A `names` attribute that is a character vector the same length as the
#'   underlying list.
#'
#' @section Behavior of `ipums_extract`:
#'
#' Objects of class `ipums_extract`:
#' * Can be created from scratch with a function that has a name of the form
#'   `define_extract_<collection>()` (e.g. [define_extract_usa()]).
#' * Can be created from existing extract definitions with functions
#'   [define_extract_from_json()] and [get_extract_info()].
#' * Can be submitted for processing with [submit_extract()]. After submission,
#'   you can have your R session periodically check the status of the submitted
#'   extract, and wait until it is ready to download, with [wait_for_extract()].
#'   You can also check whether it is ready to download directly with
#'   [is_extract_ready()].
#' * Can be revised with [add_to_extract()] and [remove_from_extract()].
#' * Can be saved to a JSON-formatted file with [save_extract_as_json()].
#' @name ipums_extract-class
NULL





# > Define extract ----

#' Define an IPUMS USA extract request
#'
#' Define an IPUMS USA extract request to be submitted via the IPUMS microdata
#' extract API. For an overview of ipumsr microdata API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @md
#'
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   [sample ID values](https://usa.ipums.org/usa-action/samples/sample_ids).
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file (one of
#'   "fixed_width", "csv", "stata", "spss", or "sas9").
#' @param data_structure Currently, this must be "rectangular", which is also
#'   the default. In the future, the API will also support "hierarchical"
#'   extracts.
#' @param rectangular_on Currently, this must be "P", indicating that the
#'   extract will be rectangularized on person records. In the future, the API
#'   will also support household-only extracts (`rectangular_on = "H"`).
#'
#' @family ipums_api
#' @return An object of class [`c("usa_extract", "ipums_extract")`][ipums_extract-class]
#'   containing the extract definition.
#'
#' @examples
#' my_extract <- define_extract_usa("Example", "us2013a", "YEAR")
#' @export
define_extract_usa <- function(description,
                               samples,
                               variables,
                               data_format = c("fixed_width", "csv", "stata",
                                               "spss", "sas9"),
                               data_structure = "rectangular",
                               rectangular_on = "P") {

  data_format <- match.arg(data_format)

  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(data_structure), length(data_structure) == 1)
  stopifnot(is.character(rectangular_on), length(rectangular_on) == 1)
  stopifnot(is.character(data_format), length(data_format) == 1)
  stopifnot(is.character(samples))
  stopifnot(is.character(variables))

  # For now this next block is irrelevant; uncomment it whenever we add
  # support for rectangular_on = "H"
  # rectangular_on <- if (data_structure == "rectangular") {
  #   match.arg(rectangular_on)
  # } else NA_character_

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
#' @md
#'
#' @inheritParams define_extract_usa
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   [sample ID values](https://cps.ipums.org/cps-action/samples/sample_ids).
#'
#' @family ipums_api
#' @return An object of class [`c("cps_extract", "ipums_extract")`][ipums_extract-class]
#'   containing the extract definition.
#'
#' @examples
#' my_extract <- define_extract_cps("Example", "cps2020_03s", "YEAR")
#' @export
define_extract_cps <- function(description,
                               samples,
                               variables,
                               data_format = c("fixed_width", "csv", "stata",
                                               "spss", "sas9"),
                               data_structure = "rectangular",
                               rectangular_on = "P") {

  data_format <- match.arg(data_format)

  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(data_structure), length(data_structure) == 1)
  stopifnot(is.character(rectangular_on), length(rectangular_on) == 1)
  stopifnot(is.character(data_format), length(data_format) == 1)
  stopifnot(is.character(samples))
  stopifnot(is.character(variables))

  # For now this next block is irrelevant; uncomment it whenever we add
  # support for rectangular_on = "H"
  # rectangular_on <- if (data_structure == "rectangular") {
  #   match.arg(rectangular_on)
  # } else NA_character_

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
#' \href{https://www.nhgis.org/data-availability}{here}. For the NHGIS FAQ,
#' click \href{https://www.nhgis.org/frequently-asked-questions-faq}{here}.
#'
#' For general information on the IPUMS NHGIS Extract API, click
#' \href{https://developer.ipums.org/docs/workflows/create_extracts/nhgis_data/}{here}.
#' Note that some of the terminology used in the API has been altered
#' for use in the \code{ipumsr} package.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table. Dataset subfields are
#' prefixed with \code{ds_}, while time series table subfields are prefixed with
#' \code{tst_}.
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' \itemize{
#'   \item{If values are passed as a \strong{vector}, they will be
#'   applied to all of the datasets or time series tables in the extract
#'   definition.}
#'   \item{If values are passed as an \strong{unnamed list}, they will be
#'   matched to the datasets or time series tables by index. That
#'   is, the first element will be associated with the first dataset in the
#'   extract definition, the second element with the second dataset in the
#'   extract, etc.}
#'   \item{If values are passed as a \strong{named list}, they
#'   will be matched to the datasets or time series tables by name. Names should
#'   correspond to datasets or time series tables that exist in the extract
#'   definition (unrecognized names will be ignored).}
#' }
#'
#' Users can find more information on the values that can be passed to these
#' subfield arguments for a given dataset or time series table by using
#' \code{\link{get_nhgis_metadata}()}.
#'
#' @param description Description of the extract.
#' @param datasets Character vector of datasets to include in the extract,
#'   if any. For more information on NHGIS datasets, click
#'   \href{https://www.nhgis.org/overview-nhgis-datasets}{here}.
#' @param ds_tables Character vector or list of summary tables to include for
#'   the provided datasets. This is a subfield of \code{datasets} (see details
#'   for available
#'   syntax options). Required if any datasets are included in the extract.
#' @param ds_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in the
#'   requested summary tables. This is a subfield of \code{datasets} (see
#'   details for available syntax options). Required if any datasets are
#'   included in the extract.
#' @param ds_years Character or integer vector or list of years for which to
#'   obtain the data contained in the requested summary tables. Use \code{"*"}
#'   to select all available years for the specified dataset. This is a subfield
#'   of \code{datasets} (see details for available syntax options). Not all
#'   datasets allow year selection; see \code{\link{get_nhgis_metadata}()} to
#'   determine if a dataset allows year selection.
#' @param ds_breakdown_values Character vector or list of selected breakdown
#'   values to apply to the requested summary tables. If more than one breakdown
#'   value is requested, \code{breakdown_and_data_type_layout} must also be
#'   specified. This is a subfield of \code{datasets} (see details for available
#'   syntax options).
#' @param geographic_extents Character vector of geographic instances to use as
#'   extents for all datasets included in the extract. Use \code{"*"}
#'   to select all available extents. Required when any dataset in the extract
#'   includes a \code{ds_geog_levels} value that requires extent selection.
#'   See \code{\link{get_nhgis_metadata}()} to determine whether a requested
#'   \code{ds_geog_level} requires extent selection. Currently, NHGIS supports
#'   extent selection only for blocks and block groups.
#' @param breakdown_and_data_type_layout The desired layout
#'   of any datasets that have multiple data types or breakdown values. One of
#'   \code{"single_file"} (the default), which keeps all data types and
#'   breakdown values in one file, or \code{"separate_files"}, which splits each
#'   data type or breakdown value into its own file. Required if any datasets
#'   included in the extract consist of multiple data types (for instance,
#'   estimates and margins of error) or have multiple breakdown values
#'   specified. See \code{\link{get_nhgis_metadata}()} to determine whether a
#'   requested dataset has multiple data types.
#' @param time_series_tables Character vector of time series tables to include
#'   in the extract, if any. For more information on NHGIS time series tables,
#'   click \href{https://www.nhgis.org/time-series-tables}{here}.
#' @param tst_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in the
#'   provided times series tables. This is a subfield of
#'   \code{time_series_tables} (see details for available syntax options).
#'   Required if any time series tables are included in the extract.
#' @param tst_layout The desired layout of all time series tables included in
#'   the extract. One of \code{"time_by_column_layout"} (the default),
#'   \code{"time_by_row_layout"}, or \code{"time_by_file_layout"}. Required when
#'   any time series tables are specified.
#' @param shapefiles Character vector of shapefiles to include in the extract,
#'   if any. For more information on NHGIS shapefiles, click
#'   \href{https://www.nhgis.org/gis-files}{here}.
#' @param data_format The desired format of the extract data file. One of
#'   \code{"csv_no_header"}, \code{"csv_header"}, or \code{"fixed_width"}.
#'   The default \code{"csv_no_header"} provides only a minimal header in the
#'   first row, while \code{"csv_header"} adds a second, more descriptive header
#'   row. Required when any datasets or time series tables are included in the
#'   extract.
#'
#' @family ipums_api
#'
#' @return An object of class \code{nhgis_extract} containing the extract
#'   definition.
#'
#' @examples
#' define_extract_nhgis(
#'   description = "Test NHGIS extract",
#'   datasets = "1990_STF3",
#'   ds_tables = "NP57",
#'   ds_geog_levels = c("county", "tract"),
#' )
#'
#' # For extracts with multiple datasets or time series tables, subfield
#' # arguments are recycled to all datasets:
#' define_extract_nhgis(
#'   description = "Extract with multiple datasets",
#'   datasets = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
#'   ds_tables = "B01001",
#'   ds_geog_levels = c("state", "county")
#' )
#'
#' # To attach specific subfield values to each dataset or time series table,
#' # pass a list to the subfield argument.
#' # With an unnamed list, items are matched by index:
#' define_extract_nhgis(
#'   description = "Extract with multiple time series tables",
#'   time_series_tables = c("CW3", "CW5"),
#'   tst_geog_levels = list("state", "county")
#' )
#'
#' # With a named list, items are matched by name:
#' define_extract_nhgis(
#'   description = "Extract with multiple time series tables",
#'   time_series_tables = c("CW3", "CW5"),
#'   tst_geog_levels = list(CW5 = "county", CW3 = "state")
#' )
#'
#' @export
define_extract_nhgis <- function(description = "",
                                 datasets = NULL,
                                 ds_tables = NULL,
                                 ds_geog_levels = NULL,
                                 ds_years = NULL,
                                 ds_breakdown_values = NULL,
                                 geographic_extents = NULL,
                                 breakdown_and_data_type_layout = NULL,
                                 time_series_tables = NULL,
                                 tst_geog_levels = NULL,
                                 tst_layout = NULL,
                                 shapefiles = NULL,
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
    ds_tables = ds_tables,
    ds_geog_levels = ds_geog_levels,
    ds_years = ds_years,
    ds_breakdown_values = ds_breakdown_values,
    geographic_extents = geog_extent_lookup(
      unlist(geographic_extents),
      state_geog_lookup$abbs
    ),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_tables = unlist(time_series_tables),
    tst_geog_levels = tst_geog_levels,
    tst_layout = tst_layout,
    shapefiles = unlist(shapefiles),
    data_format = data_format
  )

  extract <- recycle_subfields(
    extract,
    datasets = c("ds_tables",
                 "ds_geog_levels",
                 "ds_years",
                 "ds_breakdown_values"),
    time_series_tables = "tst_geog_levels"
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Create an [`ipums_extract`][ipums_extract-class] object from a JSON-formatted
#' definition
#'
#' Create an [`ipums_extract`][ipums_extract-class] object based on an extract
#' definition formatted as JSON. For an overview of ipumsr microdata API functionality,
#' see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @param extract_json The path to a file containing the JSON definition, or a
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
#' @md
#'
#' @inheritParams submit_extract
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
#' @md
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param api_key API key associated with your user account. Defaults to the
#'   value of environment variable "IPUMS_API_KEY".
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

  extract <- validate_ipums_extract(extract)

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
  if (extract$status == "unsubmitted") extract$status <- "submitted"

  message(
    sprintf(
      "Successfully submitted %s extract number %d",
      format_collection_for_printing(extract$collection),
      extract$number
    )
  )

  extract

}

#' Get information about a submitted extract
#'
#' Get information about a submitted extract via the IPUMS API. For an overview
#' of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @param extract One of:
#'
#' * An [`ipums_extract`][ipums_extract-class] object
#' * The data collection and extract number formatted as a single string of the
#'   form `"collection:number"`
#' * The data collection and extract number formatted as a vector of the form
#'   `c("collection", "number")`
#'
#' The extract number does not need to be zero-padded (e.g., use `"usa:1"`
#' or `c("usa", "1")`, not `"usa:00001"` or `c("usa", "00001")`).
#' See Examples section below for examples of each form.
#'
#' For a list of codes used to refer to each collection, see
#' [ipums_data_collections()].
#' @inheritParams define_extract_usa
#' @inheritParams download_extract
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
#' }
#'
#' @export
get_extract_info <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  stopifnot(length(extract$collection) == 1)
  stopifnot(length(extract$number) == 1)

  if (is.na(extract$number)) {
    stop(
      "Extract number cannot be a missing value; please supply an ",
      "extract object returned by `submit_extract()`, or the data ",
      "collection and number of a submitted extract.",
      call. = FALSE
    )
  }

  collection <- tolower(extract$collection)

  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = paste0(api_extracts_path(), "/", extract$number),
    api_key = api_key
  )

  extract_list_from_json(response)[[1]]

}


#' Wait for extract to finish
#'
#' Wait for an extract to finish by periodically checking its status via the
#' IPUMS API and returning when the extract is ready to download. For an
#' overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @inheritParams define_extract_usa
#' @inheritParams download_extract
#' @inheritParams get_extract_info
#' @inheritParams submit_extract
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
    extract <- get_extract_info(extract, api_key)

    is_downloadable <- is_extract_ready(extract)

    is_failed <- !(is_downloadable ||
                     extract$status %in% c("queued", "started", "produced"))
    is_timed_out <- !is.null(timeout_seconds) &&
      as.numeric(Sys.time() - wait_start, units = "secs") > timeout_seconds


    if (is_downloadable) {
      if (verbose) {
        message("Extract ready to download")
      }
      is_finished <- TRUE
    } else if (is_failed) {
      err_message <- paste0(
        "Extract has finished, but is not in a downloadable state, likely ",
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


#' Is the extract ready to download?
#'
#' This function uses the IPUMS API to check whether the given extract is ready
#' to download, returning TRUE for extracts that are ready and FALSE for those
#' that are not. For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @details
#' This function checks the "download_links" element of the supplied extract to
#' determine whether the extract files are available to download.
#' The "status" of a submitted extract is one of "queued", "started", "produced",
#' "canceled", "failed", or "completed". Only "completed" extracts can be ready
#' to download, but not all "completed" extracts are ready to download, because
#' extract files are subject to removal from the IPUMS servers 72 hours after
#' they first become available. Completed extracts older than 72 hours will
#' still have a "completed" status, but will return `FALSE` from
#' `is_extract_ready()`, because the extract files are no longer available.
#'
#' @inheritParams get_extract_info
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
#' }
#'
#' @export
is_extract_ready <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  if (is.na(extract$number)) {
    stop(
      "extract object has a missing value in the 'number' field. If ",
      "an extract object is supplied, it must be a submitted extract with a ",
      "non-missing extract number.", call. = FALSE
    )
  }

  # First check if extract object already contains download info...
  if (inherits(extract, "ipums_extract") &&
      extract_is_completed_and_has_links(extract)) {
    return(TRUE)
  }

  # ... if it doesn't contain download info, make sure we have the latest
  # status by fetching it via the API and checking again
  extract <- get_extract_info(extract, api_key)

  extract_is_completed_and_has_links(extract)
}


#' Download an IPUMS data extract
#'
#' Download an IPUMS data extract via the IPUMS API. For an overview of ipumsr
#' microdata API functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @details
#' For NHGIS extracts, data files and GIS files will be saved in separate zip
#' archives. \code{download_extract()} will return a character vector including
#' the file paths to all downloaded files.
#'
#' For microdata extracts, only the file path to the downloaded .xml DDI file
#' will be returned, as it is sufficient for loading the data provided in the
#' associated .gz data file.
#'
#' @md
#'
#' @inheritParams get_extract_info
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
#' }
#'
#' @export
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  # Make sure we get latest extract status, but also make sure we don't check
  # the status twice
  is_ipums_extract_object <- inherits(extract, "ipums_extract")

  if (is_ipums_extract_object && extract_is_completed_and_has_links(extract)) {
    is_downloadable <- TRUE
  } else {
    extract <- get_extract_info(extract, api_key)
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
#' Add or update values for specific fields in an existing \code{ipums_extract}
#' object. This function is an S3 generic whose behavior will depend on the
#' class (i.e. collection) of the extract being modified.
#'
#' In general, for a given collection, the arguments to \code{add_to_extract}
#' are identical to those used when defining an extract for that collection. For
#' more about defining an extract, click \link[=define_extract]{here}.
#'
#' For collection-specific documentation, see the links below:
#'
#' \itemize{
#'   \item To add to an \strong{NHGIS} extract, click
#'     \link[=add_to_extract.nhgis_extract]{here}
#'   \item To add to a \strong{microdata} extract, click
#'     \link[=add_to_extract.usa_extract]{here}
#' }
#'
#' To remove existing values from an extract, see
#' \code{\link{remove_from_extract}()}.
#'
#' @param extract An object inheriting from \code{ipums_extract}
#' @param ... Additional arguments specifying the extract fields and values to
#'   add to the extract. The available arguments correspond to the available
#'   arguments in the extract definition function for the class of the extract
#'   specified in \code{extract}.
#'
#' @family ipums_api
#'
#' @return An object of the same class as \code{extract} containing the modified
#'   extract definition
#'
#' @export
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
}

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

#' Add values to an existing NHGIS extract
#'
#' @description
#' Add new values to any extract fields of an NHGIS extract. This can include
#' adding new datasets and/or time series tables along with associated
#' subfields, modifying dataset and/or time series table subfields, or
#' updating extract-wide parameters.
#'
#' To remove existing values from an NHGIS extract, click
#' \code{\link[=remove_from_extract.nhgis_extract]{remove_from_extract}()}.
#'
#' In general, adding to an extract follows the same syntax conventions as used
#' in \code{\link{define_extract_nhgis}()}. See details for more information
#' on how values passed to dataset and time series table subfields are
#' interpreted.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table. Dataset subfields are
#' prefixed with \code{ds_}, while time series table subfields are prefixed with
#' \code{tst_}.
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' \itemize{
#'   \item{If values are passed as a \strong{vector}, they will be
#'   applied across all datasets or time series tables specified in the
#'   \code{datasets} or \code{time_series_tables} arguments}
#'   \item{If values are passed as an \strong{unnamed list}, they will be
#'   matched to datasets or time series tables by index. That
#'   is, the first element will be associated with the first dataset included
#'   in \code{datasets}, the second element with the second dataset in
#'   \code{datasets}, etc.}
#'   \item{If values are passed as a \strong{named list}, they
#'   will be matched to datasets or time series tables by name. Names should
#'   correspond to datasets or time series tables that exist in the extract
#'   definition (unrecognized names will be ignored).}
#' }
#'
#' If no values are provided to the \code{datasets}
#' or \code{time_series_tables} arguments, all the existing datasets and/or
#' time series tables will be considered to be eligible for modification. That
#' is, the values passed to any dataset or time series table subfield arguments
#' will be evaluated relative to all of the datasets or time series tables that
#' exist in the extract.
#'
#' If any values are passed to the \code{datasets} or
#' \code{time_series_tables} arguments, the values passed to any of their
#' associated subfield arguments will be evaluated relative only to
#' the \emph{specified} datasets or time series tables.
#'
#' For example, if new datasets are added, then the values provided in
#' \code{ds_tables} will apply
#' to the new datasets only. Existing datasets can be modified either
#' by supplying their name to the \code{datasets} argument or by including the
#' desired additions to their subfields as a named list in the subfield
#' argument.
#'
#' For extract fields that take a single value, \code{add_to_extract} will
#' replace the existing value with the new value provided for that field.
#' It is not necessary to first remove this value using
#' \code{remove_from_extract}.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams submit_extract
#' @param extract A \code{nhgis_extract} object. This can be a user-defined
#'   extract (see \code{\link{define_extract_nhgis}()}) or an extract object
#'   returned from another \code{ipumsr} API function.
#' @param datasets Character vector of datasets to add or modify in the extract.
#'   Dataset names that do not already exist in the extract will be added along
#'   with the values passed to any dataset subfield arguments. Dataset names
#'   that already exist in the extract will not be added, but their subfields
#'   will be modified based on the values provided to any dataset subfield
#'   arguments. See details.
#' @param ds_tables Character vector or list of summary tables to add to the
#'   extract. This is a subfield of \code{datasets}: values provided to this argument
#'   will be applied to the datasets included in \code{datasets} (see details for
#'   available syntax options). Required if any new datasets are being added to
#'   the extract.
#' @param ds_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in
#'   the requested summary tables. This is a subfield of \code{datasets}: values
#'   provided to this argument will be applied to the datasets included in
#'   \code{datasets} (see details for available syntax options). Required if
#'   any new datasets are being added to the extract.
#' @param ds_years Character or integer vector or list of years for which to
#'   obtain the data contained in the requested summary tables. Use "*" to
#'   select all available years for the specified dataset. This is a subfield of
#'   \code{datasets}: values provided to this argument will be applied to the datasets
#'   included in \code{datasets} (see details for available syntax options).
#'   Not all datasets allow year selection; see \code{\link{get_nhgis_metadata}}
#'   to determine if a dataset allows year selection.
#' @param ds_breakdown_values Character vector or list of selected breakdown
#'   values to apply to the requested summary tables. If more than one breakdown
#'   value is requested, \code{breakdown_and_data_type_layout} must also be specified.
#'   This is a subfield of \code{datasets}: values provided to this argument will be
#'   applied to the datasets included in \code{datasets} (see details for
#'   available syntax options).
#' @param time_series_tables Character vector of time series tables to add or
#'   modify in the extract. Time series table names that do not already exist in
#'   the extract will be added along with the values passed to any time series
#'   table subfield arguments. Time series table names that already exist in the
#'   extract will not be added, but their subfields will be modified based on
#'   the values provided to any time series table subfield arguments.
#' @param tst_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in the
#'   provided times series tables. This is a subfield of
#'   \code{time_series_tables}: values provided to this argument will be applied
#'   to the time series tables included in \code{time_series_tables} (see details
#'   for available syntax options). Required if any new time series tables are
#'   being added to the extract.
#' @param shapefiles Character vector of shapefiles to add to the extract, if
#'   any. For more information on NHGIS shapefiles, click here.
#' @param ... Ignored
#'
#' @return A modified \code{nhgis_extract} object
#'
#' @export
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = "1990_STF1",
#'   ds_tables = c("NP1", "NP2"),
#'   ds_geog_levels = "county"
#' )
#'
#' # Modify existing datasets in extract:
#' extract <- add_to_extract(
#'   extract,
#'   ds_tables = "NP3"
#' )
#'
#' # Add new dataset. New subfields will only be attached to datasets specified
#' # in the datasets argument:
#' extract <- add_to_extract(
#'   extract,
#'   datasets = "1980_STF1",
#'   ds_tables = "NT1A",
#'   ds_geog_levels = c("county", "state")
#' )
#'
#' # Modify existing datasets.
#' # Vectors recycle to all datasets, lists match by index or name.
#' add_to_extract(
#'   extract,
#'   ds_tables = list(`1990_STF1` = "NP4", `1980_STF1` = "NT1B"),
#'   ds_geog_levels = "nation"
#' )
add_to_extract.nhgis_extract <- function(extract,
                                         description = NULL,
                                         datasets = NULL,
                                         ds_tables = NULL,
                                         ds_geog_levels = NULL,
                                         ds_years = NULL,
                                         ds_breakdown_values = NULL,
                                         geographic_extents = NULL,
                                         breakdown_and_data_type_layout = NULL,
                                         time_series_tables = NULL,
                                         tst_geog_levels = NULL,
                                         tst_layout = NULL,
                                         shapefiles = NULL,
                                         data_format = NULL,
                                         ...) {

  extract <- copy_ipums_extract(extract)

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

  extract <- add_nested_fields(
    extract,
    datasets = datasets,
    ds_tables = ds_tables,
    ds_geog_levels = ds_geog_levels,
    ds_years = ds_years,
    ds_breakdown_values = ds_breakdown_values
  )

  extract <- add_nested_fields(
    extract,
    time_series_tables = time_series_tables,
    tst_geog_levels = tst_geog_levels
  )

  extract <- modify_flat_fields(
    extract,
    shapefiles = shapefiles,
    geographic_extents = geographic_extents,
    modification = "add"
  )

  extract <- modify_flat_fields(
    extract,
    description = description,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    tst_layout = tst_layout,
    modification = "replace"
  )

  if (!is.null(extract$datasets)) {
    extract$data_format <- extract$data_format %||% "csv_header"
    extract$breakdown_and_data_type_layout <-
      extract$breakdown_and_data_type_layout %||% "separate_files"
  }

  if (!is.null(extract$time_series_tables)) {
    extract$data_format <- extract$data_format %||% "csv_header"
    extract$tst_layout <- extract$tst_layout %||% "time_by_column_layout"
  }

  extract <- validate_ipums_extract(extract)

  extract

}

#' Remove values from an existing IPUMS extract
#'
#' @description
#' Remove values for specific fields in an existing \code{ipums_extract}
#' object. This function is an S3 generic whose behavior will depend on the
#' class (i.e. collection) of the extract being modified.
#'
#' In general, for a given collection, the arguments to
#' \code{remove_from_extract} are identical to those used when defining an
#' extract for that collection. For more about defining an extract, click
#' \link[=define_extract]{here}.
#'
#' For collection-specific documentation, see the links below:
#'
#' \itemize{
#'   \item To remove from an \strong{NHGIS} extract, click
#'     \link[=remove_from_extract.nhgis_extract]{here}
#'   \item To remove from a \strong{microdata} extract, click
#'     \link[=remove_from_extract.usa_extract]{here}
#' }
#'
#' To add new values to an extract, see
#' \code{\link{add_to_extract}()}.
#'
#' @param extract An object inheriting from \code{ipums_extract}
#' @param ... Additional arguments specifying the extract fields and values to
#'   remove from the extract. The available arguments correspond to the
#'   available arguments in the extract definition function for the class of the
#'   extract specified in \code{extract}.
#'
#' @family ipums_api
#'
#' @return An object of the same class as \code{extract} containing the modified
#'   extract definition
#'
#' @export
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

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
#' Remove existing values present in extract fields of an NHGIS extract.
#' This can include removing entire datasets and/or time series tables (along
#' with their associated subfields) or removing subfield values for existing
#' datasets and/or time series tables.
#'
#' To add new values or replace existing values in an NHGIS extract, see
#' \code{\link[=remove_from_extract.nhgis_extract]{remove_from_extract}()}.
#' When replacing values, it is recommended to first add new values using
#' \code{add_to_extract} before removing unwanted values with
#' \code{remove_from_extract} to avoid the possibility of producing an invalid
#' extract specification.
#'
#' In general, removing from an extract follows the same syntax conventions
#' as used in \code{\link{define_extract_nhgis}()}. See details for more
#' information on how values passed to dataset and/or time series table
#' subfields are interpreted.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table. Dataset subfields are
#' prefixed with \code{ds_}, while time series table subfields are prefixed with
#' \code{tst_}.
#'
#' There are three ways the values passed to these arguments can be provided:
#'
#' \itemize{
#'   \item{If values are passed as a \strong{vector}, they will be
#'   applied across all datasets or time series tables present in the extract}
#'   \item{If values are passed as an \strong{unnamed list}, they will be
#'   matched to datasets or time series tables by index. That
#'   is, the first element will be associated with the first dataset in the
#'   extract, the second element with the second dataset in
#'   the extract, etc.}
#'   \item{If values are passed as a \strong{named list}, they
#'   will be matched to datasets or time series tables by name. Names should
#'   correspond to datasets or time series tables that exist in the extract
#'   definition (unrecognized names will be ignored).}
#' }
#'
#' Importantly, subfields are modified after the removal of any datasets
#' and time series tables specified in either the \code{datasets} or
#' \code{time_series_tables} arguments. This can cause confusion if providing
#' unnamed lists to subfield arguments (i.e. removing values from datasets
#' or time series tables by index). It is safest to use the named list syntax
#' for subfields to avoid this ambiguity.
#'
#' Any extract fields that are rendered irrelevant after modifying the extract
#' will be automatically removed. (For instance, if all time
#' series tables are removed from an extract, \code{tst_layout} will also be
#' removed.) Thus, it is not necessary to explicitly remove these values. To
#' replace the existing values for these fields, see \code{add_to_extract}.
#'
#' Note that it is possible to produce invalid extracts using
#' \code{remove_from_extract} (for instance, an extract that includes a
#' time series table without associated geographic levels). This can occur if
#' you intend to replace the existing values for a required extract field.
#' If your goal is not simply to add or remove values, but to replace values in
#' an extract, it is recommended that you first use \code{add_to_extract} and
#' then use \code{remove_from_extract}, as this will avoid the possibility
#' of temporarily producing an invalid extract. Alternatively, you can set
#' \code{validate = FALSE} in \code{remove_from_extract} to prevent extract
#' validation while you make the replacement.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams submit_extract
#' @param extract A \code{nhgis_extract} object. This can be a user-defined
#'   extract (see \code{\link{define_extract_nhgis}()}) or an extract object
#'   returned from another \code{ipumsr} API function.
#' @param datasets Character vector of datasets to remove from the extract.
#'   All dataset subfields associated with these datasets will also be removed.
#' @param ds_tables Character vector or list of summary tables to remove from
#'   datasets in the extract. This is a subfield of \code{datasets}: values
#'   provided to this argument will be applied to all the datasets that exist
#'   in the extract after removing the datasets specificed in \code{datasets}.
#'   See details for available syntax options.
#' @param ds_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") to remove from datasets in the extract.
#'   This is a subfield of \code{datasets}: values provided to this argument
#'   will be applied to all the datasets that exist in the extract after
#'   removing the datasets specified in \code{datasets}. See details for
#'   available syntax options.
#' @param ds_years Character or integer vector or list of years to remove from
#'   datasets in the extract. This is a subfield of \code{datasets}: values
#'   provided to this argument will be applied to all the datasets that exist in
#'   the extract after removing the datasets specified in \code{datasets}. See
#'   details for available syntax options.
#' @param ds_breakdown_values Character vector or list of selected breakdown
#'   values to remove from datasets in the extract. This is a subfield of
#'   \code{datasets}: values provided to this argument will be applied to all
#'   the datasets that exist in the extract after removing the datasets
#'   specified in \code{datasets}. See details for available syntax options.
#' @param time_series_tables Character vector of time series tables to remove
#'   from the extract. All time series table subfields associated with these
#'   time series tables will also be removed.
#' @param tst_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") to remove from time series tables in the
#'   extract. This is a subfield of \code{time_series_tables}: values
#'   provided to this argument will be applied to all the time series tables
#'   that exist in the extract after removing the time series tables specified
#'   in \code{time_series_tables}. See details for available syntax options.
#' @param shapefiles Character vector of shapefiles to remove from the extract,
#'   if any. For more information on NHGIS shapefiles, click here.
#' @param ... Ignored
#'
#' @return A modified \code{nhgis_extract} object
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' extract <- define_extract_nhgis(
#'   datasets = "1990_STF1",
#'   ds_tables = c("NP1", "NP2", "NP3"),
#'   ds_geog_levels = "county",
#'   time_series_tables = c("CW3", "CW5"),
#'   tst_geog_levels = c("state", "county")
#' )
#'
#' # Modify existing datasets in extract:
#' remove_from_extract(
#'   extract,
#'   ds_tables = "NP3"
#' )
#'
#' # Remove an entire dataset
#' remove_from_extract(
#'   extract,
#'   datasets = "1990_STF1"
#' )
#'
#' # Use a list to remove subfield values from specific datasets or time
#' # series tables. Named lists map by name, unnamed lists map by index.
#' remove_from_extract(
#'   extract,
#'   tst_geog_levels = list(CW5 = "state", CW3 = "county")
#' )
remove_from_extract.nhgis_extract <- function(extract,
                                              datasets = NULL,
                                              ds_tables = NULL,
                                              ds_geog_levels = NULL,
                                              ds_years = NULL,
                                              ds_breakdown_values = NULL,
                                              geographic_extents = NULL,
                                              time_series_tables = NULL,
                                              tst_geog_levels = NULL,
                                              shapefiles = NULL,
                                              ...) {

  extract <- copy_ipums_extract(extract)

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

  # Remove full fields first
  extract <- remove_nested_fields(
    extract,
    datasets = datasets,
    subfields = c("ds_tables", "ds_geog_levels",
                  "ds_years", "ds_breakdown_values"),
    ancillary_fields = c("geographic_extents",
                         "breakdown_and_data_type_layout")
  )

  extract <- remove_nested_fields(
    extract,
    time_series_tables = time_series_tables,
    subfields = "tst_geog_levels",
    ancillary_fields = "tst_layout"
  )

  extract <- remove_subfields(
    extract,
    field = "datasets",
    ds_tables = ds_tables,
    ds_geog_levels = ds_geog_levels,
    ds_years = ds_years,
    ds_breakdown_values = ds_breakdown_values
  )

  extract <- remove_subfields(
    extract,
    field = "time_series_tables",
    tst_geog_levels = tst_geog_levels
  )

  extract <- modify_flat_fields(
    extract,
    shapefiles = shapefiles,
    geographic_extents = geog_extent_lookup(
      unlist(geographic_extents),
      state_geog_lookup$abbs
    ),
    modification = "remove"
  )

  # If removal results in extract with no ds/tst, remove irrelevant values
  # for data format
  if (is.null(extract$datasets) && is.null(extract$time_series_tables)) {
    extract["data_format"] <- list(NULL)
  }

  tryCatch(
    extract <- validate_ipums_extract(extract),
    error = function(cond) {
      stop(
        conditionMessage(cond),
        "\nTo replace existing values in an extract, first add new values ",
        "with `add_to_extract()`, then remove existing ones.",
        call. = FALSE
      )
    }
  )

  extract

}

# > Get info on recent extracts ----

#' Get information on recent extracts
#'
#' @description
#' Get information on recent extracts for a given IPUMS collection
#' via the IPUMS API, returned either as a list or tibble. For an overview of
#' ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @details
#' `get_last_extract_info` is a convenience function that is
#' similar to `get_recent_extracts_info_list(..., how_many = 1)`,
#' but returns an [`ipums_extract`][ipums_extract-class] rather than
#' a list of `ipums_extract` objects.
#'
#' @param collection The code for an IPUMS data collection. For a list of the
#'   codes used to refer to the data collections, see [ipums_data_collections()].
#' @param how_many Number of recent extracts for which you'd like information.
#'   Defaults to 10 extracts.
#' @inheritParams submit_extract
#'
#' @family ipums_api
#' @return For `get_recent_extracts_info_list()`, a list of extract objects. For
#'   `get_recent_extracts_info_tbl()`, a [`tibble`][tibble::tbl_df-class] with
#'   information on one extract in each row.
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
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submit_extract(my_extract)
#'
#' # Oops, forgot to capture the return object from submit_extract. Grab it with:
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
#'
#' @export
get_recent_extracts_info_list <- function(collection,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {

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
get_recent_extracts_info_tbl <- function(collection,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract_list <- get_recent_extracts_info_list(
    collection,
    how_many,
    api_key
  )

  extract_list_to_tbl(extract_list)
}

#' @rdname get_recent_extracts_info
#' @export
get_last_extract_info <- function(collection,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  get_recent_extracts_info_list(collection, 1, api_key)[[1]]
}


#' Convert a tibble of extract definitions to a list
#'
#' Convert a [`tibble`][tibble::tbl_df-class] (or
#' [`data.frame`][base::data.frame()]) of extract definitions, such as that
#' returned by [get_recent_extracts_info_tbl()], to a list of
#' [`ipums_extract`][ipums_extract-class] objects. For an overview of ipumsr microdata API
#' functionality, see `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @param extract_tbl A [`tibble`][tibble::tbl_df-class] (or
#'   [`data.frame`][base::data.frame()]) that contains
#'   the specifications for one or more [`ipums_extract`][ipums_extract-class] objects.
#' @param validate Logical (`TRUE` or `FALSE`) value indicating whether to
#'   check that each row of `extract_tbl` contains a valid and complete extract
#'   definition. Defaults to `TRUE`.
#'
#' @family ipums_api
#' @return A list of length equal to the number of extracts represented in
#'   `extract_tbl`. Unique extracts can be identified by their extract
#'   number, which is contained in the `number` column of
#'   `extract_tbl`
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

  # Internal logic in lieu of new S3 extract_tbl class needed to handle
  # dispatch...This could potentially be improved.
  if (collection == "nhgis") {

    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop(
        "Package \"tidyr\" must be installed to convert NHGIS extracts from ",
        "tbl to list format.",
        call. = FALSE
      )
    }

    extract_list <- purrr::pmap(
      collapse_nhgis_extract_tbl(extract_tbl),
      new_ipums_extract
    )

    extract_list <- purrr::map(
      extract_list,
      ~recycle_subfields(
        .x,
        datasets = c("ds_tables",
                     "ds_geog_levels",
                     "ds_years",
                     "ds_breakdown_values"),
        time_series_tables = "tst_geog_levels"
      )
    )

  } else {

    extract_list <- purrr::pmap(extract_tbl, new_ipums_extract)

  }

  if (validate) {
    extract_list <- purrr::walk(extract_list, validate_ipums_extract)
  }

  extract_list

}

#' Convert a list of extract definitions to a tibble
#'
#' Convert a list of [`ipums_extract`][ipums_extract-class] objects to a
#' [`tibble`][tibble::tbl_df-class] in containing the specifications for
#' one or more extracts. For an overview of ipumsr microdata API functionality, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @md
#'
#' @param extract_list A list of [`ipums_extract`][ipums_extract-class] objects.
#'
#' @family ipums_api
#' @return A [`tibble`][tibble::tbl_df-class] representing the specifications
#'   for each of the extracts represented in `extract_list`
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
#' @md
#'
#' @family ipums_api
#' @return A [`tibble`][tibble::tbl_df-class] with three columns containing the
#'   full collection name, the corresponding code used by the IPUMS API, and the
#'   status of API support for the collection.
#'
#' @export
#'
#' @examples
#' # Print a tibble of all IPUMS data collections:
#' ipums_data_collections()
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
#' Set your IPUMS API key for the duration of your session, or indefinitely by
#' adding it to the file ".Renviron" in your home directory. In either case,
#' this function works by assigning your API key as the value of the environment
#' variable `IPUMS_API_KEY`. If you choose to save your key to ".Renviron", this
#' function will create a backup copy of the file before modifying. This
#' function is modeled after the `census_api_key()` function
#' from the R package [tidycensus](https://walker-data.com/tidycensus/).
#'
#' @md
#'
#' @param api_key API key associated with your user account, formatted in
#'   quotes.
#' @param save Do you want to save this value for future sessions by adding it
#'   to the file ".Renviron" in your home directory? Defaults to `FALSE`.
#' @param overwrite Do you want to overwrite any existing value of
#'   `IPUMS_API_KEY` in the file ".Renviron"? Defaults to `FALSE`.
#'
#' @return The value of `api_key`, invisibly.
#' @family ipums_api
#' @export
set_ipums_api_key <- function(api_key, save = FALSE, overwrite = FALSE) {
  if (save) {
    home_dir <- Sys.getenv("HOME")
    renviron_file <- file.path(home_dir, ".Renviron")
    if (!file.exists(renviron_file)) {
      file.create(renviron_file)
      renviron_lines <- character(0)
    } else {
      file.copy(renviron_file, to = file.path(home_dir, ".Renviron_backup"))
      message(
        "Existing .Renviron file copied to '", home_dir, "/.Renviron_backup' ",
        "for backup purposes."
      )

      renviron_lines <- readLines(renviron_file)

      if (isTRUE(overwrite)) {
        renviron_lines <- renviron_lines[
          !fostr_detect(renviron_lines, "^IPUMS_API_KEY")
        ]
        writeLines(renviron_lines, con = renviron_file)
      } else {
        if (any(fostr_detect(renviron_lines, "^IPUMS_API_KEY"))) {
          stop(
            "A saved IPUMS_API_KEY already exists. To overwrite it, set ",
            "argument `overwrite` to TRUE.",
            call. = FALSE
          )
        }
      }
    }

    line_to_set_ipums_api_key <- paste0("IPUMS_API_KEY = \"", api_key, "\"")
    writeLines(
      c(renviron_lines, line_to_set_ipums_api_key),
      con = renviron_file
    )
    Sys.setenv(IPUMS_API_KEY = api_key)
    message("Your IPUMS API key has been set and saved for future sessions.")
  }
  Sys.setenv(IPUMS_API_KEY = api_key)
  message(
    "Your IPUMS API key has been set. To save your key for future sessions, ",
    "set argument `save` to TRUE."
  )
  invisible(api_key)
}

# Non-exported functions ---------------------------------------------------

#' Create a new extract object
#'
#' @description
#' Creates an object inheriting from class \code{ipums_extract} for use in
#' interacting with the IPUMS extract API.
#'
#' @param collection Character indicating the IPUMS collection for this extract.
#'   See \code{\link{ipums_data_collections}()}.
#' @param description Description of the extract.
#' @param submitted Logical indicating whether this extract has been sumitted
#'   to the IPUMS extract API. See \code{\link{submit_extract}()}.
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
#' @return An object inheriting from class \code{ipums_extract} containing the
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
#' @description
#' Creates a classed JSON object to allow for collection-specific method
#' dispatch when reading extract definitions from JSON.
#'
#' @param json A JSON-formatted extract definition
#' @param collection The collection of the extract represented in \code{json}
#'
#' @return JSON-formatted text with class \code{ipums_json}. Each collection
#'  produces an object of its own class.
#'
#' @noRd
new_ipums_json <- function(json, collection) {
  structure(
    json,
    class = c(paste0(collection, "_json"), "ipums_json", class(json))
  )
}

standardize_extract_identifier <- function(extract) {
  if (inherits(extract, "ipums_extract")) return(extract)
  if (length(extract) == 1) {
    extract <- fostr_split(extract, ":")[[1]]
  }

  if (length(extract) != 2) {
    stop(
      paste0(
        "Expected extract to be an extract object, a ",
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


validate_ipums_extract <- function(x) {
  UseMethod("validate_ipums_extract")
}

#' Validate the structure of an NHGIS extract object
#'
#' @description
#' Ensures that the structure of a provided \code{nhgis_extract} object is
#' compatible with the structure expected for interacting with the IPUMS extract
#' API.
#'
#' Validation checks currently implemented are:
#'
#' \itemize{
#'   \item{An extract contains at least one dataset, time series table, or
#'     shapefile}
#'   \item{Fields that take a single value are of length one and have accepted
#'     values}
#'   \item{An extract contains a collection and description}
#'   \item{All datasets in an extract have associated tables and geog levels}
#'   \item{All dataset subfields are of the same length as the number of
#'     datasets in the extract}
#'   \item{All time series tables in an extract have associated geog levels}
#'   \item{All time series table subfields are of the same length as the number
#'     of time series tables in the extract}
#' }
#'
#' @param x An \code{nhgis_extract} object
#'
#' @return Returns the input extract object
#'
#' @export
#'
#' @noRd
validate_ipums_extract.nhgis_extract <- function(x) {

  types <- nhgis_extract_types(x)

  if (length(types) == 0) {
    stop(
      "An nhgis_extract must contain at least one of `datasets`, ",
      "`time_series_tables`, or `shapefiles`.",
      call. = FALSE
    )
  }

  has_na <- purrr::map_lgl(
    types,
    ~any(is.na(x[[.]]))
  )

  if (any(has_na)) {
    stop(
      "An nhgis_extract cannot include missing values in `",
      paste0(types[has_na], collapse = "`, `"), "`.",
      call. = FALSE
    )
  }

  must_be_single <- c("description",
                      "breakdown_and_data_type_layout",
                      "tst_layout",
                      "data_format")

  is_too_long <- purrr::map_lgl(
    must_be_single,
    ~length(x[[.]]) > 1
  )

  if (any(is_too_long)) {
    stop(
      "The following fields of an `nhgis_extract` must be of length 1: `",
      paste0(must_be_single[is_too_long], collapse = "`, `"), "`.",
      call. = FALSE
    )
  }

  is_missing <- purrr::map_lgl(
    c("collection", "description"),
    ~is_na(x[[.]]) | is_empty(x[[.]])
  )

  is_present <- !purrr::map_lgl(
    names(x),
    ~is_empty(x[[.]]) || any(is.na(x[[.]]))
  )

  vars_present <- names(x)[is_present]

  if (any(is_missing)) {
    stop(
      "An nhgis_extract must contain values for: `",
      paste0(c("collection", "description")[is_missing], collapse = "`, `"),
      "`.",
      call. = FALSE
    )
  }

  if (!is_null(x$data_format)) {
    if (!x$data_format %in% c("csv_header", "csv_no_header", "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_header`, `csv_no_header`",
        " or `fixed_width`.",
        call. = FALSE
      )
    }
  }

  if (!is_null(x$breakdown_and_data_type_layout)) {
    if (!x$breakdown_and_data_type_layout %in% c("single_file",
                                                 "separate_files")) {
      stop(
        "`breakdown_and_data_type_layout` must be one of `single_file`",
        " or `separate_files`.",
        call. = FALSE
      )
    }
  }

  if (!is_null(x$tst_layout)) {
    if (!x$tst_layout %in% c("time_by_row_layout",
                             "time_by_column_layout",
                             "time_by_file_layout")) {
      stop(
        "`tst_layout` must be one of `time_by_row_layout`, ",
        "`time_by_column_layout` or `time_by_file_layout`.",
        call. = FALSE
      )
    }
  }

  if ("datasets" %in% types) {

    must_be_non_missing <- c("ds_tables", "ds_geog_levels", "data_format")
    must_have_same_length <- c("ds_tables", "ds_geog_levels",
                               "ds_years", "ds_breakdown_values")

    is_missing <- purrr::map_lgl(
      must_be_non_missing[1:2],
      function(y) any(
        purrr::map_lgl(
          x[[y]],
          ~any(is.na(.x)) || is_empty(.x)
        )
      )
    )

    missing_df <- is_null(x$data_format) || is_na(x$data_format)

    is_missing <- c(is_missing, missing_df)

    if (any(is_missing)) {
      stop(
        "An nhgis_extract that contains datasets must also contain ",
        "values for: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`.",
        call. = FALSE
      )
    }

    # Non-list elements will be recycled, so only need to worry about
    # var lengths if lists are provided in args.
    var_length <- purrr::map_dbl(must_have_same_length, ~length(x[[.]]))

    wrong_length <- purrr::map_lgl(
      must_have_same_length,
      ~is_list(x[[.]]) && length(x[[.]]) != length(x$datasets)
    )

    length_msg <- purrr::imap_chr(
      must_have_same_length,
      ~paste0("`", .x, "` (", var_length[.y], ")")
    )

    if (any(wrong_length)) {
      stop(
        "The number of selections provided in ",
        paste0(length_msg[wrong_length], collapse = ", "),
        " does not match the number of datasets (", length(x$datasets),
        "). \nTo recycle selections across datasets, ensure values are stored ",
        "in a vector, not a list.",
        call. = FALSE
      )
    }

  } else {

    ds_sub_vars <- c("ds_tables", "ds_geog_levels",
                     "ds_years", "ds_breakdown_values",
                     "geographic_extents",
                     "breakdown_and_data_type_layout")

    extra_vars <- ds_sub_vars[ds_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "The following parameters are not relevant for an nhgis_extract that ",
        "does not include any datasets: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if ("time_series_tables" %in% types) {

    must_be_non_missing <- c("tst_geog_levels",
                             "data_format",
                             "tst_layout")

    is_missing <- purrr::map_lgl(
      must_be_non_missing[1],
      function(y) any(
        purrr::map_lgl(
          x[[y]],
          ~any(is.na(.x)) || is_empty(.x)
        )
      )
    )

    missing_df <- is_null(x$data_format) || is_na(x$data_format)
    missing_tstl <- is_null(x$tst_layout) ||
      is_na(x$tst_layout)

    is_missing <- c(is_missing, missing_df, missing_tstl)

    if (any(is_missing)) {
      stop(
        "An nhgis_extract that contains time series tables must also contain ",
        "values for: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`.",
        call. = FALSE
      )
    }

    if (is_list(x$tst_geog_levels) &&
        length(x$tst_geog_levels) != length(x$time_series_tables)) {
      stop(
        "The number of selections provided in `tst_geog_levels` (",
        length(x$tst_geog_levels),
        ") does not match the number of time series tables (",
        length(x$time_series_tables),
        "). \nTo recycle selections across time series tables, ensure values ",
        "are stored in a vector, not a list.",
        call. = FALSE
      )
    }

  } else {

    tst_sub_vars <- c("tst_geog_levels", "tst_layout")
    extra_vars <- tst_sub_vars[tst_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "The following parameters are not relevant for an nhgis_extract that ",
        "does not include any time series tables: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if ("shapefiles" %in% types && length(types) == 1) {

    not_relevant <- c("data_format")
    extra_vars <- intersect(not_relevant, vars_present)

    if (length(extra_vars) > 0) {
      warning(
        "The following parameters are not relevant for an nhgis_extract that ",
        "does not include any time series tables or datasets: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }
  }

  x

}

#' Validate the structure of an IPUMS USA extract object
#'
#' @description
#' Ensures that the structure of a provided \code{usa_extract} object is
#' compatible with the structure expected for interacting with the IPUMS extract
#' API.
#'
#' Validation checks currently implemented are:
#'
#' \itemize{
#'   \item{An extract contains samples, variables, a description,
#'     data structure, and data format,}
#'   \item{Fields that take a single value have accepted values}
#'   \item{rectangular_on is only provided if data structure is rectangular}
#' }
#'
#' @param x A \code{usa_extract} object
#'
#' @return Returns the input extract object
#'
#' @export
#'
#' @noRd
validate_ipums_extract.usa_extract <- function(x) {

  must_be_non_missing <- c("description", "data_structure",
                           "data_format", "samples", "variables")

  is_missing <- purrr::map_lgl(
    must_be_non_missing,
    ~any(is.null(x[[.]])) || any(is.na(x[[.]])) || any(is_empty(x[[.]]))
  )

  if (any(is_missing)) {
    stop(
      "The following elements of a `", x$collection, "_extract` ",
      "must not contain missing values: ",
      paste0(must_be_non_missing[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  # Remove these once we allow for hierarchical and rectangular on H extracts
  if (x$data_structure != "rectangular") {
    stop(
      "Currently, the `data_structure` argument must be equal to ",
      "\"rectangular\"; in the future, the API will also support ",
      "\"hierarchical\" extracts.",
      call. = FALSE
    )
  }
  if (x$rectangular_on != "P") {
    stop(
      "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
      "the future, the API will also support `rectangular_on = \"H\".",
      call. = FALSE
    )
  }

  stopifnot(x$data_format %in% c("fixed_width", "csv","stata", "spss", "sas9"))
  stopifnot(x$data_structure %in% c("rectangular", "hierarchical"))

  if (x$data_structure == "rectangular" & !x$rectangular_on %in% c("H", "P")) {
    stop("If `data_structure` is 'rectangular', `rectangular_on` must be one ",
         "of 'H' or 'P'", call. = FALSE)
  }

  if (x$data_structure == "hierarchical" & !is.na(x$rectangular_on)) {
    stop("If `data_structure` is 'hierarchical', `rectangular_on` must be ",
         "missing", call. = FALSE)
  }

  x

}

#' @export
validate_ipums_extract.cps_extract <- function(x) {

  must_be_non_missing <- c("description", "data_structure",
                           "data_format", "samples", "variables")

  is_missing <- purrr::map_lgl(
    must_be_non_missing,
    ~any(is.null(x[[.]])) || any(is.na(x[[.]])) || any(is_empty(x[[.]]))
  )

  if (any(is_missing)) {
    stop(
      "The following elements of a `", x$collection, "_extract` ",
      "must not contain missing values: ",
      paste0(must_be_non_missing[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  # Remove these once we allow for hierarchical and rectangular on H extracts
  if (x$data_structure != "rectangular") {
    stop(
      "Currently, the `data_structure` argument must be equal to ",
      "\"rectangular\"; in the future, the API will also support ",
      "\"hierarchical\" extracts.",
      call. = FALSE
    )
  }
  if (x$rectangular_on != "P") {
    stop(
      "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
      "the future, the API will also support `rectangular_on = \"H\".",
      call. = FALSE
    )
  }

  stopifnot(x$data_format %in% c("fixed_width", "csv","stata", "spss", "sas9"))
  stopifnot(x$data_structure %in% c("rectangular", "hierarchical"))

  if (x$data_structure == "rectangular" & !x$rectangular_on %in% c("H", "P")) {
    stop("If `data_structure` is 'rectangular', `rectangular_on` must be one ",
         "of 'H' or 'P'", call. = FALSE)
  }

  if (x$data_structure == "hierarchical" & !is.na(x$rectangular_on)) {
    stop("If `data_structure` is 'hierarchical', `rectangular_on` must be ",
         "missing", call. = FALSE)
  }

  x

}

#' @export
validate_ipums_extract.ipums_extract <- function(x) {

  # Throw error if no API for collection
  ipums_api_version(x$collection)

  warning(
    paste0("Unable to validate extract for collection `", x$collection, "`.")
  )

  x

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

  types <- nhgis_extract_types(x)

  style_shp <- extract_field_styler(NHGIS_SHP_COLOR, "bold")
  style_extents <- extract_field_styler(NHGIS_DS_COLOR, "bold")

  if("datasets" %in% types) {

    ds_to_cat <- purrr::map(
      seq_along(x$datasets),
      ~format_dataset_for_printing(
        x$datasets[[.x]],
        x$ds_tables[[.x]],
        x$ds_geog_levels[[.x]],
        x$ds_years[[.x]],
        x$ds_breakdown_values[[.x]]
      )
    )

    if (!is.null(x$geographic_extents)) {
      extents_to_cat <- paste0(
        "\n\n",
        print_truncated_vector(
          x$geographic_extents,
          style_extents("Geographic extents: "),
          FALSE
        )
      )

      ds_to_cat <- c(ds_to_cat, extents_to_cat)
    }

    ds_to_cat <- purrr::reduce(ds_to_cat, paste0)

  } else {
    ds_to_cat <- NULL
  }

  if("time_series_tables" %in% types) {

    tst_to_cat <- purrr::map(
      seq_along(x$time_series_tables),
      ~format_tst_for_printing(
        x$time_series_tables[[.x]],
        x$tst_geog_levels[[.x]]
      )
    )

    tst_to_cat <- purrr::reduce(tst_to_cat, paste0)

  } else {
    tst_to_cat <- NULL
  }

  if("shapefiles" %in% types) {

    shp_to_cat <- paste0(
      "\n\n",
      print_truncated_vector(
        x$shapefiles,
        style_shp("Shapefiles: "),
        FALSE
      )
    )

  } else {
    shp_to_cat <- NULL
  }

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    ds_to_cat,
    tst_to_cat,
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


format_dataset_for_printing <- function(dataset,
                                        ds_tables,
                                        geog_levels,
                                        ds_years = NULL,
                                        ds_breakdown_values = NULL) {

  style_field <- extract_field_styler(NHGIS_DS_COLOR, "bold")
  style_subfield <- extract_field_styler("bold")

  output <- paste0(
    "\n\n",
    print_truncated_vector(
      dataset,
      style_field("Dataset: "),
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      ds_tables,
      style_subfield("Tables: "),
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      geog_levels,
      style_subfield("Geog Levels: "),
      FALSE
    )
  )

  if (!is.null(ds_years)) {
    output <- paste0(
      output,
      "\n  ",
      print_truncated_vector(
        ds_years,
        style_subfield("Years: "),
        FALSE
      )
    )
  }

  if (!is.null(ds_breakdown_values)) {
    output <- paste0(
      output,
      "\n  ",
      print_truncated_vector(
        ds_breakdown_values,
        style_subfield("Breakdowns: "),
        FALSE
      )
    )
  }

  output

}

format_tst_for_printing <- function(time_series_table, tst_geog_levels) {

  style_field <- extract_field_styler(NHGIS_TST_COLOR, "bold")
  style_subfield <- extract_field_styler("bold")

  paste0(
    "\n\n",
    print_truncated_vector(
      time_series_table,
      style_field("Time Series Table: "),
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      tst_geog_levels,
      style_subfield("Geog Levels: "),
      FALSE
    )
  )

}

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
      data_tables = extract$ds_tables,
      geog_levels = extract$ds_geog_levels,
      years = extract$ds_years,
      breakdown_values = extract$ds_breakdown_values
    ),
    time_series_tables = format_nhgis_field_for_json(
      time_series_tables = extract$time_series_tables,
      geog_levels = extract$tst_geog_levels
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

  if (include_endpoint_info){
    endpoint_info <- list(
      collection = jsonlite::unbox(extract$collection),
      api_version = jsonlite::unbox(ipums_api_version(extract$collection))
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list)

}

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

  if (include_endpoint_info){
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

  if (include_endpoint_info){
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
#' @importFrom utils packageVersion
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
#'   be validated using \code{validate_ipums_extract}
#'
#' @return An object inheriting from class \code{ipums_extract} containing the
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
        ds_tables = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$data_tables))
        },
        ds_geog_levels = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$geog_levels))
        },
        ds_years = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$years))
        },
        ds_breakdown_values = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$breakdown_values))
        },
        geographic_extents = geog_extent_lookup(
          unlist(x$geographic_extents),
          state_geog_lookup$abbs
        ),
        breakdown_and_data_type_layout = x$breakdown_and_data_type_layout,
        time_series_tables = names(x$time_series_tables),
        tst_geog_levels = if (no_tsts) {
          NULL
        } else {
          purrr::map(x$time_series_tables, ~unlist(.x$geog_levels))
        },
        tst_layout = x$time_series_table_layout,
        shapefiles = unlist(x$shapefiles),
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
    "Received status code 400, with the following details:\n\n",
    paste0(response_detail, collapse = "\n\n")
  )
  return(error_message)
}

#' Check if an extract is ready for download
#'
#' @param extract An \code{ipums_extract} object
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

#' Modify an extract's non-nested fields
#'
#' Add new values, remove existing values, or replace existing values in
#' a selection of extract fields.
#'
#' @param extract ipums_extract object to revise
#' @param ... Arbitrary number of named arguments, where names correspond to
#'   extract fields to be modified and values correspond to the values that
#'   should be modified in those fields.
#' @param modification One of "add", "remove", or "replace" indicating how the
#'   values in \code{...} should be modified in the extract. If "add", values in
#'   \code{...} that do not yet exist in the extract will be added. If "remove",
#'   values in \code{...} that already exist in the extract will be removed. If
#'   "replace", values in \code{...} will replace the values that currently
#'   exist in the extract.
#'
#' @return A modified ipums_extract object
#'
#' @noRd
modify_flat_fields <- function(extract,
                               ...,
                               modification = c("add", "remove", "replace")) {

  modification <- match.arg(modification)
  dots <- rlang::list2(...)

  stopifnot(is_named(dots))

  if (modification == "add") {

    purrr::walk(
      names(dots),
      ~{
        if (is.null(dots[[.x]]) && is.null(extract[[.x]])) {
          extract[.x] <<- list(NULL)
        } else {
          extract[[.x]] <<- unlist(union(extract[[.x]], dots[[.x]]))
        }
      }
    )

  } else if (modification == "remove") {

    purrr::walk(
      names(dots),
      function(x) {
        values <- setdiff(extract[[x]], unlist(dots[[x]]))
        if (length(values) > 0) {
          extract[[x]] <<- values
        } else {
          extract[x] <<- list(NULL)
        }
      }
    )

  } else if (modification == "replace") {

    purrr::walk(
      names(dots),
      ~{
        if (!is.null(dots[[.x]])) {
          if (length(dots[[.x]]) > 1) {
            warning(
              "Multiple values passed to `", .x, "`, which must be length 1. ",
              "Only the first value will be used.",
              call. = FALSE
            )
          }
          extract[[.x]] <<- dots[[.x]][1]
        }
      }
    )

  }

  extract

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

  extract <- copy_ipums_extract(extract)

  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    warning(
      "The following fields were either not found in the provided extract ",
      "or cannot be modified: `",
      paste0(names(dots), collapse = "`, `"), "`",
      call. = FALSE
    )
  }

  if (!is.null(data_structure) && data_structure != "rectangular") {
    stop(
      "Currently, the `data_structure` argument must be equal to ",
      "\"rectangular\"; in the future, the API will also support ",
      "\"hierarchical\" extracts.",
      call. = FALSE
    )
  }

  if (!is.null(rectangular_on) && rectangular_on != "P") {
    stop(
      "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
      "the future, the API will also support `rectangular_on = \"H\".",
      call. = FALSE
    )
  }

  add_vars <- list(
    samples = samples,
    variables = variables
  )

  purrr::map(
    names(add_vars),
    ~if (any(add_vars[[.x]] %in% extract[[.x]])) {
      warning(
        "The following ", .x, " are already included in the ",
        "supplied extract definition, and thus will not be added: \"",
        paste0(
          intersect(add_vars[[.x]], extract[[.x]]),
          collapse = "\", \""
        ),
        "\"",
        call. = FALSE
      )
    }
  )

  extract <- modify_flat_fields(
    extract,
    samples = samples,
    variables = variables,
    modification = "add"
  )

  extract <- modify_flat_fields(
    extract,
    description = description,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    modification = "replace"
  )

  extract <- validate_ipums_extract(extract)

  extract

}

#' Add nested fields to an extract object
#'
#' Adds new values for parent fields and associates them with the provided
#' subfield values.
#'
#' @param extract An ipums_extract object to revise
#' @param ... Arbitrary number of named arguments, where names correspond to
#'   extract fields and values correspond to the values that should be added in
#'   those fields. The first entry in the list of arguments is interpreted as
#'   the parent field that the remaining subfield arguments nest within.
#'   The syntax for attaching subfield values to parent field values mirrors
#'   that used when defining an extract. Using this syntax, subfield arguments
#'   are evaluated relative to the values provided in the parent field argument
#'   (i.e. not relative to all values of the parent field in the extract).
#'
#' @return A modified ipums_extract object
#'
#' @noRd
add_nested_fields <- function(extract, ...) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))

  has_new_fields <- !is.null(dots[[1]])

  field <- names(dots[1])
  new_field_vals <- dots[[1]] %||% extract[[field]]

  new_subfield_vals <- dots[2:length(dots)]
  subfields <- names(new_subfield_vals)

  all_field_vals <- union(extract[[field]], new_field_vals)

  if (!is.null(all_field_vals)) {
    extract[[field]] <- all_field_vals
  }

  purrr::walk(
    subfields,
    function(var) {

      input_vals <- new_subfield_vals[[var]]
      input_is_list <- is_list(input_vals)
      input_is_named <- any(have_name(input_vals))

      not_in_field <- names(input_vals)[!names(input_vals) %in% all_field_vals]

      # Named vector is ambiguous. Named values should be provided in list
      if (!input_is_list && input_is_named) {
        warning(
          "Ignoring names in the specification for `", var,
          "`. To apply ",
          "values to ", field, " by name",
          ", ensure values are stored in a list",
          ", not a vector.",
          call. = FALSE
        )
      }

      # Index lists should match length of input parent fields
      if (input_is_list &&
          !input_is_named &&
          length(input_vals) != length(new_field_vals)) {

        if (has_new_fields) {
          warning(
            "The number of values in `", var, "` (",
            length(input_vals), ") does not match",
            " the number of ", field, " to be modified (",
            length(new_field_vals),
            "). Values will be matched to the specified ", field,
            " in index order. To recycle selections across ", field,
            ", ensure values are stored in a vector, not a list.",
            call. = FALSE
          )
        } else {
          warning(
            "The number of values in `", var, "` (",
            length(input_vals), ") does not match",
            " the number of ", field, " in this extract (",
            length(new_field_vals),
            "). Values will be matched to this extract's ", field,
            " in index order. To recycle selections across ", field,
            ", ensure values are stored in a vector, not a list.",
            call. = FALSE
          )
        }
      }

      # All names should exist in parent field
      if (length(not_in_field) > 0 && !(!input_is_list && input_is_named)) {
        warning(
          "The specification for `", var, "` references ",
          field, " that do not exist in this extract (\"",
          paste0(unique(not_in_field), collapse = "\", \""),
          "\"). These values will be ignored.",
          call. = FALSE
        )
      }
    }
  )

  if (has_new_fields) {
    subfield_vals_recycled <- purrr::map(
      new_subfield_vals,
      ~{
        if (any(have_name(.x))) {
          # If named, match to all possible datasets in extract
          recycle_to_named_list(.x, all_field_vals)
        } else {
          # If unnamed, map only to provided datasets
          recycle_to_named_list(.x, new_field_vals)
        }
      }
    )
  } else {
    subfield_vals_recycled <- purrr::map(
      new_subfield_vals,
      ~recycle_to_named_list(.x, all_field_vals)
    )
  }

  purrr::walk(
    subfields,
    ~{
      new_val <- reduce_list_by_name(
        c(extract[[.x]], subfield_vals_recycled[[.x]]),
        f = union
      )

      if (is_empty(new_val)) {
        extract[.x] <<- list(NULL)
      } else {
        extract[[.x]] <<- new_val
      }
    }
  )

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

  extract <- copy_ipums_extract(extract)


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

  to_remove <- list(
    samples = samples,
    variables = variables
  )

  purrr::walk(
    names(to_remove),
    ~if (any(!to_remove[[.x]] %in% extract[[.x]])) {
      warning(
        "The following ", .x, " are not included in the ",
        "supplied extract definition, and thus will not be removed: \"",
        paste0(
          setdiff(to_remove[[.x]], extract[[.x]]),
          collapse = "\", \""
        ),
        "\"",
        call. = FALSE
      )
    }
  )

  extract <- modify_flat_fields(
    extract,
    samples = samples,
    variables = variables,
    modification = "remove"
  )

  # I believe the only way to produce an invalid extract from removal is
  # to remove all fields of a certain value. This takes advantage of this fact
  # to improve the validation error message.
  tryCatch(
    extract <- validate_ipums_extract(extract),
    error = function(cond) {
      stop(
        conditionMessage(cond),
        "\nTo replace existing values in an extract, first add new values ",
        "with `add_to_extract()`, then remove existing ones.",
        call. = FALSE
      )
    }
  )

  extract

}

#' Remove nested fields from an extract object
#'
#' Removes fields that contain subfields along with their associated subfield
#' values from an extract.
#'
#' Ancillary fields are included as an option to help prevent the creation of
#' invalid extracts if all values in a given extract field are removed. For
#' instance, if all \code{time_series_tables} are removed from an
#' \code{nhgis_extract} object, the extract should not contain a value for
#' \code{tst_layout}, but \code{tst_layout} is not a nested field within
#' \code{time_series_tables} because it applies to all time series tables in an
#' extract.
#'
#' @param extract An ipums_extract object to revise
#' @param ... A single named argument, where the name corresponds to a
#'   field in \code{extract} that contains subfields. The values provided
#'   to this argument indicate the values of the specified field that should be
#'   removed from the extract along with all of their associated subfield
#'   values.
#' @param subfields Character vector indicating the names of the extract fields
#'   that are subfields of the field provided in \code{...}. For instance,
#'   for NHGIS extracts, "tst_geog_levels" is a subfield of
#'   "time_series_tables". The values provided to this argument indicate the
#'   subfields that will be removed along with any of the field values provided
#'   in \code{...}
#' @param ancillary_fields Character vector indicating the names of extract
#'   fields that are not subfields within the field provided in \code{...}, but
#'   are not relevant if no values exist for that field. See details.
#'
#' @return A modified ipums_extract object
#'
#' @noRd
remove_nested_fields <- function(extract, ..., subfields, ancillary_fields) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))
  stopifnot(length(dots) == 1)

  old_field_vals <- dots[[1]]
  field <- names(dots[1])

  not_in_extract <- old_field_vals[!old_field_vals %in% extract[[field]]]

  if (length(not_in_extract) > 0) {
    warning(
      "Some ", field, " (\"",
      paste0(not_in_extract, collapse = "\", \""),
      "\") could not be removed because they were not found in this ",
      "extract's ", field, " (\"",
      paste0(extract[[field]], collapse = "\", \""), "\").",
      call. = FALSE
    )
  }

  new_field_vals <- setdiff(extract[[field]], old_field_vals)

  if (length(new_field_vals) == 0) {

    no_new_field <- TRUE
    new_field_vals <- NULL

    extract[field] <- list(NULL)

    if (!is.null(ancillary_fields)) {
      purrr::walk(
        ancillary_fields,
        ~{
          extract[.x] <<- list(NULL)
        }
      )
    }

  } else {
    no_new_field <- FALSE
    extract[[field]] <- new_field_vals
  }

  purrr::walk(
    subfields,
    ~{
      if (no_new_field) {
        extract[.x] <<- list(NULL)
      } else {
        extract[.x] <<- list(extract[[.x]][new_field_vals])
      }
    }
  )

  extract

}

#' Remove values in extract subfields
#'
#' Remove specified values from indicated subfields without altering the
#' parent fields to which those values belong. To modify parent fields,
#' see \code{remove_nested_fields()}
#'
#' @param extract ipums_extract object to revise
#' @param field Character indicating the name of the parent field that the
#'   the subfields provided in \code{...} correspond to.
#' @param ... Arbitrary number of named arguments where names correspond to the
#'   names of the subfields to be modified and the values correspond to the
#'   values for those subfields that should be removed from the extract, if they
#'   exist. The names provided to this argument should correspond to subfields
#'   of the field indicated in \code{field}.
#'
#' @return A modified ipums_extract object
#'
#' @noRd
remove_subfields <- function(extract, field, ...) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))

  old_field_vals <- extract[[field]]

  if (is.null(old_field_vals)) {
    return(extract)
  }

  subfields <- names(dots)

  purrr::walk(
    subfields,
    function(var) {

      input_vals <- dots[[var]]
      input_is_list <- is_list(input_vals)
      input_is_named <- any(have_name(input_vals))

      not_in_field <- names(input_vals)[!names(input_vals) %in% old_field_vals]

      # Named vector is ambiguous. Named values should be provided in list
      if (!input_is_list && input_is_named) {
        warning(
          "Ignoring names in the specification for `", var,
          "`. To apply ",
          "values to ", field, " by name",
          ", ensure values are stored in a list",
          ", not a vector.",
          call. = FALSE
        )
      }

      # Index lists should match length of input parent fields
      if (input_is_list &&
          !input_is_named &&
          length(input_vals) != length(old_field_vals)) {
        warning(
          "The number of values in `", var, "` (",
          length(input_vals), ") does not match",
          " the number of ", field, " in this extract (",
          length(old_field_vals),
          "). Values will be matched to this extract's ", field,
          " in index order. To recycle selections across ", field,
          ", ensure values are stored in a vector, not a list.",
          call. = FALSE
        )
      }

      # All names should exist in parent field
      if (length(not_in_field) > 0 && !(!input_is_list && input_is_named)) {
        warning(
          "The specification for `", var, "` references ",
          field, " that do not exist in this extract (\"",
          paste0(unique(not_in_field), collapse = "\", \""),
          "\"). These values will be ignored.",
          call. = FALSE
        )
      }
    }
  )

  subfields <- names(dots)

  subfield_vals_recycled <- purrr::map(
    dots,
    ~recycle_to_named_list(.x, old_field_vals)
  )

  purrr::walk(
    subfields,
    ~{
      extract[[.x]] <<- reduce_list_by_name(
        c(extract[[.x]], subfield_vals_recycled[[.x]]),
        setdiff_null
      )
    }
  )

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
#'   \code{purrr::reduce}. See \code{purrr::reduce}
#'
#' @return Named list with a single entry for each unique name found in \code{l}
#'
#' @noRd
reduce_list_by_name <- function(l, f) {

  if (!is_named(l)) {
    return(l)
  }

  labs <- unique(names(l))

  l <- purrr::map(
    labs,
    ~purrr::reduce(l[.x == names(l)], f)
  )

  l <- setNames(l, labs)

  l

}

#' Calculate set difference with the empty set represented as NULL
#'
#' Convenience function to allow for easier removal of values from extracts
#' whose extract fields can contain NULL values.
#'
#' @param x,y Vectors to use to calculate set difference
#'
#' @return Same output as \code{setdiff}, except that empty set return values
#'   are NULL rather than length-0 vectors.
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
#' extract objects to tibble format. collection-specific functionality is
#' needed because NHGIS extracts return a tibble whose extracts are spread
#' across multiple rows. However, we cannot perform dispatch on a list of
#' extract objects directly, as is done in extract_list_to_tbl.
#'
#' @param x An object inheriting from \code{ipums_extract}
#'
#' @return A tibble representing the specifications for the extract \code{x}.
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
    geographic_extents = list(x$geographic_extents) %||% list(NULL),
    submitted = x$submitted,
    download_links = list(x$download_links),
    number = x$number,
    status = x$status
  )

  ds <- c(
    list(
      name = unlist(x$datasets) %||% NA_character_,
      ds_tables = unname(x$ds_tables),
      ds_geog_levels = unname(x$ds_geog_levels),
      ds_years = if (is_empty(x$ds_years)) {
        list(NULL)
      } else {
        unname(purrr::map(x$ds_years, ~.x))
      },
      ds_breakdown_values = if (is_empty(x$ds_breakdown_values)) {
        list(NULL)
      } else {
        unname(purrr::map(x$ds_breakdown_values, ~.x))
      },
      tst_layout = NA_character_
    ),
    base_vars
  )

  ts <- c(
    list(
      name = unlist(x$time_series_tables) %||% NA_character_,
      tst_geog_levels = unname(x$tst_geog_levels),
      tst_layout = x$tst_layout,
      ds_tables = list(NULL),
      ds_years = list(NULL),
      ds_breakdown_values = list(NULL)
    ),
    base_vars
  )

  shp <- c(
    list(
      name = unlist(x$shapefiles) %||% NA_character_,
      ds_tables = list(NULL),
      ds_years = list(NULL),
      ds_breakdown_values = list(NULL),
      tst_geog_levels = list(NULL),
      ds_geog_levels = list(NULL),
      tst_layout = NA_character_
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
                 "name", "ds_tables", "ds_geog_levels", "tst_geog_levels",
                 "ds_years", "ds_breakdown_values",  "geographic_extents",
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
#'   \code{get_recent_extracts_info_tbl(collection = "nhgis")}
#'
#' @return A tibble where each row represents a single NHGIS extract. Fields
#'   with multiple values are collapsed as list-columns.
#'
#' @noRd
collapse_nhgis_extract_tbl <- function(extract_tbl) {

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop(
      "Package \"tidyr\" must be installed to convert NHGIS extracts from tbl ",
      "to list format.",
      call. = FALSE
    )
  }

  stopifnot(unique(extract_tbl$collection) == "nhgis")

  # Convert pseudo-long extract_tbl format to extract-row format
  extract_tbl <- dplyr::group_by(extract_tbl, number, data_type)

  extract_tbl <- dplyr::mutate(
    extract_tbl,
    dplyr::across(
      c(ds_tables,
        ds_geog_levels,
        ds_years,
        ds_breakdown_values,
        tst_geog_levels),
      ~if (is.null(unlist(.x))) { .x } else { list(.x) }
    )
  )

  extract_tbl <- tidyr::pivot_wider(
    extract_tbl,
    names_from = data_type,
    values_from = name,
    values_fn = list
  )

  tbl_cols <- colnames(extract_tbl)

  extract_tbl <- dplyr::distinct(
    tidyr::fill(extract_tbl, dplyr::all_of(tbl_cols), .direction = "updown")
  )

  join_df <- tibble::tibble(
    shapefiles = list(NULL),
    time_series_tables = list(NULL),
    datasets = list(NULL)
  )

  join_cols <- intersect(tbl_cols, colnames(join_df))

  # Join to ensure all extract parameters are present
  extract_tbl <- dplyr::left_join(
    extract_tbl,
    join_df,
    by = join_cols
  )

  # For consistency of output after conversion to list
  # define_extract_nhgis() and tbl to list conversion should align on all fields
  extract_tbl <- dplyr::mutate(
    extract_tbl,
    dplyr::across(
      c(data_format, breakdown_and_data_type_layout, tst_layout),
      ~ifelse(is.na(.x), list(NULL), list(.x))
    )
  )

  # Reorder
  var_sort <- c("collection", "number",
                "description", "datasets", "ds_tables", "ds_geog_levels",
                "ds_years", "ds_breakdown_values", "geographic_extents",
                "breakdown_and_data_type_layout", "time_series_tables",
                "tst_geog_levels", "tst_layout", "shapefiles", "data_format",
                "submitted", "download_links", "status")

  extract_tbl <- extract_tbl[, var_sort]

  extract_tbl

}

#' This is currently used only to catch unexpected names in
#' \code{extract_tbl_to_list()}. However, unexpected names vary across
#' collections, so we use an S3 generic.
#'
#' @noRd
get_extract_tbl_fields <- function(x) {
  UseMethod("get_extract_tbl_fields")
}

#' @export
get_extract_tbl_fields.nhgis_extract <- function(x) {
  c(
    "collection", "description", "datasets", "ds_tables", "ds_geog_levels",
    "ds_years", "ds_breakdown_values", "geographic_extents",
    "breakdown_and_data_type_layout", "time_series_tables", "tst_geog_levels",
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

#' @importFrom rlang .data
#' @noRd
ipums_api_version <- function(collection) {

  versions <- dplyr::filter(
    ipums_data_collections(),
    .data$api_support != "none"
  )

  if(!collection %in% versions$code_for_api) {
    stop(
      paste0(
        "No API version found for collection \"", collection, "\"\n",
        "IPUMS API is currently available for the following collections: \"",
        paste0(versions$code_for_api, collapse = "\", \""), "\""
      ),
      call. = FALSE
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

#' Identify data types specified in an NHGIS extract
#'
#' An NHGIS extract will request at least one of dataset, time series tables or
#' shapefiles.
#'
#' @param extract An nhgis_extract object
#' @noRd
nhgis_extract_types <- function(extract) {

  stopifnot(extract$collection == "nhgis")

  possible_types <- c("datasets", "time_series_tables", "shapefiles")

  is_missing <- purrr::map_lgl(
    possible_types,
    ~all(is_empty(extract[[.]]))
  )

  types <- possible_types[!is_missing]

  if(length(types) == 0) {
    NULL
  } else {
    types
  }

}

#' Recycle subfields within nested extract fields
#'
#' Convenience function to implement list-recycling provided in
#' \code{recycle_to_named_list()} for an arbitrary selection of nested extract
#' fields. Provides warnings for common syntax issues for extract subfield
#' values.
#'
#' @param extract An extract inheriting from class \code{ipums_extract}
#' @param ... An arbitrary number of named arguments of the form
#'   \code{parent_field = c("subfield1", "subfield2", ...)}.
#'   \code{parent_field} should correspond to the name of a parent field in the
#'   extract, and it should be passed a character vector whose values correspond
#'   to the names of the subfields that should be recycled within that parent
#'   field.
#'
#' @return An object of the same class as \code{extract}, with subfields
#'   formatted as named lists based on list/vector syntax used in nested
#'   extract fields.
#'
#' @noRd
recycle_subfields <- function(extract, ...) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))

  fields <- names(dots)

  purrr::walk(
    fields,
    function(field) {

      subfields <- dots[[field]]

      n_field_vals <- length(extract[[field]])

      if (n_field_vals > 0) {

        purrr::walk(
          subfields,
          function(var) {

            input_vals <- extract[[var]]

            is_named_vector <- !is_list(input_vals) &&
              any(have_name(input_vals))

            # Named vector is ambiguous. Named values should be provided in list
            if (is_named_vector) {
              warning(
                "Ignoring names in the specification for `", var,
                "`. To apply ",
                "values to ", field, " by name",
                ", ensure values are stored in a list",
                ", not a vector.",
                call. = FALSE
              )
            }

            not_in_field <- names(input_vals)[!names(input_vals) %in%
                                                extract[[field]]]

            if (length(not_in_field) > 0 && !is_named_vector) {
              warning(
                "The specification for `", var, "` references ",
                field, " that do not exist in this extract (\"",
                paste0(unique(not_in_field), collapse = "\", \""),
                "\"). These values will be ignored.",
                call. = FALSE
              )
            }

            if (is_list(input_vals) &&
                length(input_vals) != n_field_vals &&
                !any(have_name(input_vals)) &&
                length(input_vals) > 0) {
              warning(
                "The number of values in `", var, "` (",
                length(input_vals), ") does not match",
                " the number of ", field, " (", n_field_vals,
                "). Values will be matched to this extract's ", field,
                " in index order. To recycle selections across ", field,
                ", ensure values are stored in a vector, not a list.",
                call. = FALSE
              )
            }

            extract[[var]] <<- recycle_to_named_list(
              input_vals,
              extract[[field]]
            )

          }
        )
      }
    }
  )

  extract

}

#' Recycle values to a named list
#'
#' For use in recycling values for nested extract fields. Supports a syntax
#' used in define_extract_nhgis() to allow users to provide either vectors (to
#' recycle subfield arguments to all parent fields), unnamed lists (to attach
#' subfield arguments to their parent fields in index order) or named
#' lists (to attach subfield arguments to their parent fields by name).
#'
#' @param l List or vector to recycle
#' @param names Labels to serve as names for the entries in the output list.
#'
#' @return A list of same length as \code{names} whose entries are named with
#' \code{names}.
#'
#' @noRd
recycle_to_named_list <- function(l, names) {

  if (is.null(names)) {
    return(l)
  }

  l <- recycle_to_list(l, length(names))

  if (any(have_name(l))) {

    # If list is named, consolidate any entries with duplicate names
    if (any(duplicated(names(l)))) {
      labs <- unique(names(l))

      l <- purrr::map(
        labs,
        ~unlist(l[.x == names(l)], use.names = FALSE)
      )

      names(l) <- labs
    }

  } else {

    # If list is unnamed, attach names in index order
    names(l) <- names[1:length(l)]

  }

  # Add NULL entries for any names that are not present in l
  null_list <- setNames(
    recycle_to_list(NULL, length(names)),
    names
  )

  l <- setNames(
    purrr::map(
      names,
      ~union(null_list[[.x]], l[[.x]])
    ),
    names
  )

  l

}

#' Recycle vector to list of given length
#'
#' Low-level helper to recycle vectors to lists while leaving input lists
#' unchanged.
#'
#' @param x Vector or list to recycle
#' @param n Length of output list
#'
#' @return If \code{x} is a list, returns \code{x}. If \code{x} is a vector,
#'   returns a list of length \code{n}, where each element is \code{x}.
#'
#' @noRd
recycle_to_list <- function(x, n, labels = NULL) {

  if (n < 1) {
    return(x)
  }

  # EMPTY_NAMED_LIST is special case:
  if (is_list(x) && is_empty(x) && is_named(x)) {
    l <- rep(list(x), n)
  } else if (!is_list(x)) { # But otherwise we don't want to recycle lists
    l <- rep(list(x), n)
  } else {
    l <- x
  }

  # Can be removed? Not sure if used.
  if (length(labels) == length(l)) {
    l <- setNames(l, labels)
  }

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

  if (length(values_lower) == 0){
    return(NULL)
  }

  recoded <- toupper(dplyr::recode(values_lower, !!!lookup_key))

  # if (!is.null(print_style)) {
  #   print_style <- rlang::as_function(print_style)
  #   recoded <- print_style(recoded)
  # }

  recoded

}

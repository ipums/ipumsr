# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' `ipums_extract` class
#'
#' @description
#' The `ipums_extract` class provides a data structure for storing the
#' definition and status of a submitted or unsubmitted IPUMS data extract
#' request for the purpose of interacting with the IPUMS API.
#'
#' `ipums_extract` is a superclass encompassing all of the collection-specific
#' extract classes. Currently supported collections are:
#'
#' * IPUMS USA
#' * IPUMS CPS
#' * IPUMS NHGIS
#'
#' All objects with class `ipums_extract` will also have a collection-specific
#' subclass (e.g. `usa_extract`, `cps_extract`) to accommodate
#' collection-specific differences in extract options and content. All
#' of these classes share the similarities described below.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @section Properties:
#' Objects of class `ipums_extract` have:
#' * A `class` attribute of the form
#'   `c("{collection}_extract", "ipums_extract")`. For instance,
#'   `c("cps_extract", "ipums_extract")`
#' * A base type of `"list"`
#' * A `names` attribute that is a character vector the same length as the
#'   underlying list
#'
#' @section Creating an extract:
#' * Create an `ipums_extract` object from scratch with the appropriate
#'   [define_extract] function. These functions take the form
#'   `define_extract_{collection}`
#' * Use [get_extract_info()] to get the latest status of a submitted extract
#'   request
#' * Use [get_extract_history()] to obtain the extract definitions of
#'   previously-submitted extracts
#'
#' @section Submitting an extract:
#' * Use [submit_extract()] to submit an extract request for processing through
#'   the IPUMS API
#' * [wait_for_extract()] will periodically check the status of a submitted
#'   extract request until it is ready to download
#' * Use [is_extract_ready()] to manually check whether a submitted extract
#'   request is ready to download
#'
#' @section Downloading an extract:
#' * Download the data contained in a completed extract with
#'   [download_extract()]
#'
#' @section Revising an extract definition:
#' * Modify values in an existing extract definition with [add_to_extract()] and
#'   [remove_from_extract()]
#' * Combine definitions from multiple extracts into a single extract with
#'   [combine_extracts()]
#'
#' @section Saving an extract:
#' * Save an extract to a JSON-formatted file with [save_extract_as_json()]
#' * Create an `ipums_extract` object from a saved JSON-formatted definition
#'   with [define_extract_from_json()]
#'
#' @name ipums_extract-class
#' @aliases ipums_extract
NULL

#' Define an IPUMS extract object
#'
#' @description
#' Specify the parameters for a new IPUMS extract request object to be
#' submitted via the IPUMS API. An extract request contains the specifications
#' required to obtain a particular set of data from an IPUMS collection.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @section Supported collections:
#' Currently, ipumsr supports extract definitions for the following
#' collections:
#'
#' - **IPUMS USA**: [define_extract_usa()]
#' - **IPUMS CPS**: [define_extract_cps()]
#' - **IPUMS NHGIS**: [define_extract_nhgis()]
#'
#' @section Value:
#' These functions produce an [`ipums_extract`][ipums_extract-class] object
#' with a subclass based on the collection corresponding to the extract request.
#' For instance, an IPUMS NHGIS extract created with
#' [define_extract_nhgis()] will return an object of classes
#' `nhgis_extract` and `ipums_extract`. These objects are compatible
#' with the rest of the API functionality provided by ipumsr.
#'
#' @keywords internal
#'
#' @name define_extract
NULL

#' Define an IPUMS USA extract request
#'
#' @description
#' Define an IPUMS USA extract request to be submitted via the IPUMS
#' API. An extract request contains the specifications required
#' to identify and obtain a particular set of data from the IPUMS USA system.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using IPUMS USA
#'   [sample ID values](https://usa.ipums.org/usa-action/samples/sample_ids).
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file. One of
#'   `"fixed_width"`, `"csv"`, `"stata"`, `"spss"`, or `"sas9"`.
#' @param data_structure The desired structure of the extract data.
#'
#'   `"rectangular"` provides person records with all requested household
#'   information attached to respective household members.
#'
#'   `"hierarchical"` provides household records followed by person records.
#' @param rectangular_on Records on which to rectangularize. `"P"`
#'   rectangularizes on person records. `"H"` provides household-only records.
#'
#'   Currently, only `"P"` is supported.
#'
#' @return An object of class [`usa_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @seealso
#' [submit_extract()], [download_extract()], and [get_extract_info()] to
#'   process and manage an extract request.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' [`add_to_extract()`][add_to_extract.usa_extract()],
#' [`remove_from_extract()`][remove_from_extract.usa_extract()], and
#' [combine_extracts()] to
#'   revise an extract definition.
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
#' my_extract
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(my_extract)
#' }
define_extract_usa <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL) {
  if (data_structure == "rectangular") {
    rectangular_on <- rectangular_on %||% "P"

    if (rectangular_on != "P") {
      rlang::abort(
        paste0(
          "Currently, the `rectangular_on` argument must be equal to \"P\"; ",
          "in the future, the API will also support `rectangular_on = \"H\"."
        ),
      )
    }
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
#' @description
#' Define an IPUMS CPS extract request to be submitted via the IPUMS
#' API. An extract request contains the specifications required
#' to identify and obtain a particular set of data from the IPUMS CPS system.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @inheritParams define_extract_usa
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using IPUMS CPS
#'   [sample ID values](https://cps.ipums.org/cps-action/samples/sample_ids).
#'
#' @return An object of class [`cps_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @seealso
#' [submit_extract()], [download_extract()], and [get_extract_info()] to
#'   process and manage an extract request.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' [`add_to_extract()`][add_to_extract.cps_extract()],
#' [`remove_from_extract()`][remove_from_extract.cps_extract()], and
#' [combine_extracts()] to
#'   revise an extract definition.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_cps(
#'   description = "Example CPS extract definition",
#'   samples = c("cps2020_02s", "cps2020_03s"),
#'   variables = c("AGE", "SEX", "YEAR")
#' )
#'
#' my_extract
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(my_extract)
#' }
define_extract_cps <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL) {
  if (data_structure == "rectangular") {
    rectangular_on <- rectangular_on %||% "P"

    if (rectangular_on != "P") {
      rlang::abort(
        paste0(
          "Currently, the `rectangular_on` argument must be equal to \"P\"; ",
          "in the future, the API will also support `rectangular_on = \"H\"."
        ),
      )
    }
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

#' Define an IPUMS NHGIS extract request
#'
#' @description
#' Define an IPUMS NHGIS request to be submitted via the IPUMS
#' API. An extract request contains the specifications required
#' to obtain a particular set of data from the IPUMS NHGIS system.
#'
#' Use [get_nhgis_metadata()] to browse and identify data sources for use
#' in NHGIS extract definitions. For general information, see the NHGIS
#' [data source overview](https://www.nhgis.org/data-availability) and the
#' [FAQ](https://www.nhgis.org/frequently-asked-questions-faq).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' An NHGIS extract definition must include at least one dataset, time series
#' table, or shapefile specification.
#'
#' - Create an NHGIS dataset specification with `new_dataset()`. Each dataset
#' must be associated with a selection of `data_tables` and `geog_levels`. Some
#' datasets also support the selection of `years` and `breakdown_values`.
#' - Create an NHGIS time series table specification with `new_tst()`. Each time
#' series table must be associated with a selection of `geog_levels` and
#' optionally with a selection of `years`.
#'
#' See examples for more details about specifying datasets and time series
#' tables in an NHGIS extract definition.
#'
#' @param description Description of the extract.
#' @param datasets List of `ipums_dataset` objects created by `new_dataset()`
#'   containing the specifications
#'   for the [datasets](https://www.nhgis.org/overview-nhgis-datasets)
#'   to include in the extract request. See examples.
#' @param time_series_tables List of `nhgis_tst` objects created by `new_tst()`
#'   containing the specifications for the
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to include in the extract request. See examples.
#' @param shapefiles [Shapefiles](https://www.nhgis.org/gis-files) to include
#'   in the extract request.
#' @param geographic_extents Vector of geographic extents to use for
#'   all of the `datasets` in the extract definition (for instance, to obtain
#'   data within a particular state). Use `"*"` to select all available extents.
#'
#'   Required when any of the `datasets` included in the extract definition
#'   include `geog_levels` that require extent selection. See
#'   [get_nhgis_metadata()] to determine if a geographic level requires extent
#'   selection. At the time of writing, NHGIS supports extent selection only
#'   for blocks and block groups.
#' @param breakdown_and_data_type_layout The desired layout
#'   of any `datasets` that have multiple data types or breakdown values.
#'
#'   - `"single_file"` (default) keeps all data types and breakdown values in
#'   one file
#'   - `"separate_files"` splits each data type or breakdown value into its
#'   own file
#'
#'   Required if any `datasets` included in the extract definition consist of
#'   multiple data types (for instance, estimates and margins of error) or have
#'   multiple breakdown values specified. See [get_nhgis_metadata()] to
#'   determine whether a requested dataset has multiple data types.
#' @param tst_layout The desired layout of all `time_series_tables` included in
#'   the extract definition. One of:
#'
#'   - `"time_by_column_layout"` (wide format, default): rows correspond to
#'     geographic units, columns correspond to different times in the time
#'     series
#'   - `"time_by_row_layout"` (long format): rows correspond to a single
#'     geographic unit at a single point in time
#'   - `"time_by_file_layout"`: data for different times are provided in
#'     separate files
#'
#'   Required when an extract definition includes any `time_series_tables`.
#' @param data_format The desired format of the extract data file. One of:
#'
#'   - `"csv_no_header"` (default) includes only a minimal header in the first
#'     row
#'   - `"csv_header"` includes a second, more descriptive header row.
#'     (By default this header is removed when reading data with [read_nhgis()],
#'     but it can be retained.)
#'   - `"fixed_width"` provides data in a fixed width format
#'
#'   Required when an extract definition includes any `datasets` or
#'   `time_series_tables`.
#'
#' @return An object of class [`nhgis_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @seealso
#' [get_nhgis_metadata()] to find data to include in an extract definition.
#'
#' [submit_extract()], [download_extract()], and [get_extract_info()] to
#'   process and manage an extract request.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' [`add_to_extract()`][add_to_extract.nhgis_extract()],
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract()] and
#' [combine_extracts()] to
#'   revise an extract definition.
#'
#' @export
#'
#' @examples
#' # Extract definition for tables from an NHGIS dataset
#' # Use `new_dataset()` to create an NHGIS dataset specification
#' my_extract_def <- define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   datasets = new_dataset(
#'     "1990_STF3",
#'     data_tables = "NP57",
#'     geog_levels = c("county", "tract")
#'   )
#' )
#'
#' my_extract_def
#'
#' # Use `new_tst()` to create an NHGIS time series table specification
#' define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   time_series_tables = new_tst("CL8", geog_levels = "county"),
#'   tst_layout = "time_by_row_layout"
#' )
#'
#' # To request multiple datasets, provide a list of `ipums_dataset` objects
#' # to the `datasets` argument
#' define_extract_nhgis(
#'   description = "Extract definition with multiple datasets",
#'   datasets = list(
#'     new_dataset("2014_2018_ACS5a", "B01001", c("state", "county")),
#'     new_dataset("2015_2019_ACS5a", "B01001", c("state", "county"))
#'   )
#' )
#'
#' # You can store dataset and time series table specs outside of a definition
#' ds <- new_dataset("1990_STF1", c("NP1", "NP2"), "county")
#' tst <- new_tst("CL6", "state")
#'
#' define_extract_nhgis(
#'   description = "Extract with datasets and time series tables",
#'   datasets = ds,
#'   time_series_tables = tst,
#'   shapefiles = "us_county_1990_tl2008"
#' )
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(my_extract)
#' }
define_extract_nhgis <- function(description = "",
                                 datasets = NULL,
                                 time_series_tables = NULL,
                                 shapefiles = NULL,
                                 geographic_extents = NULL,
                                 breakdown_and_data_type_layout = NULL,
                                 tst_layout = NULL,
                                 data_format = NULL) {
  if (!is_null(datasets)) {
    data_format <- data_format %||% "csv_no_header"
    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      "single_file"
  }

  if (!is_null(time_series_tables)) {
    data_format <- data_format %||% "csv_no_header"
    tst_layout <- tst_layout %||% "time_by_column_layout"
  }

  if (inherits(datasets, "ipums_dataset")) {
    datasets <- list(datasets)
  }

  if (inherits(time_series_tables, "ipums_tst")) {
    time_series_tables <- list(time_series_tables)
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = datasets,
    time_series_tables = time_series_tables,
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

#' @param name Name of the dataset or time series table to include in this
#'   specification.
#' @param data_tables Vector of summary tables to retrieve for this dataset.
#' @param geog_levels Geographic levels (e.g. `"county"` or `"state"`)
#'   at which to obtain data for this dataset or time series table.
#' @param years Years for which to obtain the data for this dataset or time
#'   series table. Use `"*"` to select all available years.
#'
#'   Use [get_nhgis_metadata()] to determine if a dataset allows year selection.
#' @param breakdown_values [Breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to apply to this dataset.
#'
#' @export
#'
#' @rdname define_extract_nhgis
new_dataset <- function(name,
                        data_tables,
                        geog_levels,
                        years = NULL,
                        breakdown_values = NULL) {
  dataset <- new_nested_field(
    name,
    data_tables = data_tables,
    geog_levels = geog_levels,
    years = years,
    breakdown_values = breakdown_values,
    class = "ipums_dataset"
  )

  dataset <- validate_ipums_extract(dataset)

  dataset
}

#' @export
#' @rdname define_extract_nhgis
new_tst <- function(name,
                    geog_levels,
                    years = NULL) {
  tst <- new_nested_field(
    name,
    geog_levels = geog_levels,
    years = years,
    class = "ipums_tst"
  )

  tst <- validate_ipums_extract(tst)

  tst
}

#' Store an extract definition in JSON format
#'
#' @description
#' Read and write an [`ipums_extract`][ipums_extract-class] object to a JSON
#' file that contains the extract definition specifications.
#'
#' Use these functions to store a copy of an extract definition outside of your
#' R environment and/or share an extract definition with another registered
#' IPUMS user.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @inheritParams add_to_extract
#' @param extract_json Path to a file containing a JSON-formatted
#'   extract definition.
#'
#' @return An [`ipums_extract`][ipums_extract-class] object.
#'
#' @seealso
#' [define_extract_usa()], [define_extract_cps()], or [define_extract_nhgis()]
#'   to define an extract request to save.
#'
#' [add_to_extract()], [remove_from_extract()] and [combine_extracts()] to
#'   revise an extract definition.
#'
#' [submit_extract()], [download_extract()], and [get_extract_info()] to
#'   process and manage an extract request.
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
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path)
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' file.remove(extract_json_path)
define_extract_from_json <- function(extract_json) {
  collection <- jsonlite::fromJSON(extract_json)$collection

  if (is.null(collection)) {
    rlang::abort(c(
      paste0(
        "Could not determine the collection associated with this extract ",
        "definition."
      ),
      paste0(
        "Ensure that the JSON file includes an element containing ",
        "the IPUMS collection associated with the extract. ",
        "(For example, `\"collection\": \"usa\"`)"
      )
    ))
  }

  extract_json <- new_ipums_json(extract_json, collection)
  json_api_version <- api_version_from_json(extract_json)

  if (is.null(json_api_version)) {
    rlang::abort(c(
      "Could not determine the IPUMS API version for `extract_json`.",
      "*" = paste0(
        "As of ipumsr 0.6.0, only IPUMS API version ", ipums_api_version(),
        " is supported."
      ),
      "i" = paste0(
        "Use `define_extract_", collection, "()` ",
        "to recreate this extract definition"
      )
    ))
  } else if (ipums_api_version() != json_api_version) {
    rlang::abort(c(
      paste0(
        "`extract_json` was created with IPUMS API version \"",
        json_api_version, "\"."
      ),
      "*" = paste0(
        "As of ipumsr 0.6.0, only IPUMS API version ", ipums_api_version(),
        " is supported."
      ),
      "i" = paste0(
        "Use `define_extract_", collection, "()` ",
        "to recreate this extract definition"
      )
    ))
  }

  list_of_extracts <- extract_list_from_json(extract_json, validate = TRUE)

  if (length(list_of_extracts) != 1) {
    rlang::abort(
      "`extract_json` should only contain the definition for a single extract."
    )
  }

  list_of_extracts[[1]]
}

#' @param file File path to which to write the JSON-formatted extract
#'   definition.
#' @param overwrite If `TRUE`, overwrite `file` if it already exists.
#'   Defaults to `FALSE`.
#'
#' @rdname define_extract_from_json
#' @export
save_extract_as_json <- function(extract, file, overwrite = FALSE) {
  extract_as_json <- extract_to_request_json(extract)

  if (file.exists(file) && !overwrite) {
    rlang::abort(c(
      paste0("File \"", file, "\" already exists."),
      "i" = "To overwrite, set `overwrite = TRUE`."
    ))
  }

  writeLines(jsonlite::prettify(extract_as_json), con = file)
  invisible(file)
}

#' Add values to an existing IPUMS extract definition
#'
#' @description
#' Add or replace values in an existing `ipums_extract` object.
#' This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To add to an **IPUMS USA** extract definition, click
#'   [here][add_to_extract.usa_extract]
#' - To add to an **IPUMS CPS** extract definition, click
#'   [here][add_to_extract.cps_extract]
#' - To add to an **IPUMS NHGIS** extract definition, click
#'   [here][add_to_extract.nhgis_extract]
#'
#' In general, for a given collection, the arguments to `add_to_extract()`
#' are identical to those used when defining an extract for that collection. For
#' more information about defining an extract, click [here][define_extract].
#'
#' To remove existing values from an extract definition,use
#' [remove_from_extract()]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   add to the extract definition.
#'
#'   All arguments available in a collection's [define_extract] function can be
#'   passed to `add_to_extract()`.
#'
#' @return An object of the same class as `extract` containing the modified
#'   extract definition
#'
#' @seealso
#' [remove_from_extract()] to remove values from an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_usa()], [define_extract_cps()], or [define_extract_nhgis()]
#'   to create a new extract definition.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "2013 ACS Data",
#'   samples = "us2013a",
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' add_to_extract(usa_extract, samples = "us2014a")
#' add_to_extract(usa_extract, variables = c("MARST", "INCTOT"))
#'
#' nhgis_extract <- define_extract_nhgis(
#'   datasets = new_dataset(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2"),
#'     geog_levels = "county"
#'   )
#' )
#'
#' # Add a new dataset/time series table
#' add_to_extract(
#'   nhgis_extract,
#'   datasets = new_dataset(
#'     "1980_STF1",
#'     data_tables = "NT1A",
#'     geog_levels = c("county", "state")
#'   )
#' )
#'
#' # Add to existing datasets/time series tables
#' add_to_extract(
#'   nhgis_extract,
#'   datasets = new_dataset("1990_STF1", c("NP1", "NP2"), "state")
#' )
#'
#' # Values that can only take a single value are replaced
#' add_to_extract(nhgis_extract, data_format = "fixed_width")$data_format
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
}

#' Add values to an existing IPUMS USA extract definition
#'
#' @description
#' Add new values or replace existing values in an IPUMS USA extract definition.
#' All fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS USA extract definition, use
#' [`remove_from_extract()`][remove_from_extract.usa_extract]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams define_extract_usa
#' @inheritParams add_to_extract
#' @param samples Character vector of samples to add to the extract.
#'   Values should correspond to IPUMS USA
#'   [sample ID values](https://usa.ipums.org/usa-action/samples/sample_ids)
#' @param variables Character vector of variables to add to the extract.
#' @param ... Ignored
#'
#' @return A modified `usa_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`remove_from_extract()`][remove_from_extract.usa_extract()] to remove values
#' from an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_usa()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' extract <- define_extract_usa(
#'   description = "2013 ACS Data",
#'   samples = "us2013a",
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' add_to_extract(extract, samples = "us2014a")
#'
#' extract2 <- add_to_extract(
#'   extract,
#'   samples = "us2014a",
#'   variables = c("MARST", "BIRTHYR")
#' )
#'
#' # Values that only take a single value are replaced
#' add_to_extract(extract, description = "New description")$description
#'
#' # You can also combine two separate extract requests together
#' combine_extracts(extract, extract2)
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

#' Add values to an existing IPUMS CPS extract definition
#'
#' @description
#' Add new values or replace existing values in an IPUMS CPS extract definition.
#' All fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS CPS extract definition, use
#' [`remove_from_extract()`][remove_from_extract.cps_extract]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams define_extract_cps
#' @inheritParams add_to_extract
#' @param samples Character vector of samples to add to the extract.
#'   Values should correspond to IPUMS CPS
#'   [sample ID values](https://cps.ipums.org/cps-action/samples/sample_ids)
#' @param variables Character vector of variables to add to the extract.
#' @param ... Ignored
#'
#' @return A modified `cps_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`remove_from_extract()`][remove_from_extract.cps_extract()] to remove values
#' from an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_cps()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' extract <- define_extract_cps(
#'   description = "Example CPS extract definition",
#'   samples = c("cps2020_02s", "cps2020_03s"),
#'   variables = c("AGE", "SEX", "YEAR")
#' )
#'
#' add_to_extract(extract, samples = "cps2021_03s")
#'
#' extract2 <- add_to_extract(
#'   extract,
#'   samples = "cps2021_03s",
#'   variables = c("MARST", "RELATE")
#' )
#'
#' # Values that only take a single value are replaced
#' add_to_extract(extract, description = "New description")$description
#'
#' # You can also combine two separate extract requests together
#' combine_extracts(extract, extract2)
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

#' Add values to an existing IPUMS NHGIS extract definition
#'
#' @description
#' Add new values to an IPUMS NHGIS extract definition.
#' All fields are optional, and if omitted, will be unchanged.
#' Supplying a value for fields that take a single value, such as
#' `description` and `data_format`, will replace the existing value with
#' the supplied value.
#'
#' In general, adding to an extract follows the same syntax conventions as used
#' in [define_extract_nhgis()]. Use `new_dataset()` and `new_tst()` to create
#' dataset and time series table specifications.
#'
#' To remove existing values from an IPUMS NHGIS extract definition, use
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If any of the specifications provided to `datasets` or `time_series_tables`
#' reference dataset or time series table names that already exist in the
#' extract definition, their specifications will be merged with those present
#' in the definition. An extract will never contain two records for the same
#' dataset or time series table name. See examples.
#'
#' For extract fields that take a single value, `add_to_extract()` will
#' replace the existing value with the new value provided for that field.
#' It is not necessary to first remove this value using
#' `remove_from_extract()`.
#'
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams add_to_extract
#' @param ... Ignored
#'
#' @return A modified `nhgis_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract()] to remove
#' values from an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [define_extract_nhgis()] to create a new extract definition.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' @export
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = new_dataset("1990_STF1", c("NP1", "NP2"), "county")
#' )
#'
#' # Add a new dataset or time series table to the extract
#' add_to_extract(
#'   extract,
#'   datasets = new_dataset("1990_STF2a", "NPA1", "county")
#' )
#'
#' add_to_extract(
#'   extract,
#'   time_series_tables = new_tst("A00", "state")
#' )
#'
#' # If a dataset/time series table name already exists in the definition
#' # its specification will be modified
#' add_to_extract(
#'   extract,
#'   datasets = new_dataset("1990_STF1", "NP4", "nation")
#' )
#'
#' # Values that can only take a single value are replaced
#' add_to_extract(extract, data_format = "fixed_width")$data_format
add_to_extract.nhgis_extract <- function(extract,
                                         description = NULL,
                                         datasets = NULL,
                                         time_series_tables = NULL,
                                         geographic_extents = NULL,
                                         shapefiles = NULL,
                                         breakdown_and_data_type_layout = NULL,
                                         tst_layout = NULL,
                                         data_format = NULL,
                                         ...) {
  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    rlang::warn(c(
      paste0(
        "The following fields were either not found in the provided extract ",
        "or cannot be modified: "
      ),
      paste0(paste0("`", names(dots), "`"), collapse = ", ")
    ))
  }

  if (inherits(datasets, "ipums_dataset")) {
    datasets <- list(datasets)
  }

  if (inherits(time_series_tables, "ipums_tst")) {
    time_series_tables <- list(time_series_tables)
  }

  if (!all(purrr::map_lgl(datasets, ~ inherits(.x, "ipums_dataset")))) {
    rlang::abort(paste0(
      "Expected `datasets` to be an `ipums_dataset` object ",
      "or a list of `ipums_dataset` objects."
    ))
  } else {
    purrr::walk(datasets, validate_ipums_extract)
  }

  if (!all(purrr::map_lgl(time_series_tables, ~ inherits(.x, "ipums_tst")))) {
    rlang::abort(paste0(
      "Expected `time_series_tables` to be an `ipums_tst` object ",
      "or a list of `ipums_tst` objects."
    ))
  } else {
    purrr::walk(time_series_tables, validate_ipums_extract)
  }

  new_ds <- purrr::map_chr(datasets, ~ .x$name)
  old_ds <- purrr::map_chr(extract$datasets, ~ .x$name)

  new_tst <- purrr::map_chr(time_series_tables, ~ .x$name)
  old_tst <- purrr::map_chr(extract$time_series_tables, ~ .x$name)

  if (anyDuplicated(new_ds) != 0) {
    rlang::abort("Cannot add two `ipums_dataset` objects of same name.")
  }

  if (anyDuplicated(new_tst) != 0) {
    rlang::abort("Cannot add two `ipums_tst` objects of same name.")
  }

  if (!is_null(extract$datasets)) {
    extract$datasets <- purrr::map(
      extract$datasets,
      function(x) {
        name_exists <- x$name %in% new_ds

        if (any(name_exists)) {
          i <- which(x$name == new_ds)
          new_dataset(
            name = x$name,
            data_tables = union(x$data_tables, datasets[[i]]$data_tables),
            geog_levels = union(x$geog_levels, datasets[[i]]$geog_levels),
            years = union(x$years, datasets[[i]]$years),
            breakdown_values = union(x$breakdown_values, datasets[[i]]$breakdown_values)
          )
        } else {
          x
        }
      }
    )
  }

  if (!is_null(extract$time_series_tables)) {
    extract$time_series_tables <- purrr::map(
      extract$time_series_tables,
      function(x) {
        name_exists <- x$name %in% new_tst

        if (any(name_exists)) {
          i <- which(x$name == new_tst)
          new_tst(
            name = x$name,
            geog_levels = union(x$geog_levels, time_series_tables[[i]]$geog_levels),
            years = union(x$years, time_series_tables[[i]]$years)
          )
        } else {
          x
        }
      }
    )
  }

  new_datasets <- datasets[which(!new_ds %in% old_ds)]
  new_tsts <- time_series_tables[which(!new_tst %in% old_tst)]

  datasets <- unique(c(extract$datasets, new_datasets))
  time_series_tables <- unique(c(extract$time_series_tables, new_tsts))

  # Set defaults for extracts that may not already have them included
  if (!is.null(datasets)) {
    data_format <- data_format %||%
      extract$data_format %||%
      "csv_no_header"

    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      extract$breakdown_and_data_type_layout %||%
      "single_file"
  }

  if (!is.null(time_series_tables)) {
    data_format <- data_format %||%
      extract$data_format %||%
      "csv_no_header"

    tst_layout <- tst_layout %||%
      extract$tst_layout %||%
      "time_by_column_layout"
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description %||% extract$description,
    datasets = datasets,
    time_series_tables = time_series_tables,
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

#' Remove values from an existing IPUMS extract definition
#'
#' @description
#' Remove values for specific fields in an existing `ipums_extract`
#' object. This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To remove from an **IPUMS USA** extract definition, click
#'   [here][remove_from_extract.usa_extract]
#' - To remove from an **IPUMS CPS** extract definition, click
#'   [here][remove_from_extract.cps_extract]
#' - To remove from an **IPUMS NHGIS** extract definition, click
#'   [here][remove_from_extract.nhgis_extract]
#'
#' In general, for a given collection, the arguments to
#' `remove_from_extract()` are identical to those used when defining an
#' extract for that collection. For more about defining an extract, click
#' [here][define_extract].
#'
#' To add new values to an extract, see [add_to_extract()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   remove from the extract definition.
#'
#'   All arguments available in a collection's [define_extract] function can be
#'   passed to `add_to_extract()`.
#'
#' @return An object of the same class as `extract` containing the modified
#'   extract definition
#'
#' @seealso
#' [add_to_extract()] or [combine_extracts()] to add values to an extract
#'   definition.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_usa()], [define_extract_cps()], or [define_extract_nhgis()]
#'   to create a new extract definition.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("AGE", "SEX", "YEAR")
#' )
#'
#' remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a",
#'   variables = c("AGE", "SEX")
#' )
#'
#' nhgis_extract <- define_extract_nhgis(
#'   datasets = new_dataset("1990_STF1", c("NP1", "NP2", "NP3"), "county"),
#'   time_series_tables = new_tst("A00", geog_levels = "county")
#' )
#'
#' # Remove an existing dataset
#' remove_from_extract(nhgis_extract, datasets = "1990_STF1")
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' Remove values from an existing IPUMS USA extract definition
#'
#' @description
#' Remove existing values from an IPUMS USA extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS USA extract definition, see
#' [`add_to_extract()`][add_to_extract.usa_extract]. When replacing values,
#' it is best to first add new values using
#' `add_to_extract()` before removing values with
#' `remove_from_extract()`. This limits the possibility of producing a
#' temporarily invalid extract specification.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams define_extract_usa
#' @inheritParams remove_from_extract
#' @param samples Character vector of samples to remove from the extract.
#' @param variables Character vector of variables to remove from the extract.
#' @param ... Ignored
#'
#' @return A modified `usa_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`add_to_extract()`][add_to_extract.usa_extract()] to add values
#' to an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_usa()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("AGE", "SEX", "YEAR")
#' )
#'
#' remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a",
#'   variables = c("AGE", "SEX")
#' )
#'
#' # To replace values, use add_to_extract() first to avoid invalid
#' # extract definitions:
#' revised_extract <- add_to_extract(usa_extract, samples = "us2015a")
#' remove_from_extract(revised_extract, samples = c("us2013a", "us2014a"))
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

#' Remove values from an existing IPUMS CPS extract definition
#'
#' @description
#' Remove existing values from an IPUMS CPS extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS CPS extract definition, see
#' [`add_to_extract()`][add_to_extract.cps_extract]. When replacing values,
#' it is best to first add new values using
#' `add_to_extract()` before removing values with
#' `remove_from_extract()`. This limits the possibility of producing a
#' temporarily invalid extract specification.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams remove_from_extract.usa_extract
#'
#' @return A modified `cps_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`add_to_extract()`][add_to_extract.cps_extract()] to add values
#' to an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_cps()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' cps_extract <- define_extract_cps(
#'   description = "CPS example",
#'   samples = c("cps2019_03s", "cps2020_03s"),
#'   variables = c("AGE", "SEX", "YEAR")
#' )
#'
#' remove_from_extract(
#'   cps_extract,
#'   samples = "cps2020_03s",
#'   variables = c("AGE", "SEX")
#' )
#'
#' # To replace values, use add_to_extract() first to avoid invalid
#' # extract definitions:
#' revised_extract <- add_to_extract(cps_extract, variables = "MARST")
#' remove_from_extract(revised_extract, variables = c("AGE", "SEX", "YEAR"))
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

#' Remove values from an existing NHGIS extract definition
#'
#' @description
#' Remove existing values from an IPUMS NHGIS extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' In general, removing from an extract follows the same syntax conventions as
#' used in [`define_extract_nhgis()`][define_extract_nhgis].
#'
#' To add new values to an IPUMS NHGIS extract definition, use
#' [`add_to_extract()`][add_to_extract.nhgis_extract].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' Any extract fields that are rendered irrelevant after modifying the extract
#' will be automatically removed. (For instance, if all `time_series_tables`
#' are removed from an extract, `tst_layout` will also be
#' removed.) Thus, it is not necessary to explicitly remove these values.
#'
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams remove_from_extract
#' @param datasets Names of the datasets
#'   to remove from the extract definition. All `data_tables`, `geog_levels`,
#'   `years`, and `breakdown_values` associated with the specified
#'   `datasets` will also be removed.
#' @param time_series_tables Names of the time series tables
#'   to remove from the extract definition. All `geog_levels` and `years`
#'   associated  with the specified `time_series_tables` will also be removed.
#' @param geographic_extents Geographic extents to remove from the extract
#'   definition.
#' @param shapefiles [Shapefiles](https://www.nhgis.org/gis-files) to remove
#'   from the extract definition.
#' @param ... Ignored
#'
#' @return A modified `nhgis_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`add_to_extract()`][add_to_extract.nhgis_extract()] to add values
#' to an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_nhgis()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = new_dataset(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2", "NP3"),
#'     geog_levels = "county"
#'   ),
#'   time_series_tables = list(
#'     new_tst("CW3", c("state", "county")),
#'     new_tst("CW5", c("state", "county"))
#'   )
#' )
#'
#' remove_from_extract(
#'   extract,
#'   time_series_tables = c("CW3", "CW5")
#' )
#'
#' remove_from_extract(
#'   extract,
#'   datasets = "1990_STF1"
#' )
remove_from_extract.nhgis_extract <- function(extract,
                                              datasets = NULL,
                                              time_series_tables = NULL,
                                              geographic_extents = NULL,
                                              shapefiles = NULL,
                                              ...) {
  dots <- rlang::list2(...)

  if (length(dots) > 0) {
    rlang::warn(c(
      paste0(
        "The following fields were either not found in the provided extract ",
        "or cannot be removed: "
      ),
      "*" = paste0(paste0("`", names(dots), "`"), collapse = ", "),
      "i" = paste0(
        "Use `add_to_extract()` to replace existing values in valid extract ",
        "fields."
      )
    ))
  }

  ds_to_remove <- unlist(
    purrr::compact(purrr::map(
      datasets,
      function(d) {
        which(purrr::map_lgl(extract$datasets, ~ .x$name == d))
      }
    ))
  )

  ts_to_remove <- unlist(
    purrr::compact(purrr::map(
      time_series_tables,
      function(d) {
        which(purrr::map_lgl(extract$time_series_tables, ~ .x$name == d))
      }
    ))
  )

  no_ds <- length(ds_to_remove) == length(extract$datasets)
  no_ts <- length(ts_to_remove) == length(extract$time_series_tables)

  # If removal results in extract with no ds/tst, remove irrelevant values
  if (no_ds) {
    datasets <- NULL
    breakdown_and_data_type_layout <- NULL
    geographic_extents <- NULL
  } else {
    datasets <- extract$datasets[setdiff(seq_along(extract$datasets), ds_to_remove)]
    breakdown_and_data_type_layout <- extract$breakdown_and_data_type_layout
    geographic_extents <- setdiff_null(
      extract$geographic_extents,
      geog_extent_lookup(
        unlist(geographic_extents),
        state_geog_lookup$abbs
      )
    )
  }

  if (no_ts) {
    time_series_tables <- NULL
    tst_layout <- NULL
  } else {
    time_series_tables <- extract$time_series_tables[setdiff(seq_along(extract$time_series_tables), ts_to_remove)]
    tst_layout <- extract$tst_layout
  }

  if (no_ds && no_ts) {
    data_format <- NULL
  } else {
    data_format <- extract$data_format
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = extract$description,
    datasets = datasets,
    time_series_tables = time_series_tables,
    geographic_extents = geographic_extents,
    shapefiles = setdiff_null(extract$shapefiles, unlist(shapefiles)),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    tst_layout = tst_layout,
    data_format = data_format
  )

  extract <- validate_ipums_extract(extract)

  extract
}

#' Combine multiple IPUMS extract definitions into one
#'
#' @description
#' Create a single extract definition that includes all of the
#' specifications included in a set of `ipums_extract` objects of the same
#' collection.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' Values that exist in more than one of the provided extract definitions will
#' be de-duplicated such that only one entry for the duplicated specification is
#' included in the final extract.
#'
#' Some extract fields can only take a single value. In the event that the
#' definitions being combined each have different values for such a field, the
#' value found in the first extract definition provided to `...` is retained in
#' the final extract.
#'
#' To modify values in a single extract definition, see
#' [`add_to_extract()`][add_to_extract] or
#' [`remove_from_extract()`][remove_from_extract].
#'
#' @param ... Arbitrary number of `ipums_extract` objects to be combined. All
#'   extracts must belong to the same collection.
#'
#' @return An `ipums_extract` object of the same collection as the extracts
#'   provided in `...` containing the combined extract definition.
#'
#' @seealso
#' [add_to_extract()] and [remove_from_extract()] to revise an extract
#'   definition.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_usa()], [define_extract_cps()], or [define_extract_nhgis()]
#'   to create a new extract definition.
#'
#' @export
#'
#' @examples
#' ext1 <- define_extract_nhgis(
#'   datasets = new_dataset(
#'     "2011_2015_ACS5a",
#'     data_tables = "B00001",
#'     geog_levels = "county"
#'   )
#' )
#'
#' ext2 <- define_extract_nhgis(
#'   datasets = list(
#'     new_dataset("2011_2015_ACS5a", c("B00002", "B01001"), "county"),
#'     new_dataset("2012_2016_ACS5a", c("B00002", "B01001"), "county")
#'   )
#' )
#'
#' combine_extracts(ext1, ext2)
combine_extracts <- function(...) {
  UseMethod("combine_extracts")
}

#' @export
combine_extracts.usa_extract <- function(...) {
  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~ .x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    ~ add_to_extract(
      .x,
      description = .x$description,
      samples = .y$samples,
      variables = .y$variables,
      data_format = .x$data_format %||% .y$data_format,
      data_structure = .x$data_structure %||% .y$data_structure,
      rectangular_on = .x$rectangular_on %||% .y$rectangular_on
    )
  )

  extract
}

#' @export
combine_extracts.cps_extract <- function(...) {
  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~ .x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    ~ add_to_extract(
      .x,
      description = .x$description,
      samples = .y$samples,
      variables = .y$variables,
      data_format = .x$data_format %||% .y$data_format,
      data_structure = .x$data_structure %||% .y$data_structure,
      rectangular_on = .x$rectangular_on %||% .y$rectangular_on
    )
  )

  extract
}

#' @export
combine_extracts.nhgis_extract <- function(...) {
  extracts <- rlang::list2(...)

  collection <- purrr::map_chr(extracts, ~ .x$collection)

  if (length(unique(collection)) != 1) {
    rlang::abort("Can only combine extracts of the same collection.")
  }

  extract <- purrr::reduce(
    extracts,
    ~ add_to_extract(
      .x,
      description = .x$description,
      datasets = .y$datasets,
      time_series_tables = .y$time_series_tables,
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

#' Create a new extract object
#'
#' Creates an object inheriting from class `ipums_extract` for use in
#' interacting with the IPUMS API.
#'
#' @param collection Character indicating the IPUMS collection for this extract.
#'   See [ipums_data_collections()].
#' @param description Description of the extract.
#' @param submitted Logical indicating whether this extract has been submitted
#'   to the IPUMS API. See [submit_extract()].
#' @param download_links List of paths to the data included in the API response
#'   after submitting an extract.
#' @param number Number of the extract returned by the API on
#'   submission.
#' @param status Character indicating the current status of the extract as
#'   returned by the API. If the extract has not been submitted, the
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

new_nested_field <- function(name, ..., class) {
  nested_field <- c(
    list(name = name),
    purrr::compact(rlang::list2(...))
  )

  structure(
    nested_field,
    class = c(class, "ipums_nested", class(nested_field))
  )
}

#' Validate the structure of an IPUMS extract object
#'
#' @description
#' Ensures that the structure of an extract object is consistent with what is
#' required by the IPUMS API.
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
  NextMethod()

  includes_ds <- !is_empty(x$datasets) && !is_na(x$datasets)
  includes_tst <- !is_empty(x$time_series_tables) && !is_na(x$time_series_tables)
  includes_shp <- !is_empty(x$shapefiles) && !is_na(x$shapefiles)

  if (!any(includes_ds, includes_tst, includes_shp)) {
    rlang::abort(
      paste0(
        "An `nhgis_extract` must contain at least one of `datasets`, ",
        "`time_series_tables`, or `shapefiles`."
      )
    )
  }

  if (!all(purrr::map_lgl(x$datasets, ~ inherits(.x, "ipums_dataset")))) {
    rlang::abort(paste0(
      "Expected `datasets` to be an `ipums_dataset` object ",
      "or a list of `ipums_dataset` objects."
    ))
  } else {
    purrr::walk(x$datasets, validate_ipums_extract)
  }

  if (!all(purrr::map_lgl(x$time_series_tables, ~ inherits(.x, "ipums_tst")))) {
    rlang::abort(paste0(
      "Expected `time_series_tables` to be an `ipums_tst` object ",
      "or a list of `ipums_tst` objects."
    ))
  } else {
    purrr::walk(x$time_series_tables, validate_ipums_extract)
  }

  # Specify the validation requirements for each extract field
  extract_field_spec <- list(
    list(
      field = "geographic_extents",
      allowed = includes_ds,
      must_be_missing_msg = " when no `datasets` are specified"
    ),
    list(
      field = "breakdown_and_data_type_layout",
      allowed = includes_ds,
      choices = c("single_file", "separate_files"),
      length = 1,
      must_be_missing_msg = " when no `datasets` are specified"
    ),
    list(
      field = "tst_layout",
      required = includes_tst,
      allowed = includes_tst,
      choices = c(
        "time_by_row_layout", "time_by_column_layout",
        "time_by_file_layout"
      ),
      length = 1,
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
      length = 1,
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
    ~ tryCatch(
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
      length = 1,
      type = "character"
    ),
    list(
      field = "rectangular_on",
      required = isTRUE(x$data_structure == "rectangular"),
      allowed = isTRUE(x$data_structure == "rectangular"),
      choices = c("P", "H"),
      must_be_present_msg = " when `data_structure == \"rectangular\"`",
      must_be_missing_msg = " when `data_structure != \"rectangular\"`",
      length = 1,
      type = "character"
    ),
    list(
      field = "data_format",
      required = TRUE,
      choices = c("fixed_width", "csv", "stata", "spss", "sas9"),
      length = 1,
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
    ~ tryCatch(
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
      length = 1,
      type = "character"
    ),
    list(
      field = "rectangular_on",
      required = isTRUE(x$data_structure == "rectangular"),
      allowed = isTRUE(x$data_structure == "rectangular"),
      choices = c("P", "H"),
      must_be_present_msg = " when `data_structure == \"rectangular\"`",
      must_be_missing_msg = " when `data_structure != \"rectangular\"`",
      length = 1,
      type = "character"
    ),
    list(
      field = "data_format",
      required = TRUE,
      choices = c("fixed_width", "csv", "stata", "spss", "sas9"),
      length = 1,
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
    ~ tryCatch(
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
  extract_field_spec <- list(
    list(
      field = "collection",
      required = TRUE,
      type = "character",
      length = 1
    ),
    list(
      field = "description",
      required = TRUE,
      length = 1,
      type = "character"
    ),
    list(
      field = "status",
      choices = c(
        "unsubmitted", "queued", "started", "produced",
        "canceled", "failed", "completed"
      ),
      length = 1
    ),
    list(
      field = "download_links",
      type = "list"
    ),
    list(
      field = "number",
      length = 1,
      type = c("character", "integer", "double")
    )
  )

  # Validate based on each argument's validation specifications
  # Collect errors and display together.
  extract_issues <- purrr::map(
    extract_field_spec,
    ~ tryCatch(
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

  # Throw error if no API for collection
  check_api_support(x$collection)

  x
}

#' @export
validate_ipums_extract.ipums_dataset <- function(x) {
  unexpected_names <- names(x)[!names(x) %in% c(
    "name",
    "data_tables",
    "geog_levels",
    "years",
    "breakdown_values"
  )]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `ipums_dataset` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      ))
    )
  }

  spec <- list(
    list(
      field = "name",
      required = TRUE,
      length = 1,
      type = "character"
    ),
    list(
      field = "data_tables",
      required = TRUE,
      type = "character"
    ),
    list(
      field = "geog_levels",
      required = TRUE,
      type = "character"
    ),
    list(
      field = "years",
      required = FALSE,
      type = "character"
    ),
    list(
      field = "breakdown_values",
      required = FALSE,
      type = "character"
    )
  )

  # Validate based on each argument's validation specifications
  # Collect errors and display together.
  extract_issues <- purrr::map(
    spec,
    ~ tryCatch(
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
        "Invalid `ipums_dataset` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)
}

#' @export
validate_ipums_extract.ipums_tst <- function(x) {
  unexpected_names <- names(x)[!names(x) %in% c("name", "geog_levels", "years")]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `ipums_tst` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      ))
    )
  }

  spec <- list(
    list(
      field = "name",
      required = TRUE,
      length = 1,
      type = "character"
    ),
    list(
      field = "geog_levels",
      required = TRUE,
      type = "character"
    ),
    list(
      field = "years",
      required = FALSE,
      type = "character"
    )
  )

  # Validate based on each argument's validation specifications
  # Collect errors and display together.
  extract_issues <- purrr::map(
    spec,
    ~ tryCatch(
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
        "Invalid `ipums_tst` object: ",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)
}

#' Validate the structure of a single field in an IPUMS extract
#'
#' @description
#' Check whether an extract field has the structure expected based on the
#' values for several input arguments.
#'
#' `validate_extract_field()` is a wrapper for several helper functions:
#'   * `validate_subfield_names()` checks whether a child field of
#'     another extract field (e.g. `data_tables` nests within `datasets` in
#'     NHGIS extracts) has names that match the parent field values.
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
                                   length = NULL,
                                   must_be_present_msg = NULL,
                                   must_be_missing_msg = NULL) {

  if (required) {
    allowed <- TRUE
  }

  # Check that the correct number of field values are provided
  validate_length(extract, field, length = length)

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
  validate_choices(extract, field, choices = choices)

  # Check that field is of the correct data type
  validate_type(extract, field, type = type)

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
      paste0(
        "`", field,
        "` must not contain missing values", must_be_present_msg, "."
      )
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
#' @param length Numeric or character value indicating the number
#'   of values expected in the given `field`. If a character value is provided,
#'   it should reference the name of another extract field. The length of the
#'   given `field` is then expected to be the same length as the field provided
#'   to `length`.
#'
#' @return `NULL`, invisibly
#'
#' @noRd
validate_length <- function(extract, field, length = NULL) {
  values <- extract[[field]]

  empty_field <- is_null(values) || length(values) == 0 ||
    is_null(length) || length == 0

  if (!empty_field) {
    if (!is.null(length) && length(values) != length) {
      rlang::abort(paste0("`", field, "` must be length ", length, "."))
    }
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

  if (!is_null(choices) && !is_null(values)) {
    if (!all(values %in% choices)) {
      rlang::abort(paste0(
        "`", field, "` must be one of \"",
        paste0(choices, collapse = "\", \""), "\""
      ))
    }
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

  if (!is_null(type) && !is_null(values)) {
    if (!typeof(values) %in% type) {
      rlang::abort(
        paste0(
          "`", field, "` must be of type `", paste0(type, collapse = "` or `"),
          "`, not `", typeof(values), "`."
        )
      )
    }
  }

  invisible(NULL)
}

#' Convert JSON containing extract specifications to an extract object
#'
#' @param extract_json JSON containing the extract specification as returned
#'   by the API
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
  extract_info <- jsonlite::fromJSON(extract_json, simplifyDataFrame = FALSE)

  # If the response if from a paginated endpoint (i.e. recent extracts)
  # it will include a "data" field containing the extract definitions.
  # Otherwise, we are dealing with a single extract, which needs to be
  # converted to list for consistency with paginated responses.
  list_of_extract_info <- extract_info$data %||% list(extract_info)

  purrr::map(
    list_of_extract_info,
    function(x) {
      # extractDefinition won't exist if it is from a saved JSON file
      def <- x$extractDefinition %||% x

      if (is_null(names(def$datasets))) {
        datasets <- NULL
      } else {
        datasets <- purrr::map(
          names(def$datasets),
          ~ new_dataset(
            .x,
            data_tables = def$datasets[[.x]]$dataTables,
            geog_levels = def$datasets[[.x]]$geogLevels,
            years = def$datasets[[.x]]$years,
            breakdown_values = def$datasets[[.x]]$breakdownValues
          )
        )
      }

      if (is_null(names(def$timeSeriesTables))) {
        time_series_tables <- NULL
      } else {
        time_series_tables <- purrr::map(
          names(def$timeSeriesTables),
          ~ new_tst(
            .x,
            geog_levels = def$timeSeriesTables[[.x]]$geogLevels,
            years = def$timeSeriesTables[[.x]]$years
          )
        )
      }

      out <- new_ipums_extract(
        collection = "nhgis",
        description = def$description,
        datasets = datasets,
        time_series_tables = time_series_tables,
        geographic_extents = geog_extent_lookup(
          unlist(def$geographicExtents),
          state_geog_lookup$abbs
        ),
        shapefiles = unlist(def$shapefiles),
        breakdown_and_data_type_layout = def$breakdownAndDataTypeLayout,
        tst_layout = def$timeSeriesTableLayout,
        data_format = def$dataFormat,
        submitted = "number" %in% names(x),
        download_links = x$downloadLinks %||% EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = x$status %||% "unsubmitted"
      )

      if (validate) {
        out <- validate_ipums_extract(out)
      }

      out
    }
  )
}

#' @export
extract_list_from_json.usa_json <- function(extract_json, validate = FALSE) {
  extract_info <- jsonlite::fromJSON(extract_json, simplifyVector = FALSE)

  # If the response if from a paginated endpoint (i.e. recent extracts)
  # it will include a "data" field containing the extract definitions.
  # Otherwise, we are dealing with a single extract, which needs to be
  # converted to list for consistency with paginated responses.
  list_of_extract_info <- extract_info$data %||% list(extract_info)

  purrr::map(
    list_of_extract_info,
    function(x) {
      # extractDefinition won't exist if it is from a saved JSON file
      def <- x$extractDefinition %||% x

      if ("number" %in% names(x)) {
        submitted <- TRUE
        number <- x$number
      } else {
        submitted <- FALSE
        number <- NA_integer_
      }

      out <- new_ipums_extract(
        collection = "usa",
        description = def$description,
        data_structure = names(def$dataStructure),
        rectangular_on = def$dataStructure$rectangular$on,
        data_format = def$dataFormat,
        samples = names(def$samples),
        variables = names(def$variables),
        submitted = submitted,
        download_links = x$downloadLinks %||% EMPTY_NAMED_LIST,
        number = number,
        status = x$status %||% "unsubmitted"
      )

      if (validate) {
        out <- validate_ipums_extract(out)
      }

      out
    }
  )
}


#' @export
extract_list_from_json.cps_json <- function(extract_json, validate = FALSE) {
  extract_info <- jsonlite::fromJSON(extract_json, simplifyVector = FALSE)

  # If the response if from a paginated endpoint (i.e. recent extracts)
  # it will include a "data" field containing the extract definitions.
  # Otherwise, we are dealing with a single extract, which needs to be
  # converted to list for consistency with paginated responses.
  list_of_extract_info <- extract_info$data %||% list(extract_info)

  purrr::map(
    list_of_extract_info,
    function(x) {
      # extractDefinition won't exist if it is from a saved JSON file
      def <- x$extractDefinition %||% x

      if ("number" %in% names(x)) {
        submitted <- TRUE
        number <- x$number
      } else {
        submitted <- FALSE
        number <- NA_integer_
      }

      out <- new_ipums_extract(
        collection = "cps",
        description = def$description,
        data_structure = names(def$dataStructure),
        rectangular_on = def$dataStructure$rectangular$on,
        data_format = def$dataFormat,
        samples = names(def$samples),
        variables = names(def$variables),
        submitted = submitted,
        download_links = x$downloadLinks %||% EMPTY_NAMED_LIST,
        number = number,
        status = x$status %||% "unsubmitted"
      )

      if (validate) {
        out <- validate_ipums_extract(out)
      }

      out
    }
  )
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
    rlang::warn(c(
      paste0(
        "The following fields were either not found in the provided extract ",
        "or cannot be modified: "
      ),
      paste0(paste0("`", names(dots), "`"), collapse = ", ")
    ))
  }

  data_structure <- data_structure %||% extract$data_structure

  if (data_structure == "rectangular") {
    rectangular_on <- rectangular_on %||% extract$rectangular_on %||% "P"

    if (rectangular_on != "P") {
      rlang::abort(
        paste0(
          "Currently, the `rectangular_on` argument must be equal to \"P\"; ",
          "in the future, the API will also support `rectangular_on = \"H\"."
        )
      )
    }
  }

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = description %||% extract$description,
    data_structure = data_structure %||% extract$data_structure,
    rectangular_on = rectangular_on,
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
    rlang::warn(c(
      paste0(
        "The following fields were either not found in the provided extract ",
        "or cannot be removed: "
      ),
      "*" = paste0(paste0("`", names(dots), "`"), collapse = ", "),
      "i" = paste0(
        "Use `add_to_extract()` to replace existing values in valid extract ",
        "fields."
      )
    ))
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

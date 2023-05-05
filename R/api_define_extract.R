# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' `ipums_extract` class
#'
#' @description
#' The `ipums_extract` class provides a data structure for storing the
#' extract definition and status of an IPUMS data extract request. Both
#' submitted and unsubmitted extract requests are stored in `ipums_extract`
#' objects.
#'
#' `ipums_extract` objects are futher divided into microdata
#' and aggregate data classes, and will also include
#' a collection-specific extract subclass to accommodate differences in
#' extract options and content across collections.
#'
#' Currently supported collections are:
#'
#' * IPUMS microdata (`"micro_extract"`)
#'     + IPUMS USA (`"usa_extract"`)
#'     + IPUMS CPS (`"cps_extract"`)
#'     + IPUMS International (`"ipumsi_extract"`)
#' * IPUMS aggregate data (`"agg_extract"`)
#'     + IPUMS NHGIS (`"nhgis_extract"`)
#'
#' Learn more about the IPUMS API in vignette("ipums-api").
#'
#' @section Properties:
#' Objects of class `ipums_extract` have:
#' * A `class` attribute of the form
#'   `c("{collection}_extract", "{data_type}_extract", "ipums_extract")`.
#'   For instance, `c("cps_extract", "micro_extract", "ipums_extract")`.
#' * A base type of `"list"`.
#' * A `names` attribute that is a character vector the same length as the
#'   underlying list.
#'
#' All `ipums_extract` objects will include several core fields identifying
#' the extract and its status:
#' * `collection`: the collection for the extract request.
#' * `description`: the description of the extract request.
#' * `submitted`: logical indicating whether the extract request has been
#'   submitted to the IPUMS API for processing.
#' * `download_links`: links to the downloadable data, if the extract request
#'   was completed at the time it was last checked.
#' * `number`: the number of the extract request. With `collection`, this
#'   uniquely identifies an extract request.
#' * `status`: status of the extract request at the time it was last checked.
#'   One of `"unsubmitted"`, `"queued"`, `"started"`, `"produced"`,
#'   `"canceled"`, `"failed"`, or `"completed"`.
#'
#' @section Creating an extract:
#' * Create an `ipums_extract` object from scratch with the appropriate
#'   [`define_extract_*()`][define_extract] function. These functions take the
#'   form `define_extract_{collection}`.
#' * Use [get_extract_info()] to get the latest status of a submitted extract
#'   request
#' * Use [get_extract_history()] to obtain the extract definitions of
#'   previously-submitted extracts
#'
#' @section Submitting an extract:
#' * Use [submit_extract()] to submit an extract request for processing through
#'   the IPUMS API.
#' * Use [wait_for_extract()] to periodically check the status of a submitted
#'   extract request until it is ready to download.
#' * Use [is_extract_ready()] to manually check whether a submitted extract
#'   request is ready to download.
#'
#' @section Downloading an extract:
#' * Download the data contained in a completed extract with
#'   [download_extract()].
#'
#' @section Revising an extract definition:
#' * Modify values in an existing extract definition with [add_to_extract()] and
#'   [remove_from_extract()].
#' * Combine definitions from multiple extracts into a single extract with
#'   [combine_extracts()].
#'
#' @section Saving an extract:
#' * Save an extract to a JSON-formatted file with [save_extract_as_json()].
#' * Create an `ipums_extract` object from a saved JSON-formatted definition
#'   with [define_extract_from_json()].
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
#' - **IPUMS International**: [define_extract_ipumsi()]
#' - **IPUMS NHGIS**: [define_extract_nhgis()]
#'
#' @section Value:
#' These functions produce an [`ipums_extract`][ipums_extract-class] object
#' with a subclass based on the collection corresponding to the extract request.
#' The core ipumsr API client tools are designed to handle these objects.
#'
#' @keywords internal
#'
#' @name define_extract
NULL

#' Define an extract request for an IPUMS microdata collection
#'
#' @description
#' Define the parameters of an IPUMS microdata extract request to be submitted
#' via the IPUMS API.
#'
#' Currently supported microdata collections include:
#' - **IPUMS USA**: `define_extract_usa()`
#' - **IPUMS CPS**: `define_extract_cps()`
#' - **IPUMS International**: `define_extract_ipumsi()`
#'
#' Use the [var_spec()] helper to generate detailed variable-level
#' specifications for an extract request.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param description Description of the extract.
#' @param samples Character vector of sample names to include in the extract.
#'   Use [get_sample_info()] to identify sample IDs for a given collection.
#' @param variables Character vector of variable names or a list of detailed
#'   variable specifications for all variables in the extract request. Use
#'   [var_spec()] to create a `var_spec` object containing a detailed variable
#'   specification. See examples.
#' @param data_format Format for the output extract data file. One of
#'   `"fixed_width"`, `"csv"`, `"stata"`, `"spss"`, or `"sas9"`. Defaults to
#'   `"fixed_width"`.
#' @param data_structure Data structure for the output the extract data.
#'
#'   `"rectangular"` provides person records with all requested household
#'   information attached to respective household members.
#'
#'   `"hierarchical"` provides household records followed by person records.
#' @param rectangular_on If `data_structure` is `"rectangular"`,
#'   records on which to rectangularize. Currently only `"P"` (person records)
#'   is supported.
#'
#'   Defaults to `"P"` if `data_structure` is `"rectangular"` and `NULL`
#'   otherwise.
#'
#' @return An object of class [`micro_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @seealso
#' [submit_extract()], [download_extract()], and [get_extract_info()] to
#'   process and manage an extract request.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' [`add_to_extract()`][add_to_extract.micro_extract()],
#' [`remove_from_extract()`][remove_from_extract.micro_extract()], and
#' [combine_extracts()] to revise an extract definition.
#'
#' @export
#'
#' @name define_extract_micro
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' usa_extract
#'
#' # Use `var_spec()` to created detailed variable specifications:
#' usa_extract <- define_extract_usa(
#'   description = "Example USA extract definition",
#'   samples = c("us2013a", "us2014a"),
#'   variables = var_spec(
#'     "SEX",
#'     case_selections = "2",
#'     attached_characteristics = c("mother", "father")
#'   )
#' )
#'
#' # For multiple variables, provide a list of `var_spec` objects and/or
#' # variable names.
#' cps_extract <- define_extract_cps(
#'   description = "Example CPS extract definition",
#'   samples = c("cps2020_02s", "cps2020_03s"),
#'   variables = list(
#'     var_spec("AGE", data_quality_flags = TRUE),
#'     var_spec("SEX", case_selections = "2"),
#'     "RACE"
#'   )
#' )
#'
#' cps_extract
#'
#' # To recycle specifications to many variables, it may be useful to
#' # create variables prior to defining the extract:
#' var_names <- c("AGE", "SEX")
#'
#' var_spec <- purrr::map(
#'   var_names,
#'   ~ var_spec(.x, attached_characteristics = "mother")
#' )
#'
#' ipumsi_extract <- define_extract_ipumsi(
#'   description = "Extract definition with predefined variables",
#'   samples = c("br2010a", "cl2017a"),
#'   variables = var_spec
#' )
#'
#' # Extract specifications can be indexed by name
#' names(ipumsi_extract$samples)
#'
#' names(ipumsi_extract$variables)
#'
#' ipumsi_extract$variables$AGE
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(usa_extract)
#' }
NULL

#' @export
#' @rdname define_extract_micro
define_extract_usa <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL) {
  define_extract_micro(
    collection = "usa",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on
  )
}

#' @export
#' @rdname define_extract_micro
define_extract_cps <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL) {
  define_extract_micro(
    collection = "cps",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on
  )
}

#' @export
#' @rdname define_extract_micro
define_extract_ipumsi <- function(description,
                                  samples,
                                  variables,
                                  data_format = "fixed_width",
                                  data_structure = "rectangular",
                                  rectangular_on = NULL) {
  define_extract_micro(
    collection = "ipumsi",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on
  )
}

#' Define an IPUMS NHGIS extract request
#'
#' @description
#' Define the parameters of an IPUMS NHGIS extract request to be submitted via
#' the IPUMS API.
#'
#' Use [get_metadata_nhgis()] to browse and identify data sources for use
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
#' Create an NHGIS dataset specification with [ds_spec()]. Each dataset
#' must be associated with a selection of `data_tables` and `geog_levels`. Some
#' datasets also support the selection of `years` and `breakdown_values`.
#'
#' Create an NHGIS time series table specification with [tst_spec()]. Each time
#' series table must be associated with a selection of `geog_levels` and
#' may optionally be associated with a selection of `years`.
#'
#' See examples for more details about specifying datasets and time series
#' tables in an NHGIS extract definition.
#'
#' @param description Description of the extract.
#' @param datasets List of dataset specifications for any
#'   [datasets](https://www.nhgis.org/overview-nhgis-datasets)
#'   to include in the extract request. Use [ds_spec()] to create a
#'   `ds_spec` object containing a dataset specification. See examples.
#' @param time_series_tables List of time series table specifications for any
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to include in the extract request. Use [tst_spec()] to create a
#'   `tst_spec` object containing a time series table specification. See
#'   examples.
#' @param shapefiles Names of any [shapefiles](https://www.nhgis.org/gis-files)
#'   to include in the extract request.
#' @param geographic_extents Vector of geographic extents to use for
#'   all of the `datasets` in the extract definition (for instance, to obtain
#'   data within a particular state). Use `"*"` to select all available extents.
#'
#'   Required when any of the `datasets` included in the extract definition
#'   include `geog_levels` that require extent selection. See
#'   [get_metadata_nhgis()] to determine if a geographic level requires extent
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
#'   multiple breakdown values specified. See [get_metadata_nhgis()] to
#'   determine whether a requested dataset has multiple data types.
#' @param tst_layout The desired layout of all `time_series_tables` included in
#'   the extract definition.
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
#' @param data_format The desired format of the extract data file.
#'
#'   - `"csv_no_header"` (default) includes only a minimal header in the first
#'     row
#'   - `"csv_header"` includes a second, more descriptive header row.
#'   - `"fixed_width"` provides data in a fixed width format
#'
#'   Note that [read_nhgis()] removes the additional header row in
#'   `"csv_header"` files by default.
#'
#'   Required when an extract definition includes any `datasets` or
#'   `time_series_tables`.
#'
#' @return An object of class [`nhgis_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @seealso
#' [get_metadata_nhgis()] to find data to include in an extract definition.
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
#' # Use `ds_spec()` to create an NHGIS dataset specification
#' nhgis_extract <- define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   datasets = ds_spec(
#'     "1990_STF3",
#'     data_tables = "NP57",
#'     geog_levels = c("county", "tract")
#'   )
#' )
#'
#' nhgis_extract
#'
#' # Use `tst_spec()` to create an NHGIS time series table specification
#' define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   time_series_tables = tst_spec("CL8", geog_levels = "county"),
#'   tst_layout = "time_by_row_layout"
#' )
#'
#' # To request multiple datasets, provide a list of `ds_spec` objects
#' define_extract_nhgis(
#'   description = "Extract definition with multiple datasets",
#'   datasets = list(
#'     ds_spec("2014_2018_ACS5a", "B01001", c("state", "county")),
#'     ds_spec("2015_2019_ACS5a", "B01001", c("state", "county"))
#'   )
#' )
#'
#' # If you need to specify the same table or geographic level for
#' # many datasets, you may want to make a set of datasets before defining
#' # your extract request:
#' dataset_names <- c("2014_2018_ACS5a", "2015_2019_ACS5a")
#'
#' dataset_spec <- purrr::map(
#'   dataset_names,
#'   ~ ds_spec(
#'     .x,
#'     data_tables = "B01001",
#'     geog_levels = c("state", "county")
#'   )
#' )
#'
#' define_extract_nhgis(
#'   description = "Extract definition with multiple datasets",
#'   datasets = dataset_spec
#' )
#'
#' # You can request datasets, time series tables, and shapefiles in the same
#' # definition:
#' define_extract_nhgis(
#'   description = "Extract with datasets and time series tables",
#'   datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "county"),
#'   time_series_tables = tst_spec("CL6", "state"),
#'   shapefiles = "us_county_1990_tl2008"
#' )
#'
#' # Extract specifications can be indexed by name
#' names(nhgis_extract$datasets)
#'
#' nhgis_extract$datasets[["1990_STF3"]]
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(nhgis_extract)
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

  if (inherits(datasets, "ds_spec")) {
    datasets <- list(datasets)
  }

  if (inherits(time_series_tables, "tst_spec")) {
    time_series_tables <- list(time_series_tables)
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = set_nested_names(datasets),
    time_series_tables = set_nested_names(time_series_tables),
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

#' Create variable and sample specifications for IPUMS microdata extract
#' requests
#'
#' @description
#' Provide specifications for individual variables when defining an
#' IPUMS microdata extract request.
#'
#' Currently, no additional specifications are available for IPUMS samples.
#'
#' @param name Name of the sample or variable.
#' @param case_selections Cases to select for the given variable.
#' @param case_selection_type One of `"general"` or `"detailed"` indicating
#'   the type of case selection to use for the cases in `case_selections`.
#'   Defaults to `"general"` if any `case_selections` are specified.
#' @param attached_characteristics Characteristics to attach for the given
#'   variable. Accepted values are `"mother"`, `"father"`, `"spouse"`, `"head"`,
#'   or a combination.
#' @param data_quality_flags Logical indicating whether to include data quality
#'   flags for the given variable. By default, data quality flags are not
#'   included.
#' @param preselected Logical indicating whether the variable is preselected.
#'   This is unlikely to be needed.
#'
#' @return A `var_spec` or `samp_spec` object.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' var1 <- var_spec(
#'   "SCHOOL",
#'   case_selections = c("1", "2"),
#'   data_quality_flags = TRUE
#' )
#'
#' var2 <- var_spec(
#'   "RACE",
#'   case_selections = c("140", "150"),
#'   case_selection_type = "detailed",
#'   attached_characteristics = c("mother", "spouse")
#' )
#'
#' # Use variable specifications in a microdata extract definition:
#' define_extract_usa(
#'   description = "Example extract",
#'   samples = "us2017b",
#'   variables = list(var1, var2)
#' )
var_spec <- function(name,
                     case_selections = NULL,
                     case_selection_type = NULL,
                     attached_characteristics = NULL,
                     data_quality_flags = NULL,
                     preselected = NULL) {
  if (!is_null(case_selections) && !is_empty(case_selections)) {
    case_selection_type <- case_selection_type %||% "general"
  }

  variable <- new_nested_field(
    name,
    case_selections = case_selections,
    attached_characteristics = attached_characteristics,
    data_quality_flags = data_quality_flags,
    case_selection_type = case_selection_type,
    preselected = preselected,
    class = "var_spec"
  )

  variable <- validate_ipums_extract(variable)

  variable
}

#' @export
#' @rdname var_spec
samp_spec <- function(name) {
  sample <- new_nested_field(name, class = "samp_spec")

  variable <- validate_ipums_extract(sample)

  sample
}

#' Create dataset and time series table specifications for IPUMS NHGIS extract
#' requests
#'
#' @description
#' Provide specifications for individual datasets and time series
#' tables when defining an IPUMS NHGIS extract request.
#'
#' Use [get_metadata_nhgis()] to browse dataset and time series
#' table specification parameters.
#'
#' @param name Name of the dataset or time series table.
#' @param data_tables Vector of summary tables to retrieve for the given
#'   dataset.
#' @param geog_levels Geographic levels (e.g. `"county"` or `"state"`)
#'   at which to obtain data for the given dataset or time series table.
#' @param years Years for which to obtain the data for the given dataset or time
#'   series table.
#'
#'   For time series tables, all years are selected by default.
#'
#'   For datasets, use `"*"` to select all available years. Use
#'   [get_metadata_nhgis()] to determine if a dataset allows year selection.
#' @param breakdown_values [Breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to apply to the given dataset.
#'
#' @return A `ds_spec` or `tst_spec` object.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' dataset <- ds_spec(
#'   "2013_2017_ACS5a",
#'   data_tables = c("B00001", "B01002"),
#'   geog_levels = "state"
#' )
#'
#' tst <- tst_spec(
#'   "CW5",
#'   geog_levels = c("county", "tract"),
#'   years = "1990"
#' )
#'
#' # Use variable specifications in an extract definition:
#' define_extract_nhgis(
#'   description = "Example extract",
#'   datasets = dataset,
#'   time_series_tables = tst
#' )
ds_spec <- function(name,
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
    class = "ds_spec"
  )

  dataset <- validate_ipums_extract(dataset)

  dataset
}

#' @export
#' @rdname ds_spec
tst_spec <- function(name,
                     geog_levels,
                     years = NULL) {
  tst <- new_nested_field(
    name,
    geog_levels = geog_levels,
    years = years,
    class = "tst_spec"
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
#' [`define_extract_*()`][define_extract] to define an extract request to save.
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
      "*" = paste0(
        "As of ipumsr 0.6.0, all JSON-formatted extract definitions must ",
        "contain a `collection` field."
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
        "`extract_json` was created with IPUMS API version ",
        json_api_version, "."
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
#' - To add to an **IPUMS Microdata** extract definition, click
#'   [here][add_to_extract.micro_extract]. This includes:
#'     + IPUMS USA
#'     + IPUMS CPS
#'     + IPUMS International
#' - To add to an **IPUMS NHGIS** extract definition, click
#'   [here][add_to_extract.nhgis_extract]
#'
#' In general, for a given collection, the arguments to `add_to_extract()`
#' are identical to those used when defining an extract for that collection. For
#' more information about defining an extract, click [here][define_extract].
#'
#' To remove existing values from an extract definition, use
#' [remove_from_extract()]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   add to the extract definition.
#'
#'   All arguments available in a collection's
#'   [`define_extract_*()`][define_extract] function can be
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
#' [`define_extract_*()`][define_extract] to create a new extract request from
#' scratch.
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
#' nhgis_extract <- define_extract_nhgis(
#'   datasets = ds_spec(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2"),
#'     geog_levels = "county"
#'   )
#' )
#'
#' # Add new samples and variables
#' add_to_extract(
#'   usa_extract,
#'   samples = c("us2014a", "us2015a"),
#'   variables = var_spec("MARST", data_quality_flags = TRUE)
#' )
#'
#' # Update existing variables
#' add_to_extract(
#'   usa_extract,
#'   variables = var_spec("SEX", case_selections = "1")
#' )
#'
#' # Add a new dataset or time series table
#' add_to_extract(
#'   nhgis_extract,
#'   datasets = ds_spec(
#'     "1980_STF1",
#'     data_tables = "NT1A",
#'     geog_levels = c("county", "state")
#'   )
#' )
#'
#' # Update existing datasets/time series tables
#' add_to_extract(
#'   nhgis_extract,
#'   datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "state")
#' )
#'
#' # Values that can only take a single value are replaced
#' add_to_extract(nhgis_extract, data_format = "fixed_width")$data_format
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
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
#' in [define_extract_nhgis()]. Use `ds_spec()` and `tst_spec()` to create
#' dataset and time series table specifications.
#'
#' To remove existing values from an IPUMS NHGIS extract definition, use
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
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
#' @param datasets List of `ds_spec` objects created by [ds_spec()]
#'   containing the specifications
#'   for the [datasets](https://www.nhgis.org/overview-nhgis-datasets)
#'   to include in the extract request. See examples.
#'
#'   If a dataset already exists in the extract, its new specifications
#'   will be added to those that already exist for that dataset.
#' @param time_series_tables List of `tst_spec` objects created by [tst_spec()]
#'   containing the specifications for the
#'   [time series tables](https://www.nhgis.org/time-series-tables)
#'   to include in the extract request.
#'
#'   If a time series table already exists in the extract, its new
#'   specifications will be added to those that already exist for that time
#'   series table.
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
#'   datasets = ds_spec("1990_STF1", c("NP1", "NP2"), "county")
#' )
#'
#' # Add a new dataset or time series table to the extract
#' add_to_extract(
#'   extract,
#'   datasets = ds_spec("1990_STF2a", "NPA1", "county")
#' )
#'
#' add_to_extract(
#'   extract,
#'   time_series_tables = tst_spec("A00", "state")
#' )
#'
#' # If a dataset/time series table name already exists in the definition
#' # its specification will be modified by adding the new specification to
#' # the existing one
#' add_to_extract(
#'   extract,
#'   datasets = ds_spec("1990_STF1", "NP4", "nation")
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

  if (inherits(datasets, "ds_spec")) {
    datasets <- list(datasets)
  }

  if (inherits(time_series_tables, "tst_spec")) {
    time_series_tables <- list(time_series_tables)
  }

  if (!all(purrr::map_lgl(datasets, ~ inherits(.x, "ds_spec")))) {
    rlang::abort(paste0(
      "Expected `datasets` to be an `ds_spec` object ",
      "or a list of `ds_spec` objects."
    ))
  } else {
    purrr::walk(datasets, validate_ipums_extract)
  }

  if (!all(purrr::map_lgl(time_series_tables, ~ inherits(.x, "tst_spec")))) {
    rlang::abort(paste0(
      "Expected `time_series_tables` to be an `tst_spec` object ",
      "or a list of `tst_spec` objects."
    ))
  } else {
    purrr::walk(time_series_tables, validate_ipums_extract)
  }

  new_ds <- purrr::map_chr(datasets, ~ .x$name)
  old_ds <- purrr::map_chr(extract$datasets, ~ .x$name)

  tst_spec <- purrr::map_chr(time_series_tables, ~ .x$name)
  old_tst <- purrr::map_chr(extract$time_series_tables, ~ .x$name)

  if (anyDuplicated(new_ds) != 0) {
    rlang::abort("Cannot add two `ds_spec` objects of same name.")
  }

  if (anyDuplicated(tst_spec) != 0) {
    rlang::abort("Cannot add two `tst_spec` objects of same name.")
  }

  if (!is_null(extract$datasets)) {
    extract$datasets <- purrr::map(
      extract$datasets,
      function(x) {
        name_exists <- x$name %in% new_ds

        if (any(name_exists)) {
          i <- which(x$name == new_ds)
          ds_spec(
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
        name_exists <- x$name %in% tst_spec

        if (any(name_exists)) {
          i <- which(x$name == tst_spec)
          tst_spec(
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

  ds_specs <- datasets[which(!new_ds %in% old_ds)]
  tst_specs <- time_series_tables[which(!tst_spec %in% old_tst)]

  datasets <- unique(c(extract$datasets, ds_specs))
  time_series_tables <- unique(c(extract$time_series_tables, tst_specs))

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
    datasets = set_nested_names(datasets),
    time_series_tables = set_nested_names(time_series_tables),
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

#' Add values to an existing extract definition for an IPUMS microdata
#' collection
#'
#' @description
#' Add new values or replace existing values in an IPUMS microdata extract
#' definition.
#'
#' All fields are optional, and if omitted, will be unchanged. Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' To remove existing values from an IPUMS microdata extract definition, use
#' [`remove_from_extract()`][remove_from_extract.micro_extract]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' To modify variable-specific parameters for variables that already exist
#' in the extract, create a new variable specification with [var_spec()].
#'
#' @inheritParams define_extract_micro
#' @inheritParams add_to_extract
#' @param variables Character vector of variable names or a list of
#'   `var_spec` objects created by [var_spec()]
#'   containing specifications for all variables to include in the extract.
#'
#'   If a variable already exists in the extract, its specifications
#'   will be added to those that already exist for that variable.
#' @param data_format Format for the output extract data file. One of
#'   `"fixed_width"`, `"csv"`, `"stata"`, `"spss"`, or `"sas9"`.
#' @param ... Ignored
#'
#' @return A modified `micro_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`remove_from_extract()`][remove_from_extract.micro_extract()] to remove
#'   values from an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [`define_extract_*()`][define_extract] to create a new extract definition
#'   from scratch
#'
#' @export
#'
#' @name add_to_extract_micro
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
#' # Add new variable-specific specifications as you would when defining
#' # an extract, even to modify variables that already exist in the definition
#' add_to_extract(
#'   extract,
#'   samples = "us2014a",
#'   variables = list(
#'     var_spec("RACE", attached_characteristics = "mother"),
#'     var_spec("SEX", case_selections = "2")
#'   )
#' )
#'
#' # Values that only take a single value are replaced
#' add_to_extract(extract, description = "New description")$description
#'
#' # You can also combine two separate extract requests together
#' combine_extracts(extract, extract2)
add_to_extract.micro_extract <- function(extract,
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
    rectangular_on <- rectangular_on %||% "P"
  }

  if (inherits(variables, "var_spec")) {
    variables <- list(variables)
  } else if (!is_named(variables)) {
    to_coerce <- purrr::map_lgl(variables, ~ !inherits(.x, "var_spec"))

    if (any(to_coerce)) {
      variables[to_coerce] <- purrr::map(variables[to_coerce], var_spec)
    }
  }

  if (inherits(samples, "samp_spec")) {
    samples <- list(samples)
  } else if (!is_named(samples)) {
    to_coerce <- purrr::map_lgl(samples, ~ !inherits(.x, "samp_spec"))

    if (any(to_coerce)) {
      samples[to_coerce] <- purrr::map(samples[to_coerce], samp_spec)
    }
  }

  if (!all(purrr::map_lgl(variables, ~ inherits(.x, "var_spec")))) {
    rlang::abort(paste0(
      "Expected `variables` to be an `var_spec` object ",
      "or a list of `var_spec` objects."
    ))
  } else {
    purrr::walk(variables, validate_ipums_extract)
  }

  if (!all(purrr::map_lgl(samples, ~ inherits(.x, "samp_spec")))) {
    rlang::abort(paste0(
      "Expected `samples` to be an `samp_spec` object ",
      "or a list of `samp_spec` objects."
    ))
  } else {
    purrr::walk(samples, validate_ipums_extract)
  }

  new_vars <- purrr::map_chr(variables, ~ .x$name)
  old_vars <- purrr::map_chr(extract$variables, ~ .x$name)

  new_samps <- purrr::map_chr(samples, ~ .x$name)
  old_samps <- purrr::map_chr(extract$samples, ~ .x$name)

  if (anyDuplicated(new_vars) != 0) {
    rlang::abort("Cannot add two `var_spec` objects of same name.")
  }

  if (anyDuplicated(new_samps) != 0) {
    rlang::abort("Cannot add two `samp_spec` objects of same name.")
  }

  if (!is_null(extract$variables)) {
    extract$variables <- purrr::map(
      extract$variables,
      function(x) {
        name_exists <- x$name %in% new_vars

        if (any(name_exists)) {
          i <- which(x$name == new_vars)
          var_spec(
            name = x$name,
            case_selections = union(x$case_selections, variables[[i]]$case_selections),
            case_selection_type = variables[[i]]$case_selection_type %||% x$case_selection_type,
            attached_characteristics = union(x$attached_characteristics, variables[[i]]$attached_characteristics),
            data_quality_flags = variables[[i]]$data_quality_flags %||% x$data_quality_flags,
            preselected = variables[[i]]$preselected %||% x$preselected
          )
        } else {
          x
        }
      }
    )
  }

  if (!is_null(extract$samples)) {
    extract$samples <- purrr::map(
      extract$samples,
      function(x) {
        name_exists <- x$name %in% new_samps

        if (any(name_exists)) {
          i <- which(x$name == new_samps)
          samp_spec(name = x$name)
        } else {
          x
        }
      }
    )
  }

  var_specs <- variables[which(!new_vars %in% old_vars)]
  samp_specs <- samples[which(!new_samps %in% old_samps)]

  variables <- unique(c(extract$variables, var_specs))
  samples <- unique(c(extract$samples, samp_specs))

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = description %||% extract$description,
    samples = set_nested_names(samples),
    variables = set_nested_names(variables),
    data_structure = data_structure %||% extract$data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format %||% extract$data_format
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
#' - To remove from an **IPUMS Microdata** extract definition, click
#'   [here][remove_from_extract.micro_extract]. This includes:
#'     + IPUMS USA
#'     + IPUMS CPS
#'     + IPUMS International
#' - To remove from an **IPUMS NHGIS** extract definition, click
#'   [here][remove_from_extract.nhgis_extract]
#'
#' To add new values to an extract, see [add_to_extract()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   remove from the extract definition.
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
#' [`define_extract_*()`][define_extract] to create a new extract definition
#' from scratch.
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
#'   datasets = ds_spec(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2", "NP3"),
#'     geog_levels = "county"
#'   ),
#'   time_series_tables = tst_spec("A00", geog_levels = "county")
#' )
#'
#' # Remove an existing dataset
#' remove_from_extract(nhgis_extract, datasets = "1990_STF1")
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' Remove values from an existing NHGIS extract definition
#'
#' @description
#' Remove existing values from an IPUMS NHGIS extract definition. All
#' fields are optional, and if omitted, will be unchanged.
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
#' To retain a dataset or time series table while modifying its particular
#' specifications, first remove that dataset or time series table, then add a
#' new specification using `add_to_extract()`.
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
#'   datasets = ds_spec(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2", "NP3"),
#'     geog_levels = "county"
#'   ),
#'   time_series_tables = list(
#'     tst_spec("CW3", c("state", "county")),
#'     tst_spec("CW5", c("state", "county"))
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

  if (!is_null(datasets) && !is.character(datasets)) {
    rlang::abort("Expected `datasets` to be a character vector of dataset names.")
  }

  if (!is_null(time_series_tables) && !is.character(time_series_tables)) {
    rlang::abort("Expected `time_series_tables` to be a character vector of time series table names.")
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

#' Remove values from an existing extract definition for an IPUMS microdata
#' project
#'
#' @description
#' Remove existing values from an IPUMS microdata extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' To add new values to an IPUMS microdata extract definition, see
#' [`add_to_extract()`][add_to_extract.micro_extract].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' If the supplied extract definition comes from
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' To retain a variable while modifying its particular specifications, first
#' remove that variable, then add a new specification using `add_to_extract()`.
#'
#' @inheritParams define_extract_micro
#' @inheritParams remove_from_extract
#' @param samples Character vector of sample names to remove from the extract.
#' @param variables Names of the variables to remove from the extract. All
#'   variable-specific fields for the indicated variables will also be removed.
#' @param ... Ignored
#'
#' @return A modified `micro_extract` object
#'
#' @keywords internal
#'
#' @seealso
#' [`add_to_extract()`][add_to_extract.micro_extract()] to add values
#' to an extract definition.
#'
#' [combine_extracts()] to combine multiple extract definitions.
#'
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [`define_extract_*()`][define_extract] to create a new extract definition
#'   from scratch.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = list(
#'     var_spec("AGE", data_quality_flags = TRUE),
#'     var_spec("SEX", case_selections = "1"),
#'     "RACE"
#'   )
#' )
#'
#' remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a",
#'   variables = c("AGE", "RACE")
#' )
remove_from_extract.micro_extract <- function(extract,
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

  if (!is_null(samples) && !is.character(samples)) {
    rlang::abort("Expected `samples` to be a character vector of sample names.")
  }

  if (!is_null(variables) && !is.character(variables)) {
    rlang::abort("Expected `variables` to be a character vector of variable names.")
  }

  vars_to_remove <- unlist(
    purrr::compact(purrr::map(
      variables,
      function(d) {
        which(purrr::map_lgl(extract$variables, ~ .x$name == d))
      }
    ))
  )

  samps_to_remove <- unlist(
    purrr::compact(purrr::map(
      samples,
      function(d) {
        which(purrr::map_lgl(extract$samples, ~ .x$name == d))
      }
    ))
  )

  variables <- extract$variables[setdiff(seq_along(extract$variables), vars_to_remove)]
  samples <- extract$samples[setdiff(seq_along(extract$samples), samps_to_remove)]

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = extract$description,
    samples = samples,
    variables = variables,
    data_structure = extract$data_structure,
    rectangular_on = extract$rectangular_on,
    data_format = extract$data_format
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
#' [`define_extract_*()`][define_extract] to create a new extract definition
#' from scratch.
#'
#' @export
#'
#' @examples
#' ext1 <- define_extract_nhgis(
#'   datasets = ds_spec(
#'     "2011_2015_ACS5a",
#'     data_tables = "B00001",
#'     geog_levels = "county"
#'   )
#' )
#'
#' ext2 <- define_extract_nhgis(
#'   datasets = list(
#'     ds_spec("2011_2015_ACS5a", c("B00002", "B01001"), "county"),
#'     ds_spec("2012_2016_ACS5a", c("B00002", "B01001"), "county")
#'   )
#' )
#'
#' combine_extracts(ext1, ext2)
combine_extracts <- function(...) {
  UseMethod("combine_extracts")
}

#' @export
combine_extracts.usa_extract <- function(...) {
  NextMethod()
}

#' @export
combine_extracts.cps_extract <- function(...) {
  NextMethod()
}

#' @export
combine_extracts.micro_extract <- function(...) {
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
    class = c(
      paste0(collection, "_extract"),
      paste0(collection_type(collection), "_extract"),
      "ipums_extract",
      class(out)
    )
  )
}

define_extract_micro <- function(collection,
                                 description,
                                 samples,
                                 variables,
                                 data_format = "fixed_width",
                                 data_structure = "rectangular",
                                 rectangular_on = NULL) {
  if (data_structure == "rectangular") {
    rectangular_on <- rectangular_on %||% "P"
  }

  if (inherits(variables, "var_spec")) {
    variables <- list(variables)
  } else if (!is_named(variables)) {
    to_coerce <- purrr::map_lgl(variables, ~ !inherits(.x, "var_spec"))

    if (any(to_coerce)) {
      variables[to_coerce] <- purrr::map(variables[to_coerce], var_spec)
    }
  }

  if (inherits(samples, "samp_spec")) {
    samples <- list(samples)
  } else if (!is_named(samples)) {
    to_coerce <- purrr::map_lgl(samples, ~ !inherits(.x, "samp_spec"))

    if (any(to_coerce)) {
      samples[to_coerce] <- purrr::map(samples[to_coerce], samp_spec)
    }
  }

  extract <- new_ipums_extract(
    collection = collection,
    description = description,
    samples = set_nested_names(samples),
    variables = set_nested_names(variables),
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format
  )

  extract <- validate_ipums_extract(extract)

  extract
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
    class = c(
      paste0(collection, "_json"),
      paste0(collection_type(collection), "_json"),
      "ipums_json",
      class(json)
    )
  )
}

new_nested_field <- function(name, ..., class) {
  nested_field <- c(
    list(name = name),
    purrr::compact(rlang::list2(...))
  )

  structure(
    nested_field,
    class = c(class, "ipums_spec", class(nested_field))
  )
}

# Helper to add names to list of nested fields within an extract object.
# Makes it easier to index these objects while preserving possibility
# to work with lists of `ipums_*` objects.
# Having names be applied to each individual object automatically would
# require combining multiple with c(), which would remove class.
set_nested_names <- function(x) {
  # Don't catch errors here, as they should be handled in validator with
  # more standard error messages
  out <- try(
    purrr::set_names(x, purrr::map_chr(x, ~ .x$name)),
    silent = TRUE
  )

  if (inherits(out, "try-error")) {
    out <- x
  }

  out
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
validate_ipums_extract <- function(x, call = caller_env()) {
  UseMethod("validate_ipums_extract")
}

#' @export
validate_ipums_extract.nhgis_extract <- function(x, call = caller_env()) {
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

  if (!all(purrr::map_lgl(x$datasets, ~ inherits(.x, "ds_spec")))) {
    rlang::abort(c(
      paste0(
        "Expected `datasets` to be a `ds_spec` object ",
        "or a list of `ds_spec` objects."
      )
    ))
  } else {
    # purrr 1.0.1 adds additional info for errors that occur inside calls
    # to map() functions. This is not useful in our case, so we remove.
    # This should still be compatible with older versions of purrr.
    withCallingHandlers(
      purrr::walk(
        x$datasets,
        ~ validate_ipums_extract(.x, call = caller_env())
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )
  }

  if (!all(purrr::map_lgl(x$time_series_tables, ~ inherits(.x, "tst_spec")))) {
    rlang::abort(paste0(
      "Expected `time_series_tables` to be a `tst_spec` object ",
      "or a list of `tst_spec` objects."
    ))
  } else {
    withCallingHandlers(
      purrr::walk(
        x$time_series_tables,
        ~ validate_ipums_extract(.x, call = caller_env())
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )
  }

  if (anyDuplicated(purrr::map(x$datasets, ~ .x$name)) != 0) {
    rlang::abort("Cannot add multiple `datasets` of same name.")
  }

  if (anyDuplicated(purrr::map(x$time_series_tables, ~ .x$name)) != 0) {
    rlang::abort("Cannot add multiple `time_series_tables` of same name.")
  }

  # Specify the validation requirements for each extract field
  extract_field_spec <- list(
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
validate_ipums_extract.micro_extract <- function(x, call = caller_env()) {
  NextMethod()

  if (length(x$data_structure) > 0 && x$data_structure == "rectangular") {
    if (length(x$rectangular_on) > 0 && x$rectangular_on != "P") {
      rlang::abort(
        paste0(
          "Currently, the `rectangular_on` argument must be equal to \"P\"; ",
          "in the future, the API will also support `rectangular_on = \"H\"."
        )
      )
    }
  }

  if (is_empty(x$samples) || is_na(x$samples)) {
    rlang::abort(paste0(
      "A `", class(x)[1], "` must contain values for `samples`."
    ))
  }

  if (is_empty(x$variables) || is_na(x$variables)) {
    rlang::abort(paste0(
      "A `", class(x)[1], "` must contain values for `variables`."
    ))
  }

  if (!all(purrr::map_lgl(x$samples, ~ inherits(.x, "samp_spec")))) {
    rlang::abort(paste0(
      "Expected `samples` to be an `samp_spec` object ",
      "or a list of `samp_spec` objects."
    ))
  } else {
    withCallingHandlers(
      purrr::walk(
        x$samples,
        ~ validate_ipums_extract(.x, call = caller_env())
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )
  }

  if (!all(purrr::map_lgl(x$variables, ~ inherits(.x, "var_spec")))) {
    rlang::abort(paste0(
      "Expected `variables` to be an `var_spec` object ",
      "or a list of `var_spec` objects."
    ))
  } else {
    withCallingHandlers(
      purrr::walk(
        x$variables,
        ~ validate_ipums_extract(.x, call = caller_env())
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )
  }

  if (anyDuplicated(purrr::map(x$variables, ~ .x$name)) != 0) {
    rlang::abort("Cannot add multiple `variables` of same name.")
  }

  if (anyDuplicated(purrr::map(x$samples, ~ .x$name)) != 0) {
    rlang::abort("Cannot add multiple `samples` of same name.")
  }

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
      must_be_present_msg = " when `data_structure` is \"rectangular\"",
      must_be_missing_msg = " when `data_structure` is \"hierarchical\"",
      length = 1,
      type = "character"
    ),
    list(
      field = "data_format",
      required = TRUE,
      choices = c("fixed_width", "csv", "stata", "spss", "sas9"),
      length = 1,
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
validate_ipums_extract.ipums_extract <- function(x, call = caller_env()) {
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
validate_ipums_extract.samp_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% "name"]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `samp_spec` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      )
    ))
  }

  spec <- list(
    list(
      field = "name",
      required = TRUE,
      length = 1,
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
        "Invalid `samp_spec` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)
}

#' @export
validate_ipums_extract.var_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% c(
    "name",
    "case_selections",
    "attached_characteristics",
    "data_quality_flags",
    "case_selection_type",
    "preselected"
  )]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `var_spec` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      )
    ))
  }

  includes_cs <- !is_empty(x$case_selections) || !is_null(x$case_selections)

  spec <- list(
    list(
      field = "name",
      required = TRUE,
      length = 1,
      type = "character"
    ),
    list(
      field = "case_selections",
      required = FALSE,
      type = "character"
    ),
    list(
      field = "attached_characteristics",
      required = FALSE,
      type = "character"
    ),
    list(
      field = "data_quality_flags",
      required = FALSE,
      type = "logical",
      length = 1
    ),
    list(
      field = "case_selection_type",
      required = includes_cs,
      allowed = includes_cs,
      must_be_present_msg = " when `case_selection` is provided",
      must_be_missing_msg = " when `case_selection` is not provided",
      type = "character",
      choices = c("general", "detailed")
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
        "Invalid `var_spec` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)
}

#' @export
validate_ipums_extract.ds_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% c(
    "name",
    "data_tables",
    "geog_levels",
    "years",
    "breakdown_values"
  )]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `ds_spec` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      )
    ))
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
        "Invalid `ds_spec` object:",
        purrr::set_names(extract_issues, "x")
      )
    )
  }

  invisible(x)
}

#' @export
validate_ipums_extract.tst_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% c("name", "geog_levels", "years")]

  if (length(unexpected_names) > 0) {
    rlang::abort(c(
      "Invalid `tst_spec` object:",
      "x" = paste0(
        "Unrecognized fields: `", paste0(unexpected_names, collapse = "`, `"), "`"
      )
    ))
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
        "Invalid `tst_spec` object:",
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

      api_extract_warnings(x$number, def$warnings)

      if (is_null(names(def$datasets))) {
        datasets <- NULL
      } else {
        datasets <- purrr::map(
          names(def$datasets),
          ~ ds_spec(
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
          ~ tst_spec(
            .x,
            geog_levels = def$timeSeriesTables[[.x]]$geogLevels,
            years = def$timeSeriesTables[[.x]]$years
          )
        )
      }

      out <- new_ipums_extract(
        collection = def$collection,
        description = def$description,
        datasets = set_nested_names(datasets),
        time_series_tables = set_nested_names(time_series_tables),
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
extract_list_from_json.micro_json <- function(extract_json, validate = FALSE) {
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

      api_extract_warnings(x$number, def$warnings)

      samples <- purrr::map(
        names(def$samples),
        ~ samp_spec(.x)
      )

      variables <- purrr::map(
        names(def$variables),
        ~ var_spec(
          .x,
          case_selections = unlist(def$variables[[.x]]$caseSelections[[1]]),
          case_selection_type = names(def$variables[[.x]]$caseSelections),
          attached_characteristics = unlist(def$variables[[.x]]$attachedCharacteristics),
          data_quality_flags = def$variables[[.x]]$dataQualityFlags,
          preselected = def$variables[[.x]]$preselected
        )
      )

      out <- new_ipums_extract(
        collection = def$collection,
        description = def$description,
        samples = set_nested_names(samples),
        variables = set_nested_names(variables),
        data_structure = names(def$dataStructure),
        rectangular_on = def$dataStructure$rectangular$on,
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
extract_list_from_json.json <- function(extract_json) {
  extract_info <- jsonlite::fromJSON(extract_json)

  if (is_null(extract_info$collection)) {
    rlang::abort(c(
      paste0(
        "Could not determine the collection associated with this extract ",
        "definition."
      ),
      "*" = paste0(
        "As of ipumsr 0.6.0, all JSON-formatted extract definitions must ",
        "contain a `collection` field."
      )
    ))
  }

  extract_json <- new_ipums_json(extract_json, collection = extract_info$collection)
  extract_list_from_json(extract_json)
}

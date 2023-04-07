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
#' * Use [get_recent_extracts_info()] to obtain the extract definitions of
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
#'
#'   Currently, only `"rectangular"` extracts are supported.
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
define_extract_usa <- function(
    description,
    samples,
    variables,
    data_format = c("fixed_width", "csv", "stata", "spss", "sas9"),
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
define_extract_cps <- function(
    description,
    samples,
    variables,
    data_format = c("fixed_width", "csv", "stata", "spss", "sas9"),
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

#' Define an IPUMS NHGIS extract request
#'
#' @description
#' Define an IPUMS NHGIS request to be submitted via the IPUMS
#' API. An extract request contains the specifications required
#' to obtain a particular set of data from the IPUMS NHGIS system.
#'
#' Use [get_nhgis_metadata()] to browse and identify data sources for use
#' in NHGIS extract definitions.
#'
#' See Details section for information about creating valid extract
#' definitions. For general information, see the NHGIS
#' [data source overview](https://www.nhgis.org/data-availability) and the
#' [FAQ](https://www.nhgis.org/frequently-asked-questions-faq).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' An NHGIS extract definition must include at least one dataset, time series
#' table, or shapefile. For extract definitions that include any `datasets`,
#' corresponding `data_tables` and `geog_levels` must be specified. For
#' extract definitions that include any `time_series_tables`, corresponding
#' `geog_levels` must be specified.
#'
#' NHGIS extract definitions may contain multiple `datasets` or
#' `time_series_tables.` Each dataset or time series table is associated with
#' several subfields that apply only to that particular dataset or time series
#' table.
#'
#' The following arguments are applied to the requested `datasets`:
#' - `data_tables`
#' - `years`
#' - `breakdown_values`
#'
#' The following arguments are applied to the requested `datasets` *and*
#' `time_series_tables`:
#' - `geog_levels`
#'
#' To apply the values of these arguments to *all* of the `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a vector:
#'
#' ```{r}
#' define_extract_nhgis(
#'   datasets = c("2016_2020_ACS5a", "2017_2021_ACS5a"),
#'   data_tables = "B01001",
#'   geog_levels = c("county", "state")
#' )
#' ```
#'
#' To apply the values of these arguments to *specific* `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a named
#' list. In this case, names should correspond to the `datasets` and/or
#' `time_series_tables` and their associated values will only be applied to
#' the indicated dataset or time series table:
#'
#' ```{r}
#' define_extract_nhgis(
#'   datasets = "2016_2020_ACS5a",
#'   data_tables = "B01001",
#'   time_series_tables = "A00",
#'   geog_levels = list(
#'     "2016_2020_ACS5a" = "county",
#'     "A00" = "state"
#'   )
#' )
#' ```
#'
#' Note that while it is possible to match elements to `datasets` and/or
#' `time_series_tables` by index by providing an unnamed list, we recommend
#' using names to explicitly specify the values that should be applied to each
#' of the request's `datasets` and `time_series_tables`.
#'
#' Alternatively, you can create multiple extract definitions and combine
#' them with [combine_extracts()].
#'
#' @param description Description of the extract.
#' @param datasets [Datasets](https://www.nhgis.org/overview-nhgis-datasets)
#'   to include in the extract request.
#' @param data_tables Summary tables to retrieve for each of the requested
#'   `datasets`. Required for all `datasets` in the extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param time_series_tables
#'   [Time series tables](https://www.nhgis.org/time-series-tables)
#'   to include in the extract request.
#' @param geog_levels Geographic levels (e.g. `"county"` or `"state"`)
#'   at which to obtain data for the requested `datasets` or
#'   `time_series_tables`. Required for all `datasets` and `time_series_tables`
#'   in the extract definition.
#'
#'   See Details section for syntax options when an extract
#'   definition has multiple `datasets` and/or `time_series_tables`.
#' @param years Years for which to obtain the data contained in the requested
#'   `datasets`. Use `"*"` to select all available years for a given dataset.
#'   Use [get_nhgis_metadata()] to determine if a dataset allows year selection.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param breakdown_values [Breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to apply to the requested `datasets`.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param geographic_extents Vector of geographic extents to use for
#'   all of the `datasets` in the extract definition (for instance, to obtain
#'   data within a particular state). Use `"*"` to select all available extents.
#'
#'   Required when the extract definition includes any `geog_levels` that
#'   require extent selection. See [get_nhgis_metadata()] to determine if a
#'   geographic level requires extent selection. At the time of writing, NHGIS
#'   supports extent selection only for blocks and block groups.
#' @param shapefiles [Shapefiles](https://www.nhgis.org/gis-files) to include
#'   in the extract request.
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
#' my_extract <- define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   datasets = "1990_STF3",
#'   data_tables = "NP57",
#'   geog_levels = c("county", "tract")
#' )
#'
#' my_extract
#'
#' # Extract definition for an NHGIS time series table
#' define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   time_series_tables = "CL8",
#'   geog_levels = "county"
#' )
#'
#' # Use get_nhgis_metadata() to identify data source names
#' \dontrun{
#' get_nhgis_metadata("time_series_tables")
#' }
#'
#' # Fields that are attached to specific datasets/time series tables
#' # are recycled to all datasets/time series tables in an extract by default
#' define_extract_nhgis(
#'   description = "Extract definition with multiple datasets",
#'   datasets = c("2014_2018_ACS5a", "2015_2019_ACS5a"),
#'   data_tables = "B01001",
#'   geog_levels = c("state", "county")
#' )
#'
#' # To instead attach specific values to each dataset or time series table,
#' # use a named list instead of a vector:
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
#'   geog_levels = list("1990_STF1" = "county", "CL6" = "state")
#' )
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(my_extract)
#' }
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
    rlang::warn(paste0(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be applied to all datasets."
    ))
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
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2"),
#'   geog_levels = "county"
#' )
#'
#' # Add a new dataset/time series table
#' add_to_extract(
#'   nhgis_extract,
#'   datasets = "1980_STF1",
#'   data_tables = "NT1A",
#'   geog_levels = c("county", "state")
#' )
#'
#' # Add to existing datasets/time series tables
#' add_to_extract(nhgis_extract, geog_levels = "state")
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
#' in [define_extract_nhgis()]. See Details section for more
#' information on how values passed to dataset and time series table subfields
#' are interpreted.
#'
#' To remove existing values from an IPUMS NHGIS extract definition, use
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract]. To add values
#' contained in another extract definition, use [combine_extracts()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' For extract definitions that include any `datasets`,
#' corresponding `data_tables` and `geog_levels` must be specified. For
#' extract definitions that include any `time_series_tables`, corresponding
#' `geog_levels` must be specified.
#'
#' NHGIS extract definitions may contain multiple `datasets` or
#' `time_series_tables.` Each dataset or time series table is associated with
#' several subfields that apply only to that particular dataset or time series
#' table.
#'
#' The following arguments are applied to an extract definition's `datasets`:
#' - `data_tables`
#' - `years`
#' - `breakdown_values`
#'
#' The following arguments are applied to an extract definition's `datasets`
#' *and* `time_series_tables`:
#' - `geog_levels`
#'
#' To add the values of these arguments to *multiple* `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a vector.
#' In this case, if any values are
#' provided in the `datasets` or `time_series_tables` arguments, subfield values
#' will only be applied to those `datasets` and/or `time_series_tables`.
#' Otherwise, they will be applied to all `datasets` and/or
#' `time_series_tables` that exist in the extract definition.
#'
#' To add the values of these arguments to *specific* `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a named
#' list. In this case, names should correspond to the `datasets` and/or
#' `time_series_tables` and their associated values will only be added to
#' the indicated dataset or time series table. Values can be added for new
#' `datasets` and `time_series_tables` or those that already exist in the
#' extract definition.
#'
#' See examples for demonstrations of this syntax.
#'
#' Note that while it is possible to match elements to `datasets` and/or
#' `time_series_tables` by index by providing an unnamed list, we recommend
#' using names to explicitly specify the values that should be added to each
#' of the request's `datasets` and `time_series_tables`.
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
#' @param data_tables Summary tables to retrieve for each of the requested
#'   `datasets`. If no `datasets` are provided, applies to those that
#'   already exist in the extract definition. Required if any new `datasets`
#'   are specified.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param geog_levels Geographic levels (for example, `"county"` or `"state"`)
#'   at which to obtain data for the requested `datasets` and
#'   `time_series_tables`. If no `datasets` and/or `time_series_tables` are
#'   provided, applies to those that already
#'   exist in the extract definition. Required if any new `datasets` or
#'   `time_series_tables` are specified.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets` or `time_series_tables`.
#' @param years Years for which to obtain the data contained in the requested
#'   `datasets`. If no `datasets` are provided, applies to those that
#'   already exist in the extract definition.
#'
#'   Use `"*"` to select all available years for a given dataset.
#'   Use [get_nhgis_metadata()] to determine if a dataset allows year selection.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param breakdown_values [Breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to to apply to the requested `datasets`. If no `datasets` are provided,
#'   applies to those that already exist in the extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
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
#' [submit_extract()] and [download_extract()] to submit and process an
#'   extract request.
#'
#' [define_extract_nhgis()] to create a new extract definition.
#'
#' @export
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2"),
#'   geog_levels = "county"
#' )
#'
#' # Modify existing datasets in extract:
#' extract2 <- add_to_extract(
#'   extract,
#'   data_tables = "NP3"
#' )
#'
#' extract2
#'
#' # Add a new dataset or time series table. Fields that apply to each dataset
#' # or time series table are only applied to the newly added dataset or
#' # time series table:
#' extract <- add_to_extract(
#'   extract,
#'   datasets = "1980_STF1",
#'   data_tables = "NT1A",
#'   geog_levels = c("county", "state")
#' )
#'
#' add_to_extract(
#'   extract,
#'   time_series_tables = "A00",
#'   geog_levels = "state"
#' )
#'
#' # To modify existing datasets, use a named list to specify which datasets
#' # should be attached to which `data_tables` or `geog_levels`.
#' # Vector arguments are attached to all datasets.
#' add_to_extract(
#'   extract,
#'   data_tables = list(
#'     "1990_STF1" = "NP4",
#'     "1980_STF1" = "NT1B"
#'   ),
#'   geog_levels = "nation"
#' )
#'
#' # Values that can only take a single value are replaced
#' add_to_extract(extract, data_format = "fixed_width")$data_format
#'
#' # You can also combine two extract requests together
#' combine_extracts(extract, extract2)
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
    rlang::warn(paste0(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be applied to all datasets."
    ))
  }

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
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2", "NP3"),
#'   time_series_tables = "A00",
#'   geog_levels = "county"
#' )
#'
#' # Remove a table from existing datasets:
#' remove_from_extract(nhgis_extract, data_tables = "NP3")
#'
#' # Remove an entire dataset/time series table
#' remove_from_extract(nhgis_extract, time_series_tables = "A00")
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
#' used in [`define_extract_nhgis()`][define_extract_nhgis]. See Details section
#' for more information on how values passed to dataset and time series table
#' subfields are interpreted.
#'
#' To add new values to an IPUMS NHGIS extract definition, use
#' [`add_to_extract()`][add_to_extract.nhgis_extract]. When replacing values,
#' it is best to first add new values using
#' `add_to_extract()` before removing values with
#' `remove_from_extract()`. This limits the possibility of producing a
#' temporarily invalid extract specification.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' For extract definitions that include any `datasets`,
#' corresponding `data_tables` and `geog_levels` must be specified. For
#' extract definitions that include any `time_series_tables`, corresponding
#' `geog_levels` must be specified.
#'
#' NHGIS extract definitions may contain multiple `datasets` or
#' `time_series_tables.` Each dataset or time series table is associated with
#' several subfields that apply only to that particular dataset or time series
#' table.
#'
#' The following arguments are removed from the extract definition's `datasets`:
#' - `data_tables`
#' - `years`
#' - `breakdown_values`
#'
#' The following arguments are removed from the extract definition's `datasets`
#' *and* `time_series_tables`:
#' - `geog_levels`
#'
#' To remove the values of these arguments from *all* of the `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a vector.
#'
#' To remove the values of these arguments to *specific* `datasets` and/or
#' `time_series_tables` in the extract definition, pass the values as a named
#' list. In this case, names should correspond to the `datasets` and/or
#' `time_series_tables` and their associated values will only be removed from
#' the indicated dataset or time series table.
#'
#' Note that subfields are modified *after* the removal of any `datasets`
#' and `time_series_tables` specified in `datasets` or
#' `time_series_tables` arguments.
#'
#' While while it is possible to match
#' elements to `datasets` and/or
#' `time_series_tables` by index by providing an unnamed list, we recommend
#' using names to explicitly specify the values that should be removed from each
#' of the request's `datasets` and `time_series_tables`.
#'
#' Any extract fields that are rendered irrelevant after modifying the extract
#' will be automatically removed. (For instance, if all `time_series_tables`
#' are removed from an extract, `tst_layout` will also be
#' removed.) Thus, it is not necessary to explicitly remove these values.
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
#' a previously submitted extract request, this function will reset the
#' definition to an unsubmitted state.
#'
#' @inheritParams define_extract_nhgis
#' @inheritParams remove_from_extract
#' @param datasets [Datasets](https://www.nhgis.org/overview-nhgis-datasets)
#'   to remove from the extract definition. All `data_tables`, `geog_levels`,
#'   `years`, and `breakdown_values` associated with the specified
#'   `datasets` will also be removed.
#' @param data_tables Summary tables to remove from the `datasets` in the
#'   extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param time_series_tables
#'   [Time series tables](https://www.nhgis.org/time-series-tables)
#'   to remove from the extract definition. All `geog_levels`
#'   associated  with the specified `time_series_tables` will also be removed.
#' @param geog_levels Geographic levels to remove from the `datasets` and/or
#'   `time_series_tables` in the extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets` or `time_series_tables`.
#' @param years Years to remove from the `datasets` in the extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`.
#' @param breakdown_values [Breakdown
#'   values](https://www.nhgis.org/frequently-asked-questions-faq#breakdowns)
#'   to remove from the `datasets` in the extract definition.
#'
#'   See Details section for syntax options when an extract definition has
#'   multiple `datasets`..
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
#'   datasets = "1990_STF1",
#'   data_tables = c("NP1", "NP2", "NP3"),
#'   geog_levels = "county"
#' )
#'
#' # Remove a table from existing datasets:
#' remove_from_extract(extract, data_tables = "NP3")
#'
#' extract2 <- define_extract_nhgis(
#'   time_series_tables = c("CW3", "CW5"),
#'   geog_levels = c("state", "county")
#' )
#'
#' # Remove an entire time series table
#' remove_from_extract(
#'   extract2,
#'   time_series_tables = "CW3"
#' )
#'
#' # Use a named list to remove values from specific datasets or time
#' # series tables:
#' remove_from_extract(
#'   extract2,
#'   geog_levels = list(CW5 = "state", CW3 = "county")
#' )
#'
#' # Replace values by using `add_to_extract()` first to avoid invalid
#' # extract definitions
#' revised <- add_to_extract(extract2, geog_levels = list(CW3 = "tract"))
#' remove_from_extract(revised, geog_levels = list(CW3 = c("county", "state")))
remove_from_extract.nhgis_extract <- function(extract,
                                              datasets = NULL,
                                              data_tables = NULL,
                                              time_series_tables = NULL,
                                              geog_levels = NULL,
                                              years = NULL,
                                              breakdown_values = NULL,
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

  if (is.list(geographic_extents)) {
    rlang::warn(paste0(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be removed from all datasets."
    ))
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
      ~ if (is_null(extract[[.x]])) {
        ds_args[.x] <- NULL
      } else {
        ds_args[[.x]] <- reduce_list_by_name(
          c(
            extract[[.x]][new_ds],
            recycle_extract_subfield(ds_args[[.x]], new_ds)
          ),
          setdiff_null
        )
      }
    ),
    names(ds_args)
  )

  ds_tst_args <- purrr::set_names(
    purrr::map(
      names(ds_tst_args),
      ~ if (is_null(extract[[.x]])) {
        ds_tst_args[.x] <- NULL
      } else {
        ds_tst_args[[.x]] <- reduce_list_by_name(
          c(
            extract[[.x]][c(new_ds, new_tst)],
            recycle_extract_subfield(ds_tst_args[[.x]], c(new_ds, new_tst))
          ),
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

  has_no_data_sources <- any(is.na(x$datasets)) ||
    any(is.na(x$time_series_tables)) ||
    any(is.na(x$shapefiles))

  if (has_no_data_sources) {
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
  extract_field_spec <- list(
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
      choices = c(
        "time_by_row_layout", "time_by_column_layout",
        "time_by_file_layout"
      ),
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
      choices = c(
        "unsubmitted", "queued", "started", "produced",
        "canceled", "failed", "completed"
      ),
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
                                   match_length = NULL,
                                   must_be_present_msg = NULL,
                                   must_be_missing_msg = NULL,
                                   parent_field = NULL) {
  if (!is_null(parent_field)) {
    # Child field is only allowed when its parent field is provided.
    allowed <- any(purrr::map_lgl(
      parent_field,
      ~ !is_null(extract[[.x]])
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
        ~ length(extract[[.x]])
      )
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

  empty_field <- is_null(values) || length(values) == 0 ||
    is_null(match_length) || match_length == 0

  if (empty_field) {
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
    ~ extract[[.x]]
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

      no_datasets <- is.null(def$datasets)
      no_tsts <- is.null(def$timeSeriesTables)

      if (no_datasets) {
        data_tables <- NULL
        years <- NULL
        breakdown_values <- NULL
      } else {
        data_tables <- purrr::map(def$datasets, ~ unlist(.x$dataTables))
        years <- purrr::map(def$datasets, ~ unlist(.x$years))
        breakdown_values <- purrr::map(
          def$datasets,
          ~ unlist(.x$breakdownValues)
        )
      }

      if (no_datasets && no_tsts) {
        geog_levels <- NULL
      } else {
        geog_levels <- purrr::map(
          c(def$datasets, def$timeSeriesTables),
          ~ unlist(.x$geogLevels)
        )
      }

      if ("number" %in% names(x)) {
        submitted <- TRUE
        number <- x$number
      } else {
        submitted <- FALSE
        number <- NA_integer_
      }

      out <- new_ipums_extract(
        collection = "nhgis",
        description = def$description,
        datasets = names(def$datasets),
        data_tables = data_tables,
        time_series_tables = names(def$timeSeriesTables),
        geog_levels = geog_levels,
        years = years,
        breakdown_values = breakdown_values,
        geographic_extents = geog_extent_lookup(
          unlist(def$geographicExtents),
          state_geog_lookup$abbs
        ),
        shapefiles = unlist(def$shapefiles),
        breakdown_and_data_type_layout = def$breakdownAndDataTypeLayout,
        tst_layout = def$timeSeriesTableLayout,
        data_format = def$dataFormat,
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
        rectangular_on = ifelse(
          names(def$dataStructure) == "rectangular",
          def$dataStructure$rectangular$on,
          NA_character_
        ),
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
        rectangular_on = ifelse(
          names(def$dataStructure) == "rectangular",
          def$dataStructure$rectangular$on,
          NA_character_
        ),
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
reduce_list_by_name <- function(l, f = ~ union(.x, .y), name_order = NULL) {
  if (is_empty(l)) {
    return(NULL)
  }

  if (!any(have_name(l))) {
    return(l)
  }

  labs <- unique(names(l))

  l <- purrr::map(
    labs,
    ~ purrr::reduce(l[.x == names(l)], f)
  )

  l <- purrr::set_names(l, labs)

  if (!is_null(name_order)) {
    missing_idx <- which(!names(l) %in% name_order)
    l <- c(l[name_order], l[missing_idx])
  }

  l
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
#' @return A list of recycled values. See Details section.
#'
#' @noRd
recycle_extract_subfield <- function(l, names) {
  if (is_null(names)) {
    return(l)
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
      l <- purrr::map(labs, ~ unlist(l[.x == names(l)], use.names = FALSE))
      names(l) <- labs
    }
  } else {
    # If list is unnamed, attach names in index order
    names(l) <- names[seq_len(length(l))]
  }

  # Add NULL entries for any names that are not present in l
  null_list <- purrr::set_names(
    rep(list(NULL), length(names)),
    names
  )

  l_sub <- purrr::set_names(
    purrr::map(
      names,
      ~ union(null_list[[.x]], l[[.x]])
    ),
    names
  )

  l <- c(l_sub, l[!names(l) %in% names])

  l
}

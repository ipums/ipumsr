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
#' `ipums_extract` objects are further divided into microdata
#' and aggregate data classes, and will also include
#' a collection-specific extract subclass to accommodate differences in
#' extract options and content across collections.
#'
#' Currently supported collections are:
#'
#' - IPUMS microdata
#'     + IPUMS USA
#'     + IPUMS CPS
#'     + IPUMS International
#'     + IPUMS Time Use (ATUS, AHTUS, MTUS)
#'     + IPUMS Health Surveys (NHIS, MEPS)
#' - IPUMS aggregate data
#'     + IPUMS NHGIS
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @section Properties:
#' Objects of class `ipums_extract` have:
#' * A `class` attribute of the form
#'   `c("{collection}_extract", "{collection_type}_extract", "ipums_extract")`.
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
#'   uniquely identifies an extract request for a given user.
#' * `status`: status of the extract request at the time it was last checked.
#'   One of `"unsubmitted"`, `"queued"`, `"started"`, `"produced"`,
#'   `"canceled"`, `"failed"`, or `"completed"`.
#'
#' @section Creating or obtaining an extract:
#' * Create an `ipums_extract` object from scratch with the appropriate
#'   `define_extract_*()` function.
#'     + For microdata extracts, use [define_extract_micro()]
#'     + For NHGIS extracts, use [define_extract_nhgis()]
#' * Use [get_extract_info()] to get the definition and latest status of a
#'   previously-submitted extract request.
#' * Use [get_extract_history()] to get the definitions and latest status of
#'   multiple previously-submitted extract requests.
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
#' @section Saving an extract:
#' * Save an extract to a JSON-formatted file with [save_extract_as_json()].
#' * Create an `ipums_extract` object from a saved JSON-formatted definition
#'   with [define_extract_from_json()].
#'
#' @name ipums_extract-class
#' @aliases ipums_extract
NULL

#' Define an extract request for an IPUMS microdata collection
#'
#' @description
#' Define the parameters of an IPUMS microdata extract request to be submitted
#' via the IPUMS API.
#'
#' The IPUMS API currently supports the following microdata collections:
#'    + IPUMS USA
#'    + IPUMS CPS
#'    + IPUMS International
#'    + IPUMS Time Use (ATUS, AHTUS, MTUS)
#'    + IPUMS Health Surveys (NHIS, MEPS)
#'
#' Note that not all extract request parameters and options apply to all
#' collections. For a summary of supported features by collection, see the
#' [IPUMS API documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/microdata/).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")` and
#' microdata extract definitions in `vignette("ipums-api-micro")`.
#'
#' @param collection Code for the IPUMS collection represented by this
#'   extract request. See [ipums_data_collections()] for supported microdata
#'   collection codes.
#' @param description Description of the extract.
#' @param samples Vector of samples to include in the extract
#'   request. Use [get_sample_info()] to identify
#'   sample IDs for a given collection.
#' @param variables Vector of variable names or a list of detailed
#'   variable specifications to include in the extract
#'   request. Use [var_spec()] to create a `var_spec` object containing a
#'   detailed variable specification. See examples.
#' @param time_use_variables Vector of names of IPUMS-defined time use variables
#'   or a list of specifications for user-defined time use variables
#'   to include in the extract request. Use [tu_var_spec()] to create a
#'   `tu_var_spec` object containing a time use variable specification. See
#'   examples.
#'
#'   Time use variables are only available for IPUMS Time Use collections
#'   (`"atus"`, `"ahtus"`, and `"mtus"`).
#' @param sample_members Indication of whether to include additional sample
#'   members in the extract request. If provided, must be one of
#'   `"include_non_respondents"`, `"include_household_members"`, or both.
#'
#'   Sample member selection is only available for the IPUMS ATUS collection
#'   (`"atus"`).
#' @param data_format Format for the output extract data file. Either
#'   `"fixed_width"` or `"csv"`.
#'
#'   Note that while `"stata"`, `"spss"`, and `"sas9"` are also accepted, these
#'   file formats are not supported by ipumsr data-reading functions.
#'
#'   Defaults to `"fixed_width"`.
#' @param data_structure Data structure for the output extract data.
#'   - `"rectangular"` provides data in which every row has the same record type
#'   (determined by `"rectangular_on"`), with variables from other record types
#'   written onto associated records of the chosen type (e.g. household
#'   variables written onto person records).
#'   - `"hierarchical"` provides data that include rows of differing record
#'   types, with records ordered according to their hierarchical structure (e.g.
#'   each person record is followed by the activity records for that person).
#'   - `"household_only"` provides household records only. This data structure
#'   is only available for the IPUMS USA collection (`"usa"`).
#'
#'   Defaults to `"rectangular"`.
#' @param rectangular_on If `data_structure` is `"rectangular"`,
#'   records on which to rectangularize. One of `"P"` (person), `"A"`
#'   (activity), `"I"` (injury) or `"R"` (round).
#'
#'   Defaults to `"P"` if `data_structure` is `"rectangular"` and `NULL`
#'   otherwise.
#' @param case_select_who Indication of how to interpret any case selections
#'   included for variables in the extract definition.
#'
#'   - `"individuals"` includes records for all individuals who match the
#'   specified case selections.
#'   - `"households"` includes records for all members of each household that
#'    contains an individual who matches the specified case selections.
#'
#'   Defaults to `"individuals"`. Use [var_spec()] to add case selections for
#'   specific variables.
#' @param data_quality_flags Set to `TRUE` to include data quality
#'   flags for all applicable variables in the extract definition. This will
#'   override the `data_quality_flags` specification for individual variables
#'   in the definition.
#'
#'   Use [var_spec()] to add data quality flags for specific variables.
#'
#' @return An object of class [`micro_extract`][ipums_extract-class] containing
#'   the extract definition.
#'
#' @export
#'
#' @aliases define_extract_usa define_extract_cps define_extract_ipumsi
#'
#' @seealso
#' [submit_extract()] to submit an extract request for processing.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
#'
#' @examples
#' usa_extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' usa_extract
#'
#' # Use `var_spec()` to created detailed variable specifications:
#' usa_extract <- define_extract_micro(
#'   collection = "usa",
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
#' cps_extract <- define_extract_micro(
#'   collection = "cps",
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
#' my_vars <- purrr::map(
#'   var_names,
#'   ~ var_spec(.x, attached_characteristics = "mother")
#' )
#'
#' ipumsi_extract <- define_extract_micro(
#'   collection = "ipumsi",
#'   description = "Extract definition with predefined variables",
#'   samples = c("br2010a", "cl2017a"),
#'   variables = my_vars
#' )
#'
#' # Extract specifications can be indexed by name
#' names(ipumsi_extract$samples)
#'
#' names(ipumsi_extract$variables)
#'
#' ipumsi_extract$variables$AGE
#'
#' # IPUMS Time Use collections allow selection of IPUMS-defined and
#' # user-defined time use variables:
#' define_extract_micro(
#'   collection = "atus",
#'   description = "ATUS extract with time use variables",
#'   samples = "at2007",
#'   time_use_variables = list(
#'     "ACT_PCARE",
#'     tu_var_spec(
#'       "MYTIMEUSEVAR",
#'       owner = "example@example.com"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' # Use the extract definition to submit an extract request to the API
#' submit_extract(usa_extract)
#' }
define_extract_micro <- function(collection,
                                 description,
                                 samples,
                                 variables = NULL,
                                 time_use_variables = NULL,
                                 sample_members = NULL,
                                 data_format = "fixed_width",
                                 data_structure = "rectangular",
                                 rectangular_on = NULL,
                                 case_select_who = "individuals",
                                 data_quality_flags = NULL) {
  if (collection == "nhgis") {
    rlang::abort(c(
      "\"nhgis\" is not an IPUMS microdata collection",
      "i" = "Use `define_extract_nhgis()` to define an NHGIS extract request."
    ))
  }

  check_api_support(collection)

  if (data_structure == "rectangular") {
    rectangular_on <- rectangular_on %||% "P"
  }

  samples <- spec_cast(samples, "samp_spec")
  variables <- spec_cast(variables, "var_spec")
  time_use_variables <- spec_cast(time_use_variables, "tu_var_spec")

  extract <- new_ipums_extract(
    collection = collection,
    description = description,
    samples = set_nested_names(samples),
    variables = set_nested_names(variables),
    time_use_variables = set_nested_names(time_use_variables),
    sample_members = sample_members,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    case_select_who = case_select_who,
    data_quality_flags = data_quality_flags
  )

  extract <- validate_ipums_extract(extract)

  extract
}


#' @export
#' @keywords internal
define_extract_usa <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL,
                               case_select_who = "individuals",
                               data_quality_flags = NULL) {
  lifecycle::deprecate_warn(
    "0.8.0",
    "define_extract_usa()",
    "define_extract_micro()"
  )

  define_extract_micro(
    collection = "usa",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    case_select_who = case_select_who,
    data_quality_flags = data_quality_flags
  )
}

#' @export
#' @keywords internal
define_extract_cps <- function(description,
                               samples,
                               variables,
                               data_format = "fixed_width",
                               data_structure = "rectangular",
                               rectangular_on = NULL,
                               case_select_who = "individuals",
                               data_quality_flags = NULL) {
  lifecycle::deprecate_warn(
    "0.8.0",
    "define_extract_cps()",
    "define_extract_micro()"
  )

  define_extract_micro(
    collection = "cps",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    case_select_who = case_select_who,
    data_quality_flags = data_quality_flags
  )
}

#' @export
#' @keywords internal
define_extract_ipumsi <- function(description,
                                  samples,
                                  variables,
                                  data_format = "fixed_width",
                                  data_structure = "rectangular",
                                  rectangular_on = NULL,
                                  case_select_who = "individuals",
                                  data_quality_flags = NULL) {
  lifecycle::deprecate_warn(
    "0.8.0",
    "define_extract_ipumsi()",
    "define_extract_micro()"
  )

  define_extract_micro(
    collection = "ipumsi",
    description = description,
    samples = samples,
    variables = variables,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    case_select_who = case_select_who,
    data_quality_flags = data_quality_flags
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
#' Learn more about the IPUMS API in `vignette("ipums-api")` and
#' NHGIS extract definitions in `vignette("ipums-api-nhgis")`.
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
#' See examples or `vignette("ipums-api-nhgis")` for more details about
#' specifying datasets and time series tables in an NHGIS extract definition.
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
#'   Note that by default, [read_nhgis()] removes the additional header row in
#'   `"csv_header"` files.
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
#' [submit_extract()] to submit an extract request for processing.
#'
#' [save_extract_as_json()] and [define_extract_from_json()] to share an
#'   extract definition.
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
#' # Geographic extents are applied to all datasets in the definition
#' define_extract_nhgis(
#'   description = "Extent selection",
#'   datasets = list(
#'     ds_spec("2018_2022_ACS5a", "B01001", "blck_grp"),
#'     ds_spec("2017_2021_ACS5a", "B01001", "blck_grp")
#'   ),
#'   geographic_extents = c("010", "050")
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

  if (inherits(datasets, "ipums_spec")) {
    datasets <- list(datasets)
  }

  if (inherits(time_series_tables, "ipums_spec")) {
    time_series_tables <- list(time_series_tables)
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = set_nested_names(datasets),
    time_series_tables = set_nested_names(time_series_tables),
    geographic_extents = unlist(geographic_extents),
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
#' Provide specifications for individual variables and time use variables when
#' defining an IPUMS microdata extract request.
#'
#' Currently, no additional specifications are available for IPUMS samples.
#'
#' Note that not all variable-level options are available across all IPUMS
#' data collections. For a summary of supported features by collection, see the
#' [IPUMS API documentation](https://developer.ipums.org/docs/v2/apiprogram/apis/microdata/).
#'
#' Learn more about microdata extract definitions in
#' `vignette("ipums-api-micro")`.
#'
#' @param name Name of the sample, variable, or time use variable.
#' @param case_selections A character vector of values of the given variable
#'   that should be used to select cases. Values should be specified exactly as
#'   they appear in the "CODES" tab for the given variable in the web-based
#'   extract builder, including zero-padding (e.g. see the "CODES" tab for IPUMS
#'   CPS variable
#'   [EDUC](https://cps.ipums.org/cps-action/variables/EDUC#codes_section)).
#' @param case_selection_type One of `"general"` or `"detailed"` indicating
#'   whether the values in `case_selections` should be matched against the
#'   general or detailed codes for the given variable. Only some variables have
#'   detailed codes. See IPUMS USA variable
#'   [RACE](https://usa.ipums.org/usa-action/variables/RACE#codes_section) for
#'   an example of a variable with general and detailed codes.
#'
#'   Defaults to `"general"` if any `case_selections` are specified.
#' @param attached_characteristics Whose characteristics should be attached, if
#'   any? Accepted values are `"mother"`, `"father"`, `"spouse"`, `"head"`,
#'   or a combination. Specifying attached characteristics will add variables to
#'   your extract that contain the values for the given variable for the
#'   specified household members. For example, variable "AGE_MOM" will be added
#'   if `"mother"` is specified for the variable `"AGE"`.
#'
#'   For data collections with information on same-sex couples, specifying
#'   `"mother"` or `"father"` will attach the characteristics of both mothers or
#'   both fathers for children with same-sex parents, by adding variables with
#'   names of the form "AGE_MOM" and "AGE_MOM2".
#' @param data_quality_flags Logical indicating whether to include data quality
#'   flags for the given variable. By default, data quality flags are not
#'   included.
#' @param preselected Logical indicating whether the variable is preselected.
#'   This is not needed for external use.
#'
#' @return A `var_spec`, `tu_var_spec`, or `samp_spec` object.
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
#' extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "Example extract",
#'   samples = "us2017b",
#'   variables = list(var1, var2)
#' )
#'
#' extract$variables$SCHOOL
#'
#' extract$variables$RACE
#'
#' # For IPUMS Time Use collections, use `tu_var_spec()` to include user-defined
#' # time use variables
#' my_time_use_variable <- tu_var_spec(
#'   "MYTIMEUSEVAR",
#'   owner = "example@example.com"
#' )
#'
#' # IPUMS-defined time use variables can be included either as `tu_var_spec`
#' # objects or with just the variable name:
#' define_extract_micro(
#'   collection = "atus",
#'   description = "Requesting user- and IPUMS-defined time use variables",
#'   samples = "at2007",
#'   time_use_variables = list(
#'     my_time_use_variable,
#'     tu_var_spec("ACT_PCARE"),
#'     "ACT_SOCIAL"
#'   )
#' )
#'
var_spec <- function(name,
                     case_selections = NULL,
                     case_selection_type = NULL,
                     attached_characteristics = NULL,
                     data_quality_flags = NULL,
                     preselected = NULL) {
  if (!is_null(case_selections) && !is_empty(case_selections)) {
    case_selection_type <- case_selection_type %||% "general"
  }

  new_ipums_spec(
    name,
    case_selections = case_selections,
    attached_characteristics = attached_characteristics,
    data_quality_flags = data_quality_flags,
    case_selection_type = case_selection_type,
    preselected = preselected,
    class = "var_spec"
  )
}

#' @export
#' @rdname var_spec
#'
#' @param owner For user-defined time use variables, the email of the user
#'   account associated with the time use variable. Currently, only the email
#'   of the user submitting the extract request is supported.
tu_var_spec <- function(name, owner = NULL) {
  new_ipums_spec(name, owner = owner, class = "tu_var_spec")
}

#' @export
#' @rdname var_spec
samp_spec <- function(name) {
  new_ipums_spec(name, class = "samp_spec")
}


#' Create dataset and time series table specifications for IPUMS NHGIS extract
#' requests
#'
#' @description
#' Provide specifications for individual datasets and time series
#' tables when defining an IPUMS NHGIS extract request.
#'
#' Use [get_metadata_nhgis()] to identify available values for dataset and
#' time series table specification parameters.
#'
#' Learn more about NHGIS extract definitions in
#' `vignette("ipums-api-nhgis")`.
#'
#' @details
#' In general, `data_tables` and `geog_levels` are required for all
#' dataset specifications, and `geog_levels` are required for all
#' time series table specifications.
#'
#' However, it is possible to make a temporary specification for an incomplete
#' dataset or time series table by omitting these values. This supports the
#' syntax used when modifying an existing extract (see
#' [`add_to_extract()`][add_to_extract.nhgis_extract()] or
#' [`remove_from_extract()`][remove_from_extract.nhgis_extract()]).
#'
#' @param name Name of the dataset or time series table.
#' @param data_tables Vector of summary tables to retrieve for the given
#'   dataset.
#' @param geog_levels Geographic levels (e.g. `"county"` or `"state"`)
#'   at which to obtain data for the given dataset or time series table.
#' @param years Years for which to obtain the data for the given dataset or time
#'   series table.
#'
#'   For time series tables, all years are selected by default. For datasets,
#'   use `"*"` to select all available years. Use
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
                    data_tables = NULL,
                    geog_levels = NULL,
                    years = NULL,
                    breakdown_values = NULL) {
  new_ipums_spec(
    name,
    data_tables = data_tables,
    geog_levels = geog_levels,
    years = years,
    breakdown_values = breakdown_values,
    class = "ds_spec"
  )
}

#' @export
#' @rdname ds_spec
tst_spec <- function(name,
                     geog_levels = NULL,
                     years = NULL) {
  new_ipums_spec(
    name,
    geog_levels = geog_levels,
    years = years,
    class = "tst_spec"
  )
}

#' Store an extract definition in JSON format
#'
#' @description
#' Write an [`ipums_extract`][ipums_extract-class] object to a JSON file, or
#' read an extract definition from such a file.
#'
#' Use these functions to store a copy of an extract definition outside of your
#' R environment and/or share an extract definition with another registered
#' IPUMS user.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @section API Version Compatibility:
#' As of v0.6.0, ipumsr only supports IPUMS API version 2. If you have stored
#' an extract definition made using version beta or version 1 of the IPUMS
#' API, you will not be able to load it using `define_extract_from_json()`. The
#' API version for the request should be stored in the saved JSON file. (If
#' there is no `"api_version"` or `"version"` field in the JSON file, the
#' request was likely made under version beta or version 1.)
#'
#' If the extract definition was originally made under your user account and
#' you know its corresponding extract number, use [get_extract_info()] to obtain
#' a definition compliant with IPUMS API version 2. You can then save this
#' definition to JSON with `save_extract_as_json()`.
#'
#' Otherwise, you will need to update the JSON file to be compliant with
#' IPUMS API version 2. In general, this should only require renaming
#' all JSON fields written in `snake_case` to `camelCase`. For instance,
#' `"data_tables"` would become `"dataTables"`, `"data_format"` would become
#' `"dataFormat"`, and so on. You will also need to change the `"api_version"`
#' field to `"version"` and set it equal to `2`. If you are unable to create
#' a valid extract by modifying the file, you may have to recreate the
#' definition manually using the [define_extract_micro()] or
#' [define_extract_nhgis()].
#'
#' See the IPUMS developer documentation for more details on
#' [API versioning](https://developer.ipums.org/docs/apiprogram/versioning/) and
#' [breaking changes](https://developer.ipums.org/docs/apiprogram/changelog/)
#' introduced in version 2.
#'
#' @inheritParams add_to_extract
#' @param file File path to which to write the JSON-formatted extract
#'   definition.
#' @param overwrite If `TRUE`, overwrite `file` if it already exists.
#'   Defaults to `FALSE`.
#'
#' @return An [`ipums_extract`][ipums_extract-class] object.
#'
#' @seealso
#' [define_extract_micro()] or [define_extract_nhgis()] to define an
#' extract request manually
#'
#' [get_extract_info()] to obtain a past extract to save.
#'
#' [submit_extract()] to submit an extract request for processing.
#'
#' [add_to_extract()] and [remove_from_extract()] to
#'   revise an extract definition.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_micro(
#'   collection = "usa",
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

#' @param extract_json Path to a file containing a JSON-formatted
#'   extract definition.
#'
#' @export
#' @rdname save_extract_as_json
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

#' Add values to an existing IPUMS extract definition
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Add or replace values in an existing `ipums_extract` object.
#' This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To add to an **IPUMS Microdata** extract definition, click
#'   [here][add_to_extract.micro_extract]. This includes:
#'     + IPUMS USA
#'     + IPUMS CPS
#'     + IPUMS International
#'     + IPUMS Time Use (ATUS, AHTUS, MTUS)
#'     + IPUMS Health Surveys (NHIS, MEPS)
#' - To add to an **IPUMS NHGIS** extract definition, click
#'   [here][add_to_extract.nhgis_extract]
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_micro()] or [define_extract_nhgis()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
#'
#' To remove existing values from an extract definition, use
#' [remove_from_extract()].
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param ... Additional arguments specifying the extract fields and values to
#'   add to the extract definition.
#'
#'   All arguments available in [define_extract_micro()] (for microdata
#'   extract requests) or [define_extract_nhgis()] (for NHGIS extract requests)
#'   can be passed to `add_to_extract()`.
#'
#' @return An object of the same class as `extract` containing the modified
#'   extract definition
#'
#' @keywords internal
#'
#' @seealso
#' [remove_from_extract()] to remove values from an extract definition.
#'
#' [define_extract_micro()] or [define_extract_nhgis()] to define an
#' extract request manually.
#'
#' [submit_extract()] to submit an extract request for processing.
#'
#' @export
#'
#' @examples
#' # Microdata extracts
#' usa_extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "2013 ACS Data",
#'   samples = "us2013a",
#'   variables = c("SEX", "AGE", "YEAR")
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
#' # Modify/add multiple variables
#' add_to_extract(
#'   usa_extract,
#'   variables = list(
#'     var_spec("SEX", case_selections = "1"),
#'     var_spec("RELATE")
#'   )
#' )
#'
#' # NHGIS extracts
#' nhgis_extract <- define_extract_nhgis(
#'   datasets = ds_spec(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2"),
#'     geog_levels = "county"
#'   )
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
#' # Modify/add multiple datasets or time series tables
#' add_to_extract(
#'   nhgis_extract,
#'   time_series_tables = list(
#'     tst_spec("CW3", geog_levels = "state"),
#'     tst_spec("CW4", geog_levels = "state")
#'   )
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
#' `r lifecycle::badge("experimental")`
#'
#' Add new values to an IPUMS NHGIS extract definition.
#' All fields are optional, and if omitted, will be unchanged.
#' Supplying a value for fields that take a single value, such as
#' `description` and `data_format`, will replace the existing value with
#' the supplied value.
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_nhgis()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
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
#' [define_extract_nhgis()] to create a new extract definition.
#'
#' [submit_extract()] to submit an extract request.
#'
#' [download_extract()] to download extract data files.
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
#' # its specification will be modified by adding the new specifications to
#' # the existing ones
#' add_to_extract(
#'   extract,
#'   datasets = ds_spec("1990_STF1", "NP4", "nation")
#' )
#'
#' # You can add new datasets and modify existing ones simultaneously by
#' # providing a list of `ds_spec` objects
#' add_to_extract(
#'   extract,
#'   datasets = list(
#'     ds_spec("1990_STF1", "NP4", "nation"),
#'     ds_spec("1990_STF2a", "NPA1", "county")
#'   )
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

  datasets <- spec_cast(datasets, "ds_spec")
  time_series_tables <- spec_cast(time_series_tables, "tst_spec")

  ds_names <- purrr::map_chr(datasets, ~ .x$name)
  tst_names <- purrr::map_chr(time_series_tables, ~ .x$name)

  if (anyDuplicated(ds_names) != 0) {
    ipums_extract_error(
      "Invalid `nhgis_extract` object:",
      "Extract definition cannot contain multiple `datasets` of same name."
    )
  }

  if (anyDuplicated(tst_names) != 0) {
    ipums_extract_error(
      "Invalid `nhgis_extract` object:",
      paste0(
        "Extract definition cannot contain multiple `time_series_tables` of ",
        "same name."
      )
    )
  }

  datasets <- spec_add(extract$datasets, datasets)
  time_series_tables <- spec_add(extract$time_series_tables, time_series_tables)

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
    geographic_extents = union(
      extract$geographic_extents,
      unlist(geographic_extents)
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
#' `r lifecycle::badge("experimental")`
#'
#' Add new values or replace existing values in an IPUMS microdata extract
#' definition. All fields are optional, and if omitted, will be unchanged.
#' Supplying a value
#' for fields that take a single value, such as `description` and `data_format`,
#' will replace the existing value with the supplied value.
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_micro()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
#'
#' To remove existing values from an IPUMS microdata extract definition, use
#' [`remove_from_extract()`][remove_from_extract.micro_extract].
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
#' @param time_use_variables Vector of names of IPUMS-defined time use variables
#'   or a list of specifications for user-defined time use variables
#'   to include in the extract request. Use [tu_var_spec()] to create a
#'   `tu_var_spec` object containing a time use variable specification.
#' @param data_format Format for the output extract data file. Either
#'   `"fixed_width"` or `"csv"`.
#'
#'   Note that while `"stata"`, `"spss"`, or `"sas9"` are also accepted, these
#'   file formats are not supported by ipumsr data-reading functions.
#' @param data_structure Data structure for the output extract data.
#'   - `"rectangular"` provides data in which every row has the same record type
#'   (determined by `"rectangular_on"`), with variables from other record types
#'   written onto associated records of the chosen type (e.g. household
#'   variables written onto person records).
#'   - `"hierarchical"` provides data that include rows of differing record
#'   types, with records ordered according to their hierarchical structure (e.g.
#'   each person record is followed by the activity records for that person).
#'   - `"household_only"` provides household records only. This data structure
#'   is only available for the IPUMS USA collection (`"usa"`).
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
#' [submit_extract()] to submit an extract request.
#'
#' [download_extract()] to download extract data files.
#'
#' [define_extract_micro()] to create a new extract
#'   definition from scratch
#'
#' @export
#'
#' @name add_to_extract_micro
#'
#' @examples
#' extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "2013 ACS Data",
#'   samples = "us2013a",
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' # Add a single sample
#' add_to_extract(extract, samples = "us2014a")
#'
#' # Add samples and variables
#' extract2 <- add_to_extract(
#'   extract,
#'   samples = "us2014a",
#'   variables = c("MARST", "BIRTHYR")
#' )
#'
#' # Modify specifications for variables in the extract by using `var_spec()`
#' # with the existing variable name:
#' add_to_extract(
#'   extract,
#'   samples = "us2014a",
#'   variables = var_spec("SEX", case_selections = "2")
#' )
#'
#' # You can make multiple modifications or additions by providing a list
#' # of `var_spec()` objects:
#' add_to_extract(
#'   extract,
#'   samples = "us2014a",
#'   variables = list(
#'     var_spec("RACE", attached_characteristics = "mother"),
#'     var_spec("SEX", case_selections = "2"),
#'     var_spec("RELATE")
#'   )
#' )
#'
#' # Values that only take a single value are replaced
#' add_to_extract(extract, description = "New description")$description
add_to_extract.micro_extract <- function(extract,
                                         description = NULL,
                                         samples = NULL,
                                         variables = NULL,
                                         time_use_variables = NULL,
                                         sample_members = NULL,
                                         data_format = NULL,
                                         data_structure = NULL,
                                         rectangular_on = NULL,
                                         case_select_who = NULL,
                                         data_quality_flags = NULL,
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
  }

  samples <- spec_cast(samples, "samp_spec")
  variables <- spec_cast(variables, "var_spec")
  time_use_variables <- spec_cast(time_use_variables, "tu_var_spec")

  error_header <- paste0("Invalid `", class(extract)[1], "` object:")

  if (!all(purrr::map_lgl(variables, ~ inherits(.x, "var_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `variables` to be a `var_spec` object ",
        "or a list of `var_spec` objects."
      )
    )
  }

  if (!all(purrr::map_lgl(samples, ~ inherits(.x, "samp_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `samples` to be a `samp_spec` object ",
        "or a list of `samp_spec` objects."
      )
    )
  }

  if (!all(purrr::map_lgl(time_use_variables, ~ inherits(.x, "tu_var_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `time_use_variables` to be a `tu_var_spec` object ",
        "or a list of `tu_var_spec` objects."
      )
    )
  }

  samples <- spec_add(extract$samples, samples)
  variables <- spec_add(extract$variables, variables)
  time_use_variables <- spec_add(extract$time_use_variables, time_use_variables)

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = description %||% extract$description,
    samples = set_nested_names(samples),
    variables = set_nested_names(variables),
    time_use_variables = set_nested_names(time_use_variables),
    sample_members = union(extract$sample_members, sample_members),
    data_format = data_format %||% extract$data_format,
    data_structure = data_structure %||% extract$data_structure,
    rectangular_on = rectangular_on,
    case_select_who = case_select_who %||% extract$case_select_who,
    data_quality_flags = data_quality_flags %||% extract$data_quality_flags
  )

  extract <- validate_ipums_extract(extract)

  extract
}

#' Remove values from an existing IPUMS extract definition
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Remove values for specific fields in an existing `ipums_extract`
#' object. This function is an S3 generic whose behavior will depend on the
#' subclass (i.e. collection) of the extract being modified.
#'
#' - To remove from an **IPUMS Microdata** extract definition, click
#'   [here][remove_from_extract.micro_extract]. This includes:
#'     + IPUMS USA
#'     + IPUMS CPS
#'     + IPUMS International
#'     + IPUMS Time Use (ATUS, AHTUS, MTUS)
#'     + IPUMS Health Surveys (NHIS, MEPS)
#' - To remove from an **IPUMS NHGIS** extract definition, click
#'   [here][remove_from_extract.nhgis_extract]
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_micro()] or [define_extract_nhgis()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
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
#' [add_to_extract()] to add values to an extract definition.
#'
#' [define_extract_micro()] or [define_extract_nhgis()] to define an
#' extract request manually
#'
#' [submit_extract()] to submit an extract request for processing.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' # Microdata extracts
#' usa_extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = list(
#'     var_spec("AGE"),
#'     var_spec("SEX", case_selections = "2"),
#'     var_spec("YEAR")
#'   )
#' )
#'
#' # Remove variables from an extract definition
#' remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a",
#'   variables = c("AGE", "SEX")
#' )
#'
#' # Remove detailed specifications for an existing variable
#' remove_from_extract(
#'   usa_extract,
#'   variables = var_spec("SEX", case_selections = "2")
#' )
#'
#' # NHGIS extracts
#' nhgis_extract <- define_extract_nhgis(
#'   datasets = ds_spec(
#'     "1990_STF1",
#'     data_tables = c("NP1", "NP2", "NP3"),
#'     geog_levels = "county"
#'   ),
#'   time_series_tables = tst_spec("A00", geog_levels = "county")
#' )
#'
#' # Remove an existing dataset or time series table
#' remove_from_extract(nhgis_extract, datasets = "1990_STF1")
#'
#' # Remove detailed specifications from an existing dataset or
#' # time series table
#' remove_from_extract(
#'   nhgis_extract,
#'   datasets = ds_spec("1990_STF1", data_tables = "NP1")
#' )
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' Remove values from an existing NHGIS extract definition
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Remove existing values from an IPUMS NHGIS extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_nhgis()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
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
#' @param datasets Dataset specifications to remove from the extract definition.
#'   All `data_tables`, `geog_levels`,
#'   `years`, and `breakdown_values` associated with the specified
#'   `datasets` will also be removed.
#' @param time_series_tables Names of the time series tables
#'   to remove from the extract definition. All `geog_levels` and `years`
#'   associated  with the specified `time_series_tables` will also be removed.
#' @param geographic_extents Geographic extents to remove from the extract
#'   definition.
#' @param shapefiles Shapefiles to remove from the extract definition.
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
#' [submit_extract()] to submit an extract request.
#'
#' [download_extract()] to download extract data files.
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
#' # Providing names of datasets or time series tables will remove them and
#' # all of their associated specifications from the extract:
#' remove_from_extract(
#'   extract,
#'   time_series_tables = c("CW3", "CW5")
#' )
#'
#' # To remove detailed specifications from a dataset or time series table,
#' # use `ds_spec()` or `tst_spec()`. The named dataset or time series table
#' # will be retained in the extract, but modified by removing the indicated
#' # specifications:
#' remove_from_extract(
#'   extract,
#'   datasets = ds_spec("1990_STF1", data_tables = c("NP2", "NP3"))
#' )
#'
#' # To make multiple modifications, use a list of `ds_spec()` or `tst_spec()`
#' # objects:
#' remove_from_extract(
#'   extract,
#'   time_series_tables = list(
#'     tst_spec("CW3", geog_levels = "county"),
#'     tst_spec("CW5", geog_levels = "state")
#'   )
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

  datasets <- spec_cast(datasets, "ds_spec")
  time_series_tables <- spec_cast(time_series_tables, "tst_spec")

  datasets <- spec_remove(extract$datasets, datasets)
  time_series_tables <- spec_remove(
    extract$time_series_tables,
    time_series_tables
  )

  no_ds <- is_null(datasets)
  no_tst <- is_null(time_series_tables)

  # If removal results in extract with no ds/tst, remove irrelevant values
  if (no_ds) {
    datasets <- NULL
    breakdown_and_data_type_layout <- NULL
    geographic_extents <- NULL
  } else {
    breakdown_and_data_type_layout <- extract$breakdown_and_data_type_layout
    geographic_extents <- setdiff_null(
      extract$geographic_extents,
      unlist(geographic_extents)
    )
  }

  if (no_tst) {
    time_series_tables <- NULL
    tst_layout <- NULL
  } else {
    tst_layout <- extract$tst_layout
  }

  if (no_ds && no_tst) {
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
#' `r lifecycle::badge("experimental")`
#'
#' Remove existing values from an IPUMS microdata extract definition. All
#' fields are optional, and if omitted, will be unchanged.
#'
#' This function is marked as experimental because it is typically not the best
#' option for maintaining reproducible extract definitions and may be retired
#' in the future. For reproducibility, users should strive to build extract
#' definitions with [define_extract_micro()].
#'
#' If you have a complicated extract definition to revise, but do not have
#' the original extract definition code that created it, we suggest that you
#' save the revised extract as a JSON file with [save_extract_as_json()]. This
#' will create a stable version of the extract definition that
#' can be used in the future as needed.
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
#' @inheritParams define_extract_micro
#' @inheritParams remove_from_extract
#' @param samples Character vector of sample names to remove from the extract
#'   definition.
#' @param variables Names of the variables to remove from the extract
#'   definition. All variable-specific fields for the indicated variables
#'   will also be removed. For removing values from variable-specific fields
#'   while retaining the variable, see examples.
#' @param time_use_variables Names of the time use variables to remove from the
#'   extract definition. All time use variable-specific fields for the indicated
#'   time use variables will also be removed. For removing time use
#'   variable-specific fields while retaining the time use variable, see
#'   examples.
#' @param sample_members Sample members to remove from the extract definition.
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
#' [submit_extract()] to submit an extract request.
#'
#' [download_extract()] to download extract data files.
#'
#' [define_extract_micro()] to create a new extract
#'   definition from scratch.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_micro(
#'   collection = "usa",
#'   description = "USA example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = list(
#'     var_spec("AGE", data_quality_flags = TRUE),
#'     var_spec("SEX", case_selections = "1"),
#'     "RACE"
#'   )
#' )
#'
#' # Providing names of samples or variables will remove them and
#' # all of their associated specifications from the extract:
#' remove_from_extract(
#'   usa_extract,
#'   samples = "us2014a",
#'   variables = c("AGE", "RACE")
#' )
#'
#' # To remove detailed specifications from a variable or time use variable,
#' # indicate the specifications to remove within `var_spec()` or
#' # `tu_var_spec()`. The named variable will be retained in the extract, but
#' # modified by removing the indicated specifications.
#' remove_from_extract(
#'   usa_extract,
#'   variables = var_spec("SEX", case_selections = "1")
#' )
#'
#' # To make multiple modifications, use a list of `var_spec()` objects.
#' remove_from_extract(
#'   usa_extract,
#'   variables = list(
#'     var_spec("SEX", case_selections = "1"),
#'     var_spec("AGE")
#'   )
#' )
remove_from_extract.micro_extract <- function(extract,
                                              samples = NULL,
                                              variables = NULL,
                                              time_use_variables = NULL,
                                              sample_members = NULL,
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

  # Coerce all samples/variables to `spec` objects for consistency
  samples <- spec_cast(samples, "samp_spec")
  variables <- spec_cast(variables, "var_spec")
  time_use_variables <- spec_cast(time_use_variables, "tu_var_spec")

  samples <- spec_remove(extract$samples, samples)
  variables <- spec_remove(extract$variables, variables)
  time_use_variables <- spec_remove(extract$time_use_variables, time_use_variables)

  sample_members <- setdiff_null(extract$sample_members, sample_members)

  extract <- new_ipums_extract(
    collection = extract$collection,
    description = extract$description,
    samples = samples,
    variables = variables,
    time_use_variables = time_use_variables,
    sample_members = sample_members,
    data_format = extract$data_format,
    data_structure = extract$data_structure,
    rectangular_on = extract$rectangular_on,
    case_select_who = extract$case_select_who,
    data_quality_flags = extract$data_quality_flags
  )

  extract <- validate_ipums_extract(extract)

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

new_ipums_spec <- function(name, ..., class) {
  ipums_spec <- c(
    list(name = name),
    purrr::compact(rlang::list2(...))
  )

  structure(
    ipums_spec,
    class = c(class, "ipums_spec", class(ipums_spec))
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

  error_header <- "Invalid `nhgis_extract` object:"

  if (!any(includes_ds, includes_tst, includes_shp)) {
    ipums_extract_error(
      error_header,
      paste0(
        "Extract definition must contain at least one of `datasets`, ",
        "`time_series_tables`, or `shapefiles`."
      )
    )
  }

  if (!all(purrr::map_lgl(x$datasets, ~ inherits(.x, "ds_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `datasets` to be a `ds_spec` object ",
        "or a list of `ds_spec` objects."
      )
    )
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
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `time_series_tables` to be a `tst_spec` object ",
        "or a list of `tst_spec` objects."
      )
    )
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
    ipums_extract_error(
      error_header,
      "Extract definition cannot contain multiple `datasets` of same name."
    )
  }

  if (anyDuplicated(purrr::map(x$time_series_tables, ~ .x$name)) != 0) {
    ipums_extract_error(
      error_header,
      paste0(
        "Extract definition cannot contain multiple `time_series_tables` of ",
        "same name."
      )
    )
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
    ipums_extract_error(error_header, extract_issues)
  }

  invisible(x)
}

#' @export
validate_ipums_extract.micro_extract <- function(x, call = caller_env()) {
  NextMethod()

  # Use collection-specific class for clarity
  error_header <- paste0("Invalid `", class(x)[1], "` object:")

  if (is_empty(x$samples) || is_na(x$samples)) {
    ipums_extract_error(
      error_header,
      "Extract definition must contain values for `samples`"
    )
  }

  if (!all(purrr::map_lgl(x$samples, ~ inherits(.x, "samp_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `samples` to be a `samp_spec` object ",
        "or a list of `samp_spec` objects."
      )
    )
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
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `variables` to be a `var_spec` object ",
        "or a list of `var_spec` objects."
      )
    )
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

  if (!all(purrr::map_lgl(x$time_use_variables, ~ inherits(.x, "tu_var_spec")))) {
    ipums_extract_error(
      error_header,
      paste0(
        "Expected `time_use_variables` to be a `tu_var_spec` object ",
        "or a list of `tu_var_spec` objects."
      )
    )
  } else {
    withCallingHandlers(
      purrr::walk(
        x$time_use_variables,
        ~ validate_ipums_extract(.x, call = caller_env())
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )
  }

  if (anyDuplicated(purrr::map(x$variables, ~ .x$name)) != 0) {
    ipums_extract_error(
      error_header,
      "Extract definition cannot contain multiple `variables` of same name."
    )
  }

  if (anyDuplicated(purrr::map(x$samples, ~ .x$name)) != 0) {
    ipums_extract_error(
      error_header,
      "Extract definition cannot contain multiple `samples` of same name."
    )
  }

  if (anyDuplicated(purrr::map(x$time_use_variables, ~ .x$name)) != 0) {
    ipums_extract_error(
      error_header,
      "Extract definition cannot contain multiple `time_use_variables` of same name."
    )
  }

  extract_field_spec <- list(
    list(
      field = "sample_members",
      required = FALSE,
      choices = c("include_household_members", "include_non_respondents"),
      type = "character"
    ),
    list(
      field = "data_structure",
      required = TRUE,
      choices = c("rectangular", "hierarchical", "household_only"),
      length = 1,
      type = "character"
    ),
    list(
      field = "rectangular_on",
      required = isTRUE(x$data_structure == "rectangular"),
      allowed = isTRUE(x$data_structure == "rectangular"),
      choices = c("P", "A", "I", "R"),
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
    ),
    list(
      field = "case_select_who",
      choices = c("individuals", "households"),
      length = 1,
      type = "character"
    ),
    list(
      field = "data_quality_flags",
      length = 1,
      type = "logical"
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
    ipums_extract_error(error_header, extract_issues)
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
    ipums_extract_error("Invalid `ipums_extract` object:", extract_issues)
  }

  # Throw error if no API for collection
  check_api_support(x$collection)

  x
}

#' @export
validate_ipums_extract.samp_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% "name"]

  if (length(unexpected_names) > 0) {
    ipums_extract_error(
      "Invalid `samp_spec` specification:",
      paste0(
        "Unrecognized fields: `",
        paste0(unexpected_names, collapse = "`, `"), "`"
      )
    )
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
    ipums_extract_error("Invalid `samp_spec` specification:", extract_issues)
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
    ipums_extract_error(
      "Invalid `var_spec` specification:",
      paste0(
        "Unrecognized fields: `",
        paste0(unexpected_names, collapse = "`, `"), "`"
      )
    )
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
      must_be_present_msg = " when `case_selections` is provided",
      must_be_missing_msg = " when `case_selections` is not provided",
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
    ipums_extract_error("Invalid `var_spec` specification:", extract_issues)
  }

  invisible(x)
}

#' @export
validate_ipums_extract.tu_var_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% c("name", "owner")]

  if (length(unexpected_names) > 0) {
    ipums_extract_error(
      "Invalid `tu_var_spec` specification:",
      paste0(
        "Unrecognized fields: `",
        paste0(unexpected_names, collapse = "`, `"), "`"
      )
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
      field = "owner",
      required = FALSE,
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
    ipums_extract_error("Invalid `tu_var_spec` specification:", extract_issues)
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
    ipums_extract_error(
      "Invalid `ds_spec` specification:",
      paste0(
        "Unrecognized fields: `",
        paste0(unexpected_names, collapse = "`, `"), "`"
      )
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
    ipums_extract_error("Invalid `ds_spec` specification:", extract_issues)
  }

  invisible(x)
}

#' @export
validate_ipums_extract.tst_spec <- function(x, call = caller_env()) {
  unexpected_names <- names(x)[!names(x) %in% c("name", "geog_levels", "years")]

  if (length(unexpected_names) > 0) {
    ipums_extract_error(
      "Invalid `tst_spec` specification:",
      paste0(
        "Unrecognized fields: `",
        paste0(unexpected_names, collapse = "`, `"), "`"
      )
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
    ipums_extract_error("Invalid `tst_spec` specification:", extract_issues)
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

# Helper for easier error formatting for invalid ipums_extract objects
ipums_extract_error <- function(header,
                                errors,
                                call = caller_env()) {
  rlang::abort(
    c(header, purrr::set_names(errors, "x")),
    call = call
  )
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

      if (!is_empty(x$downloadLinks)) {
        names(x$downloadLinks) <- to_snake_case(names(x$downloadLinks))
      }

      out <- new_ipums_extract(
        collection = def$collection,
        description = def$description,
        datasets = set_nested_names(datasets),
        time_series_tables = set_nested_names(time_series_tables),
        geographic_extents = unlist(def$geographicExtents),
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

      api_extract_warnings(x$number, x$warnings)

      samples <- set_nested_names(
        purrr::map(names(def$samples), ~ samp_spec(.x))
      )

      if (!is_empty(names(def$variables))) {
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

        variables <- set_nested_names(variables)
      } else {
        variables <- NULL
      }

      if (!is_empty(names(def$timeUseVariables))) {
        time_use_variables <- purrr::map(
          names(def$timeUseVariables),
          ~ tu_var_spec(.x, owner = unlist(def$timeUseVariables[[.x]]$owner))
        )

        time_use_variables <- set_nested_names(time_use_variables)
      } else {
        time_use_variables <- NULL
      }

      if (!is_empty(x$downloadLinks)) {
        names(x$downloadLinks) <- to_snake_case(names(x$downloadLinks))
      }

      if (any(unlist(def$sampleMembers))) {
        sm_opts <- c("include_non_respondents", "include_household_members")
        sample_members <- sm_opts[unlist(def$sampleMembers)]
      } else {
        sample_members <- NULL
      }

      out <- new_ipums_extract(
        collection = def$collection,
        description = def$description,
        samples = samples,
        variables = variables,
        time_use_variables = time_use_variables,
        sample_members = sample_members,
        data_format = def$dataFormat,
        data_structure = to_snake_case(names(def$dataStructure)),
        rectangular_on = def$dataStructure$rectangular$on,
        case_select_who = def$caseSelectWho,
        data_quality_flags = def$dataQualityFlags,
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
extract_list_from_json.json <- function(extract_json, validate = FALSE) {
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

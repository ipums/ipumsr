
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

# > Define extract ----

#' Define a microdata extract request
#'
#' Define an extract request object to be submitted via the IPUMS microdata
#' extract API. For an overview of ipumsr microdata API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param collection The IPUMS data collection for the extract.
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   \href{https://usa.ipums.org/usa-action/samples/sample_ids}{Sample ID values}.
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file (one of
#'   "fixed_width", "csv", "stata", "spss", or "sas9").
#' @param data_structure Currently, this must be "rectangular", which is also
#'   the default. In the future, the API will also support "hierarchical"
#'   extracts.
#' @param rectangular_on Currently, this must be "P", indicating that the
#'   extract will be rectangularized on person records. In the future, the API
#'   will also support household-only extracts (\code{rectangular_on = "H"}).
#'
#' @family ipums_api
#' @return An object of class \code{usa_extract} containing the extract
#'   definition.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#' @export
define_extract_micro <- function(collection,
                                 description,
                                 samples,
                                 variables,
                                 data_format = c("fixed_width", "csv", "stata",
                                                 "spss", "sas9"),
                                 data_structure = "rectangular",
                                 rectangular_on = "P") {

  data_format <- match.arg(data_format)
  if (data_structure != "rectangular") {
    stop(
      "Currently, the `data_structure` argument must be equal to ",
      "\"rectangular\"; in the future, the API will also support ",
      "\"hierarchical\" extracts.",
      call. = FALSE
    )
  }
  if (rectangular_on != "P") {
    stop(
      "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
      "the future, the API will also support `rectangular_on = \"H\".",
      call. = FALSE
    )
  }
  # For now this next block is irrelevant, but leave it for whenever we add
  # support for rectangular_on = "H"
  rectangular_on <- if(data_structure == "rectangular") {
    match.arg(rectangular_on)
  } else NA_character_

  stopifnot(is.character(collection), length(collection) == 1)
  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(data_structure), length(data_structure) == 1)
  stopifnot(is.character(rectangular_on), length(rectangular_on) == 1)
  stopifnot(is.character(data_format), length(data_format) == 1)
  stopifnot(is.character(samples))
  stopifnot(is.character(variables))

  extract <- new_ipums_extract(
    collection = collection,
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
#' Define an extract request object to be submitted via the IPUMS NHGIS
#' extract API.
#'
#' An extract can contain multiple datasets, each of which may or may not
#' contain the same values for the associated dataset subfields
#' (\code{ds_tables}, \code{ds_geog_levels}, \code{ds_years}, and
#' \code{ds_breakdown_values}). When specifying an extract that contains multiple
#' datasets, you can either choose to recycle these subfield specifications to
#' all datasets in the extract, or to specify specific values for the subfields
#' for each dataset.
#'
#' To recycle subfield specifications to all datasets, pass
#' a vector to the subfield argument. To provide unique subfield values for each
#' dataset in the extract, instead pass a list of values to the subfield
#' argument. In this case, the values for that subfield will be linked to the
#' datasets in index order (for instance, if 2 datasets are specified, the
#' first element in the list of \code{ds_tables} would be linked to the first
#' dataset, and the second element in the list of \code{ds_tables} would be
#' linked to the second dataset). The same holds true for time series tables.
#'
#' If a subfield argument is passed as a list, the list must be the same length
#' as the number of datasets (for all arguments with prefix \code{ds_}) or
#' time series tables (for all arguments with prefix \code{tst_}) specified in
#' the extract definition. Note that individual \emph{elements} of these lists
#' can be of arbitrary length.
#'
#' @param description Description of the extract.
#' @param datasets Character vector of datasets to include in the extract,
#'   if any. For more information on NHGIS datasets, click
#'   \href{https://www.nhgis.org/overview-nhgis-datasets}{here}.
#' @param ds_tables Character vector or list of summary tables to include in the
#'   extract. If passed as a list, elements will be matched to the extract's
#'   datasets by index (see details). Required if any datasets are included in
#'   the extract.
#' @param ds_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in the
#'   requested summary tables. If passed as a list, elements will be matched to
#'   the extract's datasets by index (see details). Required if any datasets are
#'   included in the extract.
#' @param ds_years Character or integer vector or list of years for which to
#'   obtain the data contained in the requested summary tables. Use \code{"*"}
#'   to select all available years for the specified dataset. If passed as a
#'   list, elements will be matched to the extract's datasets by index (see
#'   details). Not all datasets allow year selection; see
#'   \code{\link{get_nhgis_metadata}} to determine if a dataset allows year
#'   selection.
#' @param ds_breakdown_values Character vector or list of selected breakdown
#'   values to apply to the requested summary tables. If more than one breakdown
#'   value is requested, \code{breakdown_and_data_type_layout} must also be
#'   specified. If passed as a list, elements will be matched to the extract's
#'   datasets by index (see details).
#' @param geographic_extents A list of geographic instances to use as extents
#'   for all datasets included in the extract. Use \code{"*"}
#'   to select all available extents. Required when any dataset in the extract
#'   includes a \code{ds_geog_levels} value that requires extent selection.
#'   See \code{\link{get_nhgis_metadata}} to determine whether a requested
#'   \code{ds_geog_level} requires extent selection. Currently, NHGIS supports
#'   extent selection only for blocks and block groups.
#' @param breakdown_and_data_type_layout Character indicating the desired layout
#'   of any datasets that have multiple data types or breakdown values.
#'   The default \code{"single_file"} keeps all data types and breakdown values
#'   in one file. \code{"separate_files"} splits each
#'   data type or breakdown value into its own file. Required if any datasets
#'   included in the extract consist of multiple data types (for instance,
#'   estimates and margins of error) or have multiple breakdown values
#'   specified. See \code{\link{get_nhgis_metadata}} to determine whether a
#'   requested dataset has multiple data types.
#' @param time_series_tables Character vector of time series tables to include
#'   in the extract, if any. For more information on NHGIS time series tables,
#'   click \href{https://www.nhgis.org/time-series-tables}{here}.
#' @param tst_geog_levels Character vector or list of geographic levels (for
#'   example, "county" or "state") for which to obtain the data contained in the
#'   provided times series tables. If passed as a list, elements will be matched to
#'   the extract's time series tables by index (see details). Required if any
#'   time series tables are included in the extract.
#' @param tst_layout Character indicating the layout of all
#'   time series tables included in the extract. One of
#'   \code{"time_by_column_layout"} (the default), \code{"time_by_row_layout"},
#'   or \code{"time_by_file_layout"}. Required when any time series tables are
#'   specified.
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
#' # pass a list to the subfield argument:
#' define_extract_nhgis(
#'   description = "Extract with multiple time series tables",
#'   time_series_tables = c("CW3", "CW5"),
#'   tst_geog_levels = list("state", "county")
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
    geographic_extents = unlist(geographic_extents),
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_tables = unlist(time_series_tables),
    tst_geog_levels = tst_geog_levels,
    tst_layout = tst_layout,
    shapefiles = unlist(shapefiles),
    data_format = data_format
  )

  extract <- recycle_nhgis_extract_args(extract)
  extract <- validate_ipums_extract(extract)

  extract

}

# > Define extract from json ----

#' Create an extract object from a JSON-formatted definition
#'
#' Create an object of class "ipums_extract" based on an extract definition
#' formatted as JSON. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_json A JSON string, or the path to file containing JSON.
#' @inheritParams define_extract_micro
#'
#' @family ipums_api
#' @return An object of class "ipums_extract".
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
define_extract_from_json <- function(extract_json, collection) {

  if (missing(collection)) {
    stop("`collection` is a required argument", call. = FALSE)
  }

  if (!collection %in% ipums_data_collections()$code_for_api) {
    stop(
      paste0(
        '"', collection, '"', " is not a valid code for an IPUMS data ",
        "collection. To see all valid collection codes, use ",
        "`ipums_data_collections()`"
      ),
      call. = FALSE
    )
  }

  extract_json <- new_ipums_json(extract_json, collection)

  tryCatch(
    list_of_extracts <- extract_list_from_json(
      extract_json,
      validate = TRUE
    ),
    error = function(cnd) {
      stop(
        conditionMessage(cnd),
        "\n\nDid you specify the correct collection for this extract?",
        call. = FALSE
      )
    }
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

  list_of_extracts[[1]]

}

# > Save extract as json ----

#' Save an extract definition to disk as JSON
#'
#' Save an extract definition to a JSON-formatted file. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams submit_extract
#' @param file File path at which to write the JSON-formatted extract
#'   definition.
#'
#' @details Note that this function only saves out the properties of an extract
#'   that are required to submit a new extract request, namely, the description,
#'   data structure, data format, samples, and variables.
#' @family ipums_api
#' @return The file path where the extract definition was written, invisibly.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
save_extract_as_json <- function(extract, file) {
  extract_as_json <- extract_to_request_json(extract)
  writeLines(jsonlite::prettify(extract_as_json), con = file)
  invisible(file)
}

# > Print methods -----------

#' @export
print.ipums_extract <- function(x) {

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
print.usa_extract <- function(x) {

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
print.nhgis_extract <- function(x) {

  # Ensure proper printing for extracts created via new_ipums_extract()
  # (which have not been recycled yet)
  x <- recycle_nhgis_extract_args(x)

  types <- nhgis_extract_types(x)

  if("datasets" %in% types) {

    n_datasets <- length(x$datasets)

    ds_to_cat <- purrr::map(
      1:n_datasets,
      ~format_dataset_for_printing(
        x$datasets[[.x]],
        x$ds_tables[[.x]],
        x$ds_geog_levels[[.x]],
        x$ds_years[[.x]],
        x$ds_breakdown_values[[.x]],
        x$geographic_extents
      )
    )

    ds_to_cat <- purrr::reduce(ds_to_cat, paste0)

  } else {
    ds_to_cat <- NULL
  }

  if("time_series_tables" %in% types) {

    tst_to_cat <- purrr::map(
      1:length(x$time_series_tables),
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
      "\n",
      print_truncated_vector(x$shapefiles, "Shapefiles: ", FALSE)
    )

  } else {
    shp_to_cat <- NULL
  }

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n",
    ds_to_cat,
    tst_to_cat,
    shp_to_cat,
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

# Internal ---------------------------------------------------------------------

# > Constructors ---------------

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
    class = c(paste0(collection, "_extract"), "ipums_extract", class(out))
  )

}

new_ipums_json <- function(json, collection) {
  structure(
    json,
    class = c(paste0(collection, "_json"), "ipums_json")
  )
}

# > Validation methods -----------

validate_ipums_extract <- function(x) {
  UseMethod("validate_ipums_extract")
}

#' @export
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

  is_missing <- purrr::map_lgl(
    c("collection", "description"),
    ~is.na(x[[.]]) || is_empty(x[[.]])
  )

  is_present <- purrr::map_lgl(
    names(x),
    ~!is.na(x[[.]]) && !is_empty(x[[.]])
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

  if (!is.null(x$data_format)) {
    if (!x$data_format %in% c("csv_header", "csv_no_header", "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_header`, `csv_no_header`",
        " or `fixed_width`.",
        call. = FALSE
      )
    }
  }

  if (!is.null(x$breakdown_and_data_type_layout)) {
    if (!x$breakdown_and_data_type_layout %in% c("single_file",
                                                 "separate_files")) {
      stop(
        "`breakdown_and_data_type_layout` must be one of `single_file`",
        " or `separate_files`.",
        call. = FALSE
      )
    }
  }

  if (!is.null(x$tst_layout)) {
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
      function(y) any(purrr::map_lgl(x[[y]], ~is_empty(.x) || is.na(.x)))
    )

    missing_df <- is.null(x$data_format) || is.na(x$data_format)

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
      ~is.list(x[[.]]) && length(x[[.]]) != length(x$datasets)
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

    # TODO: Add additional logic to catch other requirements here?
    #   1. years required when dataset has multiple years
    #   2. breakdown_and_data_type_layout required when dataset has multiple
    #      breakdowns or data types
    #
    # Would need to put together metadata functionality first.

  } else {

    ds_sub_vars <- c("ds_tables", "ds_geog_levels",
                     "ds_years", "ds_breakdown_values",
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

    missing_tst_geog <- any(
      purrr::map_lgl(
        x$tst_geog_levels,
        ~is.na(.x) || is_empty(.x)
      )
    )
    missing_df <- is.null(x$data_format) || is.na(x$data_format)
    missing_tstl <- is.null(x$tst_layout) ||
      is.na(x$tst_layout)

    is_missing <- c(missing_tst_geog, missing_df, missing_tstl)

    if (any(is_missing)) {
      stop(
        "An nhgis_extract that contains time series tables must also contain ",
        "values for: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`.",
        call. = FALSE
      )
    }

    if (is.list(x$tst_geog_levels) &&
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

#' @export
validate_ipums_extract.usa_extract <- function(x) {

  must_be_non_missing <- c("description", "data_structure",
                           "data_format", "samples", "variables")

  is_missing <- purrr::map_lgl(
    must_be_non_missing,
    ~any(is.null(x[[.]]) || is.na(x[[.]]) || is_empty(x[[.]]))
  )

  if (any(is_missing)) {
    stop(
      "The following elements of a usa_extract must not contain missing ",
      "values: ",
      paste0(must_be_non_missing[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  stopifnot(x$data_format %in% c("fixed_width", "csv","stata", "spss", "sas9"))
  stopifnot(x$data_structure == "rectangular")
  stopifnot(x$rectangular_on == "P")

  if (x$data_structure == "rectangular" & !x$rectangular_on %in% c("H", "P")) {
    stop("If `data_structure` is 'rectangular', `rectangular_on` must be one ",
         "of 'H' or 'P'")
  }

  if (x$data_structure == "hierarchical" & !is.na(x$rectangular_on)) {
    stop("If `data_structure` is 'hierarchical', `rectangular_on` must be ",
         "missing")
  }

  x

}

#' @export
validate_ipums_extract.ipums_extract <- function(x) {

  if(!x$collection %in% ipums_collection_versions()$collection) {
    stop(
      paste0(
        "No API version found for collection `", x$collection, "`\n",
        "IPUMS API is currently available for the following collections: ",
        paste0(ipums_collection_versions()$collection, collapse = ", ")
      ),
      call. = FALSE
    )
  } else {
    warning(
      paste0("Unable to validate extract for collection `", x$collection, "`.")
    )
  }

  x

}

# > Helpers -----------------

format_collection_for_printing <- function(collection) {
  collection_info <- dplyr::filter(
    ipums_data_collections(),
    code_for_api == collection
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
                                        ds_breakdown_values = NULL,
                                        geographic_extents = NULL) {
  output <- paste0(
    "\n",
    print_truncated_vector(
      dataset,
      "Dataset: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      ds_tables,
      "Tables: ",
      FALSE
      #!is_empty(ds_tables)
    ),
    "\n  ",
    print_truncated_vector(
      geog_levels,
      "Geog Levels: ",
      FALSE
      #!is_empty(geog_levels)
    ),
    "\n"
  )

  if (!is.null(ds_years)) {
    output <- paste0(
      output,
      "  ",
      print_truncated_vector(
        ds_years,
        "Years: ",
        FALSE
        #!is_empty(ds_years)
      ),
      "\n"
    )
  }

  if (!is.null(ds_breakdown_values)) {
    output <- paste0(
      output,
      "  ",
      print_truncated_vector(
        ds_breakdown_values,
        "Breakdowns: ",
        FALSE
        #!is_empty(ds_breakdown_values)
      ),
      "\n"
    )
  }

  if (!is.null(geographic_extents)) {
    output <- paste0(
      output,
      "  ",
      print_truncated_vector(
        geographic_extents,
        "Extents: ",
        FALSE
        #!is_empty(geographic_extents)
      ),
      "\n"
    )
  }

  output

}

format_tst_for_printing <- function(time_series_table, tst_geog_levels) {
  paste0(
    "\n",
    print_truncated_vector(
      time_series_table,
      "Time Series Table: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      tst_geog_levels,
      "Geog Levels: ",
      FALSE
      #!is_empty(tst_geog_levels)
    ),
    "\n"
  )
}

print_truncated_vector <- function(x, label = NULL, include_length = TRUE) {
  max_width <- min(getOption("width"), 80)
  max_width <- max(max_width, 20) # don't allow width less than 20
  full_list <- paste0(x, collapse = ", ")
  untruncated <- ifelse(
    include_length,
    paste0(label, "(", length(x), " total) ", full_list),
    paste0(label, full_list)
  )
  if (nchar(untruncated) > max_width) {
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

UNKNOWN_DATA_COLLECTION_LABEL <- "Unknown data collection"

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

#' Recycle all subfield arguments in an NHGIS extract.
#'
#' Convenience function to implement list-recycling provided in
#' \code{recycle_to_list()} for all subfield arguments in an NHGIS extract.
#'
#' @param extract An extract of class \code{nhgis_extract}
#'
#' @return
#'
#' @noRd
recycle_nhgis_extract_args <- function(extract) {

  stopifnot(extract$collection == "nhgis")

  n_datasets <- length(extract$datasets)
  n_tsts <- length(extract$time_series_tables)

  ds_to_recycle <- c("ds_tables", "ds_geog_levels",
                     "ds_years", "ds_breakdown_values")

  tst_to_recycle <- "tst_geog_levels"

  if (n_datasets > 0) {
    purrr::walk(
      ds_to_recycle,
      function(var)
        extract[[var]] <<- recycle_to_list(
          extract[[var]],
          n_datasets,
          labels = extract$datasets
        )
    )
  }

  if (n_tsts > 0) {
    purrr::walk(
      tst_to_recycle,
      function(var)
        extract[[var]] <<- recycle_to_list(
          extract[[var]],
          n_tsts,
          labels = extract$time_series_tables
        )
    )
  }

  extract

}

#' Recycle vector to list of given length
#'
#' Helper function to support the list recycling syntax used when defining or
#' revising NHGIS extracts.
#'
#' @param x Vector or list to recycle
#' @param n Length of output list
#' @param labels Character vector of names to apply to the elements of the
#'   output list. If \code{NULL}, returns an unnamed list.
#'
#' @return If \code{x} is a list, returns \code{x}. If \code{x} is a vector,
#'   returns a list of length n, where each element consists of \code{x}.
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

  if (length(labels) == length(l)) {
    l <- setNames(l, labels)
  }

  l

}

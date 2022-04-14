# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------

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
#' @return An object of class \code{ipums_extract} containing the extract
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
#' @param description Description of the extract.
#' @param datasets Character name of datasets to include in the extract, if any.
#' @param data_tables Character vector of data tables associated with the
#'   specified datasets to include in the extract. Required if a dataset is
#'   specified.
#' @param ds_geog_levels Character vector of geographic levels associated with
#'   the specified dataset to include in the extract. Required if a dataset is
#'   specified.
#' @param years Character or integer vector of years associated with the
#'   specified dataset to include in the extract. Use "*" to select all
#'   available years for the specified dataset.
#' @param breakdown_values Character vector of selected breakdown values to
#'   associated with the specified dataset to include in the extract. If more
#'   than one breakdown value is requested, `breakdown_and_data_type_layout`
#'   must also be specified.
#' @param time_series_tables Character name of time series table to include in
#'   the extract, if any.
#' @param ts_geog_levels Character vector of geographic levels associated with
#'   the specified time series table to include in the extract. Required if a
#'   time series table is specified.
#' @param shapefiles Character vector of shapefiles to include in the extract,
#'   if any.
#' @param data_format The desired format of the extract data file (one of
#'   \code{"csv_no_header"}, \code{"csv_header"}, or \code{"fixed_width"}).
#'   \code{"csv_no_header"} provides a minimal header in the first row, while
#'   \code{"csv_header"} adds a second, more descriptive header row.
#'   Required when any dataset or time_series_table is selected.
#' @param breakdown_and_data_type_layout Character indicating the layout of the
#'   dataset when multiple data types or breakdown values are specified.
#'   \code{"separate_files"} splits up each data type or breakdown combo into
#'   its own file. \code{"single_file"} keeps all datatypes and breakdown combos
#'   in one file. Required when a dataset has multiple breakdowns or data types.
#' @param time_series_table_layout Character indicating the layout of the
#'   specified time series table. One of \code{"time_by_column_layout"},
#'   \code{"time_by_row_layout"}, or \code{"time_by_file_layout"}. Required when
#'   a time series table is specified.
#' @param geographic_extents A list of geographic instances to use as extents
#'   for the dataset specified in the request. To select all extents, use
#'   \code{"*"}. Required when a geographic level on a dataset is specified
#'   where \code{has_geog_extent_selection} is true.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition.
#'
#' @examples
#' my_extract <- define_extract_nhgis(
#'   description = "test nhgis extract 1",
#'   datasets = "1990_STF3",
#'   data_tables = "NP57",
#'   ds_geog_levels = "tract",
#'   data_format = "csv_no_header"
#' )
#'
#' @export
define_extract_nhgis <- function(description = "",
                                 datasets = NULL,
                                 data_tables = NULL,
                                 ds_geog_levels = NULL,
                                 years = NULL,
                                 breakdown_values = NULL,
                                 time_series_tables = NULL,
                                 ts_geog_levels = NULL,
                                 shapefiles = NULL,
                                 data_format = NULL,
                                 breakdown_and_data_type_layout = NULL,
                                 time_series_table_layout = NULL,
                                 geographic_extents = NULL) {

  n_datasets <- length(datasets)
  n_tsts <- length(time_series_tables)

  if (n_datasets > 0) {
    data_format <- data_format %||% "csv_header"
    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      "separate_files"
  }

  if (n_tsts > 0) {
    data_format <- data_format %||% "csv_header"
    time_series_table_layout <- time_series_table_layout %||%
      "time_by_column_layout"
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = unlist(datasets),
    data_tables = data_tables,
    ds_geog_levels = ds_geog_levels,
    years = years,
    breakdown_values = breakdown_values,
    time_series_tables = unlist(time_series_tables),
    ts_geog_levels = ts_geog_levels,
    shapefiles = shapefiles,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout,
    geographic_extents = geographic_extents
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

# Print methods ---------------------------------------------

#' @export
print.usa_extract <- function(x) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: ")
  )

  cat(to_cat)

  invisible(x)

}

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
print.nhgis_extract <- function(x) {

  types <- nhgis_extract_types(x)

  if("datasets" %in% types) {

    n_datasets <- length(x$datasets)

    ds_to_cat <- purrr::map(
      1:n_datasets,
      ~format_dataset_for_printing(
        x$datasets[[.x]],
        x$data_tables[[.x]],
        x$ds_geog_levels[[.x]],
        x$years[[.x]],
        x$breakdown_values[[.x]]
      )
    )

    ds_to_cat <- purrr::reduce(ds_to_cat, paste0)

  } else {
    ds_to_cat <- NULL
  }

  if("time_series_tables" %in% types) {

    ts_to_cat <- purrr::map(
      1:length(x$time_series_tables),
      ~format_tst_for_printing(
        x$time_series_tables[[.x]],
        x$ts_geog_levels[[.x]]
      )
    )

    ts_to_cat <- purrr::reduce(ts_to_cat, paste0)

  } else {
    ts_to_cat <- NULL
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
    ts_to_cat,
    shp_to_cat,
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

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
                                        data_tables,
                                        geog_levels,
                                        years,
                                        breakdown_values) {
  paste0(
    "\n",
    print_truncated_vector(
      dataset,
      "Dataset: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      data_tables,
      "Tables: ",
      FALSE
      #!is_empty(data_tables)
    ),
    "\n  ",
    print_truncated_vector(
      geog_levels,
      "Geog Levels: ",
      FALSE
      #!is_empty(geog_levels)
    ),
    "\n  ",
    print_truncated_vector(
      years,
      "Years: ",
      FALSE
      #!is_empty(years)
    ),
    "\n  ",
    print_truncated_vector(
      breakdown_values,
      "Breakdowns: ",
      FALSE
      #!is_empty(breakdown_values)
    ),
    "\n"
  )
}

format_tst_for_printing <- function(time_series_table, ts_geog_levels) {
  paste0(
    "\n",
    print_truncated_vector(
      time_series_table,
      "Time Series Table: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      ts_geog_levels,
      "Geog Levels: ",
      FALSE
      #!is_empty(ts_geog_levels)
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

# Internal ---------------------------------------------------------------------

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

# > validate_ipums_extract() ---------------

validate_ipums_extract <- function(x) {
  UseMethod("validate_ipums_extract")
}

#' @export
validate_ipums_extract.nhgis_extract <- function(x) {

  types <- nhgis_extract_types(x)

  if(length(types) == 0) {
    stop(
      "At least one of `datasets`, `time_series_tables` or `shapefiles` ",
      "must be specified.",
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
      "The following elements of an nhgis_extract must not contain missing ",
      "values: ",
      paste0(c("collection", "description")[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  if("datasets" %in% types) {

    must_be_non_missing <- c("data_tables", "ds_geog_levels", "data_format")
    must_have_same_length <- c("data_tables", "ds_geog_levels",
                               "years", "breakdown_values")

    is_missing <- purrr::map_lgl(
      must_be_non_missing[1:2],
      function(y) any(purrr::map_lgl(x[[y]], ~is_empty(.x) || is.na(.x)))
    )

    missing_df <- is.null(x$data_format) || is.na(x$data_format)

    is_missing <- c(is_missing, missing_df)

    if (any(is_missing)) {
      stop(
        "When a dataset is specified, the following must not contain ",
        "missing values: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`",
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

    if(!x$data_format %in% c("csv_no_header",
                             "csv_header",
                             "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_no_header`, `csv_header`, ",
        "or `fixed_width`",
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

    ds_sub_vars <- c("data_tables", "ds_geog_levels",
                     "years", "breakdown_values")

    extra_vars <- ds_sub_vars[ds_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "No dataset provided for the following arguments: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if("time_series_tables" %in% types) {

    must_be_non_missing <- c("ts_geog_levels",
                             "data_format",
                             "time_series_table_layout")

    missing_ts_geog <- any(
      purrr::map_lgl(
        x$ts_geog_levels,
        ~is.na(.x) || is_empty(.x)
      )
    )
    missing_df <- is.null(x$data_format) || is.na(x$data_format)
    missing_tstl <- is.null(x$time_series_table_layout) ||
      is.na(x$time_series_table_layout)

    is_missing <- c(missing_ts_geog, missing_df, missing_tstl)

    if (any(is_missing)) {
      stop(
        "When a time series table is specified, the following must not contain",
        " missing values: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`",
        call. = FALSE
      )
    }

    if (is.list(x$ts_geog_levels) &&
        length(x$ts_geog_levels) != length(x$time_series_tables)) {
      stop(
        "The number of selections provided in `ts_geog_levels` (",
        length(x$ts_geog_levels),
        ") does not match the number of time series tables (",
        length(x$time_series_tables),
        "). \nTo recycle selections across time series tables, ensure values ",
        "are stored in a vector, not a list.",
        call. = FALSE
      )
    }

    if(!x$data_format %in% c("csv_no_header",
                             "csv_header",
                             "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_no_header`, `csv_header`, ",
        "or `fixed_width`",
        call. = FALSE
      )
    }

    if(!x$time_series_table_layout %in% c("time_by_column_layout",
                                          "time_by_row_layout",
                                          "time_by_file_layout")) {
      stop(
        "`time_series_table_layout` must be one of `time_by_column_layout`, ",
        "`time_by_row_layout`, or `time_by_file_layout`",
        call. = FALSE
      )
    }

  } else {

    ts_sub_vars <- c("ts_geog_levels", "time_series_table_layout")
    extra_vars <- ts_sub_vars[ts_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "No time series table provided for the following arguments: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if ("shapefiles" %in% types && length(types) == 1) {

    not_relevant <- c("data_format", "breakdown_and_data_type_layout",
                      "time_series_table_layout", "geographic_extents")
    extra_vars <- intersect(not_relevant, vars_present)

    if (length(extra_vars) > 0) {
      warning(
        "The following parameters are not relevant for extracts that consist ",
        "only of shapefiles: `",
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
    ~any(
      is.null(extract[[.]]),
      is.na(extract[[.]]),
      is_list(extract[[.]]) && is_empty(extract[[.]]) && is_named(extract[[.]])
    )
  )

  types <- possible_types[!is_missing]

  if(length(types) == 0) {
    NULL
  } else {
    types
  }

}

recycle_nhgis_extract_args <- function(extract) {

  stopifnot(extract$collection == "nhgis")

  n_datasets <- length(extract$datasets)
  n_tsts <- length(extract$time_series_tables)

  ds_to_recycle <- c("data_tables", "ds_geog_levels",
                     "years", "breakdown_values")

  ts_to_recycle <- "ts_geog_levels"

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
      ts_to_recycle,
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

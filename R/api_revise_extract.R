# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------


# > Revise extract definition ----

#' Revise a microdata extract definition
#'
#' Revise a microdata extract definition. If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An object of class \code{ipums_extract}.
#' @param description The modified extract description. If NULL (the default),
#'   leave the description unchanged.
#' @param samples_to_add Samples to add to the extract definition, as a
#'   character vector. If NULL (the default), no samples will be added.
#' @param samples_to_remove Samples to remove from the extract definition, as a
#'   character vector. If NULL (the default), no samples will be removed.
#' @param vars_to_add Names of variables to add to the extract definition, as
#'   a character vector. If NULL (the default), no variables will be added.
#' @param vars_to_remove Names of variables to remove from the extract
#'   definition, as a character vector. If NULL (the default), no variables will
#'   be removed.
#' @param data_format The desired data file format for the modified extract
#'   definition. If NULL (the default), leave the data format unchanged.
#' @param data_structure The desired data structure. Currently, this can only be
#'   "rectangular", but "hierarchical" extracts will be supported in the future.
#'   If NULL (the default), leave the data structure unchanged.
#' @param rectangular_on Currently, this can only be "P", but in the future,
#'   household-only extracts (\code{rectangular_on = "H"}) will also be
#'   supported. If NULL, (the default), leave the \code{rectangular_on} field
#'   unchanged.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the modified
#'   extract definition.
#'
#' @examples
#' \dontrun{
#' old_extract <- get_extract_info("usa:33")
#'
#' revised_extract <- revise_extract_micro(
#'   old_extract,
#'   samples_to_add = "us2018a",
#'   vars_to_add = "INCTOT"
#' )
#'
#' submitted_revised_extract <- submit_extract(revised_extract)
#' }
#'
#' @noRd
# revise_extract_micro <- function(extract,
#                                  description = NULL,
#                                  samples_to_add = NULL,
#                                  samples_to_remove = NULL,
#                                  vars_to_add = NULL,
#                                  vars_to_remove = NULL,
#                                  data_format = NULL,
#                                  data_structure = NULL,
#                                  rectangular_on = NULL) {
#
#   extract <- copy_ipums_extract(extract)
#   extract$description <- paste0("Revision of (", extract$description, ")")
#
#   extract <- add_to_extract(extract, "samples", samples_to_add)
#   extract <- remove_from_extract(extract, "samples", samples_to_remove)
#
#   extract <- add_to_extract(extract, "variables", vars_to_add)
#   extract <- remove_from_extract(extract, "variables", vars_to_remove)
#
#   if (!is.null(description)) extract$description <- description
#   if (!is.null(data_format)) extract$data_format <- data_format
#   if (!is.null(data_structure)) {
#     if (data_structure != "rectangular") {
#       stop(
#         "Currently, the `data_structure` argument must be equal to ",
#         "\"rectangular\"; in the future, the API will also support ",
#         "\"hierarchical\" extracts.",
#         call. = FALSE
#       )
#     }
#   }
#   if (!is.null(rectangular_on)) {
#     if (rectangular_on != "P") {
#       stop(
#         "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
#         "the future, the API will also support `rectangular_on = \"H\".",
#         call. = FALSE
#       )
#     }
#   }
#
#   extract <- validate_ipums_extract(extract)
#
#   extract
# }

#' @export
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
}

#' @export
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' @export
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
                                         validate = TRUE) {

  extract <- copy_ipums_extract(extract)

  datasets <- datasets %||% extract$datasets
  time_series_tables <- time_series_tables %||% extract$time_series_tables

  n_ds <- length(datasets)
  ds_in_extract <- datasets %in% extract$datasets

  old_ds_i <- which(ds_in_extract)
  new_ds_i <- which(!ds_in_extract)

  old_ds <- datasets[old_ds_i]
  new_ds <- datasets[new_ds_i]

  n_ts <- length(time_series_tables)
  tst_in_extract <- time_series_tables %in% extract$time_series_tables

  old_tst_i <- which(tst_in_extract)
  new_tst_i <- which(!tst_in_extract)

  old_ts <- time_series_tables[old_tst_i]
  new_ts <- time_series_tables[new_tst_i]

  ds_args <- purrr::map(
    list(
      ds_tables = ds_tables,
      ds_geog_levels = ds_geog_levels,
      ds_years = ds_years,
      ds_breakdown_values = ds_breakdown_values
    ),
    ~recycle_to_list(.x, n_ds, datasets)
  )

  tst_args <- purrr::map(
    list(
      tst_geog_levels = tst_geog_levels
    ),
    ~recycle_to_list(.x, n_ts, time_series_tables)
  )

  ds_arg_length <- purrr::map_dbl(ds_args, ~length(.x))
  ds_wrong_length <- purrr::map_lgl(ds_arg_length, ~.x != n_ds)

  tst_arg_length <- purrr::map_dbl(tst_args, ~length(.x))
  tst_wrong_length <- purrr::map_lgl(tst_arg_length, ~.x != n_ts)

  if (any(ds_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ds_wrong_length),
      ~paste0("`", .x, "` (", ds_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ds_wrong_length], collapse = ", "),
      " does not match the number of datasets to be modified (", n_ds,
      "). To recycle selections across datasets, ensure values are stored ",
      "in a vector, not a list.",
      call. = FALSE
    )
  }

  if (any(tst_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(tst_wrong_length),
      ~paste0("`", .x, "` (", tst_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[tst_wrong_length], collapse = ", "),
      " does not match the number of time series tables to be modified (", n_ts,
      "). To recycle selections across time series tables, ensure values ",
      "are stored in a vector, not a list.",
      call. = FALSE
    )
  }

  extract <- add_nested_field(
    extract,
    datasets = new_ds,
    ds_tables = ds_args$ds_tables[new_ds_i],
    ds_geog_levels = ds_args$ds_geog_levels[new_ds_i],
    ds_years = ds_args$ds_years[new_ds_i],
    ds_breakdown_values = ds_args$ds_breakdown_values[new_ds_i]
  )

  extract <- modify_subfields(
    extract,
    datasets = old_ds,
    ds_tables = ds_args$ds_tables[old_ds_i],
    ds_geog_levels = ds_args$ds_geog_levels[old_ds_i],
    ds_years = ds_args$ds_years[old_ds_i],
    ds_breakdown_values = ds_args$ds_breakdown_values[old_ds_i],
    add = TRUE
  )

  extract <- add_nested_field(
    extract,
    time_series_tables = new_ts,
    tst_geog_levels = tst_args$tst_geog_levels[new_tst_i]
  )

  extract <- modify_subfields(
    extract,
    time_series_tables = old_ts,
    tst_geog_levels = tst_args$tst_geog_levels[old_tst_i],
    add = TRUE
  )

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be removed for all datasets.",
      call. = FALSE
    )
  }

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

  if (n_ds > 0) {
    extract$data_format <- extract$data_format %||% "csv_header"
    extract$breakdown_and_data_type_layout <- extract$breakdown_and_data_type_layout %||%
      "separate_files"
  }

  if (n_ts > 0) {
    extract$data_format <- extract$data_format %||% "csv_header"
    extract$tst_layout <- extract$tst_layout %||%
      "time_by_column_layout"
  }

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

#' @export
remove_from_extract.nhgis_extract <- function(extract,
                                              datasets = NULL,
                                              ds_tables = NULL,
                                              ds_geog_levels = NULL,
                                              ds_years = NULL,
                                              ds_breakdown_values = NULL,
                                              time_series_tables = NULL,
                                              tst_geog_levels = NULL,
                                              shapefiles = NULL,
                                              geographic_extents = NULL,
                                              validate = TRUE) {

  extract <- copy_ipums_extract(extract)

  ds_provided <- !is.null(datasets)
  tst_provided <- !is.null(time_series_tables)

  datasets <- datasets %||% extract$datasets
  time_series_tables <- time_series_tables %||% extract$time_series_tables

  n_ds <- length(datasets)
  n_ts <- length(time_series_tables)

  new_ds <- setdiff(datasets, extract$datasets)
  new_ts <- setdiff(time_series_tables, extract$time_series_tables)

  if (length(new_ds) > 0) {
    warning(
      "Some datasets (\"",
      paste0(new_ds, collapse = "\", \""),
      "\") could not be modified because they were not found in this extract's",
      " datasets.",
      call. = FALSE
    )
  }

  if (length(new_ts) > 0) {
    warning(
      "Some time series tables (\"",
      paste0(new_ts, collapse = "\", \""),
      "\") could not be modified because they were not found in this extract's",
      " datasets.",
      call. = FALSE
    )
  }

  ds_args <- purrr::map(
    list(
      ds_tables = ds_tables,
      ds_geog_levels = ds_geog_levels,
      ds_years = ds_years,
      ds_breakdown_values = ds_breakdown_values
    ),
    ~recycle_to_list(.x, n_ds, datasets)
  )

  tst_args <- purrr::map(
    list(
      tst_geog_levels = tst_geog_levels
    ),
    ~recycle_to_list(.x, n_ts, time_series_tables)
  )

  ds_arg_length <- purrr::map_dbl(ds_args, ~length(.x))
  ds_wrong_length <- purrr::map_lgl(ds_arg_length, ~.x != n_ds)

  tst_arg_length <- purrr::map_dbl(tst_args, ~length(.x))
  tst_wrong_length <- purrr::map_lgl(tst_arg_length, ~.x != (n_ts))

  if (any(ds_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ds_wrong_length),
      ~paste0("`", .x, "` (", ds_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ds_wrong_length], collapse = ", "),
      " does not match the number of datasets to be modified (", n_ds,
      "). To recycle selections across datasets, ensure values are stored ",
      "in a vector, not a list.",
      call. = FALSE
    )
  }

  if (any(tst_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(tst_wrong_length),
      ~paste0("`", .x, "` (", tst_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[tst_wrong_length], collapse = ", "),
      " does not match the number of time series tables to be modified (", n_ts,
      "). To recycle selections across time series tables, ensure values ",
      "are stored in a vector, not a list.",
      call. = FALSE
    )
  }

  # If subarguments are null, we interpret that user wants to remove full
  # datasets/tsts, if provided.
  remove_full_ds <- all(purrr::map_lgl(ds_args, ~is.null(unlist(.x))))
  remove_full_ts <- all(purrr::map_lgl(tst_args, ~is.null(unlist(.x))))

  if (remove_full_ds && ds_provided) {
    extract <- remove_nested_field(
      extract,
      datasets = datasets,
      subfields = c("ds_tables",
                    "ds_geog_levels",
                    "ds_years",
                    "ds_breakdown_values"),
      ancillary_fields = c("geographic_extents",
                           "breakdown_and_data_type_layout")
    )
  } else {
    extract <- modify_subfields(
      extract,
      datasets = datasets,
      ds_tables = ds_tables,
      ds_geog_levels = ds_geog_levels,
      ds_years = ds_years,
      ds_breakdown_values = ds_breakdown_values,
      add = FALSE
    )
  }

  if (remove_full_ts && tst_provided) {
    extract <- remove_nested_field(
      extract,
      time_series_tables = time_series_tables,
      subfields = "tst_geog_levels",
      ancillary_fields = "tst_layout"
    )
  } else {
    extract <- modify_subfields(
      extract,
      time_series_tables = time_series_tables,
      tst_geog_levels = tst_geog_levels,
      add = FALSE
    )
  }

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be removed for all datasets.",
      call. = FALSE
    )
  }

  # Handle updates to non-nested fields
  extract <- modify_flat_fields(
    extract,
    shapefiles = shapefiles,
    geographic_extents = geographic_extents,
    modification = "remove"
  )

  # If removal results in extract with no ds/tst, remove irrelevant values
  # for data format
  if (is.null(extract$datasets) && is.null(extract$time_series_tables)) {
    extract["data_format"] <- list(NULL)
  }

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

#' @export
add_to_extract.usa_extract <- function(extract,
                                       description = NULL,
                                       samples = NULL,
                                       variables = NULL,
                                       data_format = NULL,
                                       data_structure = NULL,
                                       rectangular_on = NULL,
                                       validate = TRUE) {

  extract <- copy_ipums_extract(extract)

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

  replace_vars <- list(
    description = description,
    data_format = data_format,
    data_structure = data_structure,
    rectangular_on = rectangular_on
  )

  purrr::map(
    names(add_vars),
    ~if (any(add_vars[[.x]] %in% extract[[.x]])) {
      warning(
        "The following ", .x, " are already included in the ",
        "supplied extract definition, and thus will not be added: ",
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

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

#' @export
remove_from_extract.usa_extract <- function(extract,
                                            samples = NULL,
                                            variables = NULL,
                                            validate = TRUE) {

  extract <- copy_ipums_extract(extract)

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

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

# Internal -----------------------------------------------------------

#' Add nested fields to an extract object
#'
#' @param extract An ipums_extract object to revise
#' @param ... Arbitrary number of named arguments, where names correspond to
#'   extract fields and values correspond to the values that should be added in
#'   those fields. The first entry in the list of arguments is interpreted as
#'   the field that the remaining arguments nest within (thus, this can only be
#'   called once for each nested field).
#'
#' @return A modified ipums_extract object
#'
#' @noRd
add_nested_field <- function(extract, ...) {

  dots <- rlang::list2(...)

  new_fields <- dots[[1]]
  new_subfields <- dots[2:length(dots)]

  new_field_val <- setdiff(new_fields, extract[[names(dots)[1]]])
  n_new_field_val <- length(new_field_val)

  if (n_new_field_val == 0) {
    return(extract)
  }

  extract[[names(dots)[1]]] <- union(extract[[names(dots)[[1]]]], new_field_val)

  purrr::walk(
    names(new_subfields),
    ~{
      new_val <- c(
        extract[[.x]],
        recycle_to_list(new_subfields[[.x]], n_new_field_val, new_field_val)
      )
      extract[[.x]] <<- new_val
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
#' invalid extracts if all of a given extract field are removed. For instance,
#' if all \code{time_series_tables} are removed from an \code{nhgis_extract}
#' object, the extract should not contain a value for \code{tst_layout}, but
#' \code{tst_layout} is not a nested field within \code{time_series_tables}
#' because it applies to all time series tables in an extract.
#'
#' @param extract An ipums_extract object to revise
#' @param ... A single named argument, where the name should correspond to the
#'   field in \code{extract} that contains the subfields provided in
#'   \code{subfields}. The values provided to this argument indicate the
#'   values within the field that should be removed from the extract, along
#'   with their associated subfields.
#' @param subfields Character vector indicating the names of the extract fields
#'   that are subfields of the field provided in \code{...}. For instance,
#'   for NHGIS extracts, "tst_geog_levels" is a subfield of "time_series_tables"
#' @param ancillary_fields Character vector indicating the names of extract
#'   fields that are not nested within the field provided in \code{...}, but are
#'   not relevant if no values exist for that field. See details.
#'
#' @return A modified ipums_extract object
#'
#' @noRd
remove_nested_field <- function(extract,
                                ...,
                                subfields,
                                ancillary_fields = NULL) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))
  stopifnot(length(dots) == 1)

  new_fields <- dots[[1]]
  new_field_val <- setdiff(extract[[names(dots)[1]]], new_fields)

  if (length(new_field_val) == 0) {
    no_new_field <- TRUE
    new_field_val <- NULL
    extract[names(dots)[1]] <- list(NULL)

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
    extract[[names(dots)[1]]] <- new_field_val
  }

  purrr::walk(
    subfields,
    ~{
      if (no_new_field) {
        extract[.x] <<- list(NULL)
      } else {
        extract[.x] <<- list(extract[[.x]][new_field_val])
      }
    }
  )

  extract

}

#' Modify an extract's non-nested fields
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

# add_to_flat_fields <- function(extract, ...) {
#
#
#   dots <- rlang::list2(...)
#
#   stopifnot(is_named(dots))
#
#   purrr::walk(
#     names(dots),
#     ~{
#       if (is.null(dots[[.x]]) && is.null(extract[[.x]])) {
#         extract[.x] <<- list(NULL)
#       } else {
#         extract[[.x]] <<- unlist(union(extract[[.x]], dots[[.x]]))
#       }
#     }
#   )
#
#
#   extract
#
# }
#
# remove_from_flat_fields <- function(extract, ...) {
#
#   dots <- rlang::list2(...)
#
#   stopifnot(is_named(dots))
#
#   purrr::walk(
#     names(dots),
#     function(x) {
#       values <- setdiff(extract[[x]], unlist(dots[[x]]))
#       if (length(values) > 0) {
#         extract[[x]] <<- values
#       } else {
#         extract[x] <<- list(NULL)
#       }
#     }
#   )
#
#   extract
#
# }
#
# replace_in_flat_fields <- function(extract, ...) {
#
#   dots <- rlang::list2(...)
#
#   stopifnot(is_named(dots))
#
#   purrr::walk(
#     names(dots),
#     ~{
#       if (!is.null(dots[[.x]])) {
#         if (length(dots[[.x]]) > 1) {
#           warning(
#             "Multiple values passed to `", .x, "`, which must be length 1. ",
#             "Only the first value will be used.",
#             call. = FALSE
#           )
#         }
#         extract[[.x]] <<- dots[[.x]][1]
#       }
#     }
#   )
#
#   extract
#
# }

#' Modify values that are nested within another extract field
#'
#' @param extract Extract to revise
#' @param ... Arbitrary number of named arguments, where names correspond to
#'   extract fields to be modified and values correspond to the values that
#'   should be modified in those fields.
#'   The first entry in the list of arguments is interpreted as the field that
#'   the remaining arguments nest within (thus, this can only be called once
#'   for each nested field). Subfields will only be modified for
#'   those fields whose names appear in the values of the first argument to
#'   \code{...}
#' @param add Logical indicating whether the values in \code{...} should be
#'   added (TRUE) or removed (FALSE) from the extract.
#'
#' @return An extract object with modified subfield values
#'
#' @noRd
modify_subfields <- function(extract, ..., add = TRUE) {

  dots <- rlang::list2(...)

  stopifnot(is_named(dots))

  new_fields <- dots[[1]]
  new_subfields <- dots[2:length(dots)]

  purrr::walk(
    names(new_subfields),
    ~{
      if (!is.null(extract[[.x]])) {
        extract[[.x]] <<- modify_list(
          extract[[.x]],
          to_modify = new_fields,
          values = new_subfields[[.x]],
          add = add
        )
      }
    }
  )

  extract

}


modify_list <- function(l, to_modify = NULL, values = NULL, add = TRUE) {

  if (is.null(to_modify)) {
    to_modify <- names(l)
  }

  # values <- unlist(values)
  values <- recycle_to_list(values, length(to_modify), to_modify)

  if (add) {
    modified <- purrr::map(
      names(l),
      function(x) {
        if (x %in% to_modify) {
          union(l[[x]], unlist(values[which(x == to_modify)]))
        } else {
          l[[x]]
        }
      }
    )
  } else {
    modified <- purrr::map(
      names(l),
      function(x) {
        if (x %in% to_modify) {
          d <- setdiff(l[[x]], unlist(values[which(x == to_modify)]))
          if (length(d) == 0) {
            NULL
          } else {
            d
          }
        } else {
          l[[x]]
        }
      }
    )
  }

  setNames(modified, names(l))

}

copy_ipums_extract <- function(extract) {

  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract

}

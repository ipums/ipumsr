# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------

# > Revise extract definition ----

#' Modify values in an existing extract definition
#'
#' @description
#' Add or remove values for specific fields in an existing extract. These
#' functions are S3 generics whose behavior will depend on the class (i.e.
#' collection) of the extract being modified. Collection-specific documentation
#' can be found through the following links:
#'
#' \itemize{
#'   \item{\code{\link{revise_extract_nhgis}}}
#'   \item{\code{\link{revise_extract_micro}}}
#' }
#'
#' In general, for a given collection, the arguments to these functions are
#' identical to those used when defining an extract for that collection. For
#' documentation on defining an extract, see \code{\link{define_extract}}
#'
#' @param extract An object inheriting from \code{ipums_extract}
#' @param ... Additional arguments specifying the extract fields and values to
#'   modify. The available arguments correspond to the available arguments in
#'   the extract definition function for the class of the extract specified in
#'   \code{extract}.
#'
#' @return An object of the same class as \code{extract} containing the modified
#'   extract definition
#'
#' @seealso \code{\link{add_to_extract.nhgis_extract}},
#'   \code{\link{add_to_extract.usa_extract}}
#'   \code{\link{remove_from_extract.nhgis_extract}},
#'   \code{\link{remove_from_extract.usa_extract}}
#'
#' @name revise_extract
NULL

#' @rdname revise_extract
#' @export
add_to_extract <- function(extract, ...) {
  UseMethod("add_to_extract")
}

#' @rdname revise_extract
#' @export
remove_from_extract <- function(extract, ...) {
  UseMethod("remove_from_extract")
}

#' Modify values in an existing NHGIS extract definition
#'
#' Either add or remove values in the extract fields for an existing
#' \code{nhgis_extract} object.
#'
#' @details
#' NHGIS extracts may contain multiple datasets or time series tables. Each
#' dataset or time series table is associated with several subfields that apply
#' only to that particular dataset or time series table. Dataset subfields are
#' prefixed with \code{ds_}, while time series table subfields are prefixed with
#' \code{tst_}.
#'
#' In general, the subfield arguments abide by the same syntax options when
#' revising an extract as they do when defining an extract (see
#' \code{\link{define_extract_nhgis}} for details on the core extract definition
#' syntax). However, there are some exceptions:
#'
#' In \code{add_to_extract}, if no values are provided to the \code{datasets}
#' or \code{time_series_tables} arguments, the values passed to any of their
#' associated subfield arguments will be evaluated relative to all the
#' datasets or time series tables that \emph{already exist in the extract}. On
#' the other hand, if any values are passed to the \code{datasets} or
#' \code{time_series_tables} arguments, the values pass to any of their
#' associated subfield arguments will be evaluated relative only to
#' the \emph{specified} datasets or time series tables. For example, if new
#' datasets are added, then the values provided in \code{ds_tables} will apply
#' to the new datasets only. Existing datasets can therefore be modified either
#' by supplying their name to the \code{datasets} argument or by including the
#' desired additions to their subfields as a named list in the subfield
#' argument.
#'
#' In \code{remove_from_extract}, all values passed to either \code{datasets}
#' or \code{time_series_tables} will be removed from the extract in their
#' entirety (i.e. along with their currently associated subfield values) before
#' considering the values passed to any subfield arguments. Subfield arguments
#' will then be evaluated relative to all the datasets or time series tables
#' that still exist in the extract.
#'
#' Because of these intricacies, if you're working with an extract with multiple
#' datasets or time series tables, the most explicit way to specify subfield
#' arguments is to use the named list syntax. This ensures that subfield
#' values are matched to the appropriate datasets or time series tables.
#'
#' See the examples for demonstrations of this functionality.
#'
#' For extract fields that take a single value, \code{add_to_extract} will
#' replace the existing value with the new value provided for that field.
#'
#' Note that it is possible to produce invalid extracts using
#' \code{remove_from_extract} (for instance, an extract that includes a
#' time series table without associated geography levels). This can occur if
#' you intend to replace the existing values for a required extract field.
#' If your goal is not simply to add or remove values, but replace values in
#' an extract, it is recommended that you first use \code{add_to_extract}, and
#' then use \code{remove_from_extract}, as this will avoid temporarily producing
#' an invalid extract. Alternatively, you can set \code{validate = FALSE} in
#' \code{remove_from_extract} to prevent extract validation while you
#' make the replacement.
#'
#' @inheritParams define_extract_nhgis
#' @param validate Logical value indicating whether to check the modified
#'   extract structure for validity. Defaults to \code{TRUE}
#' @param ... Not used
#'
#' @return A modified \code{nhgis_extract} object
#'
#' @name revise_extract_nhgis
#'
#' @examples
#' extract <- define_extract_nhgis(
#'   datasets = "1980_STF1",
#'   ds_tables = c("NT1A", "NT1B"),
#'   ds_geog_levels = "county"
#' )
#'
#' # Add a new dataset to the extract.
#' # The values provided to ds_tables and ds_geog_levels only apply to the
#' # new dataset.
#' add_to_extract(
#'   extract,
#'   datasets = "1990_STF1",
#'   ds_tables = "NP1",
#'   ds_geog_levels = "county"
#' )
#'
#' # Add a table to the existing dataset in the extract
#' add_to_extract(
#'   extract,
#'   ds_tables = "NT2"
#' )
#'
#' # Combining the previous two revisions into a single call.
#' # To apply the specified ds_tables to each dataset individually, we use
#' # a list
#' add_to_extract(
#'   extract,
#'   datasets = c("1980_STF1", "1990_STF1"),
#'   ds_tables = list("NT2", "NP1")
#'   ds_geog_levels = "county"
#' )
#'
#' # However, index-based matching would not work correctly, as the ds_tables
#' # specification is evaluated relative only to the new dataset provided, not
#' # to all datasets in the extract:
#' add_to_extract(
#'   extract,
#'   datasets = "1990_STF1",
#'   ds_tables = list("NT2", "NP1")
#'   ds_geog_levels = "county"
#' )
#'
#' # Alternatively, a named list can be passed to ds_tables to explicitly
#' # indicate which values should be mapped to which datasets:
#' revised_extract <- add_to_extract(
#'   extract,
#'   datasets = "1990_STF1",
#'   ds_tables = list(`1980_STF1` = "NT2", `1990_STF1` = c("NP1", "NP2"))
#'   ds_geog_levels = "county"
#' )
#'
#' # Removes dataset and all associated subfields
#' remove_from_extract(
#'   revised_extract,
#'   datasets = "1990_STF1"
#' )
#'
#' # To remove specific subfields but retain datasets:
#' remove_from_extract(
#'   revised_extract,
#'   ds_tables = "NT2"
#' )
#'
#' # Datasets are removed before evaluating subfield arguments:
#' remove_from_extract(
#'   revised_extract,
#'   datasets = "1990_STF1",
#'   ds_tables = "NP1"
#' )
#'
#' # Specific values can be removed from subfields using a named list
#' remove_from_extract(
#'   revised_extract,
#'   ds_tables = list(`1980_STF1` = "NT2", `1990_STF1` = "NP2")
#' )
#'
#' \dontrun{
#' # Replacing values in an extract can produce invalid extracts:
#' remove_from_extract(
#'   revised_extract,
#'   ds_geog_levels = "county"
#' ) %>%
#'   add_to_extract(
#'     ds_geog_levels = "state"
#'   )
#' }
#'
#' # So it is recommended to add new values first:
#' add_to_extract(
#'   revised_extract,
#'   ds_geog_levels = "state"
#' ) %>%
#'   remove_from_extract(
#'     ds_geog_levels = "county"
#'   )
#' }
NULL

#' @export
#' @rdname revise_extract_nhgis
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

  if (is.list(geographic_extents)) {
    warning(
      "`geographic_extents` was provided as a list, but this parameter ",
      "applies to all datasets in an NHGIS extract. The provided values will ",
      "be applied to all datasets.",
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

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

#' @export
#' @rdname revise_extract_nhgis
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
                                              validate = TRUE,
                                              ...) {

  extract <- copy_ipums_extract(extract)

  # Throw more informative warning if user thinks they can remove these fields:
  extract <- validate_remove_fields(
    extract,
    bad_remove_fields = c("description", "breakdown_and_data_type_layout",
                          "tst_layout", "data_format"),
    ...
  )

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

#' Modify values in an existing microdata extract definition
#'
#' Either add or remove values in the extract fields for an existing
#' \code{usa_extract} object.
#'
#' @inheritParams define_extract_micro
#'
#' @return A modified \code{usa_extract} object
#'
#' @name revise_extract_micro
NULL

#' @export
#' @rdname revise_extract_micro
add_to_extract.usa_extract <- function(extract,
                                       description = NULL,
                                       samples = NULL,
                                       variables = NULL,
                                       data_format = NULL,
                                       data_structure = NULL,
                                       rectangular_on = NULL,
                                       validate = TRUE) {

  extract <- copy_ipums_extract(extract)

  # Move this to validate_ipums_extract.usa_extract()?
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

  # Currently don't have analogous warning for NHGIS
  # purrr::map(
  #   names(add_vars),
  #   ~if (any(add_vars[[.x]] %in% extract[[.x]])) {
  #     warning(
  #       "The following ", .x, " are already included in the ",
  #       "supplied extract definition, and thus will not be added: ",
  #       paste0(
  #         intersect(add_vars[[.x]], extract[[.x]]),
  #         collapse = "\", \""
  #       ),
  #       "\"",
  #       call. = FALSE
  #     )
  #   }
  # )

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
#' @rdname revise_extract_micro
remove_from_extract.usa_extract <- function(extract,
                                            samples = NULL,
                                            variables = NULL,
                                            validate = TRUE,
                                            ...) {

  extract <- copy_ipums_extract(extract)

  extract <- validate_remove_fields(
    extract,
    bad_remove_fields = c("description", "data_format",
                          "data_structure", "rectangular_on"),
    ...
  )

  to_remove <- list(
    samples = samples,
    variables = variables
  )

  # purrr::walk(
  #   names(to_remove),
  #   ~if (any(!to_remove[[.x]] %in% extract[[.x]])) {
  #     warning(
  #       "The following ", .x, " are not included in the ",
  #       "supplied extract definition, and thus will not be removed: \"",
  #       paste0(
  #         setdiff(to_remove[[.x]], extract[[.x]]),
  #         collapse = "\", \""
  #       ),
  #       "\"",
  #       call. = FALSE
  #     )
  #   }
  # )

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

  extract[[field]] <- all_field_vals

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
      extract[[.x]] <<- reduce_list_by_name(
        c(extract[[.x]], subfield_vals_recycled[[.x]]),
        f = union
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

#' Produce warnings for invalid extract revision requests
#'
#' Convenience function to throw more informative warnings on invalid extract
#' revision specifications. Currently used to direct users to
#' \code{add_to_extract()} when attempting to remove non-optional fields in
#' \code{remove_from_extract()}. (Otherwise, users would face a potentially
#' unexpected unused argument error)
#'
#' @param extract
#' @param bad_remove_fields Character vector of names of fields that should
#'   trigger warnings if user attempts to remove them from an extract
#' @param ... Arbitrary selection of named arguments. Used to warn against use
#'   of extract fields that do not exist in the extract.
#'
#' @noRd
validate_remove_fields <- function(extract, bad_remove_fields, ...) {

  dots <- rlang::list2(...)

  if ("collection" %in% names(dots)) {
    stop(
      "Cannot modify collection of an existing extract. To create an extract",
      " from a new collection, use define_extract_micro().",
      call. = FALSE
    )
  }

  tried_to_remove <- bad_remove_fields[bad_remove_fields %in% names(dots)]
  invalid_fields <- names(dots)[!names(dots) %in% bad_remove_fields]

  if (length(tried_to_remove) > 0) {
    warning(
      "The following fields cannot be removed from an object of class `",
      paste0(extract$collection, "_extract"), "`: `",
      paste0(tried_to_remove, collapse = "`, `"), "`.\nTo ",
      "replace these values, use add_to_extract().",
      call. = FALSE
    )
  }

  if (length(invalid_fields) > 0) {
    warning(
      "The following were not recognized as valid fields for an object of ",
      "class `", paste0(extract$collection, "_extract"), "`: `",
      paste0(invalid_fields, collapse = "`, `"),
      "`. These values will be ignored.",
      call. = FALSE
    )
  }

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

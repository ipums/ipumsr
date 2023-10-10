# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Get contextual information about variables in an IPUMS data source
#'
#' @description
#' Summarize the variable metadata for the variables found in an [ipums_ddi]
#' object or data frame. Provides descriptions of variable
#' content (`var_label` and `var_desc`) as well as labels of particular
#' values for each variable (`val_labels`).
#'
#' `ipums_var_info()` produces a [`tibble`][tibble::tbl_df-class] summary
#' of multiple variables at once.
#'
#' `ipums_var_label()`, `ipums_var_desc()`, and `ipums_val_labels()` provide
#' specific metadata for a single variable.
#'
#' @details
#' For `ipums_var_info()`, if the provided `object` is a
#' [`haven::labelled()`][haven::labelled()]
#' vector (i.e. a single column from a data frame), the summary output will
#' include the variable label, variable description, and value labels, if
#' applicable.
#'
#' If it is a data frame, the same information will be
#' provided for all variables present in the data or to those indicated in
#' `vars`.
#'
#' If it is an [ipums_ddi] object, the summary will also
#' include information used when reading the data from disk, including
#' start/end positions for columns in the fixed-width file, implied decimals,
#' and variable types.
#'
#' Providing an `ipums_ddi` object is the most robust way to access
#' variable metadata, as many data processing operations will remove these
#' attributes from data frame-like objects.
#'
#' @param object An [ipums_ddi] object, a data frame containing variable
#'   metadata (as produced by most ipumsr data-reading functions), or
#'   a [`haven::labelled()`][haven::labelled()] vector from a
#'   single column in such a data frame.
#' @param vars,var A [tidyselect selection][selection_language] identifying
#'   the variable(s) to include in the output. Only `ipums_var_info()` allows
#'   for the selection of multiple variables.
#'
#' @export
#'
#' @seealso
#' [read_ipums_ddi()] or [read_nhgis_codebook()] to read IPUMS metadata files.
#'
#' @return
#' For `ipums_var_info()`, a [`tibble`][tibble::tbl_df-class] containing
#' variable information.
#'
#' Otherwise, a length-1 character vector with the requested variable
#' information.
#'
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))
#'
#' # Info for all variables in a data source
#' ipums_var_info(ddi)
#'
#' # Metadata for individual variables
#' ipums_var_desc(ddi, MONTH)
#'
#' ipums_var_label(ddi, MONTH)
#'
#' ipums_val_labels(ddi, MONTH)
#'
#' # NHGIS also supports variable-level metadata, though many fields
#' # are not relevant and remain blank:
#' cb <- read_nhgis_codebook(ipums_example("nhgis0972_csv.zip"))
#'
#' ipums_var_info(cb)
ipums_var_info <- function(object, vars = NULL) {
  UseMethod("ipums_var_info")
}

#' @export
ipums_var_info.default <- function(object, vars = NULL) {
  obj_info <- attributes(object)

  if (length(obj_info$labels) > 0) {
    value_labels <- list(tibble::tibble(
      val = unname(obj_info$labels),
      lbl = names(obj_info$labels)
    ))
  } else {
    value_labels <- list(tibble::tibble(
      val = numeric(0),
      lbl = character(0)
    ))
  }

  tibble::tibble(
    var_label = if (is.null(obj_info[["label"]])) {
      NA_character_
    } else {
      obj_info[["label"]]
    },
    var_desc = if (is.null(obj_info$var_desc)) {
      NA_character_
    } else {
      obj_info$var_desc
    },
    val_labels = value_labels
  )
}

#' @export
ipums_var_info.ipums_ddi <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- object$var_info
  out <- select_var_rows(out, vars)
  out
}

#' @export
ipums_var_info.data.frame <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- purrr::map(object, ~ ipums_var_info.default(.))
  names(out) <- names(object)
  out <- dplyr::bind_rows(out, .id = "var_name")
  out <- select_var_rows(out, vars)
  out
}

#' @export
ipums_var_info.list <- function(object, vars = NULL) {
  # For hierarchical list datasets
  vars <- enquo(vars)
  out <- purrr::map_df(object, ~ ipums_var_info(.))
  out <- dplyr::distinct(out, .data$var_name, .keep_all = TRUE)
  out <- select_var_rows(out, vars)
  out
}

#' @export
#' @rdname ipums_var_info
ipums_var_label <- function(object, var = NULL) {
  UseMethod("ipums_var_label")
}

#' @export
ipums_var_label.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) {
    rlang::warn("Found multiple variables. Giving variable label from first.")
  }

  out$var_label[1]
}

#' @export
#' @rdname ipums_var_info
ipums_var_desc <- function(object, var = NULL) {
  UseMethod("ipums_var_desc")
}

#' @export
ipums_var_desc.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) {
    rlang::warn(
      "Found multiple variables. Giving variable description from first."
    )
  }

  out$var_desc[1]
}

#' @export
#' @rdname ipums_var_info
ipums_val_labels <- function(object, var = NULL) {
  UseMethod("ipums_val_labels")
}

#' @export
ipums_val_labels.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) {
    rlang::warn("Found multiple variables. Giving value labels from first.")
  }

  out$val_labels[[1]]
}

#' @export
print.ipums_formatted_print <- function(x, ...) {
  custom_cat(x)
}

#' Get file information for an IPUMS extract
#'
#' @description
#' Get information about the IPUMS project, date, notes,
#' conditions, and citation requirements for an extract based on an
#' [ipums_ddi] object.
#'
#' `ipums_conditions()` is a convenience function that provides conditions and
#' citation information for a recently loaded dataset.
#'
#' @param object An `ipums_ddi` object.
#'
#'   For `ipums_conditions()`, leave `NULL` to display conditions for most
#'   recently loaded dataset.
#' @param type Type of file information to display. If `NULL`, loads all types.
#'   Otherwise, one of `"ipums_project"`, `"extract_date"`,
#'   `"extract_notes"`, `"conditions"` or `"citation"`.
#'
#' @return For `ipums_file_info()`, if `type = NULL`, a named list of metadata
#'   information. Otherwise, a string containing the requested information.
#'
#' @export
#'
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))
#'
#' ipums_file_info(ddi)
ipums_file_info <- function(object, type = NULL) {
  UseMethod("ipums_file_info")
}

#' @export
ipums_file_info.default <- function(object, type = NULL) {
  return(NULL)
}

#' @export
ipums_file_info.ipums_ddi <- function(object, type = NULL) {
  if (!is.null(type)) {
    out <- object[[type]]
  } else {
    fields <- c(
      "ipums_project", "extract_date", "extract_notes",
      "conditions", "citation"
    )

    out <- object[fields]
  }

  out
}

#' @rdname ipums_file_info
#' @export
ipums_conditions <- function(object = NULL) {
  if (is.null(object)) {
    out <- last_conditions_info$conditions

    if (is.null(out)) {
      out <- "No conditions available."
    }
  } else if (inherits(object, "ipums_ddi")) {
    out <- paste0(object$conditions, "\n\n")

    if (!is.null(object$citation)) {
      out <- paste0(out, object$citation, "\n\n")
    }
  } else {
    rlang::abort("Could not find ipums condition for object.")
  }

  class(out) <- "ipums_formatted_print"
  out
}

last_conditions_info <- new.env()

short_conditions_text <- function(ddi) {
  last_conditions_info$conditions <- ipums_conditions(ddi)

  paste0(
    "Use of data from ", ipums_file_info(ddi, "ipums_project"), " is subject ",
    "to conditions including that users should cite the data appropriately. ",
    "Use command `ipums_conditions()` for more details."
  )
}

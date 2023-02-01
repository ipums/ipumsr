# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS microdata extract
#'
#' @description
#' Read a microdata dataset downloaded from the IPUMS extract system using
#' the information in its associated DDI codebook (.xml) file.
#'
#' `read_ipums_micro()` and `read_ipums_micro_list()` differ in their handling
#' of extracts that contain multiple record types. See details.
#'
#' @details
#' IPUMS microdata extracts use two associated files: a DDI codebook (.xml)
#' file and a fixed-width data file. The DDI file contains metadata about the
#' associated data file which are used to parse its data correctly upon load.
#'
#' Data are loaded with value label and variable label information
#' attached to each column, where appropriate. See
#' [`haven::labelled()`][haven::labelled()].
#'
#' ## Data structures
#'
#' Files from IPUMS projects that contain data for multiple types of records
#' (e.g. household records and person records) may be either rectangular
#' or hierarchical.
#'
#' Rectangular data are transformed such that each row of data
#' represents only one type of record. For instance, each row will represent
#' a person record, and all household-level information for that person will
#' be included in the same row.
#'
#' Hierarchical data have records of
#' different types interspersed in a single file. For instance, a household
#' record will be included in its own row followed by the person records
#' associated with that household.
#'
#' Hierarchical data can be read in two different formats:
#' - `read_ipums_micro()` reads data into a [`tibble`][tibble::tbl_df-class]
#'   where each row represents a single record, regardless of record type.
#'   Variables that do not apply to a particular record type will be filled with
#'   `NA` in rows of that record type. For instance, a person-specific variable
#'   will be missing in all rows associated with household records.
#' - `read_ipums_micro_list()` reads data into a list of
#'   `tibble` objects, where each list element contains
#'   only one record type. Each list element is named with its corresponding
#'   record type.
#'
#' @param ddi Either a path to a DDI .xml file downloaded from
#'   [IPUMS](https://www.ipums.org/), or an
#'   [ipums_ddi] object parsed by [read_ipums_ddi()].
#' @param vars Names of variables to include in the output. Accepts a
#'   vector of names or a [selection helper][tidyselect::language].
#'   If `NULL`, includes all variables in the file.
#'
#'   For hierarchical data, the `RECTYPE` variable is always included even if
#'   unspecified.
#' @param n_max The maximum number of lines to read. For
#'   `read_ipums_micro_list()`, this applies before splitting records into
#'   list components.
#' @param data_file Path to the data (.gz) file associated with
#'   the provided `ddi` file. By default, looks for the data file in the same
#'   directory as the DDI file. If the data file has been moved, specify
#'   its location here.
#' @param verbose Logical indicating whether to print progress information
#'   to the console.
#' @param var_attrs Variable attributes from the DDI to add to the columns of
#'   the output data. Defaults to all available attributes (`"val_labels"`,
#'   `"var_label"`, and `"var_desc"`). See [set_ipums_var_attributes()]
#'   for more details.
#' @param lower_vars If reading a DDI from a file,
#'   a logical indicating whether to convert variable names to lowercase.
#'   Defaults to `FALSE` for consistency with IPUMS conventions.
#'
#'   This argument will be ignored if argument `ddi` is
#'   an [ipums_ddi] object. Use [read_ipums_ddi()] to convert variable
#'   names to lowercase when reading a DDI file.
#'
#'   If `lower_vars = TRUE` and `vars` is specified, `vars` should reference the
#'   converted column names.
#' @return `read_ipums_micro()` returns a single
#'   [`tibble`][tibble::tbl_df-class] object.
#'
#'   `read_ipums_micro_list()` returns a list of `tibble` objects with one
#'   entry for each record type.
#'
#' @family ipums_read
#' @export
#'
#' @examples
#' # Rectangular example file
#' cps_rect_ddi_file <- ipums_example("cps_00006.xml")
#'
#' cps <- read_ipums_micro(cps_rect_ddi_file)
#' # Or read DDI separately to keep the metadata
#' ddi <- read_ipums_ddi(cps_rect_ddi_file)
#' cps <- read_ipums_micro(ddi)
#'
#' # Hierarchical example file
#' cps_hier_ddi_file <- ipums_example("cps_00010.xml")
#'
#' # Read in "long" format and you get 1 data frame
#' cps_long <- read_ipums_micro(cps_hier_ddi_file)
#' head(cps_long)
#'
#' # Read in "list" format and you get a list of multiple data frames
#' cps_list <- read_ipums_micro_list(cps_hier_ddi_file)
#' head(cps_list$PERSON)
#' head(cps_list$HOUSEHOLD)
#'
#' # Or you can use the \code{%<-%} operator from zeallot to unpack
#' c(household, person) %<-% read_ipums_micro_list(cps_hier_ddi_file)
#' head(person)
#' head(household)
read_ipums_micro <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    warning(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) message(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv(
      data_file,
      col_types = col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (ddi_has_lowercase_var_names(ddi)) {
      out <- dplyr::rename_all(out, tolower)
    }
  } else {
    rt_info <- ddi_to_rtinfo(ddi)
    col_spec <- ddi_to_colspec(ddi, "long", verbose)
    out <- hipread::hipread_long(
      data_file,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      n_max = n_max,
      encoding = ddi$file_encoding
    )
  }

  out <- set_ipums_var_attributes(out, ddi, var_attrs)

  out
}

#' @export
#' @rdname read_ipums_micro
read_ipums_micro_list <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    warning(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) message(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  # rectype can be removed from ddi, so keep it for use later
  rt_ddi <- get_rt_ddi(ddi)
  ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv(
      data_file,
      col_types = col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (ddi_has_lowercase_var_names(ddi)) {
      out <- dplyr::rename_all(out, tolower)
    }
    if (verbose) cat("Assuming data rectangularized to 'P' record type")
    out <- list("P" = out)
  } else {
    rt_info <- ddi_to_rtinfo(rt_ddi)
    col_spec <- ddi_to_colspec(ddi, "list", verbose)
    out <- hipread::hipread_list(
      data_file,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      n_max = n_max,
      encoding = ddi$file_encoding
    )
    names(out) <- rectype_label_names(names(out), rt_ddi)
  }

  for (rt in names(out)) {
    out[[rt]] <- set_ipums_var_attributes(out[[rt]], ddi, var_attrs)
  }

  out
}

#' Warns the user that lower_vars has been ignored when they supply an ipums_ddi
#' to a data reading function
#' @noRd
check_if_lower_vars_ignored <- function(ddi, lower_vars) {
  inherits(ddi, "ipums_ddi") & lower_vars
}

lower_vars_ignored_warning <- function() {
  paste0(
    "Argument lower_vars was set to TRUE but has been ignored because ",
    "argument ddi is an ipums_ddi object. To obtain lowercase names in both ",
    "the ipums_ddi object and the data, set lower_vars to TRUE in your call ",
    "to function `read_ipums_ddi`."
  )
}

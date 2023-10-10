# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS microdata extract
#'
#' @description
#' Read a microdata dataset downloaded from the IPUMS extract system.
#'
#' Two files are required to load IPUMS microdata extracts:
#' - A [DDI codebook](https://ddialliance.org/learn/what-is-ddi) file
#'   (.xml) used to parse the extract's data file
#' - A data file (either .dat.gz or .csv.gz)
#'
#' See *Downloading IPUMS files* below for more information about
#' downloading these files.
#'
#' `read_ipums_micro()` and `read_ipums_micro_list()` differ in their handling
#' of extracts that contain multiple record types. See *Data structures*
#' below.
#'
#' Note that Stata, SAS, and SPSS file formats are not supported by
#' ipumsr readers. Convert your extract to fixed-width or CSV format, or see
#' [haven](https://haven.tidyverse.org/index.html) for help
#' loading these files.
#'
#' @section Data structures:
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
#' @section Downloading IPUMS files:
#'
#' You must download both the DDI codebook and the data file from the IPUMS
#' extract system to load the data into R. `read_ipums_micro_*()` functions
#' assume that the data file and codebook share a common base file name and
#' are present in the same directory. If this is not the case, provide a
#' separate path to the data file with the `data_file` argument.
#'
#' If using the IPUMS extract interface:
#' - Download the data file by clicking **Download .dat** under
#'   **Download Data**.
#' - Download the DDI codebook by right clicking on the **DDI** link in the
#'   **Codebook** column of the extract interface and selecting **Save as...**
#'   (on Safari, you may have to select **Download Linked File as...**).
#'   Be sure that the codebook is downloaded in .xml format.
#'
#' If using the IPUMS API:
#' - For supported collections, use [download_extract()] to download a completed
#'   extract via the IPUMS API. This automatically downloads both the DDI
#'   codebook and the data file from the extract and
#'   returns the path to the codebook file.
#'
#' @param ddi Either a path to a DDI .xml file downloaded from
#'   [IPUMS](https://www.ipums.org/), or an
#'   [ipums_ddi] object parsed by [read_ipums_ddi()]. See
#'   *Downloading IPUMS files* below.
#' @param vars Names of variables to include in the output. Accepts a
#'   vector of names or a [tidyselect selection][selection_language].
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
#' @param verbose Logical indicating whether to display IPUMS conditions and
#'   progress information.
#' @param var_attrs Variable attributes from the DDI to add to the columns of
#'   the output data. Defaults to all available attributes.
#'   See [set_ipums_var_attributes()] for more details.
#' @param lower_vars If reading a DDI from a file,
#'   a logical indicating whether to convert variable names to lowercase.
#'   Defaults to `FALSE` for consistency with IPUMS conventions.
#'
#'   This argument will be ignored if argument `ddi` is
#'   an [ipums_ddi] object. Use [read_ipums_ddi()] to convert variable
#'   names to lowercase when reading a DDI file.
#'
#'   If `lower_vars = TRUE` and `vars` is specified, `vars` should reference the
#'   lowercase column names.
#' @return `read_ipums_micro()` returns a single
#'   [`tibble`][tibble::tbl_df-class] object.
#'
#'   `read_ipums_micro_list()` returns a list of `tibble` objects with one
#'   entry for each record type.
#'
#' @seealso [read_ipums_micro_chunked()] and
#'   [read_ipums_micro_yield()] to read data from large IPUMS
#'   microdata extracts in chunks.
#'
#'   [read_ipums_ddi()] to read metadata associated with an IPUMS microdata
#'   extract.
#'
#'   [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#'   [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @export
#'
#' @examples
#' # Codebook for rectangular example file
#' cps_rect_ddi_file <- ipums_example("cps_00157.xml")
#'
#' # Load data based on codebook file info
#' cps <- read_ipums_micro(cps_rect_ddi_file)
#'
#' head(cps)
#'
#' # Can also load data from a pre-existing `ipums_ddi` object
#' # (This may be useful to retain codebook metadata even if lost from data
#' # during processing)
#' ddi <- read_ipums_ddi(cps_rect_ddi_file)
#' cps <- read_ipums_micro(ddi, verbose = FALSE)
#'
#' # Codebook for hierarchical example file
#' cps_hier_ddi_file <- ipums_example("cps_00159.xml")
#'
#' # Read in "long" format to get a single data frame
#' read_ipums_micro(cps_hier_ddi_file, verbose = FALSE)
#'
#' # Read in "list" format and you get a list of multiple data frames
#' cps_list <- read_ipums_micro_list(cps_hier_ddi_file)
#'
#' head(cps_list$PERSON)
#'
#' head(cps_list$HOUSEHOLD)
#'
#' # Use the `%<-%` operator from zeallot to unpack into separate objects
#' c(household, person) %<-% read_ipums_micro_list(cps_hier_ddi_file)
#'
#' head(person)
#'
#' head(household)
read_ipums_micro <- function(
    ddi,
    vars = NULL,
    n_max = Inf,
    data_file = NULL,
    verbose = TRUE,
    var_attrs = c("val_labels", "var_label", "var_desc"),
    lower_vars = FALSE) {
  if (!inherits(ddi, "ipums_ddi") && (file_is_zip(ddi) || file_is_dir(ddi))) {
    rlang::abort(
      "Expected `ddi` to be an `ipums_ddi` object or the path to an .xml file."
    )
  }

  if (check_if_lower_vars_ignored(ddi, lower_vars)) {
    rlang::warn(lower_vars_ignored_warning())
  }

  if (is.character(ddi)) {
    ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  }

  if (is.null(data_file)) {
    data_file <- file.path(ddi$file_path, ddi$file_name)
  }

  tryCatch(
    data_file <- custom_check_file_exists(
      data_file,
      c(".dat.gz", ".csv", ".csv.gz")
    ),
    error = function(cnd) {
      # Append hint to error message
      rlang::abort(
        c(
          conditionMessage(cnd),
          "i" = paste0(
            "Use `data_file` to specify the path to the data file associated ",
            "with the provided `ddi`."
          )
        ),
        call = expr(custom_check_file_exists())
      )
    }
  )

  if (verbose) {
    message(short_conditions_text(ddi))
  }

  vars <- enquo(vars)

  if (!is.null(var_attrs)) {
    var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  }

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") {
      rlang::abort("Hierarchical data cannot be read as csv.")
    }

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
    lower_vars = FALSE) {
  if (!inherits(ddi, "ipums_ddi") && (file_is_zip(ddi) || file_is_dir(ddi))) {
    rlang::abort(
      "Expected `ddi` to be an `ipums_ddi` object or the path to an .xml file."
    )
  }

  if (check_if_lower_vars_ignored(ddi, lower_vars)) {
    rlang::warn(lower_vars_ignored_warning())
  }

  if (is.character(ddi)) {
    ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  }

  # If the file type is rectangular, direct users to read_ipums_micro()
  if (ddi$file_type == "rectangular") {
    rlang::abort(
      c(
        "Data file must be hierarchical, not rectangular.",
        i = "For rectangular data, use `read_ipums_micro()`."
      )
    )
  }

  if (is.null(data_file)) {
    data_file <- file.path(ddi$file_path, ddi$file_name)
  }

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz") &
      ddi$file_type == "hierarchical") {
    rlang::abort("Hierarchical data cannot be read as csv.")
  }

  data_file <- custom_check_file_exists(
    data_file,
    c(".dat.gz", ".csv", ".csv.gz")
  )

  if (verbose) {
    message(short_conditions_text(ddi))
  }

  vars <- enquo(vars)

  if (!is.null(var_attrs)) {
    var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  }

  # rectype can be removed from ddi, so keep it for use later
  rt_ddi <- get_rt_ddi(ddi)
  ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

  rt_info <- ddi_to_rtinfo(rt_ddi)
  col_spec <- ddi_to_colspec(ddi, "list", verbose)

  tryCatch(
    out <- hipread::hipread_list(
      data_file,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      n_max = n_max,
      encoding = ddi$file_encoding
    ),
    error = function(cond) {
      rlang::abort(
        c(
          cond$message,
          i = paste0(
            "Try `read_ipums_micro()` to load this file as a single data frame."
          )
        ),
        call = expr(read_ipums_micro_list())
      )
    }
  )

  names(out) <- rectype_label_names(names(out), rt_ddi)

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

# TODO: Why is this? Should we add a way to alter the ipums_ddi object
# to be consistent with lower_vars = TRUE on load?
lower_vars_ignored_warning <- function() {
  c(
    "Setting `lower_vars = FALSE` because `ddi` is an `ipums_ddi` object.",
    "i" = paste0(
      "To obtain an `ipums_ddi` with lowercase variables, set ",
      "`lower_vars = TRUE` in `read_ipums_ddi()`."
    )
  )
}

# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS microdata extract in yields
#'
#' @description
#' Read a microdata dataset downloaded from the IPUMS extract system into an
#' object that can read and operate on a group ("yield") of lines at a time.
#' Use these functions to read a file that is too large to store in memory at
#' a single time. They represent a more flexible implementation of
#' [read_ipums_micro_chunked()] using R6.
#'
#' Two files are required to load IPUMS microdata extracts:
#' - A [DDI codebook](https://ddialliance.org/learn/what-is-ddi) file
#'   (.xml) used to parse the extract's data file
#' - A data file (either .dat.gz or .csv.gz)
#'
#' See *Downloading IPUMS files* below for more information about downloading
#' these files.
#'
#' `read_ipums_micro_yield()` and `read_ipums_micro_list_yield()` differ
#' in their handling of extracts that contain multiple record types.
#' See *Data structures* below.
#'
#' Note that these functions only support fixed-width (.dat) data files.
#'
#' # Methods summary:
#' These functions return a HipYield R6 object with the following methods:
#' - `yield(n = 10000)` reads the next "yield" from the
#'   data.
#'
#'   For `read_ipums_micro_yield()`, returns a [`tibble`][tibble::tbl_df-class]
#'   with up to `n` rows.
#'
#'   For `read_ipums_micro_list_yield()`, returns a list of tibbles with a
#'   total of up to `n` rows across list elements.
#'
#'   If fewer than `n` rows are left in the data, returns all remaining rows.
#'   If no rows are left in the data, returns `NULL`.
#' - `reset()` resets the data so that the next yield will read data from the
#'   start.
#' - `is_done()` returns a logical indicating whether all rows in the file
#'   have been read.
#' - `cur_pos` contains the next row number that will be read (1-indexed).
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
#' - `read_ipums_micro_yield()` produces an object that yields data as a
#'   [`tibble`][tibble::tbl_df-class] whose rows
#'   represent single records, regardless of record type. Variables that do
#'   not apply to a particular record type will be filled with `NA` in rows of
#'   that record type. For instance, a person-specific variable will be missing
#'   in all rows associated with household records.
#' - `read_ipums_micro_list_yield()` produces an object that yields data as a
#'   list of `tibble` objects, where each list element contains
#'   only one record type. Each list element is named with its corresponding
#'   record type. In this case, when using `yield()`, `n` refers to
#'   the total number of rows *across* record types, rather than in each
#'   record type.
#'
#' @inheritSection read_ipums_micro Downloading IPUMS files
#'
#' @return A HipYield R6 object (see Details section)
#'
#' @inheritParams read_ipums_micro
#'
#' @export
#'
#' @seealso [read_ipums_micro_chunked()] to read data from large IPUMS
#'   microdata extracts in chunks.
#'
#'   [read_ipums_micro()] to read data from an IPUMS microdata extract.
#'
#'   [read_ipums_ddi()] to read metadata associated with an IPUMS microdata
#'   extract.
#'
#'   [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#'   [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @examples
#' # Create an IpumsLongYield object
#' long_yield <- read_ipums_micro_yield(ipums_example("cps_00157.xml"))
#'
#' # Yield the first 10 rows of the data
#' long_yield$yield(10)
#'
#' # Yield the next 20 rows of the data
#' long_yield$yield(20)
#'
#' # Check the current position after yielding 30 rows
#' long_yield$cur_pos
#'
#' # Reset to the beginning of the file
#' long_yield$reset()
#'
#' # Use a loop to flexibly process the data in pieces. Count all Minnesotans:
#' total_mn <- 0
#'
#' while (!long_yield$is_done()) {
#'   cur_data <- long_yield$yield(1000)
#'   total_mn <- total_mn + sum(as_factor(cur_data$STATEFIP) == "Minnesota")
#' }
#'
#' total_mn
#'
#' # Can also read hierarchical data as list:
#' list_yield <- read_ipums_micro_list_yield(ipums_example("cps_00159.xml"))
#'
#' # Yield size is based on total rows for all list elements
#' list_yield$yield(10)
read_ipums_micro_yield <- function(
    ddi,
    vars = NULL,
    data_file = NULL,
    verbose = TRUE,
    var_attrs = c("val_labels", "var_label", "var_desc"),
    lower_vars = FALSE) {
  if (!inherits(ddi, "ipums_ddi") && (file_is_zip(ddi) || file_is_dir(ddi))) {
    rlang::abort(
      "Expected `ddi` to be an `ipums_ddi` object or the path to an .xml file."
    )
  }

  vars <- enquo(vars)

  if (!is.null(var_attrs)) {
    var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  }

  IpumsLongYield$new(
    ddi = ddi,
    vars = !!vars,
    data_file = data_file,
    verbose = verbose,
    var_attrs = var_attrs,
    lower_vars = lower_vars
  )
}

#' @export
#' @rdname read_ipums_micro_yield
read_ipums_micro_list_yield <- function(
    ddi,
    vars = NULL,
    data_file = NULL,
    verbose = TRUE,
    var_attrs = c("val_labels", "var_label", "var_desc"),
    lower_vars = FALSE) {
  if (!inherits(ddi, "ipums_ddi") && (file_is_zip(ddi) || file_is_dir(ddi))) {
    rlang::abort(
      "Expected `ddi` to be an `ipums_ddi` object or the path to an .xml file."
    )
  }

  vars <- enquo(vars)

  if (!is.null(var_attrs)) {
    var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  }

  IpumsListYield$new(
    ddi = ddi,
    vars = !!vars,
    data_file = data_file,
    verbose = verbose,
    var_attrs = var_attrs,
    lower_vars = lower_vars
  )
}


#' @export
#' @rdname read_ipums_micro_yield
IpumsLongYield <- R6::R6Class(
  "IpumsLongYield",
  inherit = hipread::HipLongYield,
  cloneable = FALSE,
  private = list(ddi = NULL, var_attrs = NULL),
  public = list(
    initialize = function(ddi,
                          vars = NULL,
                          data_file = NULL,
                          verbose = TRUE,
                          var_attrs = c("val_labels", "var_label", "var_desc"),
                          lower_vars = FALSE) {
      if (check_if_lower_vars_ignored(ddi, lower_vars)) {
        rlang::warn(lower_vars_ignored_warning())
      }

      if (is.character(ddi)) {
        ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
      }

      if (is.null(data_file)) {
        data_file <- file.path(ddi$file_path, ddi$file_name)
      }

      if (fostr_detect(data_file, "\\.csv$|\\.csv\\.gz$")) {
        rlang::abort(paste0(
          "`read_ipums_micro_yield()` does not support .csv ",
          "files, only .dat (or .dat.gz) files"
        ))
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

      ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

      rt_info <- ddi_to_rtinfo(ddi)
      col_spec <- ddi_to_colspec(ddi, "long", verbose)

      super$initialize(
        file = data_file,
        var_info = col_spec,
        rt_info = rt_info,
        encoding = ddi$encoding
      )

      private$ddi <- ddi
      private$var_attrs <- var_attrs
    },
    yield = function(n = 10000) {
      out <- super$yield(n = n)
      if (is.null(out)) {
        return(out)
      }
      out <- set_ipums_var_attributes(out, private$ddi, private$var_attrs)
      out
    }
  )
)

#' @export
#' @rdname read_ipums_micro_yield
IpumsListYield <- R6::R6Class(
  "IpumsListYield",
  inherit = hipread::HipListYield,
  cloneable = FALSE,
  private = list(ddi = NULL, var_attrs = NULL, rt_ddi = NULL),
  public = list(
    initialize = function(ddi,
                          vars = NULL,
                          data_file = NULL,
                          verbose = TRUE,
                          var_attrs = c("val_labels", "var_label", "var_desc"),
                          lower_vars = FALSE) {
      if (check_if_lower_vars_ignored(ddi, lower_vars)) {
        rlang::warn(lower_vars_ignored_warning())
      }

      if (is.character(ddi)) {
        ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
      }

      if (is.null(data_file)) {
        data_file <- file.path(ddi$file_path, ddi$file_name)
      }

      if (fostr_detect(data_file, "\\.csv$|\\.csv\\.gz$")) {
        rlang::abort(paste0(
          "`read_ipums_micro_yield()` does not support .csv ",
          "files, only .dat (or .dat.gz) files"
        ))
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

      rt_info <- ddi_to_rtinfo(ddi)
      col_spec <- ddi_to_colspec(ddi, "list", verbose)

      super$initialize(
        file = data_file,
        var_info = col_spec,
        rt_info = rt_info,
        encoding = ddi$encoding
      )

      private$ddi <- ddi
      private$var_attrs <- var_attrs
      private$rt_ddi <- rt_ddi
    },
    yield = function(n = 10000) {
      out <- super$yield(n = n)
      if (is.null(out)) {
        return(out)
      }
      names(out) <- rectype_label_names(names(out), private$rt_ddi)
      for (rt in names(out)) {
        out[[rt]] <- set_ipums_var_attributes(
          out[[rt]],
          private$ddi,
          private$var_attrs
        )
      }
      out
    }
  )
)

# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS extract by chunk
#'
#' @description
#' Read a microdata dataset downloaded from the IPUMS extract system in chunks.
#'
#' Use these functions to read a file that is too large to store in memory
#' at a single time. The file is processed in chunks of a given size, with a
#' provided callback function applied to each chunk.
#'
#' See [read_ipums_micro_yield()] for an alternate approach to reading large
#' files.
#'
#' `read_ipums_micro_chunked()` and `read_ipums_micro_list_chunked()` differ
#' in their handling of extracts that contain multiple record types.
#' See details.
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
#' - `read_ipums_micro_chunked()` reads each chunk of data into a
#'   [`tibble`][tibble::tbl_df-class] where each row represents a single record,
#'   regardless of record type. Variables that do not apply to a particular
#'   record type will be filled with `NA` in rows of that record type. For
#'   instance, a person-specific variable will be missing in all rows
#'   associated with household records. The provided `callback` function should
#'   therefore operate on a `tibble` object.
#' - `read_ipums_micro_list_chunked()` reads each chunk of data into a list of
#'   `tibble` objects, where each list element contains
#'   only one record type. Each list element is named with its corresponding
#'   record type. The provided `callback` function should therefore operate
#'   on a list object.
#'
#' Callback functions should include both a data and position argument. See
#' examples.
#'
#' @inheritParams read_ipums_micro
#' @param callback An [ipums_callback] object, or a function
#'   that will be converted to an `IpumsSideEffectCallback` object.
#' @param chunk_size Integer number of observations to
#'   read per chunk. Higher values use more RAM, but
#'   typically result in faster processing. Defaults to 10,000.
#' @param lower_vars If reading a DDI from a file,
#'   a logical indicating whether to convert variable names to lowercase.
#'   Defaults to `FALSE` for consistency with IPUMS conventions.
#'
#'   This argument will be ignored if argument `ddi` is
#'   an [ipums_ddi] object. Use [read_ipums_ddi()] to convert variable
#'   names to lowercase when reading a DDI file.
#'
#'   Note that if reading in chunks from a .csv or
#'   .csv.gz file, the callback function will be called *before* variable names
#'   are converted to lowercase, and thus should reference uppercase variable
#'   names.
#'
#' @return Depends on the provided callback object. See [ipums_callback].
#'
#' @export
#'
#' @family ipums_read
#'
#' @examples
#' # Select Minnesotan cases from CPS example (Note you can also accomplish
#' # this and avoid having to even download a huge file using the "Select Cases"
#' # functionality of the IPUMS extract system)
#' mn_only <- read_ipums_micro_chunked(
#'   ipums_example("cps_00006.xml"),
#'   IpumsDataFrameCallback$new(function(x, pos) {
#'     x[x$STATEFIP == 27, ]
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' )
#'
#' # Tabulate INCTOT average by state without storing full dataset in memory
#' library(dplyr)
#' inc_by_state <- read_ipums_micro_chunked(
#'   ipums_example("cps_00006.xml"),
#'   IpumsDataFrameCallback$new(function(x, pos) {
#'     x %>%
#'       mutate(
#'         INCTOT = lbl_na_if(
#'           INCTOT, ~.lbl %in% c("Missing.", "N.I.U. (Not in Universe)."))
#'         ) %>%
#'       filter(!is.na(INCTOT)) %>%
#'       group_by(STATEFIP = as_factor(STATEFIP)) %>%
#'       summarize(INCTOT_SUM = sum(INCTOT), n = n(), .groups = "drop")
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' ) %>%
#' group_by(STATEFIP) %>%
#' summarize(avg_inc = sum(INCTOT_SUM) / sum(n))
#'
#' # x will be a list when using `read_ipums_micro_list_chunked()`
#' read_ipums_micro_list_chunked(
#'   ipums_example("cps_00010.xml"),
#'   IpumsSideEffectCallback$new(function(x, pos) {
#'     print(paste0(nrow(x$PERSON), " persons and ", nrow(x$HOUSEHOLD), " households in this chunk."))
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' )
#'
#' # Using the biglm package, you can even run a regression without storing
#' # the full dataset in memory
#' library(dplyr)
#' if (require(biglm)) {
#'   lm_results <- read_ipums_micro_chunked(
#'     ipums_example("cps_00015.xml"),
#'     IpumsBiglmCallback$new(
#'       INCTOT ~ AGE + HEALTH, # Simple regression (may not be very useful)
#'       function(x, pos) {
#'         x %>%
#'         mutate(
#'           INCTOT = lbl_na_if(
#'             INCTOT, ~.lbl %in% c("Missing.", "N.I.U. (Not in Universe).")
#'           ),
#'           HEALTH = as_factor(HEALTH)
#'         )
#'     }),
#'     chunk_size = 1000 # Generally you want this larger, but this example is a small file
#'   )
#'   summary(lm_results)
#' }
#'
read_ipums_micro_chunked <- function(
  ddi,
  callback,
  chunk_size = 10000,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    rlang::warn(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) message(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  if (is.function(callback)) callback <- IpumsSideEffectCallback$new(callback)

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (!is.null(callback$set_ipums_fields)) {
    if (is.null(ddi$rectype_idvar)) {
      rec_vinfo <- NULL
    } else {
      rec_vinfo <- dplyr::filter(ddi$var_info, .data$var_name == ddi$rectype_idvar)
    }
    callback$set_ipums_fields("long", ddi, var_attrs, ddi)
  }

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") {
      rlang::abort("Hierarchical data cannot be read as csv.")
    }
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv_chunked(
      data_file,
      callback,
      chunk_size,
      col_types = col_types,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (ddi_has_lowercase_var_names(ddi)) {
      out <- dplyr::rename_all(out, tolower)
    }
  } else {
    rt_info <- ddi_to_rtinfo(ddi)
    col_spec <- ddi_to_colspec(ddi, "long", verbose)
    out <- hipread::hipread_long_chunked(
      data_file,
      callback,
      chunk_size,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      encoding = ddi$file_encoding
    )
  }

  out
}

#' @export
#' @rdname read_ipums_micro_chunked
read_ipums_micro_list_chunked <- function(
  ddi,
  callback,
  chunk_size = 10000,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    rlang::warn(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) message(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  if (is.function(callback)) callback <- IpumsSideEffectCallback$new(callback)

  # rectype can be removed from ddi, so keep it for use later
  rt_ddi <- get_rt_ddi(ddi)
  ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

  if (!is.null(callback$set_ipums_fields)) {
    callback$set_ipums_fields("list", ddi, var_attrs, rt_ddi)
  }

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") {
      rlang::abort("Hierarchical data cannot be read as csv.")
    }
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv_chunked(
      data_file,
      callback,
      chunk_size,
      col_types = col_types,
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
    out <- hipread::hipread_list_chunked(
      data_file,
      callback,
      chunk_size,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      encoding = ddi$file_encoding
    )
  }

  out
}

ipumsify_data <- function(
  data, data_structure, ddi, var_attrs, rt_ddi
) {
  if (data_structure == "long") {
    out <- set_ipums_var_attributes(data, ddi$var_info, var_attrs)
  } else if (data_structure == "list") {
    out <- data
    for (rt in names(out)) {
      out[[rt]] <- set_ipums_var_attributes(out[[rt]], ddi, var_attrs)
    }
    names(out) <- rectype_label_names(names(out), rt_ddi)
  } else {
    rlang::abort(
      paste0("Don't know what to do with data structure: ", data_structure)
    )
  }
  out
}

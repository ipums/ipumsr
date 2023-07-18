# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS microdata extract by chunk
#'
#' @description
#' Read a microdata dataset downloaded from the IPUMS extract system in chunks.
#'
#' Use these functions to read a file that is too large to store in memory
#' at a single time. The file is processed in chunks of a given size, with a
#' provided callback function applied to each chunk.
#'
#' Two files are required to load IPUMS microdata extracts:
#' - A [DDI codebook](https://ddialliance.org/learn/what-is-ddi) file
#'   (.xml) used to parse the extract's data file
#' - A data file (either .dat.gz or .csv.gz)
#'
#' See *Downloading IPUMS files* below for more information about downloading
#' these files.
#'
#' `read_ipums_micro_chunked()` and `read_ipums_micro_list_chunked()` differ
#' in their handling of extracts that contain multiple record types.
#' See *Data structures* below.
#'
#' Note that Stata, SAS, and SPSS file formats are not supported by
#' ipumsr readers. Convert your extract to fixed-width or CSV format, or see
#' [haven](https://haven.tidyverse.org/index.html) for help
#' loading these files.
#'
#' @inheritSection read_ipums_micro Downloading IPUMS files
#'
#' @section Data structures:
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
#'   on a list object. In this case, the chunk size references the total
#'   number of rows *across* record types, rather than in each
#'   record type.
#'
#' @inheritParams read_ipums_micro
#' @param callback An [ipums_callback] object, or a function
#'   that will be converted to an `IpumsSideEffectCallback` object. Callback
#'   functions should include both data (`x`) and position (`pos`) arguments.
#'   See examples.
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
#' @seealso [read_ipums_micro_yield()] for more flexible handling of large
#'   IPUMS microdata files.
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
#' suppressMessages(library(dplyr))
#'
#' # Example codebook file
#' cps_rect_ddi_file <- ipums_example("cps_00157.xml")
#'
#' # Function to extract Minnesota cases from CPS example
#' # (This can also be accomplished by including case selections
#' # in an extract definition)
#' #
#' # Function must take `x` and `pos` to refer to data and row position,
#' # respectively.
#' filter_mn <- function(x, pos) {
#'   x[x$STATEFIP == 27, ]
#' }
#'
#' # Initialize callback
#' filter_mn_callback <- IpumsDataFrameCallback$new(filter_mn)
#'
#' # Process data in chunks, filtering to MN cases in each chunk
#' read_ipums_micro_chunked(
#'   cps_rect_ddi_file,
#'   callback = filter_mn_callback,
#'   chunk_size = 1000,
#'   verbose = FALSE
#' )
#'
#' # Tabulate INCTOT average by state without storing full dataset in memory
#' read_ipums_micro_chunked(
#'   cps_rect_ddi_file,
#'   callback = IpumsDataFrameCallback$new(
#'     function(x, pos) {
#'       x %>%
#'         mutate(
#'           INCTOT = lbl_na_if(
#'             INCTOT,
#'             ~ grepl("Missing|N.I.U.", .lbl)
#'           )
#'         ) %>%
#'         filter(!is.na(INCTOT)) %>%
#'         group_by(STATEFIP = as_factor(STATEFIP)) %>%
#'         summarize(INCTOT_SUM = sum(INCTOT), n = n(), .groups = "drop")
#'     }
#'   ),
#'   chunk_size = 1000,
#'   verbose = FALSE
#' ) %>%
#'   group_by(STATEFIP) %>%
#'   summarize(avg_inc = sum(INCTOT_SUM) / sum(n))
#'
#' # `x` will be a list when using `read_ipums_micro_list_chunked()`
#' read_ipums_micro_list_chunked(
#'   ipums_example("cps_00159.xml"),
#'   callback = IpumsSideEffectCallback$new(function(x, pos) {
#'     print(
#'       paste0(
#'         nrow(x$PERSON), " persons and ",
#'         nrow(x$HOUSEHOLD), " households in this chunk."
#'       )
#'     )
#'   }),
#'   chunk_size = 1000,
#'   verbose = FALSE
#' )
#'
#' # Using the biglm package, you can even run a regression without storing
#' # the full dataset in memory
#' lm_results <- read_ipums_micro_chunked(
#'   ipums_example("cps_00160.xml"),
#'   IpumsBiglmCallback$new(
#'     INCTOT ~ AGE + HEALTH, # Model formula
#'     function(x, pos) {
#'       x %>%
#'         mutate(
#'           INCTOT = lbl_na_if(
#'             INCTOT,
#'             ~ grepl("Missing|N.I.U.", .lbl)
#'           ),
#'           HEALTH = as_factor(HEALTH)
#'         )
#'     }
#'   ),
#'   chunk_size = 1000,
#'   verbose = FALSE
#' )
#'
#' summary(lm_results)
read_ipums_micro_chunked <- function(
    ddi,
    callback,
    chunk_size = 10000,
    vars = NULL,
    data_file = NULL,
    verbose = TRUE,
    var_attrs = c("val_labels", "var_label", "var_desc"),
    lower_vars = FALSE) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    rlang::warn(lower_vars_ignored_warning())
  }

  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

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

  if (is.function(callback)) {
    callback <- IpumsSideEffectCallback$new(callback)
  }

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (!is.null(callback$set_ipums_fields)) {
    if (is.null(ddi$rectype_idvar)) {
      rec_vinfo <- NULL
    } else {
      rec_vinfo <- dplyr::filter(
        ddi$var_info,
        .data$var_name == ddi$rectype_idvar
      )
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
    lower_vars = FALSE) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    rlang::warn(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(
    data_file,
    c(".dat.gz", ".csv", ".csv.gz")
  )

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

ipumsify_data <- function(data,
                          data_structure,
                          ddi,
                          var_attrs,
                          rt_ddi) {
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

# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read tabular data from an NHGIS extract
#'
#' @description
#' Read a csv or fixed-width (.dat) file downloaded from the NHGIS extract
#' system.
#'
#' To read spatial data from an NHGIS extract, use [read_ipums_sf()].
#'
#' @details
#' The .do file that is included when downloading an NHGIS fixed-width
#' extract contains the necessary metadata (e.g. column positions and implicit
#' decimals) to correctly parse the data file. `read_nhgis()` uses this
#' information to parse and recode the fixed-width data appropriately.
#'
#' If you no longer have access to the .do file, consider resubmitting the
#' extract that produced the data. You can also change the desired data
#' format to produce a .csv file, which does not require additional metadata
#' files to be loaded.
#'
#' For more about resubmitting an existing extract via the IPUMS API, see
#' `vignette("ipums-api", package = "ipumsr")`.
#'
#' @param data_file Path to a .zip archive containing an NHGIS extract or
#'   a single file from an NHGIS extract.
#' @param file_select If `data_file` is a .zip archive that
#'   contains multiple files, an expression identifying the file to load.
#'   Accepts a character vector specifying the
#'   file name, a [tidyselect selection][selection_language], or an index
#'   position. This must uniquely identify a file.
#' @param vars Names of variables to include in the output. Accepts a
#'   vector of names or a [tidyselect selection][selection_language].
#'   If `NULL`, includes all variables in the file.
#' @param col_types One of `NULL`, a [`cols()`][readr::cols]
#'   specification or a string. If `NULL`, all column types will be inferred
#'   from the values in the first `guess_max` rows of each column.
#'   Alternatively, you can use a compact string representation to specify
#'   column types:
#'
#'   - c = character
#'   - i = integer
#'   - n = number
#'   - d = double
#'   - l = logical
#'   - f = factor
#'   - D = date
#'   - T = date time
#'   - t = time
#'   - ? = guess
#'   - _ or - = skip
#'
#'   See [`read_delim()`][readr::read_delim] for more details.
#' @param n_max Maximum number of lines to read.
#' @param guess_max For .csv files, maximum number of lines to use for guessing
#'   column types. Will never use more than the number of lines read.
#' @param do_file For fixed-width files, path to the .do file associated with
#'   the provided `data_file`. The .do file contains the parsing instructions
#'   for the data file.
#'
#'   By default, looks in the same path as `data_file` for
#'   a .do file with the same name. See Details section below.
#' @param var_attrs Variable attributes to add from the codebook (.txt) file
#'   included in the extract. Defaults to all available attributes.
#'
#'   See [`set_ipums_var_attributes()`] for more details.
#' @param remove_extra_header If `TRUE`, remove the additional descriptive
#'   header row included in some NHGIS .csv files.
#'
#'   This header row is not
#'   usually needed as it contains similar information to that
#'   included in the `"label"` attribute of each data column (if `var_attrs`
#'   includes `"var_label"`).
#' @param verbose Logical controlling whether to display output when loading
#'   data. If `TRUE`, displays IPUMS conditions, a progress bar, and
#'   column types. Otherwise, all are suppressed.
#'
#'   Will be overridden by `readr.show_progress` and `readr.show_col_types`
#'   options, if they are set.
#' @param data_layer `r lifecycle::badge("deprecated")` Please
#'   use `file_select` instead.
#'
#' @return A [`tibble`][tibble::tbl_df-class] containing the data found in
#'   `data_file`
#'
#' @seealso
#' [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#' [read_nhgis_codebook()] to read metadata about an IPUMS NHGIS extract.
#'
#' [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @export
#'
#' @examples
#' # Example files
#' csv_file <- ipums_example("nhgis0972_csv.zip")
#' fw_file <- ipums_example("nhgis0730_fixed.zip")
#'
#' # Provide the .zip archive directly to load the data inside:
#' read_nhgis(csv_file)
#'
#' # For extracts that contain multiple files, use `file_select` to specify
#' # a single file to load. This accepts a tidyselect expression:
#' read_nhgis(fw_file, file_select = matches("ds239"), verbose = FALSE)
#'
#' # Or an index position:
#' read_nhgis(fw_file, file_select = 2, verbose = FALSE)
#'
#' # For CSV files, column types are inferred from the data. You can
#' # manually specify column types with `col_types`. This may be useful for
#' # geographic codes, which should typically be interpreted as character values
#' read_nhgis(csv_file, col_types = list(MSA_CMSAA = "c"), verbose = FALSE)
#'
#' # Fixed-width files are parsed with the correct column positions
#' # and column types automatically:
#' read_nhgis(fw_file, file_select = contains("ts"), verbose = FALSE)
#'
#' # You can also read in a subset of the data file:
#' read_nhgis(
#'   csv_file,
#'   n_max = 15,
#'   vars = c(GISJOIN, YEAR, D6Z002),
#'   verbose = FALSE
#' )
read_nhgis <- function(data_file,
                       file_select = NULL,
                       vars = NULL,
                       col_types = NULL,
                       n_max = Inf,
                       guess_max = min(n_max, 1000),
                       do_file = NULL,
                       var_attrs = c("val_labels", "var_label", "var_desc"),
                       remove_extra_header = TRUE,
                       verbose = TRUE,
                       data_layer = deprecated()) {
  if (length(data_file) != 1) {
    rlang::abort("`data_file` must be length 1")
  }

  dir_read_deprecated(data_file)

  if (!missing(data_layer)) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "read_nhgis(data_layer = )",
      "read_nhgis(file_select = )",
    )
    file_select <- enquo(data_layer)
  } else {
    file_select <- enquo(file_select)
  }

  custom_check_file_exists(data_file)

  data_files <- find_files_in(
    data_file,
    name_ext = "csv|dat",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  has_csv <- any(grepl("[.]csv$", data_files))
  has_dat <- any(grepl("[.]dat$", data_files))

  if (!has_csv && !has_dat) {
    rlang::abort("No .csv or .dat files found in the provided `data_file`.")
  } else if (has_csv && has_dat) {
    rlang::abort(c(
      "Both .csv and .dat files found in the provided `data_file`.",
      "x" = paste0(
        "Only one type of data file can be present in the provided `data_file`."
      )
    ))
  }

  if (has_csv) {
    data <- read_nhgis_csv(
      data_file,
      file_select = !!file_select,
      col_types = col_types,
      col_select = !!enquo(vars),
      n_max = n_max,
      var_attrs = var_attrs,
      remove_extra_header = remove_extra_header,
      na = c("", "NA"),
      verbose = verbose
    )
  } else {
    data <- read_nhgis_fwf(
      data_file,
      file_select = !!file_select,
      col_types = col_types,
      col_select = !!enquo(vars),
      var_attrs = var_attrs,
      do_file = do_file,
      n_max = n_max,
      verbose = verbose,
      na = c(".", "", "NA")
    )
  }

  data
}

# Internal ---------------------

read_nhgis_fwf <- function(data_file,
                           file_select = NULL,
                           do_file = NULL,
                           col_types = NULL,
                           var_attrs = c("val_labels", "var_label", "var_desc"),
                           verbose = TRUE,
                           ...) {
  col_spec <- NULL

  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "dat",
    file_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  if (file_is_zip(data_file)) {
    # Cannot use fwf_empty() col_positions on an unz() connection
    # Must unzip file to allow for default fwf_empty() specification
    fwf_dir <- tempfile()

    on.exit(
      unlink(fwf_dir, recursive = TRUE),
      add = TRUE,
      after = FALSE
    )

    utils::unzip(data_file, exdir = fwf_dir)

    file <- file.path(fwf_dir, file)
  } else if (file_is_dir(data_file)) {
    file <- file.path(data_file, file)
  }

  default_do_file <- is_null(do_file)

  # Assume that a provided `do_file` is a relative path if `data_file` is
  # zipped. Otherwise, full path must be provided.
  if (file_is_zip(data_file)) {
    do_file <- do_file %||% fostr_replace(basename(file), "\\.dat$", ".do")
    do_file <- fostr_replace(file, basename(file), do_file)
  } else {
    do_file <- do_file %||% fostr_replace(file, "\\.dat$", ".do")
  }

  if (!file.exists(do_file)) {
    if (!default_do_file) {
      rlang::abort("Could not find the provided `do_file`.")
    } else {
      rlang::abort(c(
        "Could not find a .do file associated with the provided data file.",
        "i" = "Use the `do_file` argument to provide the path to the .do file."
      ))
    }
  } else {
    col_spec <- tryCatch(
      parse_nhgis_do_file(do_file),
      error = function(cnd) {
        rlang::abort(
          c(
            "Unexpected error parsing .do file",
            "x" = paste0(
              "This may occur if files have been reorganized from the original",
              " .zip format provided by NHGIS."
            ),
            "i" = paste0(
              "Check that `file_select` matches the intended file ",
              "or consider re-downloading this extract in .csv format."
            )
          ),
          call = expr(read_nhgis_fwf())
        )
      }
    )
  }

  cb_ddi_info <- load_codebook(
    data_file,
    filename = file,
    verbose = !is_null(var_attrs)
  )

  if (verbose) {
    message(short_conditions_text(cb_ddi_info))
  }

  data <- readr::read_fwf(
    file,
    col_positions = col_spec$col_positions,
    col_types = col_types %||% col_spec$col_types,
    locale = ipums_locale(cb_ddi_info$file_encoding),
    progress = show_readr_progress(verbose),
    show_col_types = show_readr_coltypes(verbose),
    ...
  )

  if (!is_null(col_spec$col_recode)) {
    # Rescale column values based on expressions in .do file
    purrr::walk2(
      col_spec$col_recode$cols,
      col_spec$col_recode$exprs,
      function(col, expr) {
        if (!is_null(data[[col]])) {
          # Coerce to numeric to guard against user-specified col_types
          data[[col]] <<- as.numeric(data[[col]])
          data[[col]] <<- eval(expr, data)
        }
      }
    )
  }

  data <- set_ipums_var_attributes(data, cb_ddi_info$var_info, var_attrs)

  data
}

read_nhgis_csv <- function(data_file,
                           file_select = NULL,
                           var_attrs = c("val_labels", "var_label", "var_desc"),
                           remove_extra_header = TRUE,
                           verbose = TRUE,
                           ...) {
  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "csv",
    file_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  cb_ddi_info <- load_codebook(
    data_file,
    filename = file,
    verbose = !is_null(var_attrs)
  )

  if (verbose) {
    message(short_conditions_text(cb_ddi_info))
  }

  if (file_is_zip(data_file)) {
    file <- unz(data_file, file)
  } else if (file_is_dir(data_file)) {
    file <- file.path(data_file, file)
  }

  header_info <- check_header_row(data_file, file_select = !!file_select)

  # Skip to avoid loading extra header
  # We will reattach correct column names when we load the data.
  if (header_info$has_extra_header && remove_extra_header) {
    skip <- 2
  } else {
    skip <- 1
  }

  data <- readr::read_csv(
    file,
    skip = skip,
    col_names = header_info$col_names, # Reattach skipped colnames
    locale = ipums_locale(cb_ddi_info$file_encoding),
    progress = show_readr_progress(verbose),
    show_col_types = show_readr_coltypes(verbose),
    ...
  )

  data <- set_ipums_var_attributes(data, cb_ddi_info$var_info, var_attrs)

  data
}

#' Load a codebook associated with a provided NHGIS data file
#'
#' Helper to load a codebook associated with a provided data file.
#' This is designed to handle a codebook that is bundled with a data file when
#' loading that data file. To load a codebook .txt file directly, see
#' `read_nhgis_codebook()`.
#'
#' This function is able to identify the correct codebook for a given data
#' file regardless of whether the data file is zipped or is the direct path
#' to a file. Codebooks are matched to data files by name, where the codebook
#' has the same file name with `_codebook` appended.
#'
#' An empty codebook is provided if no matching codebook can be found.
#'
#' @param data_file Path to a data file or a .zip archive containing an NHGIS
#'   extract.
#' @param filename Name of the .csv or .dat file to be loaded within the
#'   `data_file` zip archive. This allows codebooks to be identified when
#'   `data_file` contains multiple files.
#'
#'   We do not use `file_select` directly because it would not capture
#'   the case in which a string is provided containing the full .csv or .dat
#'   file name.
#' @param verbose Logical indicating whether to warn if codebook cannot be
#'   loaded.
#'
#' @return An `ipums_ddi` object
#'
#' @noRd
load_codebook <- function(data_file, filename, verbose = FALSE) {
  cb_files <- find_files_in(
    data_file,
    name_ext = "txt",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  if (length(cb_files) > 0) {
    # If any .txt files, find the one with the same base name as
    # the file being loaded
    cb_file <- fostr_subset(
      cb_files,
      fostr_replace(basename(filename), ipums_file_ext(filename), "")
    )
  }

  cb_ddi_info <- try(
    read_nhgis_codebook(data_file, file_select = tidyselect::all_of(cb_file)),
    silent = TRUE
  )

  cb_error <- inherits(cb_ddi_info, "try-error")

  if (cb_error) {
    # If error, a direct file path may have been provided.
    # Attempt to load codebook for file with same base name in same directory
    filename <- fostr_replace(
      data_file,
      paste0(ipums_file_ext(data_file), "$"),
      "_codebook.txt"
    )

    cb_ddi_info <- try(
      read_nhgis_codebook(filename),
      silent = TRUE
    )

    cb_error <- inherits(cb_ddi_info, "try-error")
  }

  # If still no codebook info, return empty DDI
  if (cb_error) {
    cb_ddi_info <- NHGIS_EMPTY_DDI

    if (verbose) {
      rlang::warn(
        c(
          "Unable to read codebook associated with this file.",
          "i" = "To load a codebook manually, use `read_nhgis_codebook()`.",
          "i" = paste0(
            "To attach codebook information to loaded data, ",
            "use `set_ipums_var_attributes()`."
          )
        )
      )
    }
  }

  # Specify encoding (assuming all nhgis extracts are ISO-8859-1 eg latin1
  # because an extract with county names has n with tildes and so is can
  # be verified as ISO-8859-1)
  cb_ddi_info$file_encoding <- "ISO-8859-1"

  cb_ddi_info
}

check_header_row <- function(data_file, file_select = NULL) {
  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "csv",
    file_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  if (file_is_zip(data_file)) {
    file <- unz(data_file, file)
  } else if (file_is_dir(data_file)) {
    file <- file.path(data_file, file)
  }

  # Read first row to determine if this data contains the NHGIS
  # "expanded" header row
  header_row <- readr::read_csv(
    file,
    n_max = 1,
    col_types = readr::cols(.default = readr::col_guess()),
    progress = FALSE,
    show_col_types = FALSE,
    na = c("", "NA")
  )

  has_extra_header <- all(purrr::map_lgl(header_row, is.character))
  header_vals <- unname(unlist(header_row))

  list(
    has_extra_header = has_extra_header,
    header_vals = header_vals,
    col_names = colnames(header_row)
  )
}

parse_nhgis_do_file <- function(file) {
  do_lines <- trimws(readr::read_lines(file, progress = FALSE))

  col_spec <- parse_col_positions(do_lines)
  col_recode <- parse_col_recode(do_lines)

  list(
    col_types = col_spec$col_types,
    col_positions = col_spec$col_positions,
    col_recode = col_recode
  )
}

parse_col_recode <- function(do_lines) {
  recode_lines <- which(grepl("^replace", do_lines))

  if (length(recode_lines) == 0) {
    return(NULL)
  }

  recode_vals <- toupper(
    fostr_replace(do_lines[recode_lines], "^replace ", "")
  )

  recode_vals <- fostr_split(recode_vals, "( +)?=( +)?")

  cols <- purrr::map_chr(recode_vals, purrr::pluck(1))
  exprs <- purrr::map_chr(recode_vals, purrr::pluck(2))

  list(
    cols = cols,
    exprs = rlang::parse_exprs(exprs)
  )
}

parse_col_positions <- function(do_lines) {
  # Get positions and labels
  start <- which(grepl("^quietly", do_lines)) + 1
  end <- which(grepl("^using", do_lines)) - 1

  col_info <- fostr_split(do_lines[start:end], "\\s{2,}")

  col_types <- convert_col_types(purrr::map_chr(col_info, purrr::pluck(1)))
  col_name <- toupper(purrr::map_chr(col_info, purrr::pluck(2)))
  col_index <- fostr_split(purrr::map_chr(col_info, purrr::pluck(3)), "-")

  col_start <- as.numeric(purrr::map_chr(col_index, purrr::pluck(1)))
  col_end <- as.numeric(purrr::map_chr(col_index, purrr::pluck(2)))

  list(
    col_types = col_types,
    col_positions = readr::fwf_positions(col_start, col_end, col_name)
  )
}

convert_col_types <- function(types) {
  types <- fostr_replace(types, "^str.+", "str")

  recode_key <- c(
    str = "c",
    byte = "i",
    int = "i",
    long = "i",
    float = "d",
    double = "d"
  )

  paste0(dplyr::recode(types, !!!recode_key), collapse = "")
}

warn_default_fwf_parsing <- function() {
  rlang::warn(
    c(
      paste0(
        "Data loaded from NHGIS fixed-width files may not be consistent with ",
        "the information included in the data codebook when parsing column ",
        "positions manually."
      ),
      "i" = paste0(
        "Please consult the .txt and .do files associated with this extract ",
        "to ensure data is recoded correctly."
      )
    )
  )
}

# Fills in a default condition if we can't find codebook for nhgis
NHGIS_EMPTY_DDI <- new_ipums_ddi(
  ipums_project = "NHGIS",
  file_type = "rectangular",
  conditions = paste0(
    "Use of data from NHGIS is subject to conditions including that users ",
    "should cite the data appropriately. ",
    "Please see www.nhgis.org for more information."
  )
)

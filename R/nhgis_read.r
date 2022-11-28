# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an NHGIS extract
#'
#' @description
#' Load a dataset downloaded from the NHGIS extract system.
#'
#' @param data_file Path to the data file, a .zip archive from an NHGIS
#'   extract, or a directory containing the data file.
#' @param data_layer For .zip extracts with multiple datasets, the name of the
#'   data to load. Accepts a character vector specifying the file name, or
#'  [`dplyr_select_style`] conventions. Data layer must uniquely identify
#'  a dataset.
#' @param var_attrs Variable attributes to add from the codebook, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   [`set_ipums_var_attributes()`] for more details.
#' @param show_conditions If `TRUE`, print IPUMS conditions to console when
#'   loading data. Defaults to `TRUE`.
#' @param na Character vector of strings to interpret as missing values.
#'   If `NULL`, defaults to `c("", "NA")` for csv files and `c(".", "", "NA")`
#'   for fixed-width files. See [`read_csv()`][readr::read_csv].
#' @param do_file For fixed-width files, path to the .do file associated with
#'   the provided `data_file`. The .do file contains the specifications that
#'   indicate how to parse the fixed-width file to be loaded.
#'   If `NULL`, looks in the directory of the .dat file to be loaded for a .do
#'   file with the same name. If `FALSE` or if the .do file cannot be found,
#'   the .dat file will be parsed by the
#'   values provided to `col_positions` (see [`read_fwf()`][readr::read_fwf]).
#'
#'   Note that without a corresponding .do file, some columns may
#'   include implicit decimal values. When working with fixed-width data,
#'   consult the provided .do file if you are concerned that columns are not
#'   being parsed and recoded correctly.
#'
#'   If you no longer have access to the .do file for this `data_file`, consider
#'   resubmitting the extract that produced the data.
#' @param ... Additional arguments passed to [`read_csv()`][readr::read_csv] or
#'   [`read_fwf()`][readr::read_fwf].
#'
#' @return A [`tibble`][tibble::tbl_df-class] of the data found in `data_file`.
#'
#' @examples
#' csv_file <- ipums_example("nhgis0707_csv.zip")
#' data_only <- read_nhgis(csv_file)
#'
#' @family ipums_read
#' @export
read_nhgis <- function(data_file,
                       data_layer = NULL,
                       var_attrs = c("val_labels", "var_label", "var_desc"),
                       show_conditions = TRUE,
                       na = NULL,
                       do_file = NULL,
                       ...) {

  if (length(data_file) != 1) {
    rlang::abort("`data_file` must be length 1")
  }

  data_layer <- enquo(data_layer)

  custom_check_file_exists(data_file)

  data_files <- find_files_in(
    data_file,
    name_ext = "csv|dat",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  has_csv <- any(grepl(".csv$", data_files))
  has_dat <- any(grepl(".dat$", data_files))

  if (!has_csv && !has_dat) {
    rlang::abort("No .csv or .dat files found in the provided `data_file`.")
  } else if (has_csv && has_dat) {
    rlang::abort(
      c(
        "Both .csv and .dat files found in the provided `data_file`.",
        "i" = paste0(
          "If `data_file` is a zip archive or directory, it should contain ",
          "either .csv files or .dat files, not both."
        )
      )
    )
  }

  if (file_is_zip(data_file)) {
    cb_ddi_info <- try(
      read_nhgis_codebook(data_file, !!data_layer),
      silent = TRUE
    )
  } else if (file_is_dir(data_file)) {
    cb_ddi_info <- try(
      read_nhgis_codebook(data_file, !!data_layer),
      silent = TRUE
    )
  } else {
    # If direct file provided, look for codebook with analogous name:
    cb <- file.path(
      dirname(data_file),
      fostr_replace(basename(data_file), "\\..+$",  "_codebook.txt")
    )

    cb_ddi_info <- try(
      read_nhgis_codebook(cb),
      silent = TRUE
    )
  }

  # If user passes exact file name as string to data_layer, codebook read will
  # fail. Try once more by constructing cb name out of the provided data_layer
  if (inherits(cb_ddi_info, "try-error")) {
    cb_name <- fostr_replace(
      quo_name(data_layer),
      paste0(ipums_file_ext(quo_name(data_layer)), "$"),
      "_codebook.txt"
    )
    cb_ddi_info <- try(
      read_nhgis_codebook(data_file, data_layer = cb_name),
      silent = TRUE
    )
  }

  # If still error, use empty var info
  cb_error <- inherits(cb_ddi_info, "try-error")

  if (cb_error) {
    cb_ddi_info <- NHGIS_EMPTY_DDI
  }

  if (show_conditions) {
    message(short_conditions_text(cb_ddi_info))
  }

  # Specify encoding (assuming all nhgis extracts are ISO-8859-1 eg latin1
  # because an extract with county names has n with tildes and so is can
  # be verified as ISO-8859-1)
  cb_ddi_info$file_encoding <- "ISO-8859-1"
  extract_locale <- ipums_locale(cb_ddi_info$file_encoding)

  if (has_csv) {
    data <- read_nhgis_csv(
      data_file,
      data_layer = !!data_layer,
      na = na %||% c("", "NA"),
      ...
    )
  } else {
    data <- read_nhgis_fwf(
      data_file,
      data_layer = !!data_layer,
      do_file = do_file,
      na = na %||% c(".", "", "NA"),
      ...
    )
  }

  if (cb_error && !is_null(var_attrs)) {
    rlang::warn(
      c(
        "Unable to read codebook associated with this file.",
        "i" =  "To load a codebook manually, use `read_nhgis_codebook()`.",
        "i" = paste0(
          "To attach codebook information to loaded data, ",
          "use `set_ipums_var_attributes()`."
        )
      )
    )
  }

  data <- set_ipums_var_attributes(data, cb_ddi_info$var_info, var_attrs)

  data

}

#' @inheritParams read_ipums_sf
#'
#' @rdname read_nhgis
#'
#' @export
read_nhgis_sf <- function(shape_file,
                          shape_layer = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = FALSE,
                          add_layer_var = NULL,
                          ...) {

  read_ipums_sf(
    shape_file,
    shape_layer = !!enquo(shape_layer),
    vars = !!enquo(vars),
    encoding = encoding,
    bind_multiple = bind_multiple,
    add_layer_var = add_layer_var,
    ...
  )

}

#' @inheritParams read_ipums_sf
#'
#' @rdname read_nhgis
#'
#' @export
read_nhgis_sp <- function(shape_file,
                          shape_layer = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = FALSE,
                          add_layer_var = NULL,
                          ...) {

  read_ipums_sp(
    shape_file,
    shape_layer = !!enquo(shape_layer),
    vars = !!enquo(vars),
    encoding = encoding,
    bind_multiple = bind_multiple,
    add_layer_var = add_layer_var,
    ...
  )

}

# Internal ---------------------

read_nhgis_fwf <- function(data_file,
                           data_layer = NULL,
                           do_file = NULL,
                           na = c(".", "", "NA"),
                           col_positions = NULL,
                           col_types = NULL,
                           skip = 0,
                           n_max = Inf,
                           guess_max = min(n_max, 1000),
                           ...) {

  if (!is_null(col_positions) && !is_FALSE(do_file)) {
    rlang::warn(
      paste0(
        "Only one of `col_positions` or `do_file` can be provided. ",
        "Setting `do_file = FALSE`"
      )
    )
    do_file <- FALSE
  }

  col_spec <- NULL

  data_layer <- enquo(data_layer)

  file <- find_files_in(
    data_file,
    name_ext = "dat",
    name_select = data_layer,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  if (file_is_zip(data_file)) {

    # Cannot use fwf_empty() col_positions on an unz() connection
    # Must unzip file to allow for fwf_empty() specification
    fwf_dir <- tempfile()
    dir.create(fwf_dir)
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

  do_file <- do_file %||% fostr_replace(file, ".dat$", ".do")

  if (is_FALSE(do_file)) {

    warn_default_fwf_parsing()

  } else if (!file.exists(do_file)) {

    if (!is_null(do_file)) {
      rlang::warn(
        c(
          "Could not find the provided `do_file`.",
          "i" = paste0(
            "Make sure the provided `do_file` exists ",
            "or use `col_positions` to specify column positions manually ",
            "(see `?readr::read_fwf`)"
          )
        )
      )
      warn_default_fwf_parsing()
    } else {
      rlang::warn(
        c(
          "Could not find a .do file associated with the provided file.",
          "i" = paste0(
            "Use the `do_file` argument to provide an associated .do file ",
            "or use `col_positions` to specify column positions manually ",
            "(see `?readr::read_fwf`)"
          )
        )
      )
      warn_default_fwf_parsing()
    }

  } else if (file.exists(do_file)) {

    col_spec <- tryCatch(
      parse_nhgis_do_file(do_file),
      error = function(cnd) {
        rlang::warn(
          c(
            "Problem parsing .do file.",
            "i" = paste0(
              "Using default `col_positions` to parse file. ",
              "(see `?readr::read_fwf`)"
            )
          )
        )
        warn_default_fwf_parsing()
        NULL
      }
    )

  }

  if (!is_null(col_spec)) {

    # If we have succesfully parsed the .do file, use its info to parse cols
    data <- readr::read_fwf(
      file,
      col_positions = col_spec$col_positions,
      col_types = col_types %||% col_spec$col_types,
      na = na,
      skip = skip,
      n_max = n_max,
      guess_max = guess_max,
      ...
    )

    if (!is_null(col_spec$col_recode)) {
      # Rescale column values based on expressions in .do file
      purrr::walk2(
        col_spec$col_recode$cols,
        col_spec$col_recode$exprs,
        function(.x, .y) {
          if (!is_null(data[[.x]])) {
            # Coerce to numeric to guard against user-specified col_types
            data[[.x]] <<- as.numeric(data[[.x]])
            data[[.x]] <<- eval(.y, data)
          }
        }
      )
    }

  } else {

    # Otherwise, use default or user-provided parsing specs
    data <- readr::read_fwf(
      file,
      na = na,
      col_positions = col_positions %||%
        readr::fwf_empty(file, skip, n = guess_max),
      col_types = col_types,
      skip = skip,
      n_max = n_max,
      guess_max = guess_max,
      ...
    )
  }

  data

}

read_nhgis_csv <- function(data_file,
                           data_layer = NULL,
                           na = c("", "NA"),
                           ...) {

  data_layer <- enquo(data_layer)

  file <- find_files_in(
    data_file,
    name_ext = "csv",
    name_select = data_layer,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  if (file_is_zip(data_file)) {
    file <- unz(data_file, file)
  } else if (file_is_dir(data_file)) {
    file <- file.path(data_file, file)
  }

  data <- readr::read_csv(file, na = na, ...)

  # TODO: testing that reformatting this stuff does handle
  # header row correctly

  # If extract is NHGIS's "enhanced" csvs with an extra header row,
  # then remove the first row.
  # (determine by checking if the first row is entirely character
  # values that can't be converted to numeric)
  first_row_char <- all(
    purrr::map_lgl(
      readr::type_convert(data[1, ], col_types = readr::cols()),
      rlang::is_character
    )
  )

  if (first_row_char) {
    data <- readr::type_convert(data[-1, ], col_types = readr::cols())
  }

  data

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
NHGIS_EMPTY_DDI <- make_ddi_from_scratch(
  ipums_project = "NHGIS",
  file_type = "rectangular",
  conditions = paste0(
    "Use of data from NHGIS is subject to conditions including that users ",
    "should cite the data appropriately. ",
    "Please see www.nhgis.org for more information."
  )
)

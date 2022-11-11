# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an NHGIS extract
#'
#' @description
#' Load a dataset downloaded from the NHGIS extract system.
#'
#' @param data_file Path to the data file, a .zip archive from an IPUMS
#'   extract, or a directory containing the data file.
#' @param data_layer For .zip extracts with multiple datasets, the name of the
#'   data to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions. Data layer must uniquely identify
#'  a dataset.
#' @param var_attrs Variable attributes to add from the codebook, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   \code{\link{set_ipums_var_attributes}} for more details.
#' @param show_conditions If `TRUE`, print IPUMS conditions to console when
#'   loading data. Defaults to `TRUE`.
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
                       ...) {

  data_layer <- enquo(data_layer)

  custom_check_file_exists(data_file)

  if (file_is_zip(data_file)) {
    files <- unzip(data_file, list = TRUE)$Name
    cb_ddi_info <- try(
      read_nhgis_codebook(data_file, !!data_layer),
      silent = TRUE
    )
  } else if (file_is_dir(data_file)) {
    files <- list.files(data_file)
    cb_ddi_info <- try(
      read_nhgis_codebook(data_file, !!data_layer),
      silent = TRUE
    )
  } else {
    files <- data_file

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

  if (inherits(cb_ddi_info, "try-error")) {
    cb_ddi_info <- NHGIS_EMPTY_DDI
  }

  if (show_conditions) {
    custom_cat(short_conditions_text(cb_ddi_info))
  }

  # Specify encoding (assuming all nhgis extracts are ISO-8859-1 eg latin1
  # because an extract with county names has n with tildes and so is can
  # be verified as ISO-8859-1)
  cb_ddi_info$file_encoding <- "ISO-8859-1"

  extract_locale <- ipums_locale(cb_ddi_info$file_encoding)

  # TODO: should check this on invalid file structures--i.e.
  # dirs with no csvs or dat files, etc.
  file_is_csv <- any(purrr::map_lgl(files, ~tools::file_ext(.x) == "csv"))

  if (file_is_csv) {
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
      na = na %||% c(".", "", "NA"),
      ...
    )
  }

  data <- set_ipums_var_attributes(data, cb_ddi_info$var_info, var_attrs)

  data

}

#' @rdname read_nhgis
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

#' @rdname read_nhgis
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
                           ...) {

  dots <- rlang::list2(...)

  user_col_positions <- "col_positions" %in% names(dots)
  user_do_file <- !is_null(do_file) && !is_FALSE(do_file)

  if (user_col_positions && user_do_file) {
    rlang::warn(
      c(
        "Only one of `col_positions` or `do_file` can be provided.",
        "i" = "Using `col_positions` to determine column positions."
      )
    )
    warn_default_fwf_parsing()
  }

  col_spec <- NULL

  data_layer <- enquo(data_layer)

  is_zip <- file_is_zip(data_file)
  is_dir <- file_is_dir(data_file)

  if (is_zip) {

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

    filename <- find_files_in(data_file, "dat", data_layer, none_ok = FALSE)
    file <- file.path(fwf_dir, filename)

  } else if (is_dir) {

    filename <- find_files_in(data_file, "dat", data_layer, none_ok = FALSE)
    file <- file.path(data_file, filename)

  } else {

    file <- data_file

  }

  do_file <- do_file %||% fostr_replace(file, ".dat$", ".do")

  if (is_FALSE(do_file) || user_col_positions) {
    col_spec <- NULL
    warn_default_fwf_parsing()
  } else if (!file.exists(do_file)) {
    if (user_do_file) {
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
  } else if (file.exists(do_file) && !user_col_positions) {
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
      col_types = col_spec$col_types,
      na = na,
      ...
    )

    if (!is_null(col_spec$col_recode)) {
      # Rescale column values based on expressions in .do file
      purrr::walk2(
        col_spec$col_recode$cols,
        col_spec$col_recode$exprs,
        function(.x, .y) {
          data[[.x]] <<- eval(.y, data)
        }
      )
    }

  } else {

    # Otherwise, use default or user-provided parsing specs
    data <- readr::read_fwf(
      file,
      na = na,
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

  is_zip <- file_is_zip(data_file)
  is_dir <- file_is_dir(data_file)

  if (is_zip || is_dir) {
    filename <- find_files_in(data_file, "csv", data_layer, none_ok = FALSE)
  }

  if (is_zip) {
    file <- unz(data_file, filename)
  } else if (is_dir) {
    file <- file.path(data_file, filename)
  } else {
    file <- data_file
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
        "Data loaded from NHGIS fixed-width files may not be consistent with the",
        " information included in the data codebook when parsing column positions ",
        "manually."
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

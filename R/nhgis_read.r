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
#' extract contains important metadata about the data file's column
#' positions and implicit decimals. `read_nhgis()` uses this information to
#' parse and recode the fixed-width data appropriately. Therefore,
#' while possible, it is not recommended to read fixed-width files without a
#' corresponding .do file.
#'
#' If you no longer have access to the .do file, consider resubmitting the
#' extract that produced the data.
#'
#' @param data_file Path to a data file, a .zip archive from an NHGIS
#'   extract, or a directory containing the data file.
#' @param file_select If `data_file` is a .zip archive or directory that
#'   contains multiple files, an expression identifying the file to load.
#'   Accepts a character vector specifying the
#'   file name, a [tidyselect selection][selection_language], or an index
#'   position. This must uniquely identify a file.
#' @param var_attrs Variable attributes to add from the codebook (.txt) file
#'   included in the extract. Defaults to
#'   all available attributes (`"val_labels"`, `"var_label"`, and `"var_desc"`).
#'
#'   If the codebook is not found, data will be loaded with no variable
#'   attributes.
#'
#'   See [`set_ipums_var_attributes()`] for more details.
#' @param remove_extra_header If `TRUE`, remove the additional descriptive
#'   header row that is included in some NHGIS .csv files. Otherwise, the
#'   additional header will appear in the first row of the output data frame.
#'
#'   The header contains the same information that is
#'   included in the `"label"` attribute of each data column (if `var_attrs`
#'   includes `"var_label"`), and is therefore not usually needed.
#' @param do_file For fixed-width files, path to the .do file associated with
#'   the provided `data_file`. The .do file contains the specifications that
#'   indicate how to parse the data file.
#'
#'   By default, looks in the same directory as `data_file` for a .do
#'   file with the same name. If `FALSE` or if the .do file cannot be found,
#'   the .dat file will be parsed by the values provided to `col_positions`
#'   in [`read_fwf()`][readr::read_fwf]. See details.
#' @param file_type One of `"csv"` (for csv files) or `"dat"` (for fixed-width
#'   files) indicating the type of file to search for in the path provided to
#'   `data_file`. If `NULL`, determines the file type automatically based on
#'   the files found in `data_file`. Only needed if `data_file` contains both
#'   .csv and .dat files.
#' @param na Character vector of strings to interpret as missing values.
#'   If `NULL`, defaults to `c("", "NA")` for csv files and `c(".", "", "NA")`
#'   for fixed-width files. See [`read_csv()`][readr::read_csv].
#' @param col_names If reading a .csv file, either `TRUE`, `FALSE` or a
#'   character vector of column names.
#'
#'   If `TRUE`, use the default column names included in NHGIS .csv files. If
#'   `FALSE` or a character vector, replace the default column names with the
#'   provided names or default placeholders.
#'
#'   Note that unlike [`readr::read_csv()`][readr::read_csv], the first row
#'   of the input (which contains NHGIS default headers) will always be removed
#'   from the data.
#' @param locale Controls defaults that vary from place to place. If `NULL`,
#'   uses a locale designed to provide appropriate defaults for NHGIS files;
#'   altering the locale may cause problems with data parsing.
#'
#'   If needed, you can use [`readr::locale()`][readr::locale] to
#'   specify a different locale to control things like default time zone,
#'   decimal mark, big mark, and day/month names. If you do so, specify
#'   `encoding = "latin1"` to ensure files are encoded properly.
#' @param show_conditions If `TRUE`, print IPUMS conditions to console when
#'   loading data.
#' @param ... Additional arguments passed to [`read_csv()`][readr::read_csv] or
#'   [`read_fwf()`][readr::read_fwf].
#' @param data_layer `r lifecycle::badge("deprecated")` Please
#'   use `file_select` instead.
#'
#' @return A [`tibble`][tibble::tbl_df-class] containing the data found in
#'   `data_file`
#'
#' @family ipums_read
#' @export
#'
#' @examples
#' csv_file <- ipums_example("nhgis0972_csv.zip")
#' data_only <- read_nhgis(csv_file)
read_nhgis <- function(data_file,
                       file_select = NULL,
                       var_attrs = c("val_labels", "var_label", "var_desc"),
                       remove_extra_header = TRUE,
                       do_file = NULL,
                       file_type = NULL,
                       na = NULL,
                       col_names = TRUE,
                       locale = NULL,
                       show_conditions = TRUE,
                       ...,
                       data_layer = deprecated()) {

  if (length(data_file) != 1) {
    rlang::abort("`data_file` must be length 1")
  }

  if (!is_null(file_type) && !file_type %in% c("csv", "dat")) {
    rlang::abort("`file_type` must be one of \"csv\", or \"dat\"")
  }

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
    name_ext = file_type %||% "csv|dat",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  has_csv <- any(grepl(".csv$", data_files))
  has_dat <- any(grepl(".dat$", data_files))

  if (!has_csv && !has_dat) {

    if (is_null(file_type)) {
      msg <- ".csv or .dat"
    } else {
      msg <- paste0(".", file_type)
    }

    rlang::abort(
      paste0("No ", msg, " files found in the provided `data_file`.")
    )

  } else if (has_csv && has_dat) {
    rlang::abort(
      c(
        "Both .csv and .dat files found in the provided `data_file`.",
        "i" = paste0(
          "Use the `file_type` argument to specify which file type to load."
        )
      )
    )
  }

  if (has_csv) {
    data <- read_nhgis_csv(
      data_file,
      file_select = !!file_select,
      var_attrs = var_attrs,
      remove_extra_header = remove_extra_header,
      show_conditions = show_conditions,
      na = na %||% c("", "NA"),
      locale = locale,
      col_names = col_names,
      ...
    )
  } else {
    data <- read_nhgis_fwf(
      data_file,
      file_select = !!file_select,
      var_attrs = var_attrs,
      do_file = do_file,
      show_conditions = show_conditions,
      na = na %||% c(".", "", "NA"),
      locale = locale,
      ...
    )
  }

  data

}


#' Read and combine tabular and spatial data from an NHGIS extract
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' The simultaneous loading of tabular and spatial data has been deprecated.
#' Splitting this functionality allows for more control over the data reading
#' and joining process.
#'
#' Please read spatial and tabular data separately using [`read_ipums_sf()`] and
#' [`read_nhgis()`]. To join spatial and tabular data, use an
#' [ipums_shape_*_join][ipums_shape_left_join] function.
#'
#' To convert a `SpatialPolygonsDataFrame` or other `sp` object to an `sf`
#' object, use [`sf::as_Spatial()`].
#'
#' @param data_file  Path to a data file, a .zip archive from an NHGIS
#'   extract, or a directory containing the data file.
#' @param shape_file Path to a .shp file, a .zip archive from an NHGIS
#'   extract, or a directory containing the .shp file.
#' @param data_layer If `data_file` is a .zip archive or directory that
#'   contains multiple files, an expression identifying the file to load.
#'   Accepts a character vector specifying the
#'   file name, a [tidyselect selection][selection_language], or an index
#'   position.
#' @param shape_layer If `shape_file` is a .zip archive or directory that
#'   contains multiple files, an expression identifying the file to load.
#'   Accepts a character vector specifying the
#'   file name, a [tidyselect selection][selection_language], or an index
#'   position.
#' @param shape_encoding The text encoding to use when reading the shape file.
#'   Defaults to `"latin1"`, which should be appropriate for most files.
#' @param verbose Logical indicating whether to print progress information to
#'   the console.
#' @param var_attrs Variable attributes to add from the codebook. Defaults to
#'   all available attributes (`"val_labels"`, `"var_label"` and `"var_desc"`).
#'   See [`set_ipums_var_attributes()`] for more details.
#'
#' @rdname read_nhgis_sf
#'
#' @keywords internal
#'
#' @export
read_nhgis_sf <- function(data_file,
                          shape_file,
                          data_layer = NULL,
                          shape_layer = data_layer,
                          shape_encoding = "latin1",
                          verbose = TRUE,
                          var_attrs = c("val_labels", "var_label", "var_desc")) {

  lifecycle::deprecate_warn(
    "0.6.0",
    "read_nhgis_sf()",
    details = c(
      "i" = paste0(
        "Please use `read_ipums_sf()` and `read_nhgis()` to load spatial and ",
        "tabular data separately. Join with `ipums_shape_left_join()`."
      )
    )
  )

  data <- read_nhgis(
    data_file,
    file_select = !!enquo(data_layer),
    show_conditions = verbose,
    progress = verbose,
    show_col_types = verbose
  )

  shape_layer <- enquo(shape_layer)
  if (quo_text(shape_layer) == "data_layer") shape_layer <- data_layer
  if (verbose) cat("Reading geography...\n")

  sf_data <- read_ipums_sf(
    shape_file,
    file_select = !!shape_layer,
    quiet = !verbose,
    encoding = shape_encoding,
    bind_multiple = TRUE
  )

  # Only join on vars that are in both and are called "GISJOIN*"
  join_vars <- intersect(names(data), names(sf_data))
  join_vars <- fostr_subset(join_vars, "GISJOIN.*")

  # Drop overlapping vars besides join var from shape file
  drop_vars <- dplyr::intersect(names(data), names(sf_data))
  drop_vars <- dplyr::setdiff(drop_vars, join_vars)
  sf_data <- dplyr::select(sf_data, -one_of(drop_vars))

  # Avoid a warning by adding attributes from the join_vars in data to
  # join_vars in sf_data
  purrr::walk(join_vars, function(vvv) {
    attributes(sf_data[[vvv]]) <<- attributes(data[[vvv]])
  })

  # Coerce to data.frame to avoid sf#414 (fixed in development version of sf)
  data <- dplyr::full_join(as.data.frame(sf_data), as.data.frame(data), by = join_vars)
  data <- sf::st_as_sf(tibble::as_tibble(data))

  # Check if any data rows are missing (merge failures where not in shape file)
  if (verbose) {
    missing_in_shape <- purrr::map_lgl(data$geometry, is.null)
    if (any(missing_in_shape)) {
      gis_join_failures <- data$GISJOIN[missing_in_shape]
      cat(paste(
        custom_format_text(
          "There are ", sum(missing_in_shape), " rows of data that ",
          "have data but no geography. This can happen because:"
        ),
        custom_format_text(
          "Shape files do not include some census geographies such ",
          "as 'Crews of Vessels' tracts that do not have a defined area",
          indent = 2, exdent = 2
        ),
        custom_format_text(
          "Shape files have been simplified which sometimes drops ",
          "entire geographies (especially small ones)."
        ),
        sep = "\n"
      ))
    }
  }
  data
}

#' @rdname read_nhgis_sf
#'
#' @export
read_nhgis_sp <- function(data_file,
                          shape_file,
                          data_layer = NULL,
                          shape_layer = data_layer,
                          shape_encoding = "latin1",
                          verbose = TRUE,
                          var_attrs = c("val_labels", "var_label", "var_desc")) {

  lifecycle::deprecate_warn(
    "0.6.0",
    "read_nhgis_sp()",
    details = c(
      "i" = paste0(
        "Please use `read_ipums_sf()` and `read_nhgis()` to load spatial and ",
        "tabular data separately. Join with `ipums_shape_left_join()`."
      ),
      "i" = "To convert from an `sf` to an `sp` format, use `sf::as_Spatial()`."
    )
  )

  data <- read_nhgis(
    data_file,
    file_select = !!enquo(data_layer),
    show_conditions = verbose,
    progress = verbose,
    show_col_types = verbose
  )

  shape_layer <- enquo(shape_layer)
  if (quo_text(shape_layer) == "data_layer") shape_layer <- data_layer
  if (verbose) cat("Reading geography...\n")

  sp_data <- read_ipums_sp(
    shape_file,
    !!shape_layer,
    verbose = verbose,
    encoding = shape_encoding,
    bind_multiple = TRUE
  )

  # Only join on vars that are in both and are called "GISJOIN*"
  join_vars <- intersect(names(data), names(sp_data@data))
  join_vars <- fostr_subset(join_vars, "GISJOIN.*")

  # Drop overlapping vars besides join var from shape file
  drop_vars <- dplyr::intersect(names(data), names(sp_data@data))
  drop_vars <- dplyr::setdiff(drop_vars, join_vars)
  sp_data@data <- dplyr::select(sp_data@data, -one_of(drop_vars))

  out <- sp::merge(sp_data, data, by = join_vars, all.x = TRUE)

  # Check if any data rows are missing (merge failures where not in shape file)
  if (verbose) {
    missing_in_shape <- dplyr::anti_join(
      dplyr::select(data, one_of(join_vars)),
      dplyr::select(out@data, one_of(join_vars)),
      by = join_vars
    )
    if (nrow(missing_in_shape) > 0) {
      gis_join_failures <- purrr::pmap_chr(missing_in_shape, function(...) paste(..., sep = "-"))
      message(paste0(
        "There are ", nrow(missing_in_shape), " rows of data that ",
        "have data but no geography. This can happen because:\n  Shape files ",
        "do not include some census geographies such as 'Crews of Vessels' ",
        "tracts that do not have a defined area\n  Shape files have been simplified ",
        "which sometimes drops entire geographies (especially small ones)."
      ))
    }
  }
  out
}

# Internal ---------------------

read_nhgis_fwf <- function(data_file,
                           file_select = NULL,
                           do_file = NULL,
                           var_attrs = c("val_labels", "var_label", "var_desc"),
                           show_conditions = TRUE,
                           locale = NULL,
                           ...) {

  dots <- rlang::list2(...)

  if (!is_null(dots$col_positions) && !is_FALSE(do_file)) {
    rlang::warn(
      paste0(
        "Only one of `col_positions` or `do_file` can be provided. ",
        "Setting `do_file = FALSE`"
      )
    )
    do_file <- FALSE
  }

  col_spec <- NULL

  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "dat",
    name_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  cb_files <- find_files_in(
    data_file,
    name_ext = "txt",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  if (length(cb_files) > 0) {
    cb_file <- fostr_subset(
      cb_files,
      fostr_replace(
        basename(file),
        ipums_file_ext(file),
        ""
      )
    )
  }

  cb_ddi_info <- try(
    read_nhgis_codebook(data_file, file_select = tidyselect::all_of(cb_file)),
    silent = TRUE
  )

  cb_error <- inherits(cb_ddi_info, "try-error")

  if (cb_error) {
    cb_ddi_info <- NHGIS_EMPTY_DDI
  }

  if (show_conditions) {
    message(short_conditions_text(cb_ddi_info))
  }

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

  do_file <- do_file %||% fostr_replace(file, "\\.dat$", ".do")

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

  # Specify encoding (assuming all nhgis extracts are ISO-8859-1 eg latin1
  # because an extract with county names has n with tildes and so is can
  # be verified as ISO-8859-1)
  cb_ddi_info$file_encoding <- "ISO-8859-1"

  # Update dots with parsing info from .do file before passing to read_fwf()
  dots$col_positions <- dots$col_positions %||% col_spec$col_positions
  dots$col_types <- dots$col_types %||% col_spec$col_types
  dots$locale <- locale %||% ipums_locale(cb_ddi_info$file_encoding)

  data <- rlang::inject(
    readr::read_fwf(file, !!!dots)
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
                           show_conditions = TRUE,
                           locale = NULL,
                           skip = 0,
                           col_names = TRUE,
                           ...) {

  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "csv",
    name_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  cb_files <- find_files_in(
    data_file,
    name_ext = "txt",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  if (length(cb_files) > 0) {
    cb_file <- fostr_subset(
      cb_files,
      fostr_replace(
        basename(file),
        ipums_file_ext(file),
        ""
      )
    )
  }

  cb_ddi_info <- try(
    read_nhgis_codebook(data_file, file_select = tidyselect::all_of(cb_file)),
    silent = TRUE
  )

  cb_error <- inherits(cb_ddi_info, "try-error")

  if (cb_error) {
    cb_ddi_info <- NHGIS_EMPTY_DDI
  }

  if (show_conditions) {
    message(short_conditions_text(cb_ddi_info))
  }

  if (file_is_zip(data_file)) {
    file <- unz(data_file, file)
  } else if (file_is_dir(data_file)) {
    file <- file.path(data_file, file)
  }

  header_info <- check_header_row(data_file, file_select = !!file_select)

  if (header_info$has_extra_header && remove_extra_header) {
    skip <- 2 + skip
  } else {
    skip <- 1 + skip
  }

  if (is_null(col_names) || isTRUE(col_names)) {
    col_names <- header_info$col_names
  }

  # Specify encoding (assuming all nhgis extracts are ISO-8859-1 eg latin1
  # because an extract with county names has n with tildes and so is can
  # be verified as ISO-8859-1)
  cb_ddi_info$file_encoding <- "ISO-8859-1"
  locale <- locale %||% ipums_locale(cb_ddi_info$file_encoding)

  data <- readr::read_csv(
    file,
    skip = skip,
    col_names = col_names,
    locale = locale,
    ...
  )

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

check_header_row <- function(data_file, file_select = NULL) {

  file_select <- enquo(file_select)

  file <- find_files_in(
    data_file,
    name_ext = "csv",
    name_select = file_select,
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
NHGIS_EMPTY_DDI <- make_ddi_from_scratch(
  ipums_project = "NHGIS",
  file_type = "rectangular",
  conditions = paste0(
    "Use of data from NHGIS is subject to conditions including that users ",
    "should cite the data appropriately. ",
    "Please see www.nhgis.org for more information."
  )
)

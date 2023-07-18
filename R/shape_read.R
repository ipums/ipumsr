# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read spatial data from an IPUMS extract
#'
#' @description
#' Read a spatial data file (also referred to as a GIS file or shapefile) from
#' an IPUMS extract into an `sf` object from the
#' [sf](https://r-spatial.github.io/sf/) package.
#'
#' @details
#' Some IPUMS products provide shapefiles in a "nested" .zip archive. That is,
#' each shapefile (including a .shp as well as accompanying files) is
#' compressed in its own archive, and the collection of all
#' shapefiles provided in an extract is also compressed into a single .zip
#' archive.
#'
#' `read_ipums_sf()` is designed to handle this structure. However, if any files
#' are altered such that an internal .zip archive contains *multiple*
#' shapefiles, this function will throw an error. If this is the case, you may
#' need to manually unzip the downloaded file before loading it into R.
#'
#' @param shape_file Path to a single .shp file, or a .zip archive or
#'   directory containing at least one .shp file. See Details section.
#' @param file_select If `shape_file` is a .zip archive or directory that
#'   contains multiple files, an expression identifying the files to load.
#'   Accepts a character string specifying the
#'   file name, a [tidyselect selection][selection_language], or index
#'   position. If multiple files are selected, `bind_multiple` must be
#'   equal to `TRUE`.
#' @param vars Names of variables to include in the output. Accepts a
#'   character vector of names or a [tidyselect selection][selection_language].
#'   If `NULL`, includes all variables in the file.
#' @param encoding Encoding to use when reading the shape file. If `NULL`,
#'   defaults to `"latin1"` unless the file includes a .cpg metadata file
#'   with encoding information. The default value should generally be
#'   appropriate.
#' @param bind_multiple If `TRUE` and `shape_file` contains multiple .shp files,
#'   row-bind the files into a single `sf` object. Useful when `shape_file`
#'   contains multiple files that represent the same geographic units for
#'   different extents (e.g. block-level data for multiple states).
#' @param add_layer_var If `TRUE`, add a variable to the output data indicating
#'   the file that each row originates from. Defaults to `FALSE` unless
#'   `bind_multiple = TRUE` and multiple files exist in `shape_file`.
#'
#'   The column name will always be prefixed with `"layer"`, but will be
#'   adjusted to avoid name conflicts if another column named `"layer"` already
#'   exists in the data.
#' @param verbose If `TRUE` report additional progress information on load.
#' @param shape_layer `r lifecycle::badge("deprecated")` Please use
#'   `file_select` instead.
#'
#' @return An [sf][sf::sf()] object
#'
#' @seealso [read_ipums_micro()] or [read_nhgis()] to read tabular data from
#'   an IPUMS extract.
#'
#'  [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @export
#'
#' @examplesIf requireNamespace("sf")
#' # Example shapefile from NHGIS
#' shape_ex1 <- ipums_example("nhgis0972_shape_small.zip")
#' data_ex1 <- read_nhgis(ipums_example("nhgis0972_csv.zip"), verbose = FALSE)
#'
#' sf_data <- read_ipums_sf(shape_ex1)
#'
#' sf_data
#'
#' # To combine spatial data with tabular data without losing the attributes
#' # included in the tabular data, use an ipums shape join:
#' ipums_shape_full_join(data_ex1, sf_data, by = "GISJOIN")
#'
#' shape_ex2 <- ipums_example("nhgis0712_shape_small.zip")
#'
#' # Shapefiles are provided in .zip archives that may contain multiple
#' # files. Select a single file with `file_select`:
#' read_ipums_sf(shape_ex2, file_select = matches("us_pmsa_1990"))
#'
#' # Or row-bind files with `bind_multiple`. This may be useful for files of
#' # the same geographic level that cover different extents)
#' read_ipums_sf(
#'   shape_ex2,
#'   file_select = matches("us_pmsa"),
#'   bind_multiple = TRUE
#' )
read_ipums_sf <- function(shape_file,
                          file_select = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = FALSE,
                          add_layer_var = NULL,
                          verbose = FALSE,
                          shape_layer = deprecated()) {
  custom_check_file_exists(shape_file)

  if (!missing(shape_layer)) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "read_ipums_sf(shape_layer = )",
      "read_ipums_sf(file_select = )",
    )
    file_select <- enquo(shape_layer)
  } else {
    file_select <- enquo(file_select)
  }

  # dots <- rlang::list2(...)
  #
  # if (!missing(verbose)) {
  #   lifecycle::deprecate_soft(
  #     "0.6.0",
  #     "read_ipums_sf(verbose = )",
  #     "read_ipums_sf(quiet = )"
  #   )
  #
  #   if (!"quiet" %in% names(dots)) {
  #     dots$quiet <- !verbose
  #   }
  # }

  vars <- enquo(vars)
  load_sf_namespace()

  # For zipped files, make a temp folder that will be cleaned
  shape_temp <- tempfile()
  dir.create(shape_temp)
  on.exit(
    unlink(shape_temp, recursive = TRUE),
    add = TRUE,
    after = FALSE
  )

  read_shape_files <- shape_file_prep(
    shape_file,
    file_select,
    bind_multiple,
    shape_temp
  )

  encoding <- determine_encoding(read_shape_files, encoding)
  #
  # if (!any(grepl("ENCODING", dots$options))) {
  #   dots$options <- c(paste0("ENCODING=", encoding), dots$options)
  # } else {
  #   rlang::warn(
  #     paste0(
  #       "Encoding specified in both `encoding` and `options` arguments. ",
  #       "Using the value provided to `options`"
  #     )
  #   )
  # }
  #
  # out <- purrr::map2(
  #   read_shape_files,
  #   encoding,
  #   function(.x, .y) {
  #     args <- c(list(.x), dots)
  #
  #     this_sf <- rlang::exec(sf::read_sf, !!!args)
  #
  #     if (!rlang::quo_is_null(vars)) {
  #       this_sf <- dplyr::select(this_sf, !!vars)
  #     }
  #
  #     this_sf
  #   }
  # )

  out <- purrr::map2(
    read_shape_files,
    encoding,
    function(.x, .y) {
      this_sf <- sf::read_sf(
        .x,
        quiet = !verbose,
        options = paste0("ENCODING=", .y)
      )

      if (!rlang::quo_is_null(vars)) {
        this_sf <- dplyr::select(this_sf, !!vars)
      }

      this_sf
    }
  )

  names(out) <- fostr_sub(basename(read_shape_files), 1, -5)
  out <- careful_sf_rbind(out, add_layer_var)

  out
}

# Takes a list of sf's, fills in empty columns for you and binds them together.
# Warns if types don't match and are coerced
careful_sf_rbind <- function(sf_list, add_layer_var = NULL) {
  add_layer_var <- add_layer_var %||% (length(sf_list) > 1)

  if (add_layer_var) {
    existing_cols <- unique(unlist(purrr::map(sf_list, colnames)))
    lyr_name <- make.unique(c(existing_cols, "layer"))
    lyr_name <- lyr_name[length(lyr_name)]

    if (lyr_name != "layer") {
      rlang::warn(
        paste0(
          "Adding layer information to column \"", lyr_name,
          "\", as \"layer\" is already present in data."
        )
      )
    }

    sf_list <- purrr::imap(
      sf_list,
      ~ dplyr::mutate(.x, {{ lyr_name }} := .y, .before = dplyr::everything())
    )
  }

  if (length(sf_list) == 1) {
    return(sf_list[[1]])
  }

  # Get var info for all columns
  all_var_info <- purrr::map_df(
    sf_list,
    .id = "id",
    ~ tibble::tibble(name = names(.x), type = purrr::map(.x, class))
  )

  all_var_info <- dplyr::group_by(all_var_info, .data$name)

  var_type_check <- dplyr::summarize(
    all_var_info,
    check = length(unique(.data$type))
  )

  # Warn on variable type coercion
  if (any(var_type_check$check != 1)) {
    bad_vars <- var_type_check$name[which(var_type_check$check > 1)]

    bad_types <- purrr::map_chr(
      bad_vars,
      ~ paste0(
        all_var_info[all_var_info$name == .x, ]$type,
        collapse = " vs. "
      )
    )

    rlang::warn(
      c(
        "Some variables had inconsistent types across files: ",
        purrr::set_names(
          paste0("\"", bad_vars, "\" (", bad_types, ")"),
          "*"
        )
      )
    )
  }

  # Add missing values for cols that don't exist in all data sources
  out <- purrr::map(
    sf_list,
    function(x) {
      missing_vars <- dplyr::setdiff(all_var_info$name, names(x))

      if (length(missing_vars) == 0) {
        return(x)
      }

      purrr::walk(
        missing_vars,
        function(vn) {
          x[[vn]] <<- NA
        }
      )

      x
    }
  )

  out <- do.call(rbind, out)
  out <- dplyr::relocate(
    out,
    tidyselect::all_of(attr(out, "sf_column")),
    .after = everything()
  )

  sf::st_as_sf(tibble::as_tibble(out))
}

# Encoding:
# Official spec is that shape files must be latin1. But some GIS software
# add to the spec a cpg file that can specify an encoding.
## NHGIS: Place names in 2010 have accents - and are latin1 encoded,
##        No indication of encoding.
# TODO: Confirm Brazil encoding is still the following?
## IPUMSI: Brazil has a cpg file indicating the encoding is ANSI 1252,
##         while China has UTF-8 (but only english characters)
## USA:   Also have cpg files.
## Terrapop: Always UTF-8 (and sometimes has been ruined if the
##           shape file comes from IPUMS International and wasn't
##           UTF-8 to begin with.)
# Current solution: If user specified encoding (or possibly came from
# defaults of functions eg read_terra says UTF-8, but read_nghis says latin1),
# then use that. If not, and a cpg file exists, use that. Else, assume latin1.
determine_encoding <- function(shape_file_vector, encoding = NULL) {
  if (!is.null(encoding)) {
    return(encoding)
  }

  out <- purrr::map_chr(
    shape_file_vector,
    function(x) {
      cpg_file <- dir(
        dirname(x),
        pattern = "\\.cpg$",
        ignore.case = TRUE,
        full.names = TRUE
      )

      if (length(cpg_file) == 0) {
        return("latin1")
      }

      cpg_text <- readr::read_lines(cpg_file)[1]

      if (fostr_detect(cpg_text, "ANSI 1252")) {
        return("CP1252")
      } else if (fostr_detect(cpg_text, "UTF[[-][[:blank:]]?8")) {
        return("UTF-8")
      } else {
        return("latin1")
      }
    }
  )

  out
}


# Gather the programming logic around going from an IPUMS download to
# an unzipped folder with a shape file in it (possibly in a temp folder)
shape_file_prep <- function(shape_file,
                            file_select,
                            bind_multiple,
                            shape_temp) {
  shape_is_shp <- tools::file_ext(shape_file) == "shp"
  shape_is_zip <- tools::file_ext(shape_file) == "zip"
  shape_is_dir <- tools::file_ext(shape_file) == ""

  if (shape_is_shp) {
    return(shape_file)
  }

  shape_files <- find_files_in(
    shape_file,
    name_ext = "zip|shp",
    multiple_ok = TRUE,
    none_ok = TRUE
  )

  has_shp <- any(grepl(".shp$", shape_files))
  has_zip <- any(grepl(".zip$", shape_files))

  # If there are any .shp files in this zip or dir, load them.
  # Otherwise, we should have .zip files containing .shp files
  if (has_shp) {
    # Add hint to error if bind_multiple = FALSE and multiple files found
    shape_shps <- tryCatch(
      find_files_in(
        shape_file,
        "shp",
        file_select,
        multiple_ok = bind_multiple,
        none_ok = FALSE
      ),
      error = function(cnd) {
        err_msg <- conditionMessage(cnd)

        if (grepl("^Multiple files found", err_msg)) {
          bind_hint <- c("i" = "To combine files, set `bind_multiple = TRUE`")
        } else {
          bind_hint <- NULL
        }

        rlang::abort(c(err_msg, bind_hint), call = expr(find_files_in()))
      }
    )

    read_shape_files <- purrr::map_chr(
      shape_shps,
      function(x) {
        # ignore "sbn", "sbx" because R doesn't use them
        shape_shp_files <- paste0(
          fostr_sub(x, 1, -4),
          c("shp", "dbf", "prj", "shx")
        )

        if (shape_is_zip) {
          utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

          # If there is a cpg file (encoding information) extract that
          all_files <- utils::unzip(shape_file, list = TRUE)$Name
          file_exts <- tolower(purrr::map_chr(all_files, ipums_file_ext))

          cpg_file <- ".cpg" == file_exts

          if (any(cpg_file)) {
            utils::unzip(shape_file, all_files[cpg_file], exdir = shape_temp)
          }

          file.path(shape_temp, shape_shp_files[1])
        } else {
          file.path(shape_file, x)
        }
      }
    )
  } else if (has_zip) {
    shape_zips <- tryCatch(
      find_files_in(
        shape_file,
        "zip",
        file_select,
        multiple_ok = bind_multiple,
        none_ok = FALSE
      ),
      error = function(cnd) {
        err_msg <- conditionMessage(cnd)

        if (grepl("^Multiple files found", err_msg)) {
          bind_hint <- c("i" = "To combine files, set `bind_multiple = TRUE`")
        } else {
          bind_hint <- NULL
        }

        rlang::abort(c(err_msg, bind_hint), call = expr(find_files_in()))
      }
    )

    if (shape_is_zip) {
      purrr::walk(
        shape_zips,
        function(x) {
          utils::unzip(shape_file, x, exdir = shape_temp)
          utils::unzip(file.path(shape_temp, x), exdir = shape_temp)
        }
      )
    } else {
      purrr::walk(
        file.path(shape_file, shape_zips),
        function(x) {
          utils::unzip(x, exdir = shape_temp)
        }
      )
    }

    read_shape_files <- dir(shape_temp, "\\.shp$", full.names = TRUE)
  } else {
    rlang::abort("No .shp or .zip files found in the provided `shape_file`.")
  }

  # If no shapefiles or if multiple and bind_multiple = FALSE (this can happen
  # if someone tries to read a zip of a zip/dir where there are 2 shps in the
  # inner zip) something was not as expected.
  if (length(read_shape_files) == 0 ||
      (length(read_shape_files) > 1 && !bind_multiple)) {
    rlang::abort(
      c(
        "Files in `shape_file` not formatted as expected. ",
        "i" = "See `?read_ipums_sf` for details on expected file structures."
      )
    )
  }

  read_shape_files
}

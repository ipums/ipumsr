# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' List files available for analysis in an IPUMS extract.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These function are special cases of [`ipums_list_files`].
#'
#' Please use that function instead.
#'
#' @keywords internal
#'
#' @export
ipums_list_data <- function(file, data_layer = NULL) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "ipums_list_data()",
    "ipums_list_files()"
  )

  data_layer <- enquo(data_layer)

  tibble::tibble(
    file = find_files_in(
      file,
      "(dat|csv)(\\.gz)?",
      data_layer,
      multiple_ok = TRUE
    )
  )
}

#' @rdname ipums_list_data
#' @export
ipums_list_shape <- function(file, shape_layer = NULL) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "ipums_list_shape()",
    "ipums_list_files()"
  )

  shape_layer <- enquo(shape_layer)

  tibble::tibble(
    file = find_files_in(
      file,
      "(zip|shp)",
      shape_layer,
      multiple_ok = TRUE
    )
  )
}

#' @rdname ipums_list_data
#' @export
ipums_list_raster <- function(file, raster_layer = NULL) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "ipums_list_raster()",
    "ipums_list_files()"
  )

  raster_layer <- enquo(raster_layer)

  tibble::tibble(
    file = find_files_in(
      file,
      "tiff",
      raster_layer,
      multiple_ok = TRUE
    )
  )
}

#' Read metadata from an IPUMS Terra extract codebook file
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Support for IPUMS Terra has been discontinued. `read_ipums_codebook()` has
#' been deprecated and will be removed in a future release.
#'
#' To read an NHGIS codebook, use [`read_nhgis_codebook`].
#'
#' @keywords internal
#'
#' @export
read_ipums_codebook <- function(cb_file, data_layer = NULL) {
  # nocov start
  lifecycle::deprecate_warn(
    "0.6.0",
    "read_ipums_codebook()",
    details = c(
      "To read an NHGIS codebook, use `read_nhgis_codebook()`.",
      paste0(
        "Support for IPUMS Terra has been discontinued and will be removed",
        " in a future release."
      )
    )
  )

  data_layer <- enquo(data_layer)

  custom_check_file_exists(cb_file)

  if (path_is_zip_or_dir(cb_file)) {
    cb_name <- find_files_in(cb_file, "txt", multiple_ok = TRUE)

    # There are 2 formats for extracts, so we have to do some work here.
    # IPUMS Terra always(?) has 2 text files, one is a codebook for all files
    # in the extract and another with a name that ends in "info.txt" and
    # isn't useful
    if (length(cb_name) > 1) {
      # First try to get rid of the "info" txt
      cb_name <- cb_name[!fostr_detect(cb_name, "info\\.txt$")]

      # If we still have multiple, then we should try to use the data_layer
      # filter because we're probably in a NHGIS extract
      if (length(cb_name) > 1) {
        cb_name <- find_files_in(cb_file, "txt", data_layer)
      }
    }
    if (length(cb_name) == 1) {
      if (file_is_zip(cb_file)) {
        cb <- readr::read_lines(unz(cb_file, cb_name), progress = FALSE)
      } else {
        cb <- readr::read_lines(file.path(cb_file, cb_name), progress = FALSE)
      }
    } else {
      cb <- NULL
    }
  } else {
    cb_name <- cb_file
    if (file.exists(cb_name)) {
      cb <- readr::read_lines(cb_name, progress = FALSE)
    } else {
      cb <- NULL
    }
  }

  if (is.null(cb)) {
    rlang::abort("Could not find text codebook.")
  }

  # Section markers are a line full of dashes
  # (setting to 5+ to eliminate false positives)
  section_markers <- which(fostr_detect(cb, "^[-]{5,}$"))

  # Second line tells if it is NHGIS or IPUMS Terra codebook
  if (fostr_detect(cb[2], "IPUMS Terra")) {
    type <- "IPUMS Terra"
  } else if (fostr_detect(cb[2], "NHGIS")) {
    type <- "NHGIS"
  } else {
    rlang::abort("Unknown codebook format.")
  }

  # Get table names (var_desc) and variable labels (var_label)
  # from data dictionary section using messy string parsing code
  dd <- find_cb_section(cb, "^Data Dictionary$", section_markers)

  if (type == "IPUMS Terra") {
    data_file_rows <- which(fostr_detect(dd, "^Data File:"))
    data_file_sections <- purrr::map2(
      data_file_rows,
      c(data_file_rows[-1], length(dd)),
      ~ seq(.x + 1, .y - 1)
    )
    data_file_names <- fostr_named_capture_single(
      dd[data_file_rows],
      "Data File: (?<data_file>.+)"
    )

    # Only get var info from file you're downloading (specfied in data_layer)
    if (quo_is_null(data_layer)) {
      this_file <- seq_along(data_file_names)
    } else {
      this_file <- which(
        data_file_names == tidyselect::vars_select(data_file_names, !!data_layer)
      )
    }

    if (length(this_file) > 1) {
      rlang::abort(paste0(
        "Multiple codebooks found, please specify which to use with the",
        "`data_layer` argument"
      ))
    }

    var_info <- dd[data_file_sections[[this_file]]]
    var_info <- fostr_named_capture(
      var_info,
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
      only_matches = TRUE
    )

    var_info <- make_var_info_from_scratch(
      var_name = var_info$var_name,
      var_label = var_info$var_label,
      var_desc = ""
    )
  } else if (type == "NHGIS") {
    # Check if file is a time series file
    time_series_type <- fostr_named_capture_single(
      cb,
      "^Time series layout:[[:blank:]]+(?<ts_type>.+)$",
      only_matches = TRUE
    )

    if (length(time_series_type) > 0) {
      is_time_series <- TRUE
    } else {
      is_time_series <- FALSE
    }

    context_start <- which(dd == "Context Fields ") + 1
    context_end <- which(fostr_detect(dd, "^[[:blank:]]$")) - 1
    context_end <- min(context_end[context_end > context_start])
    context_rows <- seq(context_start, context_end)

    context_vars <- dd[context_rows]
    context_vars <- fostr_named_capture(
      context_vars,
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$"
    )
    context_vars$var_desc <- ""
    context_vars <- context_vars[!is.na(context_vars$var_name), ]

    table_name_rows <- which(fostr_detect(dd, "^[[:blank:]]*Table [0-9]+:"))
    table_sections <- purrr::map2(
      table_name_rows,
      c(table_name_rows[-1], length(dd)),
      ~ seq(.x, .y - 1)
    )

    table_vars <- purrr::map_df(
      table_sections,
      function(rows) {
        if (is_time_series) {
          table_name_and_code <- fostr_named_capture(
            dd[rows[1]],
            "^[[:blank:]]*Table .+?:[[:blank:]]+\\((?<table_code>.+?)\\)[[:blank:]]+(?<table_name>.+)$"
          )
          nhgis_table_code <- table_name_and_code$table_code
          table_name <- table_name_and_code$table_name

          time_series_headers <- fostr_detect(
            dd[rows],
            "^[[:blank:]]+Time series"
          )

          vars <- dd[rows][!time_series_headers]
          vars <- vars[-1] # First row was table name/code
          vars <- fostr_named_capture(
            vars,
            "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
            only_matches = TRUE
          )
          vars$var_desc <- paste0(table_name, " (", nhgis_table_code, ")")
        } else {
          table_name <- fostr_named_capture_single(
            dd[rows[1]],
            "^[[:blank:]]*Table .+?:[[:blank:]]+(?<table_name>.+)$"
          )
          nhgis_table_code <- fostr_named_capture_single(
            dd[rows[4]],
            "^[[:blank:]]*NHGIS code:[[:blank:]]+(?<table_code>.+)$"
          )
          vars <- fostr_named_capture(
            dd[rows[-1:-4]],
            "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
            only_matches = TRUE
          )
          vars$var_desc <- paste0(table_name, " (", nhgis_table_code, ")")
        }
        vars
      }
    )

    var_info <- make_var_info_from_scratch(
      var_name = c(context_vars$var_name, table_vars$var_name),
      var_label = c(context_vars$var_label, table_vars$var_label),
      var_desc = c(context_vars$var_desc, table_vars$var_desc)
    )
  }

  # Get License and Condition section
  conditions_text <- find_cb_section(
    cb,
    "^Citation and Use of .+ Data",
    section_markers
  )

  conditions_text <- paste(conditions_text, collapse = "\n")

  new_ipums_ddi(
    file_name = cb_name,
    file_type = "rectangular",
    ipums_project = type,
    var_info = var_info,
    conditions = conditions_text
  )
  # nocov end
}


#' Read boundary (GIS) files from an IPUMS extract
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Read spatial data from an IPUMS extract into R using the `sp` package.
#'
#' This function has been deprecated because of the upcoming retirement
#' of the rgdal package. For more information, click
#' [here](https://r-spatial.org/r/2022/04/12/evolution.html).
#'
#' Please use [`read_ipums_sf()`] to load spatial data from IPUMS. To convert
#' to a `SpatialPolygonsDataFrame`, use [`sf::as_Spatial()`][sf::as_Spatial].
#'
#' @details
#' Some IPUMS products provide shapefiles in a "nested" .zip archive. That is,
#' each shapefile (including a .shp as well as accompanying files) is
#' compressed in its own archive, and the collection of all
#' shapefiles provided in an extract are also compressed into a single .zip
#' archive.
#'
#' `read_ipums_sp()` is designed to handle this structure. However, if an
#' internal .zip archive happens to contain *multiple* shapefiles, this function
#' will throw an error. If this is the case, you may need to manually unzip the
#' downloaded file before loading it into R.
#'
#' @inheritParams read_ipums_sf
#' @param shape_layer If `shape_file` contains multiple files, an expression
#'   identifying the files to load. Accepts a character string specifying the
#'   file name, a [tidyselect selection][selection_language], or index
#'   position. If multiple files are selected, `bind_multiple` must be equal
#'   to `TRUE`.
#' @param bind_multiple If `TRUE` and `shape_file` contains multiple .shp files,
#'   row-bind the files into a single output. Note that some data sources may
#'   not be able to be combined (e.g. `SpatialPolygonsDataFrame` and
#'   `SpatialPointsDataFrame`).
#' @param verbose If `TRUE`, report progress information.
#'
#' @return A `SpatialPolygonsDataFrame` or `SpatialPointsDataFrame` object.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' shape_file <- ipums_example("nhgis0972_shape_small.zip")
#'
#' # If sp package is available, can load as SpatialPolygonsDataFrame
#' if (require(sp) && require(rgdal)) {
#'   sp_data <- read_ipums_sp(shape_file, verbose = FALSE)
#' }
read_ipums_sp <- function(shape_file,
                          shape_layer = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = TRUE,
                          add_layer_var = NULL,
                          verbose = TRUE) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "read_ipums_sp()",
    "read_ipums_sf()",
  )

  shape_layer <- enquo(shape_layer)
  vars <- enquo(vars)
  load_rgdal_namespace()

  # For zipped files, make a temp folder that will be cleaned
  shape_temp <- tempfile()
  dir.create(shape_temp)
  on.exit(
    unlink(shape_temp, recursive = TRUE),
    add = TRUE,
    after = FALSE
  )

  tryCatch(
    read_shape_files <- shape_file_prep(
      shape_file,
      shape_layer,
      bind_multiple,
      shape_temp
    ),
    error = function(cnd) {
      rlang::abort(
        fostr_replace_all(conditionMessage(cnd), "file_select", "shape_layer"),
        call = NULL
      )
    }
  )

  encoding <- determine_encoding(read_shape_files, encoding)

  out <- purrr::map2(
    read_shape_files,
    encoding,
    function(.x, .y) {
      this_sp <- rgdal::readOGR(
        dsn = dirname(.x),
        layer = fostr_sub(basename(.x), 1, -5),
        verbose = verbose,
        stringsAsFactors = FALSE,
        encoding = .y,
        use_iconv = TRUE
      )

      if (!rlang::quo_is_null(vars)) {
        this_sp@data <- dplyr::select(this_sp@data, !!vars)
      }

      this_sp
    }
  )

  names(out) <- fostr_sub(basename(read_shape_files), 1, -5)
  out <- careful_sp_rbind(out, add_layer_var)

  out
}

# Takes a list of SpatialPolygonsDataFrames, fills in empty columns for you and
# binds them together.
# Warns if types don't match and are coerced
careful_sp_rbind <- function(sp_list, add_layer_var = NULL) {
  if (is.null(add_layer_var)) {
    add_layer_var <- length(sp_list) > 1
  }

  if (add_layer_var) {
    sp_list <- purrr::imap(
      sp_list,
      function(.x, .y) {
        .x@data[["layer"]] <- .y
        .x
      }
    )
  }

  if (length(sp_list) == 1) {
    return(sp_list[[1]])
  } else {
    # Get var info for all columns
    all_var_info <- purrr::map_df(
      sp_list,
      .id = "id",
      function(x) {
        tibble::tibble(
          name = names(x@data),
          type = purrr::map(x@data, ~ class(.))
        )
      }
    )

    all_var_info <- dplyr::group_by(all_var_info, .data$name)

    var_type_check <- dplyr::summarize(
      all_var_info,
      check = length(unique(.data$type))
    )

    if (any(var_type_check$check != 1)) {
      stop("Cannot combine shape files because variable types don't match.")
    }

    all_var_info <- dplyr::slice(all_var_info, 1)
    all_var_info <- dplyr::ungroup(all_var_info)
    all_var_info$id <- NULL

    out <- purrr::map(
      sp_list,
      function(x) {
        missing_vars <- dplyr::setdiff(all_var_info$name, names(x))

        if (length(missing_vars) == 0) {
          return(x)
        }

        for (vn in missing_vars) {
          vtype <- all_var_info$type[all_var_info$name == vn][[1]]

          if (identical(vtype, "character")) {
            x@data[[vn]] <- NA_character_
          } else if (identical(vtype, "numeric")) {
            x@data[[vn]] <- NA_real_
          } else {
            stop("Unexpected variable type in shape file.")
          }
        }

        x
      }
    )

    out <- do.call(rbind, out)
  }

  out
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
#' [`ipums_shape_*_join`][ipums_shape_left_join] function.
#'
#' To convert an `sf` object to a `SpatialPolygonsDataFrame` or other `sp`
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

  tryCatch(
    data <- read_nhgis(
      data_file,
      file_select = !!enquo(data_layer),
      verbose = verbose
    ),
    error = function(cnd) {
      rlang::abort(
        fostr_replace_all(conditionMessage(cnd), "file_select", "data_layer"),
        call = NULL
      )
    }
  )

  shape_layer <- enquo(shape_layer)

  if (quo_text(shape_layer) == "data_layer") {
    shape_layer <- data_layer
  }

  if (verbose) cat("Reading geography...\n")

  tryCatch(
    sf_data <- read_ipums_sf(
      shape_file,
      file_select = !!shape_layer,
      verbose = verbose,
      encoding = shape_encoding,
      bind_multiple = TRUE
    ),
    error = function(cnd) {
      rlang::abort(
        fostr_replace_all(conditionMessage(cnd), "file_select", "shape_layer"),
        call = NULL
      )
    }
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
  purrr::walk(
    join_vars,
    function(vvv) {
      attributes(sf_data[[vvv]]) <<- attributes(data[[vvv]])
    }
  )

  # Coerce to data.frame to avoid sf#414 (fixed in development version of sf)
  data <- dplyr::full_join(
    as.data.frame(sf_data),
    as.data.frame(data),
    by = join_vars
  )

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

  tryCatch(
    data <- read_nhgis(
      data_file,
      file_select = !!enquo(data_layer),
      verbose = verbose
    ),
    error = function(cnd) {
      rlang::abort(
        fostr_replace_all(conditionMessage(cnd), "file_select", "data_layer"),
        call = NULL
      )
    }
  )

  shape_layer <- enquo(shape_layer)
  if (quo_text(shape_layer) == "data_layer") shape_layer <- data_layer
  if (verbose) cat("Reading geography...\n")

  tryCatch(
    sp_data <- read_ipums_sp(
      shape_file,
      !!shape_layer,
      verbose = verbose,
      encoding = shape_encoding,
      bind_multiple = TRUE
    ),
    error = function(cnd) {
      rlang::abort(
        fostr_replace_all(conditionMessage(cnd), "file_select", "shape_layer"),
        call = NULL
      )
    }
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
      gis_join_failures <- purrr::pmap_chr(
        missing_in_shape,
        function(...) paste(..., sep = "-")
      )

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

#' Create an `ipums_ddi` object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Constructor to make a new [`ipums_ddi`] object.
#'
#' This function has been deprecated because it is a low-level function that is
#' unlikely to be useful externally. Use [`read_ipums_ddi()`] (for microdata
#' projects) or [`read_nhgis_codebook()`] (for NHGIS) to create an
#' `ipums_ddi` object.
#'
#' @keywords internal
#'
#' @export
make_ddi_from_scratch <- function(file_name = NULL,
                                  file_path = NULL,
                                  file_type = NULL,
                                  ipums_project = NULL,
                                  extract_date = NULL,
                                  extract_notes = NULL,
                                  rectypes = NULL,
                                  rectype_idvar = NULL,
                                  rectypes_keyvars = NULL,
                                  var_info = NULL,
                                  conditions = NULL,
                                  citation = NULL,
                                  file_encoding = NULL) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "make_ddi_from_scratch()",
    details = paste0(
      "Use `read_ipums_ddi()` or `read_nhgis_codebook()` to ",
      "create an `ipums_ddi` object."
    )
  )

  new_ipums_ddi(
    file_name = file_name,
    file_path = file_path,
    file_type = file_type,
    ipums_project = ipums_project,
    extract_date = extract_date,
    extract_notes = extract_notes,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    rectypes_keyvars = rectypes_keyvars,
    var_info = var_info,
    conditions = conditions,
    citation = citation,
    file_encoding = file_encoding
  )
}

load_rgdal_namespace <- function() {
  if (!requireNamespace("rgdal", quietly = TRUE)) {
    rlang::abort(c(
      "The `rgdal` package is required to read IPUMS boundary files.",
      "i" = "Install it with `install.packages(\"rgdal\")`"
    ))
  }
}

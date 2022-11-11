# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read boundary (GIS) files from an IPUMS extract
#'
#' @description
#' Read spatial data from an IPUMS extract into R.
#' Data can be loaded using [`sf`](https://r-spatial.github.io/sf/) or
#' [`sp`](https://cran.r-project.org/web/packages/sp/index.html).
#'
#' @param shape_file Path to a single .shp file, a .zip archive from an IPUMS
#'   extract, or a directory containing at least one .shp file.
#' @param shape_layer For .zip extracts with multiple files, the name of the
#'   shape files to load. Accepts a character vector specifying the file name,
#'   an integer specifying the index of the file to load, or
#'   \code{\link{dplyr_select_style}} conventions. If multiple files are
#'   selected, `bind_multiple` must be equal to `TRUE`.
#' @param vars Names of variables to include in the output data.
#'   By default, includes all variables found in the provided `shape_file`.
#' @param encoding The text encoding to use when reading the shape file. Typically
#'   the defaults should read the data correctly, but for some extracts you may
#'   need to set them manually.
#'   If `NULL`, will attempt to guess the correct encoding using the associated
#'   .cpg file. If none is available, it will default to `"latin1"`.
#'   NHGIS files default to `"latin1"`. Terra files default to `"UTF-8"`.
#' @param bind_multiple If `TRUE` and `shape_file` contains multiple .shp files,
#'   row-bind the files into a single output. Note that some data sources may
#'   not be able to be combined (e.g. `SpatialPolygonsDataFrame` and
#'   `SpatialPointsDataFrame`).
#' @param add_layer_var If `TRUE`, add a variable to the output data indicating
#'   the file that each row originates from. By default, this column will be
#'   named `"layer"`. If `"layer"` already exists in the data, a unique name
#'   will be created to avoid name conflicts. The column name will always be
#'   prefixed with `"layer"`. Defaults to `TRUE` if `bind_multiple = TRUE` and
#'   `FALSE` otherwise.
#' @param ... Additional arguments passed to [`sf::read_sf()`][readr::read_sf]
#'   or [`rgdal::readOGR()`][rgdal::readOGR]. Note that some arguments are
#'   fixed.
#'
#' @return For `read_ipums_sf`, an `sf` object. For `read_ipums_sp`,
#'   a `SpatialPolygonsDataFrame` or `SpatialPointsDataFrame` object.
#'
#' @examples
#' shape_file <- ipums_example("nhgis0707_shape_small.zip")
#' # If sf package is availble, can load as sf object
#' if (require(sf)) {
#'   sf_data <- read_ipums_sf(shape_file)
#' }
#'
#' # If sp package is available, can load as SpatialPolygonsDataFrame
#' if (require(sp) && require(rgdal)) {
#'   sp_data <- read_ipums_sp(shape_file)
#' }
#'
#' @family ipums_read
#' @export
read_ipums_sf <- function(shape_file,
                          shape_layer = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = TRUE,
                          add_layer_var = NULL,
                          ...) {

  dots <- rlang::list2(...)

  shape_layer <- enquo(shape_layer)
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
    shape_layer,
    bind_multiple,
    shape_temp
  )

  encoding <- determine_encoding(read_shape_files, encoding)

  if (!any(grepl("ENCODING", dots$options))) {
    dots$options <- c(paste0("ENCODING=", encoding), dots$options)
  } else {
    rlang::warn(
      paste0(
        "Encoding specified in both `encoding` and `options` arguments. ",
        "Using the value provided to `options`"
      )
    )
  }

  out <- purrr::map2(
    read_shape_files,
    encoding,
    function(.x, .y) {
      args <- c(list(.x), dots)

      this_sf <- rlang::exec(sf::read_sf, !!!args)

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
      ~dplyr::mutate(.x, {{ lyr_name }} := .y, .before = dplyr::everything())
    )
  }

  if (length(sf_list) == 1) {
    return(sf_list[[1]])
  }

  # Get var info for all columns
  all_var_info <- purrr::map_df(
    sf_list,
    .id = "id",
    ~tibble::tibble(name = names(.x), type = purrr::map(.x, class))
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
      ~paste0(
        all_var_info[all_var_info$name == .x, ]$type,
        collapse = " vs. "
      )
    )

    rlang::warn(
      c(
        "Coercing variables with inconsistent types across files: ",
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

  sf::st_as_sf(tibble::as_tibble(out))

}


#' @rdname read_ipums_sf
#' @export
read_ipums_sp <- function(shape_file,
                          shape_layer = NULL,
                          vars = NULL,
                          encoding = NULL,
                          bind_multiple = TRUE,
                          add_layer_var = NULL,
                          ...) {

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

  read_shape_files <- shape_file_prep(
    shape_file,
    shape_layer,
    bind_multiple,
    shape_temp
  )

  encoding <- determine_encoding(read_shape_files, encoding)

  out <- purrr::map2(
    read_shape_files,
    encoding,
    function(.x, .y) {
      this_sp <- rgdal::readOGR(
        dsn = dirname(.x),
        layer = fostr_sub(basename(.x), 1, -5),
        stringsAsFactors = FALSE,
        encoding = .y,
        use_iconv = TRUE,
        ...
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


# Takes a list of SpatialPolygonsDataFrames, fills in empty columns for you and binds
# them together.
# Warns if types don't match and are coerced
careful_sp_rbind <- function(sp_list, add_layer_var = NULL) {

  add_layer_var <- add_layer_var %||% (length(sp_list) > 1)

  if (add_layer_var) {
    existing_cols <- unique(unlist(purrr::map(sp_list, ~colnames(.x@data))))
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

    sp_list <- purrr::imap(
      sp_list,
      function(.x, .y) {
        .x@data <- dplyr::mutate(
          .x@data,
          {{ lyr_name }} := .y,
          .before = dplyr::everything()
        )
        .x
      }
    )
  }

  if (length(sp_list) == 1) {
    return(sp_list[[1]])
  }

  all_var_info <- purrr::map_df(
    sp_list,
    .id = "id",
    ~tibble::tibble(name = names(.x@data), type = purrr::map(.x@data, class))
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
      ~paste0(
        all_var_info[all_var_info$name == .x, ]$type,
        collapse = " vs. "
      )
    )

    rlang::warn(
      c(
        "Coercing variables with inconsistent types across files: ",
        purrr::set_names(
          paste0("\"", bad_vars, "\" (", bad_types, ")"),
          "*"
        )
      )
    )
  }

  # Add missing values for cols that don't exist in all data sources
  out <- purrr::map(
    sp_list,
    function(x) {
      missing_vars <- dplyr::setdiff(all_var_info$name, names(x))

      if (length(missing_vars) == 0) {
        return(x)
      }

      purrr::walk(
        missing_vars,
        function(vn) {
          x@data[[vn]] <<- NA
        }
      )

      x
    }
  )

  out <- do.call(rbind, out)

  out

}

# Encoding:
# Official spec is that shape files must be latin1. But some GIS software
# add to the spec a cpg file that can specify an encoding.
## NHGIS: Place names in 2010 have accents - and are latin1 encoded,
##        No indication of encoding.
## IPUMSI: Brazil has a cpg file indicating the encoding is ANSI 1252,  # This is not true anymore?
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
                            shape_layer,
                            bind_multiple,
                            shape_temp) {

  # Case 1: Shape file specified is a .zip file or a directory
  shape_is_shp <- tools::file_ext(shape_file) == "shp"
  shape_is_zip <- tools::file_ext(shape_file) == "zip"
  shape_is_dir <- tools::file_ext(shape_file) == ""

  # TODO: May need to check that multiple are handled?
  # What if a user wants to bind_multiple on multiple .shp files?
  # This is not currently supported because shape_file takes a single-length
  # input
  if (shape_is_shp) {
    return(shape_file)
  }

  if (shape_is_zip) {
    files <- unzip(shape_file, list = TRUE)$Name
  } else if (shape_is_dir) {
    files <- list.files(shape_file)
  } else {
    rlang::abort(
      paste0(
        "Expected `shape_file` to be a directory, .zip archive,",
        "or .shp file."
      )
    )
  }

  # If no more layers, look for shapefiles:
  if (!any(tools::file_ext(files) == "zip" | tools::file_ext(files) == "")) {

    # First layer has .shp files within it
    shape_shps <- tryCatch(
      find_files_in(
        shape_file,
        "shp",
        shape_layer,
        multiple_ok = bind_multiple,
        none_ok = FALSE
      ),
      error = function(cnd) {
        if (grepl("^Multiple files found", conditionMessage(cnd))) {
          bind_hint <- c("i" = "To combine files, set `bind_multiple = TRUE`")
        } else {
          bind_hint <- NULL
        }

        rlang::abort(
          c(conditionMessage(cnd), bind_hint),
          call = expr(find_files_in())
        )
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
          cpg_file <- ".cpg" == tolower(purrr::map_chr(all_files, ipums_file_ext))
          if (any(cpg_file)) {
            utils::unzip(shape_file, all_files[cpg_file], exdir = shape_temp)
          }

          file.path(shape_temp, shape_shp_files[1])
        } else {
          file.path(shape_file, x)
        }
      }
    )

  } else {

    # First layer has zip files of shape files within it
    shape_zips <- tryCatch(
      find_files_in(
        shape_file,
        "zip",
        shape_layer,
        multiple_ok = bind_multiple,
        none_ok = FALSE
      ),
      error = function(cnd) {
        if (grepl("^Multiple files found", conditionMessage(cnd))) {
          bind_hint <- c("i" = "To combine files, set `bind_multiple = TRUE`")
        } else {
          bind_hint <- NULL
        }

        rlang::abort(
          c(conditionMessage(cnd), bind_hint),
          call = expr(find_files_in())
        )
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

  }

  # If no shapefiles or if multiple and bind_multiple = FALSE (this can happen
  # if someone tries to read a zip of a zip/dir where there are 2 shps in the
  # inner zip) something was not as expected.
  # TODO: improve to inform users the expected structure
  if (length(read_shape_files) == 0 ||
      (length(read_shape_files) > 1 && !bind_multiple)) {
    rlang::abort(
      c(
        "Directory/zip file not formatted as expected. ",
        "i" = "Please check your `shape_layer` argument or unzip and try again."
      )
    )
  }

  read_shape_files

}

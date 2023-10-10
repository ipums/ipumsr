# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# readr does not offer users the ability to override defaults, so we
# only need to worry about encoding, which unfortunately, we are not
# consistent about.
# Default to ISO-8859-1 (eg latin1), because most IPUMS data appears to
# use this. Notably, DDI's explicitly declare it often, and NHGIS is
# (some county names have diacritics).
# However, UTF-8 appears in Terrapop Area extracts (and maybe microdata?)
ipums_locale <- function(encoding = NULL) {
  if (is.null(encoding)) {
    encoding <- "ISO-8859-1"
  }
  readr::locale(encoding = encoding)
}

# Helper function for using dplyr's select functions to select
# rows based on values in a column of a data.frame.
select_var_rows <- function(df, vars, filter_var = "var_name") {
  if (!quo_is_null(vars)) {
    varnames <- df[[filter_var]]
    names(varnames) <- varnames
    vars <- names(tidyselect::eval_select(vars, varnames))

    df <- dplyr::filter(df, .data[[!!filter_var]] %in% vars)
  }

  df
}


find_files_in <- function(file,
                          name_ext = NULL,
                          file_select = quo(NULL),
                          multiple_ok = FALSE,
                          none_ok = TRUE) {
  stopifnot(length(file) == 1)

  if (file_is_zip(file)) {
    file_names <- sort(utils::unzip(file, list = TRUE)$Name)
  } else if (file_is_dir(file)) {
    file_names <- sort(dir(file))
  } else {
    if (!grepl(name_ext, file)) {
      if (none_ok) {
        file <- character(0)
      } else {
        rlang::abort(
          paste0(
            "Expected `file` to match extension \"", name_ext,
            "\", but got \"", tools::file_ext(file), "\"."
          )
        )
      }
    }

    return(file)
  }

  if (!is.null(name_ext)) {
    file_names <- fostr_subset(file_names, paste0("\\.", name_ext, "$"))
  }

  if (!none_ok && length(file_names) == 0) {
    rlang::abort(
      paste0(
        "Did not find any files matching extension \"",
        name_ext, "\" in the provided file path."
      )
    )
  }

  if (!quo_is_null(file_select)) {
    names(file_names) <- file_names

    selection <- tryCatch(
      tidyselect::eval_select(file_select, file_names),
      error = function(cnd) {
        if (none_ok) {
          return(character(0))
        }

        # Rename tidyselect errors for increased clarity in our context
        cnd <- fostr_replace_all(
          fostr_replace_all(conditionMessage(cnd), "column", "file"),
          "Column",
          "File"
        )

        # Add available file names to error message
        rlang::abort(
          c(
            cnd,
            "Available files:", purrr::set_names(file_names, "*")
          ),
          call = expr(find_files_in())
        )
      }
    )

    file_names_sel <- file_names[selection]
  } else {
    file_names_sel <- file_names
  }

  arg_name <- deparse(substitute(file_select))

  if (!none_ok && length(file_names_sel) == 0) {
    rlang::abort(
      c(
        paste0(
          "The provided `", arg_name,
          "` did not select any of the available files:"
        ),
        purrr::set_names(file_names, "*")
      )
    )
  }

  if (!multiple_ok && length(file_names_sel) > 1) {
    rlang::abort(
      c(
        paste0(
          "Multiple files found, please use the `", arg_name, "` argument to ",
          "specify which you want to load:"
        ),
        purrr::set_names(file_names_sel, "*")
      )
    )
  }

  unname(file_names_sel)
}

#' Add IPUMS variable attributes to a data frame
#'
#' @description
#' Add variable attributes from an [ipums_ddi] object to a data frame.
#' These provide contextual information about the variables
#' and values contained in the data columns.
#'
#' Most ipumsr data-reading functions automatically add these attributes.
#' However, some data processing operations may remove attributes, or you
#' may wish to store data in an external database that does not support
#' these attributes. In these cases, use this function to manually attach
#' this information.
#'
#' @details
#' Attribute `val_labels` adds the [`haven_labelled`][haven::labelled] class
#' and the corresponding value labels for applicable variables. For more
#' about the `haven_labelled` class, see
#' `vignette("semantics", package = "haven")`.
#'
#' Attribute `var_label` adds a short summary of the variable's
#' contents to the `"label"` attribute. This label is viewable in the
#' RStudio Viewer.
#'
#' Attribute `var_desc` adds a longer description of the variable's
#' contents to the `"var_desc"` attribute, when available.
#'
#' Variable information is attached to the data by column name. If column
#' names in `data` do not match those found in `var_info`, attributes
#' will not be added.
#'
#' @param data [`tibble`][tibble::tbl_df-class] or data frame
#' @param var_info An [ipums_ddi] object or a data frame containing
#'   variable information. Variable information can be obtained by calling
#'   `ipums_var_info()` on an `ipums_ddi` object.
#' @param var_attrs Variable attributes from the DDI to add to the columns of
#'   the output data. Defaults to all available attributes.
#'
#' @return `data`, with variable attributes attached
#'
#' @export
#'
#' @examples
#' ddi_file <- ipums_example("cps_00157.xml")
#'
#' # Load metadata into `ipums_ddi` object
#' ddi <- read_ipums_ddi(ddi_file)
#'
#' # Load data
#' cps <- read_ipums_micro(ddi)
#'
#' # Data includes variable metadata:
#' ipums_var_desc(cps$INCTOT)
#'
#' # Some operations remove attributes, even if they do not alter the data:
#' cps$INCTOT <- ifelse(TRUE, cps$INCTOT, NA)
#' ipums_var_desc(cps$INCTOT)
#'
#' # We can reattach metadata from the separate `ipums_ddi` object:
#' cps <- set_ipums_var_attributes(cps, ddi)
#' ipums_var_desc(cps$INCTOT)
set_ipums_var_attributes <- function(
    data,
    var_info,
    var_attrs = c("val_labels", "var_label", "var_desc")) {
  if (inherits(var_info, "ipums_ddi")) var_info <- var_info$var_info
  if (is.null(var_info) || is.null(var_attrs)) {
    return(data)
  }

  var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  if (!"val_labels" %in% var_attrs || is.null(var_info$val_labels)) {
    var_info$val_labels <- vector(mode = "list", length = nrow(var_info))
  }
  if (!"var_label" %in% var_attrs || is.null(var_info$var_label)) {
    var_info$var_label <- NA_character_
  }
  if (!"var_desc" %in% var_attrs || is.null(var_info$var_desc)) {
    var_info$var_desc <- NA_character_
  }

  # Don't fail if we have a variable that doesn't match for some reason
  var_info <- dplyr::filter(var_info, .data$var_name %in% names(data))

  # Give error message if type doesn't match between value labels and variable
  # as haven::labelled would
  class_data <- tibble::tibble(
    var_name = names(data),
    d_type = purrr::map(data, typeof),
    d_is_num = purrr::map_lgl(data, is.numeric)
  )

  class_labels <- tibble::tibble(
    var_name = var_info$var_name,
    l_type = purrr::map(var_info$val_labels, ~ typeof(.$val)),
    l_is_num = purrr::map_lgl(
      var_info$val_labels,
      function(x) {
        if (is.null(x) || nrow(x) == 0) NA else is.numeric(x$val)
      }
    )
  )
  class_labels <- dplyr::filter(class_labels, !is.na(.data$l_is_num))

  class_join <- dplyr::inner_join(class_data, class_labels, by = "var_name")
  class_join <- dplyr::mutate(
    class_join,
    coercible = purrr::map2_lgl(.data$d_type, .data$l_type, ~ .x == .y) |
      (.data$d_is_num & .data$l_is_num)
  )
  class_join <- dplyr::filter(class_join, !.data$coercible)

  if (nrow(class_join) > 0) {
    rlang::abort(c(
      "Data types in `val_labels` are inconsistent with those found in data.",
      purrr::set_names(
        paste0("Problem with: `", class_join$var_name, "`"), "x"
      ),
      "i" = "Remove `\"val_labels\"` from `var_attrs` to avoid this error."
    ))
  }

  # Check for values with multiple labels and drop shorter ones because
  # haven will break
  var_info$val_labels <- purrr::map(
    var_info$val_labels,
    function(x) {
      duplicated_values <- unique(x$val[which(duplicated(x$val))])

      for (vvv in duplicated_values) {
        dup_labels <- x$lbl[x$val == vvv]
        longest_label <- x$lbl[x$val == vvv][which.max(nchar(dup_labels))[1]]
        x <- dplyr::distinct(
          x[x$val != vvv | (x$val == vvv & x$lbl == longest_label), ]
        )
      }

      x
    }
  )

  # Convert data frame of value labels to named vector as the labelled class
  # expects
  var_info$val_labels <- purrr::map(
    var_info$val_labels,
    function(x) {
      if (length(x) == 0 || nrow(x) == 0) {
        NULL
      } else {
        purrr::set_names(x$val, x$lbl)
      }
    }
  )

  purrr::walk(
    seq_len(nrow(var_info)),
    function(iii) {
      data[[var_info$var_name[iii]]] <<- set_single_var_attributes(
        data[[var_info$var_name[iii]]],
        var_info$val_labels[[iii]],
        var_info$var_label[[iii]],
        var_info$var_desc[[iii]]
      )
    }
  )

  data
}

set_single_var_attributes <- function(x, val_labels, var_label, var_desc) {
  if (is.na(var_label)) {
    var_label <- NULL
  }

  if (is.na(var_desc)) {
    var_desc <- NULL
  }

  if (!is.null(val_labels)) {
    structure(
      haven::labelled(x, val_labels),
      label = var_label,
      var_desc = var_desc
    )
  } else {
    structure(x, label = var_label, var_desc = var_desc)
  }
}

#' Collect data into R session with IPUMS attributes
#'
#' Convenience wrapper around dplyr's [`collect()`][dplyr::collect] and
#' [set_ipums_var_attributes()]. Use this to attach variable labels when
#' collecting data from a database.
#'
#' @param data A dplyr `tbl` object (generally a `tbl_lazy`
#'   object stored in a database).
#' @param ddi An [ipums_ddi] object created with [read_ipums_ddi()].
#' @param var_attrs  Variable attributes to add to the output. Defaults to
#'   all available attributes.
#'   See [set_ipums_var_attributes()] for more details.
#'
#' @return A local [`tibble`][tibble::tbl_df-class] with the requested
#'   attributes attached.
#'
#' @export
ipums_collect <- function(
    data,
    ddi,
    var_attrs = c("val_labels", "var_label", "var_desc")) {
  var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  set_ipums_var_attributes(dplyr::collect(data), ddi, var_attrs)
}

load_sf_namespace <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    rlang::abort(c(
      "The `sf` package is required to read IPUMS boundary files.",
      "i" = "Install it with `install.packages(\"sf\")`"
    ))
  }
}

file_is_zip <- function(file) {
  ipums_file_ext(file) == ".zip"
}

file_is_dir <- function(file) {
  ipums_file_ext(file) == ""
}

# Treat .gz as an incomplete file extension
ipums_file_ext <- function(file) {
  ext <- tools::file_ext(file)

  if (ext != "") {
    ext <- paste0(".", ext)
  }

  if (ext == ".gz") {
    ext_part1 <- tools::file_ext(tools::file_path_sans_ext(file))
    if (ext_part1 != "") {
      ext <- paste0(".", ext_part1, ext)
    }
  }

  ext
}

file_as_ext <- function(file, ext) {
  paste0(tools::file_path_sans_ext(file, compression = TRUE), ext)
}

# Adapted from readr:::show_progress
show_readr_progress <- function(verbose) {
  verbose && isTRUE(getOption("readr.show_progress")) && interactive() &&
    is.null(getOption("knitr.in.progress"))
}

show_readr_coltypes <- function(verbose) {
  if (is_null(readr::should_show_types())) {
    show_types <- verbose
  } else {
    show_types <- verbose && readr::should_show_types()
  }
  show_types
}

# TODO: Could adapt readr parse_number to be much faster than this.
#       Can't use it directly because parse_number ignores when there
#       are letters and numbers, while readr::parse_guess thinks leading
#       0's means it is string.
custom_parse_double <- function(x, var_msg_info = "variable") {
  converted <- suppressWarnings(as.double(x))
  if (all(is.na(converted) == is.na(x))) {
    return(converted)
  } else {
    rlang::abort(
      paste0("Could not convert ", var_msg_info, " from text to numeric.")
    )
  }
}

custom_parse_integer <- function(x, var_msg_info = "variable") {
  converted <- suppressWarnings(readr::parse_integer(x))
  if (all(is.na(converted) == is.na(x))) {
    return(converted)
  } else {
    rlang::abort(
      paste0("Could not convert ", var_msg_info, " from text to integer.")
    )
  }
}

custom_format_text <- function(..., indent = 0, exdent = 0) {
  text <- paste0(...)
  text <- fostr_split(text, "\n")
  text <- fostr_wrap(text[[1]], indent = indent, exdent = exdent)
  text <- paste(text, collapse = "\n")
  text
}

custom_cat <- function(..., indent = 0, exdent = 0) {
  cat(custom_format_text(..., indent = indent, exdent = exdent))
}

custom_check_file_exists <- function(file, extra_ext = NULL) {
  if (length(file) == 0) {
    rlang::abort("Expected a file path but got an empty value.")
  }
  if (length(file) != 1 && !is.null(extra_ext)) {
    rlang::abort("Bad file path argument.")
  }

  # If a path is passed with a trailing "/", file.exists returns FALSE, so
  # this is a way to remove that trailing /
  file <- file.path(dirname(file), basename(file))

  if (!is.null(extra_ext)) {
    file <- c(file, purrr::map_chr(extra_ext, ~ file_as_ext(file, .)))
  }

  exists_check <- file.exists(file)

  if (!any(exists_check)) {
    file <- file[1]
    if (dirname(file) == ".") {
      rlang::abort(paste0(
        "Could not find file `", file.path(getwd(), basename(file)), "`."
      ))
    } else {
      rlang::abort(paste0("Could not find file `", file, "`"))
    }
  } else {
    file[exists_check][1]
  }
}

path_is_zip_or_dir <- function(file) {
  ext <- tools::file_ext(file)
  ext == "zip" || ext == ""
}

hipread_type_name_convert <- function(x) {
  ifelse(x == "numeric", "double", x)
}

# stringr replacements -----
fostr_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string)
}

fostr_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string)
}

fostr_sub <- function(string, start = 1L, end = -1L) {
  if (start < 0) start <- nchar(string) + start + 1
  if (end < 0) end <- nchar(string) + end + 1
  substr(string, start, end)
}

fostr_detect <- function(string, pattern, negate = FALSE) {
  detect <- grepl(pattern, string)
  if (negate) detect <- !detect
  detect
}

fostr_subset <- function(string, pattern, negate = FALSE) {
  string[fostr_detect(string, pattern, negate)]
}

fostr_wrap <- function(string, width = 80, indent = 0, exdent = 0) {
  out <- strwrap(string, width, indent = indent, exdent = exdent)
  paste(out, collapse = "\n")
}

fostr_split <- function(string, pattern) {
  strsplit(string, pattern)
}

# Replacement for str_match that doesn't quite work the same
# but can be used in similar circumstances
# Uses perl style regexes because they allow named capture groups
# Example:
#  fostr_named_capture(
#    c("title: xyz", "type: book", "structure: unknown", "bad", "name: pair"),
#    "^(?<key>.+?): (?<value>.+)$"
#  )
#  #> # A tibble: 5 x 2
#  #>  key       value
#  #>  <chr>     <chr>
#  #>  1 title     xyz
#  #>  2 type      book
#  #>  3 structure unknown
#  #>  4 ""        ""
#  #>  5 name      pair
fostr_named_capture <- function(string, pattern, only_matches = FALSE) {
  matches <- regexpr(pattern, string, perl = TRUE)
  if (is.null(attr(matches, "capture.start"))) {
    rlang::abort("No named capture items in regex")
  }

  capture_names <- colnames(attr(matches, "capture.start"))
  capture_names <- capture_names[capture_names != ""]

  starts <- purrr::map(
    capture_names,
    ~ attr(matches, "capture.start")[, .]
  )

  ends <- purrr::map2(
    capture_names,
    starts,
    ~ attr(matches, "capture.length")[, .x] + .y - 1
  )

  out <- purrr::pmap_dfc(
    dplyr::tibble(start = starts, end = ends, nm = capture_names),
    function(start, end, nm) {
      nm <- rlang::ensym(nm)
      dplyr::tibble(!!nm := substr(string, start, end))
    }
  )

  if (only_matches) {
    out <- out[out[[1]] != "", ]
  }

  out
}

fostr_named_capture_single <- function(string, pattern, only_matches = FALSE) {
  out <- fostr_named_capture(string, pattern, only_matches)
  if (ncol(out) > 1) {
    rlang::abort("Found multiple capture groups when expected only one")
  }
  out[[1]]
}

#' Simplify GIS files in downloaded NHGIS extract data
#'
#' @description
#' Reads GIS files found in a .zip archive as downloaded from NHGIS,
#' simplifies them to reduce file size, and repackages into the same compressed
#' structure as provided when downloaded directly.
#'
#' This is used primarily to prepare data that will be used in tests,
#' particularly in data-reading tests.
#'
#' @param file Path to zip archive containing the GIS files to simplify. This
#'   is consistent with the file name provided by `download_extract()`
#'
#' @return `file`, invisibly
#'
#' @noRd
simplify_nhgis_gis_file <- function(file) {
  if (!rlang::is_installed("rmapshaper")) {
    rlang::abort("`rmapshaper` is required to simplify an NHGIS GIS extract")
  }

  orig_wd <- getwd()

  path <- fostr_split(file, "/")[[1]]
  dirname <- fostr_replace(path[length(path)], ".zip$", "")

  if (path[length(path) - 1] == "ipumsr") {
    filepath <- getwd()
    shp_dir <- dirname
  } else {
    filepath <- paste0(
      path[(which(path == "ipumsr") + 1):(length(path) - 1)],
      collapse = "/"
    )
    shp_dir <- file.path(filepath, dirname)
  }

  utils::unzip(zipfile = file, exdir = filepath)

  n_files <- length(list.files(shp_dir))

  message("Simplifying files...")

  shp_data <- purrr::map(
    1:n_files,
    ~ tryCatch(
      rmapshaper::ms_simplify(
        read_ipums_sf(file, file_select = !!.x),
        keep = 0.01
      ),
      error = function(cnd) {
        rlang::warn(paste0("Some files could not be simplified."))
        read_ipums_sf(file, file_select = !!.x)
      }
    )
  )

  setwd(shp_dir)

  inner_files_zip_name <- dir()

  inner_files <- purrr::map_chr(
    inner_files_zip_name,
    ~ fostr_replace(
      fostr_subset(utils::unzip(.x, list = TRUE)$Name, ".shp$"),
      ".shp$",
      ""
    )
  )

  unlink(inner_files_zip_name)

  purrr::walk2(
    shp_data,
    inner_files,
    ~ suppressWarnings(
      sf::st_write(
        .x,
        dsn = getwd(),
        layer = .y,
        driver = "ESRI Shapefile"
      )
    )
  )

  purrr::walk2(
    inner_files,
    inner_files_zip_name,
    ~ {
      files <- dir(pattern = .x)
      utils::zip(.y, files)
      unlink(files)
    }
  )

  setwd("..")

  utils::zip(
    paste0(dirname, ".zip"),
    file.path(dirname, dir(dirname))
  )

  unlink(dirname, recursive = TRUE)

  setwd(orig_wd)

  invisible(file)
}

#' Calculate set difference with the empty set represented as `NULL`
#'
#' Convenience function to allow for easier removal of values from extracts
#' whose extract fields can contain `NULL` values.
#'
#' @param x,y Vectors to use to calculate set difference
#'
#' @return Same output as `setdiff`, except that empty set return values
#'   are `NULL` rather than length-0 vectors.
#'
#' @noRd
setdiff_null <- function(x, y) {
  v <- setdiff(x, y)

  if (length(v) == 0) {
    v <- NULL
  }

  v
}

empty_to_null <- function(x) {
  if (is_empty(x)) {
    NULL
  } else {
    x
  }
}

dir_read_deprecated <- function(file) {
  if (is.character(file) && file_is_dir(file)) {
    lifecycle::deprecate_warn(
      "0.6.3",
      I("Reading files through a directory "),
      details = "Please provide the full path to file to be loaded.",
      id = "dir_read",
      env = caller_env(),
      user_env = caller_env(2)
    )
  }
}

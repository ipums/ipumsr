# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr


#' Join tabular data to geographic boundaries
#'
#' @description
#' These functions are analogous to dplyr's [joins][dplyr::left_join()], except
#' that:
#'
#' - They operate on a data frame and an [`sf`][sf::sf] object
#' - They retain the variable attributes provided in IPUMS files and loaded
#'   by ipumsr data-reading functions
#' - They handle minor incompatibilities between attributes in spatial and
#'   tabular data that emerge in some IPUMS files
#'
#' @param data A tibble or data frame. Typically, this will contain data that
#'   has been aggregated to a specific geographic level.
#' @param shape_data An [`sf`][sf::sf] object loaded with [read_ipums_sf()].
#' @param by Character vector of variables to join by. See [dplyr::left_join()]
#'   for syntax.
#' @param suffix If there are non-joined duplicate variables in the two
#'   data sources, these suffixes will be added to the output to disambiguate
#'   them. Should be a character vector of length 2.
#'
#'   Defaults to adding the `"SHAPE"` suffix to duplicated variables in
#'   `shape_file`.
#' @param verbose If `TRUE`, display information about any geometries that were
#'   unmatched during the join.
#'
#' @return An `sf` object containing the joined data
#'
#' @name ipums_shape_join
#'
#' @examplesIf requireNamespace("sf")
#' data <- read_nhgis(
#'   ipums_example("nhgis0972_csv.zip"),
#'   verbose = FALSE
#' )
#'
#' sf_data <- read_ipums_sf(ipums_example("nhgis0972_shape_small.zip"))
#' joined_data <- ipums_shape_inner_join(data, sf_data, by = "GISJOIN")
#'
#' colnames(joined_data)
NULL

#' @rdname ipums_shape_join
#' @export
ipums_shape_left_join <- function(data,
                                  shape_data,
                                  by,
                                  suffix = c("", "SHAPE"),
                                  verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "left", suffix, verbose)
}

#' @rdname ipums_shape_join
#' @export
ipums_shape_right_join <- function(data,
                                   shape_data,
                                   by,
                                   suffix = c("", "SHAPE"),
                                   verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "right", suffix, verbose)
}

#' @rdname ipums_shape_join
#' @export
ipums_shape_inner_join <- function(data,
                                   shape_data,
                                   by,
                                   suffix = c("", "SHAPE"),
                                   verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "inner", suffix, verbose)
}

#' @rdname ipums_shape_join
#' @export
ipums_shape_full_join <- function(data,
                                  shape_data,
                                  by,
                                  suffix = c("", "SHAPE"),
                                  verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "full", suffix, verbose)
}

ipums_shape_join <- function(data,
                             shape_data,
                             by,
                             direction = c("full", "inner", "left", "right"),
                             suffix = c("", "_SHAPE"),
                             verbose = TRUE) {
  UseMethod("ipums_shape_join", shape_data)
}

ipums_shape_join.sf <- function(data,
                                shape_data,
                                by,
                                direction = c("full", "inner", "left", "right"),
                                suffix = c("", "_SHAPE"),
                                verbose = TRUE) {
  if (is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    # Can be a mix of named arguments and unnamed ones. If unnamed
    # use the value as name.
    names(by)[names(by) == ""] <- unname(by)[names(by) == ""]
    by_shape <- unname(by)
    by_data <- names(by)
  }

  direction <- match.arg(direction)

  check_shape_join_names(by_shape, names(shape_data), "`shape_data`")
  check_shape_join_names(by_data, names(data), "`data`")

  # We're pretending like the x in the join is the data, but
  # because of the join functions dispatch, we will actually be
  # doing the reverse. Therefore, rename the variables in shape,
  # and also reverse the suffix.
  if (!is.null(names(by))) {
    shape_data <- dplyr::rename(shape_data, !!!rlang::syms(by))
    by <- names(by)
  }

  suffix <- rev(suffix)

  if (direction == "left") {
    direction <- "right"
  }

  if (direction == "right") {
    direction <- "left"
  }

  aligned <- align_id_vars(shape_data, data, by)

  merge_f <- utils::getFromNamespace(paste0(direction, "_join"), "dplyr")

  out <- merge_f(aligned$shape_data, aligned$data, by = by, suffix = suffix)
  attr(out, "sf_column") <- attr(shape_data, "sf_column")

  # message for merge failures
  if (verbose) {
    join_fail_attributes <- check_for_join_failures(
      out,
      by,
      aligned$shape_data,
      aligned$data
    )
  }

  # Bring variables in data to front of data.frame (but need to get names
  # after possibly renamed by suffix)
  dvars <- names(data)
  renamed <- dvars[dvars %in% names(shape_data) & !dvars %in% by]

  if (length(renamed) > 0) {
    dvars[dvars %in% renamed] <- paste0(renamed, suffix[2])
  }

  out <- dplyr::select(out, dplyr::one_of(dvars), dplyr::everything())

  if (verbose) {
    attr(out, "join_failures") <- join_fail_attributes
  }

  out
}

ipums_shape_join.SpatialPolygonsDataFrame <- function(
    data,
    shape_data,
    by,
    direction = c("full", "inner", "left", "right"),
    suffix = c("", "_SHAPE"),
    verbose = TRUE) {
  if (is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    # Can be a mix of named arguments and unnamed ones. If unnamed
    # use the value as name.
    names(by)[names(by) == ""] <- unname(by)[names(by) == ""]
    by_shape <- unname(by)
    by_data <- names(by)
  }

  direction <- match.arg(direction)

  # Note that directions are reversed because of dispatch
  if (direction %in% c("left", "full")) {
    rlang::abort(paste0(
      "Only inner and right joins are supported for ",
      "`SpatialPolygonsDataFrame` objects to avoid the creation of `NULL` ",
      "geomtries."
    ))
  }

  check_shape_join_names(by_shape, names(shape_data@data), "`shape_data`")
  check_shape_join_names(by_data, names(data), "`data`")

  # We're pretending like the x in the join is the data, but
  # because of the join functions dispatch, we will actually be
  # doing the reverse. Therefore, rename the variables in shape,
  # and also reverse the suffix.
  if (!is.null(names(by))) {
    shape_data@data <- dplyr::rename(shape_data@data, !!!rlang::syms(by))
    by <- names(by)
  }

  suffix <- rev(suffix)

  if (direction == "left") {
    direction <- "right"
  }

  if (direction == "right") {
    direction <- "left"
  }

  aligned <- align_id_vars(shape_data@data, data, by)
  # Use same secret ID variable as the sp package
  # (https://github.com/cran/sp/blob/a7c10d3e1b02db2451ff2bc8435a8518d0b5c692/R/merge.R#L32)
  aligned$data$DoNotUse_temp_sequential_ID_963 <- seq(1, nrow(aligned$data))

  merge_f <- utils::getFromNamespace(paste0(direction, "_join"), "dplyr")

  out <- merge_f(aligned$shape_data, aligned$data, by = by, suffix = suffix)

  if (verbose) {
    join_fail_attributes <- try(
      check_for_join_failures(out, by, aligned$shape_data, aligned$data)
    )

    if (inherits(join_fail_attributes, "try-error")) {
      rlang::warn("Join failures not available.")
      join_fail_attributes <- NULL
    }
  } else {
    join_fail_attributes <- NULL
  }

  out <- dplyr::select(out, dplyr::one_of(names(data)), dplyr::everything())

  # Construct the sp object
  shape_data_out <- shape_data[out$DoNotUse_temp_sequential_ID_963, ]
  out$DoNotUse_temp_sequential_ID_963 <- NULL
  shape_data_out@data <- out
  attr(shape_data_out, "join_failures") <- join_fail_attributes

  shape_data_out
}


check_shape_join_names <- function(by_names, data_names, display) {
  not_avail <- dplyr::setdiff(by_names, data_names)
  if (length(not_avail) > 0) {
    rlang::abort(c(
      paste0("Join columns must be present in ", display, "."),
      purrr::set_names(paste0("Problem with: `", not_avail, "`"), "x")
    ))
  }
}


align_id_vars <- function(shape_data, data, by) {
  shape_type <- dplyr::case_when(
    purrr::map_lgl(by, ~ is.character(shape_data[[.]])) ~ "character",
    purrr::map_lgl(by, ~ is.factor(shape_data[[.]])) ~ "factor",
    purrr::map_lgl(by, ~ is.double(shape_data[[.]])) ~ "double",
    purrr::map_lgl(by, ~ is.integer(shape_data[[.]])) ~ "integer"
  )

  data_type <- dplyr::case_when(
    purrr::map_lgl(by, ~ is.character(data[[.]])) ~ "character",
    purrr::map_lgl(by, ~ is.factor(data[[.]])) ~ "factor",
    purrr::map_lgl(by, ~ is.double(data[[.]])) ~ "double",
    purrr::map_lgl(by, ~ is.integer(data[[.]])) ~ "integer"
  )

  # If one is character but other is double/integer, convert if possible
  # if one is factor and the other is character, convert to character
  # If one is factor and the other is integer/double, give error because I can't
  # really imagine how this happened.
  # TODO: It seems like a lot of people may convert the data from
  #       number -> factor (using the labels) and then try to merge on the
  #       "numeric" id (which is often stored as text in shape file). Consider
  #       trying to give better error in this situation.
  for (iii in seq_along(by)) {
    if (shape_type[iii] == "character") {
      switch(data_type[iii],
        "character" = NULL,
        "factor" = {
          data[[by[iii]]] <- as.character(data[[by[iii]]])
        },
        "double" = {
          shape_data[[by[iii]]] <- custom_parse_double(shape_data[[by[iii]]])
        },
        "integer" = {
          shape_data[[by[iii]]] <- custom_parse_integer(shape_data[[by[iii]]])
        }
      )
    } else if (shape_type[iii] == "factor") {
      switch(data_type[iii],
        "character" = {
          shape_data[[by[iii]]] <- as.character(shape_data[[by[iii]]])
        },
        "factor" = {
          data[[by[iii]]] <- as.character(data[[by[iii]]])
          shape_data[[by[iii]]] <- as.character(shape_data[[by[iii]]])
        },
        "double" = {
          rlang::abort(
            "Shape data has factor id var, but data has numeric id var"
          )
        },
        "integer" = {
          rlang::abort(
            "Shape data has factor id var, but data has integer id var"
          )
        }
      )
    } else if (shape_type[iii] == "double") {
      switch(data_type[iii],
        "character" = {
          data <- custom_parse_double(data[[by[iii]]])
        },
        "factor" = {
          rlang::abort(
            "Shape data has numeric id var, but data has factor id var"
          )
        },
        "double" = NULL,
        "integer" = {
          data[[by[iii]]] <- as.double(data[[by[iii]]])
        }
      )
    } else if (shape_type[iii] == "integer") {
      switch(data_type[iii],
        "character" = {
          data <- custom_parse_integer(data[[by[iii]]])
        },
        "factor" = {
          rlang::abort(
            "Shape data has integer id var, but data has factor id var"
          )
        },
        "double" = {
          shape_data[[by[iii]]] <- as.double(shape_data[[by[iii]]])
        },
        "integer" = NULL
      )
    }

    # Combine attributes
    # (prioritzing data attributes because the DDI has more info)
    shape_attr <- attributes(shape_data[[by[iii]]])
    data_attr <- attributes(data[[by[iii]]])

    overlapping_attr <- dplyr::intersect(names(shape_attr), names(data_attr))
    shape_attr <- shape_attr[!names(shape_attr) %in% overlapping_attr]

    all_attr <- c(data_attr, shape_attr)
    attributes(shape_data[[by[iii]]]) <- all_attr
    attributes(data[[by[iii]]]) <- all_attr
  }

  list(shape_data = shape_data, data = data)
}

check_for_join_failures <- function(merged, by, shape_data, data) {
  merge_fail <- list(
    shape = dplyr::anti_join(shape_data, as.data.frame(merged), by = by),
    data = dplyr::anti_join(data, as.data.frame(merged), by = by)
  )

  sh_num <- nrow(merge_fail$shape)
  d_num <- nrow(merge_fail$data)

  if (sh_num > 0 | d_num > 0) {
    if (sh_num > 0 && d_num > 0) {
      count_message <- paste0(
        sh_num, " observations in the shape file and ",
        d_num, " obervation in data"
      )
    } else if (sh_num > 0) {
      count_message <- paste0(sh_num, " observations in the shape file")
    } else if (d_num > 0) {
      count_message <- paste0(d_num, " observations in the data")
    }

    custom_cat(
      "Some observations were lost in the join (", count_message,
      "). See `join_failures(...)` for more details."
    )

    merge_fail
  } else {
    return(NULL)
  }
}


#' Report on observations dropped during a join
#'
#' Helper to display observations that were not matched when joining tabular
#' and spatial data.
#'
#' @param join_results A data frame that has just been created by an
#'   [ipums shape join][ipums_shape_left_join()].
#'
#' @return A list of data frames, where the first element (`shape`) includes
#'   the observations dropped from the shapefile and the second (`data`)
#'   includes the
#'   observations dropped from the data file.
#'
#' @keywords internal
#'
#' @export
join_failures <- function(join_results) {
  out <- attr(join_results, "join_failures")
  if (is.null(out)) {
    message("No join failures found.")
    NULL
  } else {
    out
  }
}

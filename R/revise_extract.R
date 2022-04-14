# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------


# > Revise extract definition ----

#' Revise a microdata extract definition
#'
#' Revise a microdata extract definition. If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An object of class \code{ipums_extract}.
#' @param description The modified extract description. If NULL (the default),
#'   leave the description unchanged.
#' @param samples_to_add Samples to add to the extract definition, as a
#'   character vector. If NULL (the default), no samples will be added.
#' @param samples_to_remove Samples to remove from the extract definition, as a
#'   character vector. If NULL (the default), no samples will be removed.
#' @param vars_to_add Names of variables to add to the extract definition, as
#'   a character vector. If NULL (the default), no variables will be added.
#' @param vars_to_remove Names of variables to remove from the extract
#'   definition, as a character vector. If NULL (the default), no variables will
#'   be removed.
#' @param data_format The desired data file format for the modified extract
#'   definition. If NULL (the default), leave the data format unchanged.
#' @param data_structure The desired data structure. Currently, this can only be
#'   "rectangular", but "hierarchical" extracts will be supported in the future.
#'   If NULL (the default), leave the data structure unchanged.
#' @param rectangular_on Currently, this can only be "P", but in the future,
#'   household-only extracts (\code{rectangular_on = "H"}) will also be
#'   supported. If NULL, (the default), leave the \code{rectangular_on} field
#'   unchanged.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the modified
#'   extract definition.
#'
#' @examples
#' \dontrun{
#' old_extract <- get_extract_info("usa:33")
#'
#' revised_extract <- revise_extract_micro(
#'   old_extract,
#'   samples_to_add = "us2018a",
#'   vars_to_add = "INCTOT"
#' )
#'
#' submitted_revised_extract <- submit_extract(revised_extract)
#' }
#'
#' @export
revise_extract_micro <- function(extract,
                                 description = NULL,
                                 samples_to_add = NULL,
                                 samples_to_remove = NULL,
                                 vars_to_add = NULL,
                                 vars_to_remove = NULL,
                                 data_format = NULL,
                                 data_structure = NULL,
                                 rectangular_on = NULL) {

  extract <- copy_ipums_extract(extract)
  extract$description <- paste0("Revision of (", extract$description, ")")

  extract <- add_to_extract(extract, "samples", samples_to_add)
  extract <- remove_from_extract(extract, "samples", samples_to_remove)

  extract <- add_to_extract(extract, "variables", vars_to_add)
  extract <- remove_from_extract(extract, "variables", vars_to_remove)

  if (!is.null(description)) extract$description <- description
  if (!is.null(data_format)) extract$data_format <- data_format
  if (!is.null(data_structure)) {
    if (data_structure != "rectangular") {
      stop(
        "Currently, the `data_structure` argument must be equal to ",
        "\"rectangular\"; in the future, the API will also support ",
        "\"hierarchical\" extracts.",
        call. = FALSE
      )
    }
  }
  if (!is.null(rectangular_on)) {
    if (rectangular_on != "P") {
      stop(
        "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
        "the future, the API will also support `rectangular_on = \"H\".",
        call. = FALSE
      )
    }
  }

  extract <- validate_ipums_extract(extract)

  extract
}

add_to_nhgis_extract <- function(extract,
                                 datasets = NULL,
                                 data_tables = NULL,
                                 ds_geog_levels = NULL,
                                 years = NULL,
                                 breakdown_values = NULL,
                                 time_series_tables = NULL,
                                 ts_geog_levels = NULL,
                                 shapefiles = NULL,
                                 data_format = NULL,
                                 breakdown_and_data_type_layout = NULL,
                                 time_series_table_layout = NULL,
                                 geographic_extents = NULL,
                                 description = NULL,
                                 validate = TRUE) {

  extract <- copy_ipums_extract(extract)

  datasets <- datasets %||% extract$datasets
  time_series_tables <- time_series_tables %||% extract$time_series_tables

  n_ds <- length(datasets)
  ds_in_extract <- datasets %in% extract$datasets

  old_ds_i <- which(ds_in_extract)
  new_ds_i <- which(!ds_in_extract)

  old_ds <- datasets[old_ds_i]
  new_ds <- datasets[new_ds_i]

  n_ts <- length(time_series_tables)
  ts_in_extract <- time_series_tables %in% extract$time_series_tables

  old_ts_i <- which(ts_in_extract)
  new_ts_i <- which(!ts_in_extract)

  old_ts <- time_series_tables[old_ts_i]
  new_ts <- time_series_tables[new_ts_i]

  ds_args <- purrr::map(
    list(
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years,
      breakdown_values = breakdown_values
    ),
    ~recycle_to_list(.x, n_ds, datasets)
  )

  ts_args <- purrr::map(
    list(
      ts_geog_levels = ts_geog_levels
    ),
    ~recycle_to_list(.x, n_ts, time_series_tables)
  )

  ds_arg_length <- purrr::map_dbl(ds_args, ~length(.x))
  ds_wrong_length <- purrr::map_lgl(ds_arg_length, ~.x != (n_ds))

  ts_arg_length <- purrr::map_dbl(ts_args, ~length(.x))
  ts_wrong_length <- purrr::map_lgl(ts_arg_length, ~.x != (n_ts))

  if (any(ds_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ds_wrong_length),
      ~paste0("`", .x, "` (", ds_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ds_wrong_length], collapse = ", "),
      " does not match the number of datasets to be modified (", n_ds,
      "). To recycle selections across datasets, ensure values are stored ",
      "in a vector, not a list.",
      call. = FALSE
    )
  }

  if (any(ts_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ts_wrong_length),
      ~paste0("`", .x, "` (", ts_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ts_wrong_length], collapse = ", "),
      " does not match the number of time series tables to be modified (", n_ts,
      "). To recycle selections across time series tables, ensure values ",
      "are stored in a vector, not a list.",
      call. = FALSE
    )
  }

  extract <- add_datasets(
    extract,
    datasets = new_ds,
    data_tables = ds_args$data_tables[new_ds_i],
    ds_geog_levels = ds_args$ds_geog_levels[new_ds_i],
    years = ds_args$years[new_ds_i],
    breakdown_values = ds_args$breakdown_values[new_ds_i]
  )

  extract <- modify_datasets(
    extract,
    datasets = old_ds,
    data_tables = ds_args$data_tables[old_ds_i],
    ds_geog_levels = ds_args$ds_geog_levels[old_ds_i],
    years = ds_args$years[old_ds_i],
    breakdown_values = ds_args$breakdown_values[old_ds_i],
    add = TRUE
  )

  extract <- add_time_series_tables(
    extract,
    time_series_tables = new_ts,
    ts_geog_levels = ts_args$ts_geog_levels[new_ts_i]
  )

  extract <- modify_time_series_tables(
    extract,
    time_series_tables = old_ts,
    ts_geog_levels = ts_args$ts_geog_levels[old_ts_i],
    add = TRUE
  )

  extract <- add_to_ancillary_fields(
    extract,
    description = description,
    shapefiles = shapefiles,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout,
    geographic_extents = geographic_extents
  )

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

remove_from_nhgis_extract <- function(extract,
                                      datasets = NULL,
                                      data_tables = NULL,
                                      ds_geog_levels = NULL,
                                      years = NULL,
                                      breakdown_values = NULL,
                                      time_series_tables = NULL,
                                      ts_geog_levels = NULL,
                                      shapefiles = NULL,
                                      data_format = NULL,
                                      breakdown_and_data_type_layout = NULL,
                                      time_series_table_layout = NULL,
                                      geographic_extents = NULL,
                                      description = NULL,
                                      validate = TRUE) {

  extract <- copy_ipums_extract(extract)

  ds_provided <- !is.null(datasets)
  ts_provided <- !is.null(time_series_tables)

  datasets <- datasets %||% extract$datasets
  time_series_tables <- time_series_tables %||% extract$time_series_tables

  n_ds <- length(datasets)
  n_ts <- length(time_series_tables)

  new_ds <- setdiff(datasets, extract$datasets)
  new_ts <- setdiff(time_series_tables, extract$time_series_tables)

  if (length(new_ds) > 0) {
    warning(
      "Some datasets (\"",
      paste0(new_ds, collapse = "\", \""),
      "\") could not be modified because they were not found in this extract's ",
      "datasets.",
      call. = FALSE
    )
  }

  if (length(new_ts) > 0) {
    warning(
      "Some time series tables (\"",
      paste0(new_ts, collapse = "\", \""),
      "\") could not be modified because they were not found in this extract's ",
      "datasets.",
      call. = FALSE
    )
  }

  ds_args <- purrr::map(
    list(
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years,
      breakdown_values = breakdown_values
    ),
    ~recycle_to_list(.x, n_ds, datasets)
  )

  ts_args <- purrr::map(
    list(
      ts_geog_levels = ts_geog_levels
    ),
    ~recycle_to_list(.x, n_ts, time_series_tables)
  )

  ds_arg_length <- purrr::map_dbl(ds_args, ~length(.x))
  ds_wrong_length <- purrr::map_lgl(ds_arg_length, ~.x != n_ds)

  ts_arg_length <- purrr::map_dbl(ts_args, ~length(.x))
  ts_wrong_length <- purrr::map_lgl(ts_arg_length, ~.x != (n_ts))

  if (any(ds_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ds_wrong_length),
      ~paste0("`", .x, "` (", ds_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ds_wrong_length], collapse = ", "),
      " does not match the number of datasets to be modified (", n_ds,
      "). To recycle selections across datasets, ensure values are stored ",
      "in a vector, not a list.",
      call. = FALSE
    )
  }

  if (any(ts_wrong_length)) {

    length_msg <- purrr::imap_chr(
      names(ts_wrong_length),
      ~paste0("`", .x, "` (", ts_arg_length[.y], ")")
    )

    stop(
      "The number of selections provided in ",
      paste0(length_msg[ts_wrong_length], collapse = ", "),
      " does not match the number of time series tables to be modified (", n_ts,
      "). To recycle selections across time series tables, ensure values ",
      "are stored in a vector, not a list.",
      call. = FALSE
    )
  }

  # If subarguments are null, we interpret that user wants to remove full
  # datasets/tsts, if provided.
  remove_full_ds <- all(purrr::map_lgl(ds_args, ~is.null(unlist(.x))))
  remove_full_ts <- all(purrr::map_lgl(ts_args, ~is.null(unlist(.x))))

  if (remove_full_ds && ds_provided) {
    extract <- remove_datasets(
      extract,
      datasets = datasets
    )
  } else {
    extract <- modify_datasets(
      extract,
      datasets = datasets,
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years,
      breakdown_values = breakdown_values,
      add = FALSE
    )
  }

  if (remove_full_ts && ts_provided) {
    extract <- remove_time_series_tables(
      extract,
      time_series_tables = time_series_tables
    )
  } else {
    extract <- modify_time_series_tables(
      extract,
      time_series_tables = time_series_tables,
      ts_geog_levels = ts_geog_levels,
      add = FALSE
    )
  }

  # Handle updates to non-nested fields
  extract <- remove_from_ancillary_fields(
    extract,
    description = description,
    shapefiles = shapefiles,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout,
    geographic_extents = geographic_extents
  )

  # If removal results in extract with no ds/tst, remove irrelevant values
  # for data format
  if (is.null(extract$datasets) && is.null(extract$time_series_tables)) {
    extract["data_format"] <- list(NULL)
  }

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

# Internal -----------------------------------------------------------

add_datasets <- function(extract,
                         datasets = NULL,
                         data_tables = NULL,
                         ds_geog_levels = NULL,
                         years = NULL,
                         breakdown_values = NULL) {

  new_ds <- setdiff(datasets, extract$datasets)
  n_new_ds <- length(new_ds)

  if (n_new_ds == 0) {
    return(extract)
  }

  extract$datasets <- union(extract$datasets, new_ds)

  new_subfields <- list(
    data_tables = data_tables,
    ds_geog_levels = ds_geog_levels,
    years = years,
    breakdown_values = breakdown_values
  )

  purrr::walk(
    names(new_subfields),
    ~{
      new_val <- c(
        extract[[.x]],
        recycle_to_list(new_subfields[[.x]], n_new_ds, new_ds)
      )
      extract[[.x]] <<- new_val
    }
  )

  extract

}


add_time_series_tables <- function(extract,
                                   time_series_tables = NULL,
                                   ts_geog_levels = NULL) {

  new_ts <- setdiff(time_series_tables, extract$time_series_tables)
  n_new_ts <- length(new_ts)

  if (n_new_ts == 0) {
    return(extract)
  }

  extract$time_series_tables <- union(extract$time_series_tables, new_ts)

  extract$ts_geog_levels <- c(
    extract$ts_geog_levels,
    recycle_to_list(ts_geog_levels, n_new_ts, new_ts)
  )

  # if (!is.null(data_format)) {
  #   extract$data_format <- data_format
  # }
  #
  # if (!is.null(time_series_table_layout)) {
  #   extract$time_series_table_layout <- time_series_table_layout
  # }

  extract

}

add_to_ancillary_fields <- function(extract,
                                    description = NULL,
                                    shapefiles = NULL,
                                    data_format = NULL,
                                    breakdown_and_data_type_layout = NULL,
                                    time_series_table_layout = NULL,
                                    geographic_extents = NULL) {

  replace_vars <- list(
    description = description,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout
  )

  modify_vars <- list(
    shapefiles = shapefiles,
    geographic_extents = geographic_extents
  )

  purrr::walk(
    names(replace_vars),
    ~{
      if (!is.null(replace_vars[[.x]])) extract[[.x]] <<- replace_vars[[.x]]
    }
  )

  purrr::walk(
    names(modify_vars),
    ~{
      if (is.null(modify_vars[[.x]]) && is.null(extract[[.x]])) {
        extract[.x] <<- list(NULL)
      } else {
        extract[[.x]] <<- union(extract[[.x]], modify_vars[[.x]])
      }
    }
  )

  extract

}

remove_datasets <- function(extract, datasets = NULL) {

  datasets <- setdiff(extract$datasets, datasets)

  if (length(datasets) == 0) {
    no_ds <- TRUE
    datasets <- NULL
    extract["datasets"] <- list(NULL)
    extract["breakdown_and_data_type_layout"] <- list(NULL)
    extract["geographic_extents"] <- list(NULL)
  } else {
    no_ds <- FALSE
    extract$datasets <- datasets
  }

  purrr::walk(
    c("data_tables",
      "ds_geog_levels",
      "years",
      "breakdown_values"),
    ~{
      if (no_ds) {
        extract[.x] <<- list(NULL)
      } else {
        extract[.x] <<- list(extract[[.x]][datasets])
      }
    }
  )

  extract

}

remove_time_series_tables <- function(extract, time_series_tables = NULL) {

  time_series_tables <- setdiff(extract$time_series_tables, time_series_tables)

  if (length(time_series_tables) == 0) {

    time_series_tables <- NULL
    extract["time_series_tables"] <- list(NULL)
    extract["ts_geog_levels"] <- list(NULL)
    extract["time_series_table_layout"] <- list(NULL)

  } else {

    extract$time_series_tables <- time_series_tables
    extract$ts_geog_levels <- extract$ts_geog_levels[time_series_tables]

  }

  extract

}

remove_from_ancillary_fields <- function(extract,
                                         description = NULL,
                                         shapefiles = NULL,
                                         data_format = NULL,
                                         breakdown_and_data_type_layout = NULL,
                                         time_series_table_layout = NULL,
                                         geographic_extents = NULL) {

  modify_vars <- list(
    description = description,
    shapefiles = shapefiles,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout,
    geographic_extents = geographic_extents
  )

  purrr::walk(
    names(modify_vars),
    function(x) {
      values <- setdiff(extract[[x]], unlist(modify_vars[[x]]))
      if (length(values) > 0) {
        extract[[x]] <<- values
      } else {
        extract[x] <<- list(NULL)
      }
    }
  )

  extract

}

modify_datasets <- function(extract,
                            datasets = NULL,
                            data_tables = NULL,
                            ds_geog_levels = NULL,
                            years = NULL,
                            breakdown_values = NULL,
                            add = TRUE,
                            validate = TRUE) {

  mod_vals <- list(
    data_tables = data_tables,
    ds_geog_levels = ds_geog_levels,
    years = years,
    breakdown_values = breakdown_values
  )

  purrr::walk(
    names(mod_vals),
    ~{
      extract[[.x]] <<- modify_list(
        extract[[.x]],
        to_modify = datasets,
        values = mod_vals[[.x]],
        add = add
      )
    }
  )

  extract

}

modify_time_series_tables <- function(extract,
                                      time_series_tables = NULL,
                                      ts_geog_levels = NULL,
                                      add = TRUE) {
  mod_vals <- list(
    ts_geog_levels = ts_geog_levels
  )

  purrr::walk(
    names(mod_vals),
    ~{
      extract[[.x]] <<- modify_list(
        extract[[.x]],
        to_modify = time_series_tables,
        values = mod_vals[[.x]],
        add = add
      )
    }
  )

  extract

}

modify_list <- function(l, to_modify = NULL, values = NULL, add = TRUE) {

  if (is.null(to_modify)) {
    to_modify <- names(l)
  }

  # values <- unlist(values)
  values <- recycle_to_list(values, length(to_modify), to_modify)

  if (add) {
    modified <- purrr::map(
      names(l),
      function(x) {
        if (x %in% to_modify) {
          union(l[[x]], unlist(values[which(x == to_modify)]))
        } else {
          l[[x]]
        }
      }
    )
  } else {
    modified <- purrr::map(
      names(l),
      function(x) {
        if (x %in% to_modify) {
          d <- setdiff(l[[x]], unlist(values[which(x == to_modify)]))
          if (length(d) == 0) {
            NULL
          } else {
            d
          }
        } else {
          l[[x]]
        }
      }
    )
  }

  setNames(modified, names(l))

}

copy_ipums_extract <- function(extract) {
  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract
}

add_to_extract <- function(extract, samples_or_variables, names_to_add) {
  if (is.null(names_to_add)) {
    return(extract)
  }
  if (any(names_to_add %in% extract[[samples_or_variables]])) {
    warning(
      "The following ", samples_or_variables, " are already included in the ",
      "supplied extract definition, and thus will not be added: ",
      paste0(
        intersect(names_to_add, extract[[samples_or_variables]]),
        collapse = ", "
      ),
      call. = FALSE
    )
    names_to_add <- setdiff(names_to_add, extract[[samples_or_variables]])
  }
  extract[[samples_or_variables]] <- c(
    extract[[samples_or_variables]],
    names_to_add
  )
  extract
}

remove_from_extract <- function(extract,
                                samples_or_variables,
                                names_to_remove) {
  if (is.null(names_to_remove)) {
    return(extract)
  }
  if (!all(names_to_remove %in% extract[[samples_or_variables]])) {
    warning(
      "The following ", samples_or_variables, " are not included in the ",
      "supplied extract definition, and thus will not be removed: ",
      paste0(
        setdiff(names_to_remove, extract[[samples_or_variables]]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  extract[[samples_or_variables]] <- setdiff(
    extract[[samples_or_variables]],
    names_to_remove
  )
  extract
}


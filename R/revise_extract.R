
add_to_nhgis_extract <- function(extract,
                                 modify_ds = NULL,
                                 modify_ts = NULL,
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

  if (!is.null(modify_ds) && !is.null(datasets)) {
    stop(
      "Both `modify_ds` and `datasets` were provided. ",
      "Please either add sub-values to existing ",
      "datasets or add new datasets, not both.",
      call. = FALSE
    )
  }

  if (!is.null(modify_ts) && !is.null(time_series_tables)) {
    stop(
      "Both `modify_ts` and `time_series_tables` were provided. ",
      "Please either add sub-values to existing ",
      "time series tables or add new time series tables, not both.",
      call. = FALSE
    )
  }

  old_ds <- intersect(datasets, extract$datasets)
  old_ts <- intersect(time_series_tables, extract$time_series_tables)

  missing_ds <- setdiff(modify_ds, extract$datasets)
  missing_ts <- setdiff(modify_ts, extract$time_series_tables)

  if (length(old_ds) > 0) {
    warning(
      "Some datasets (\"",
      paste0(old_ds, collapse = "\", \""),
      "\") could not be added because they already exist in this extract. ",
      "To modify existing datasets, use the `modify_ds` ",
      "argument.",
      call. = FALSE
    )
  }

  if (length(old_ts) > 0) {
    warning(
      "Some time series tables (\"",
      paste0(old_ts, collapse = "\", \""),
      "\") could not be added because they already exist in this extract. ",
      "To modify existing time series tables, use the ",
      "`modify_ts` argument.",
      call. = FALSE
    )
  }

  if (length(missing_ds) > 0) {
    warning(
      paste0(
        "Some datasets (\"",
        paste0(missing_ds, collapse =  "\", \""),
        "\") could not be modified because they were not found in this ",
        "extract's datasets (\"",
        paste0(extract$datasets, collapse = "\", \""), "\"). ",
        "To add new datasets, use the `datasets` argument."
      ),
      call. = FALSE
    )

    modify_ds <- intersect(modify_ds, extract$datasets)
    if(length(modify_ds) == 0) modify_ds <- NULL
  }

  if (length(missing_ts) > 0) {
    warning(
      paste0(
        "Some time series tables (\"",
        paste0(missing_ts, collapse =  "\", \""),
        "\") could not be modified because they were not found in this ",
        "extract's time series tables (\"",
        paste0(extract$time_series_tables, collapse = "\", \""), "\"). ",
        "To add new time series tables, use the `time_series_tables` argument."
      ),
      call. = FALSE
    )

    modify_ts <- intersect(modify_ts, extract$time_series_tables)
    if(length(modify_ts) == 0) modify_ts <- NULL
  }

  if (!is.null(datasets)) {
    extract <- add_datasets(
      extract,
      datasets = datasets,
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years, # Needs to be coerced to character
      breakdown_values = breakdown_values
    )
  } else {
    extract <- modify_datasets(
      extract,
      datasets = modify_ds,
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years, # Needs to be coerced to character
      breakdown_values = breakdown_values,
      add = TRUE
    )
  }

  if (!is.null(time_series_tables)) {
    extract <- add_time_series_tables(
      extract,
      time_series_tables = time_series_tables,
      ts_geog_levels = ts_geog_levels
    )
  } else {
    extract <- modify_time_series_tables(
      extract,
      time_series_tables = modify_ts,
      ts_geog_levels = ts_geog_levels,
      add = TRUE
    )
  }

  # Additionally, modify any ancillary variables specified, replacing if needed
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
                                      modify_ds = NULL,
                                      modify_ts = NULL,
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

  # These cases are actually handled properly, but throwing errors to provide
  # parallel behavior to add_to_nhgis_extract()
  if (!is.null(modify_ds) && !is.null(datasets)) {
    stop(
      "Both `modify_ds` and `datasets` were provided. ",
      "Please either remove sub-values from ",
      "existing datasets or remove full datasets, not both.",
      call. = FALSE
    )
  }

  if (!is.null(modify_ts) && !is.null(time_series_tables)) {
    stop(
      "Both `modify_ts` and `time_series_tables` were provided. ",
      "Please either remove sub-values from existing time series tables ",
      "or remove full time series tables, not both.",
      call. = FALSE
    )
  }

  new_ds <- setdiff(datasets, extract$datasets)
  new_ts <- setdiff(time_series_tables, extract$time_series_tables)

  missing_ds <- setdiff(modify_ds, extract$datasets)
  missing_ts <- setdiff(modify_ts, extract$time_series_tables)

  if (length(new_ds) > 0) {
    warning(
      "Some datasets (\"",
      paste0(new_ds, collapse = "\", \""),
      "\") could not be removed because they were not found in this extract's ",
      "datasets.",
      call. = FALSE
    )
  }

  if (length(new_ts) > 0) {
    warning(
      "Some time series tables (\"",
      paste0(new_ts, collapse = "\", \""),
      "\") could not be removed because they were not found in this extract's ",
      "datasets.",
      call. = FALSE
    )
  }

  if (length(missing_ds) > 0) {
    warning(
      paste0(
        "Some datasets (\"",
        paste0(missing_ds, collapse =  "\", \""),
        "\") could not be modified because they were not found in this ",
        "extract's datasets (\"",
        paste0(extract$datasets, collapse = "\", \""), "\")."
      ),
      call. = FALSE
    )

    modify_ds <- intersect(modify_ds, extract$datasets)
    if(length(modify_ds) == 0) modify_ds <- NULL
  }

  if (length(missing_ts) > 0) {
    warning(
      paste0(
        "Some time series tables (\"",
        paste0(missing_ts, collapse =  "\", \""),
        "\") could not be modified because they were not found in this ",
        "extract's time series tables (\"",
        paste0(extract$time_series_tables, collapse = "\", \""), "\")."
      ),
      call. = FALSE
    )

    modify_ts <- intersect(modify_ts, extract$time_series_tables)
    if(length(modify_ts) == 0) modify_ts <- NULL
  }

  if (!is.null(datasets)) {
    extract <- remove_datasets(
      extract,
      datasets = datasets
    )
  } else {
    extract <- modify_datasets(
      extract,
      datasets = modify_ds,
      data_tables = data_tables,
      ds_geog_levels = ds_geog_levels,
      years = years, # Needs to be coerced to character
      breakdown_values = breakdown_values,
      add = FALSE
    )
  }

  if (!is.null(time_series_tables)) {
    extract <- remove_time_series_tables(
      extract,
      time_series_tables = time_series_tables
    )
  } else {
    extract <- modify_time_series_tables(
      extract,
      time_series_tables = modify_ts,
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

  if (is.null(extract$datasets) && is.null(extract$time_series_tables)) {
    extract["data_format"] <- list(NULL)
  }

  if (validate) {
    extract <- validate_ipums_extract(extract)
  }

  extract

}

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

  values <- unlist(values)

  if (add) {
    modified <- purrr::map(
      names(l),
      function(x) {
        if (x %in% to_modify) {
          union(l[[x]], values)
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
          d <- setdiff(l[[x]], values)
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

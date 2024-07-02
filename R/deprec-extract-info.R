# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Get information on recent extracts
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are now defunct. Use [get_extract_history()] to obtain
#' extract history as a list.
#'
#' @export
#'
#' @keywords internal
get_recent_extracts_info_list <- function(collection = NULL,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {
  lifecycle::deprecate_stop(
    "0.6.0",
    "get_recent_extracts_info_list()",
    "get_extract_history()"
  )

  get_extract_history(
    collection = collection,
    how_many = how_many,
    api_key = api_key
  )
}

#' @rdname get_recent_extracts_info_list
#' @export
get_recent_extracts_info_tbl <- function(collection = NULL,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {
  lifecycle::deprecate_stop(
    "0.6.0",
    "get_recent_extracts_info_tbl()",
    "get_extract_history()"
  )

  extracts <- get_extract_history(
    collection = collection,
    how_many = how_many,
    api_key = api_key
  )

  extract_list_to_tbl(extracts)
}

#' Convert recent extract definitions from tibble to list format
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are now defunct. Use [get_extract_history()] to obtain
#' extract history as a list.
#'
#' @export
#'
#' @keywords internal
extract_tbl_to_list <- function(extract_tbl, validate = TRUE) {
  lifecycle::deprecate_stop(
    "0.6.0",
    "extract_tbl_to_list()"
  )

  collection <- unique(extract_tbl$collection)

  if (length(collection) > 1) {
    rlang::abort(
      "All extracts in `extract_tbl` must belong to same collection."
    )
  }

  if (length(collection) == 0) {
    rlang::abort("Cannot convert empty `extract_tbl` to list")
  }

  expected_names <- get_extract_tbl_fields(
    new_ipums_extract(collection = collection)
  )

  unexpected_names <- setdiff(names(extract_tbl), expected_names)

  if (length(unexpected_names) > 0) {
    rlang::abort(
      c(
        "Unexpected names in `extract_tbl`: ",
        paste0("\"", unexpected_names, "\"", collapse = ", ")
      )
    )
  }

  # TODO: This conversion process is incredibly fragile. Extracts
  # are not designed to be represented in tabular form. I think we should
  # consider deprecating this functionality entirely.
  if (collection == "nhgis") {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      rlang::abort(
        paste0(
          "The `tidyr` package must be installed to convert NHGIS extracts ",
          "from tbl to list format."
        )
      )
    }

    ds <- extract_tbl %>%
      dplyr::filter(.data$data_type == "datasets")

    # If any datasets, coerce to ds_spec objects
    if (nrow(ds) > 0) {
      ds <- ds %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "datasets" = set_nested_names(
            list(
              ds_spec(
                .data$name,
                .data$data_tables,
                .data$geog_levels,
                .data$years,
                .data$breakdown_values
              )
            )
          )
        )
    }

    tst <- extract_tbl %>%
      dplyr::filter(.data$data_type == "time_series_tables")

    # If any tsts, coerce to tst_spec objects
    if (nrow(tst) > 0) {
      tst <- tst %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "time_series_tables" = set_nested_names(
            list(
              tst_spec(.data$name, .data$geog_levels, .data$years)
            )
          )
        )
    }

    # Relabel shapefiles as `shapefiles` instead of `name`
    shp <- extract_tbl %>%
      dplyr::filter(.data$data_type == "shapefiles") %>%
      dplyr::rename("shapefiles" = "name")

    # For each extract number, produce a single row containing all
    # datasets, time series tables, and shapefiles. This requires additional
    # handling to ensure that we create NULL objects for missing fields rather
    # than throwing errors.
    extract_tbl <- dplyr::bind_rows(ds, tst, shp) %>%
      dplyr::select(-c(
        "data_type", "name", "data_tables", "geog_levels",
        "years", "breakdown_values"
      )) %>%
      dplyr::group_by(.data$number) %>%
      dplyr::mutate(
        "datasets" = tryCatch(
          purrr::map(list(.data$datasets), purrr::compact),
          error = function(cnd) list(NULL)
        ),
        "time_series_tables" = tryCatch(
          purrr::map(list(.data$time_series_tables), purrr::compact),
          error = function(cnd) list(NULL)
        ),
        "shapefiles" = tryCatch(
          purrr::map(list(.data$shapefiles), ~ purrr::compact(.x[!is.na(.x)])),
          error = function(cnd) list(NULL)
        ),
        "datasets" = purrr::map(
          .data$datasets,
          empty_to_null
        ),
        "time_series_tables" = purrr::map(
          .data$time_series_tables,
          empty_to_null
        ),
        "shapefiles" = purrr::map(
          .data$shapefiles,
          empty_to_null
        )
      ) %>%
      tidyr::fill("shapefiles", .direction = "downup") %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(.data$number)) %>%
      dplyr::ungroup()

    # Names of output columns to be used
    out_vars <- c(
      "collection", "number",
      "description", "datasets", "time_series_tables",
      "geographic_extents", "shapefiles",
      "breakdown_and_data_type_layout",
      "tst_layout", "data_format",
      "submitted", "download_links", "status"
    )
  } else {
    # Convert samples and variables to samp_spec and var_spec objects
    # based on associated values in other tbl columns
    extract_tbl <- extract_tbl %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        samples = list(
          set_nested_names(
            purrr::map(
              .data$samples,
              ~ samp_spec(name = .x)
            )
          )
        ),
        variables = list(
          set_nested_names(
            purrr::map(
              .data$variables,
              ~ var_spec(
                name = .x,
                case_selections = .data$case_selections[[.x]],
                case_selection_type = .data$case_selection_type[[.x]],
                data_quality_flags = .data$var_data_quality_flags[[.x]],
                attached_characteristics = .data$attached_characteristics[[.x]],
                preselected = .data$preselected[[.x]]
              )
            )
          )
        )
      )

    # Names of output columns to be used
    out_vars <- c(
      "collection", "number",
      "description", "samples", "variables", "data_format",
      "data_structure", "rectangular_on", "case_select_who",
      "data_quality_flags", "submitted", "download_links", "status"
    )
  }

  extract_tbl <- dplyr::select(extract_tbl, dplyr::any_of(out_vars))

  # Remove number because otherwise it is coerced to NULL in
  # unsubmitted extracts
  numbers <- extract_tbl$number

  # Handling for fields that are NA in tbl but NULL in object
  extract_tbl <- purrr::map(
    extract_tbl[, setdiff(out_vars, "number")],
    function(c) {
      purrr::map(
        c,
        ~ if ((is_empty(.x) && !is_named(.x)) || is_na(.x)) {
          NULL
        } else {
          .x
        }
      )
    }
  )

  extract_tbl <- c(extract_tbl, list(number = numbers))

  extract_list <- purrr::pmap(extract_tbl, new_ipums_extract)

  if (validate) {
    extract_list <- purrr::walk(extract_list, validate_ipums_extract)
  }

  extract_list
}

#' @rdname extract_tbl_to_list
#' @export
extract_list_to_tbl <- function(extract_list) {
  lifecycle::deprecate_stop(
    "0.6.0",
    "extract_list_to_tbl()"
  )

  if ("ipums_extract" %in% class(extract_list)) {
    extract_list <- list(extract_list)
  }

  extract_types <- unique(
    purrr::map_chr(
      extract_list,
      function(x) x$collection
    )
  )

  if (length(extract_types) != 1) {
    rlang::abort(
      "All extracts in `extract_list` must belong to same collection."
    )
  }

  purrr::map_dfr(extract_list, extract_to_tbl)
}

#' Convert a single extract to a tibble
#'
#' @description
#' S3 generic to allow for collection-specific method dispatch when converting
#' extract objects to tibble format. Collection-specific functionality is
#' needed because NHGIS extracts return a tibble whose extracts are spread
#' across multiple rows. However, we cannot perform dispatch on a list of
#' extract objects directly, as is done in `extract_list_to_tbl`.
#'
#' @param x An `ipums_extract` object
#'
#' @return A tibble representing the specifications for the extract `x`.
#'   These can be combined to form a larger tibble for multiple recent extracts.
#'
#' @noRd
extract_to_tbl <- function(x) {
  UseMethod("extract_to_tbl")
}

#' @export
extract_to_tbl.micro_extract <- function(x) {
  base_vars <- tibble::tibble(
    collection = x$collection,
    description = x$description,
    data_structure = x$data_structure,
    rectangular_on = x$rectangular_on %||% NA_character_,
    data_format = x$data_format,
    case_select_who = x$case_select_who %||% "individuals",
    data_quality_flags = x$data_quality_flags %||% NA,
    submitted = x$submitted,
    download_links = list(x$download_links),
    number = x$number,
    status = x$status
  )

  vars <- tibble::tibble(
    number = x$number,
    variables = list(names(x$variables)),
    case_selections = list(purrr::map(x$variables, ~ .x$case_selections)),
    attached_characteristics = list(purrr::map(x$variables, ~ .x$attached_characteristics)),
    var_data_quality_flags = list(purrr::map(x$variables, ~ .x$data_quality_flags)),
    case_selection_type = list(purrr::map(x$variables, ~ .x$case_selection_type)),
    preselected = list(purrr::map(x$variables, ~ .x$preselected))
  )

  samps <- tibble::tibble(
    number = x$number,
    samples = list(names(x$samples))
  )

  tbl <- purrr::reduce(
    list(base_vars, vars, samps),
    ~ dplyr::full_join(.x, .y, by = "number")
  )

  var_order <- c(
    "collection", "number", "description", "samples",
    "variables", "case_selections", "case_selection_type",
    "attached_characteristics", "var_data_quality_flags", "preselected", "data_structure",
    "rectangular_on", "data_format", "case_select_who", "data_quality_flags", "submitted", "download_links", "status"
  )

  tbl[, var_order]
}

#' @export
extract_to_tbl.nhgis_extract <- function(x) {
  base_vars <- tibble::tibble(
    collection = x$collection,
    description = x$description %||% NA_character_,
    data_format = x$data_format %||% NA_character_,
    breakdown_and_data_type_layout = x$breakdown_and_data_type_layout %||%
      NA_character_,
    tst_layout = x$tst_layout %||% NA_character_,
    geographic_extents = list(x$geographic_extents) %||% list(NULL),
    submitted = x$submitted,
    download_links = list(x$download_links),
    number = x$number,
    status = x$status
  )

  ds <- purrr::map_dfr(
    x$datasets,
    ~ tibble::tibble(
      name = .x$name,
      data_type = "datasets",
      data_tables = list(.x$data_tables),
      geog_levels = list(.x$geog_levels),
      years = list(.x$years),
      breakdown_values = list(.x$breakdown_values)
    )
  )

  tst <- purrr::map_dfr(
    x$time_series_tables,
    ~ tibble::tibble(
      name = .x$name,
      data_type = "time_series_tables",
      data_tables = list(NULL),
      geog_levels = list(.x$geog_levels),
      years = list(.x$years),
      breakdown_values = list(NULL)
    )
  )

  shp <- tibble::tibble(
    name = x$shapefiles,
    data_type = "shapefiles",
    data_tables = list(NULL),
    geog_levels = list(NULL),
    years = list(NULL),
    breakdown_values = list(NULL),
  )

  tbl <- dplyr::bind_rows(ds, tst, shp) %>%
    dplyr::mutate(number = x$number) %>%
    dplyr::full_join(base_vars, by = "number")

  var_order <- c(
    "collection", "number", "description", "data_type",
    "name", "data_tables", "geog_levels",
    "years", "breakdown_values", "geographic_extents",
    "tst_layout", "breakdown_and_data_type_layout",
    "data_format", "submitted", "download_links", "status"
  )

  tbl[, var_order]
}

#' This is currently used only to catch unexpected names in
#' `extract_tbl_to_list()`. However, unexpected names vary across
#' collections, so we use an S3 generic.
#'
#' @noRd
get_extract_tbl_fields <- function(x) {
  UseMethod("get_extract_tbl_fields")
}

#' @export
get_extract_tbl_fields.nhgis_extract <- function(x) {
  c(
    "collection", "description", "datasets", "data_tables", "geog_levels",
    "years", "breakdown_values", "geographic_extents",
    "breakdown_and_data_type_layout", "time_series_tables",
    "tst_layout", "shapefiles", "data_format",
    "submitted", "download_links", "number", "status",
    "name", "data_type" # Used in long-format NHGIS tbl structure
  )
}

#' @export
get_extract_tbl_fields.micro_extract <- function(x) {
  c(
    "collection", "description", "samples", "variables",
    "case_selections", "case_selection_type", "attached_characteristics",
    "var_data_quality_flags", "preselected",
    "data_format", "data_structure", "rectangular_on",
    "data_quality_flags", "case_select_who",
    "submitted", "download_links", "number", "status"
  )
}

#' @export
get_extract_tbl_fields.ipums_extract <- function(x) {
  c(
    "collection", "description", "submitted",
    "download_links", "number", "status"
  )
}

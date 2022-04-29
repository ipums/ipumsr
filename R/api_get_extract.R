# > Get info on extract ----

#' Get information about a submitted extract
#'
#' Get information about a submitted extract via the IPUMS API. For an overview
#' of ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract One of:
#'   \itemize{
#'     \item{An object of class \code{ipums_extract}}
#'     \item{The data collection and extract number formatted as a single
#'           string of the form \code{"collection:number"}}
#'     \item{The data collection and extract number formatted as a vector of the
#'           form \code{c("collection", "number")}}
#'   }
#' The extract number does not need to be zero-padded (e.g., use \code{"usa:1"}
#' or \code{c("usa", "1")}, not \code{"usa:00001"} or \code{c("usa", "00001")}).
#' See Examples section below for examples of each form.
#' @inheritParams define_extract_micro
#' @inheritParams download_extract
#'
#' @family ipums_api
#' @return An \code{ipums_extract} object.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Get info by supplying extract object:
#' get_extract_info(submitted_extract)
#'
#' # Get info by supplying the data collection and extract number, as a string:
#' get_extract_info("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # Get info by supplying the data collection and extract number, as a vector:
#' get_extract_info(c("usa", "1"))
#' }
#'
#' @export
get_extract_info <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  stopifnot(length(extract$collection) == 1)
  stopifnot(length(extract$number) == 1)

  if (is.na(extract$number)) {
    stop(
      "Extract number cannot be a missing value; please supply an ",
      "ipums_extract object returned by `submit_extract()`, or the data ",
      "collection and number of a submitted extract.",
      call. = FALSE
    )
  }

  collection <- tolower(extract$collection)

  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = paste0(api_extracts_path(), "/", extract$number),
    api_key = api_key
  )

  # if(collection == "nhgis") {
  #   warning(
  #     "The current version of the NHGIS API (v1) does not provide information ",
  #     "on `shapefiles`, `breakdown_and_data_type_layout`, ",
  #     "`time_series_table_layout` or `geographic_extents` for previously ",
  #     "submitted extracts. Consult your initial extract request for ",
  #     "information on the values of these parameters.",
  #     call. = FALSE
  #   )
  # }

  extract_list_from_json(response)[[1]]

}

# > Get info on recent extracts ----

#' Get information on recent extracts
#'
#' Get information on recent extracts for a given IPUMS collection
#' via the IPUMS API, returned either as a list or tibble. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams define_extract_micro
#' @param how_many Number of recent extracts for which you'd like information.
#'   Defaults to 10 extracts.
#' @inheritParams submit_extract
#'
#' @family ipums_api
#' @return For \code{get_recent_extracts_info_list()}, a list of
#'   \code{ipums_extract} objects. For \code{get_recent_extracts_info_tbl()},
#'   a \code{\link[tibble]{tbl_df}} with information on one extract in each row.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
#' income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]
#'
#' # Convert tibble of extracts to list of extracts
#' income_extracts <- extract_tbl_to_list(income_extracts)
#'
#' # Now it's easier to operate on those elements as extract objects:
#' revised_income_extract <- revise_extract_micro(
#'   income_extracts[[1]],
#'   samples_to_add = "us2018a"
#' )
#'
#' submitted_revised_income_extract <- submit_extract(revised_income_extract)
#' }
#'
#' @name get_recent_extracts_info
NULL

#' @rdname get_recent_extracts_info
#'
#' @export
get_recent_extracts_info_list <- function(collection,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {

  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = NULL,
    queries = list(limit = how_many),
    api_key = api_key
  )

  # if(collection == "nhgis") {
  #   warning(
  #     "The current version of the NHGIS API (v1) does not provide information ",
  #     "on `shapefiles`, `breakdown_and_data_type_layout`, ",
  #     "`time_series_table_layout` or `geographic_extents` for previously ",
  #     "submitted extracts. Consult your initial extract request for ",
  #     "information on the values of these parameters.",
  #     call. = FALSE
  #   )
  # }

  extract_list_from_json(response)
}

#' @rdname get_recent_extracts_info
#' @export
get_recent_extracts_info_tbl <- function(collection,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract_list <- get_recent_extracts_info_list(
    collection,
    how_many,
    api_key
  )

  extract_list_to_tbl(extract_list)
}

# > Get info on most recent extract ----

#' Get information on last extract
#'
#' Get information on your most recent extract for a given IPUMS data
#' collection, returned as an \code{ipums_extract} object. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams get_recent_extracts_info_list
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing information on
#'   your most recent extract.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submit_extract(my_extract)
#'
#' # Oops, forgot to capture the return object from submit_extract. Grab it with:
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # View the extract number
#' submitted_extract$number
#'
#' # Check if submitted extract is ready
#' is_extract_ready(submitted_extract) # returns TRUE or FALSE
#'
#' # Or have R check periodically until the extract is ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
#'
#' @export
get_last_extract_info <- function(collection,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  get_recent_extracts_info_list(collection, 1, api_key)[[1]]
}

# > Convert extract tbl to list ----

#' Convert a tibble of extract definitions to a list
#'
#' Convert a \code{\link[tibble]{tbl_df}} (or \code{data.frame}) of extract
#' definitions, such as that returned by
#' \code{\link{get_recent_extracts_info_tbl}}, to a list of \code{ipums_extract}
#' objects. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_tbl A \code{\link[tibble]{tbl_df}} (or \code{data.frame})
#'   where each row contains the definition of one extract.
#' @param validate Logical (\code{TRUE} or \code{FALSE}) value indicating
#'   whether to check that each row of \code{extract_tbl} contains a valid and
#'   complete extract definition. Defaults to \code{TRUE}
#'
#' @family ipums_api
#' @return A list of length equal to the number of rows of \code{extract_tbl}.
#'
#' @examples
#' \dontrun{
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
#' income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]
#'
#' # Convert tibble of extracts to list of extracts
#' income_extracts <- extract_tbl_to_list(income_extracts)
#'
#' # Now it's easier to operate on those elements as extract objects:
#' revised_income_extract <- revise_extract_micro(
#'   income_extracts[[1]],
#'   samples_to_add = "us2018a"
#' )
#'
#' submitted_revised_income_extract <- submit_extract(revised_income_extract)
#' }
#'
#' @export
extract_tbl_to_list <- function(extract_tbl, validate = TRUE) {

  collection <- unique(extract_tbl$collection)

  if (length(collection) > 1) {
    stop(
      "All extracts in `extract_tbl` must belong to same collection.",
      call. = FALSE
    )
  }

  expected_names <- get_extract_tbl_fields(
    new_ipums_extract(collection = collection)
  )

  unexpected_names <- setdiff(names(extract_tbl), expected_names)

  if (length(unexpected_names) > 0) {
    stop(
      "Unexpected names in `extract_tbl`: ",
      paste0('"', unexpected_names, '"', collapse = ", "),
      call. = FALSE
    )
  }

  # Internal logic in lieu of new S3 class needed to handle dispatch...
  if (collection == "nhgis") {

    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop(
        "Package \"tidyr\" must be installed to convert NHGIS extracts from ",
        "tbl to list format.",
        call. = FALSE
      )
    }

    extract_tbl <- collapse_nhgis_extract_tbl(extract_tbl)
  }

  extract_list <- purrr::pmap(extract_tbl, new_ipums_extract)

  if (collection == "nhgis") {
    extract_list <- purrr::map(extract_list, recycle_nhgis_extract_args)
  }

  if (validate) {
    extract_list <- purrr::walk(extract_list, validate_ipums_extract)
  }

  extract_list

}


# > Convert extract list to tbl ----

#' Convert a list of extract definitions to a tibble
#'
#' Convert a list of \code{ipums_extract} objects to a
#' \code{\link[tibble]{tbl_df}} in which each row contains the definition of one
#' extract. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_list A list of \code{ipums_extract} objects.
#'
#' @family ipums_api
#' @return A \code{\link[tibble]{tbl_df}} with number of rows equal to the
#'   length of \code{extract_list}, in which each rows contains the definition
#'   of one extract.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Convert list of extracts to tibble of extracts to view in a tabular format
#' extract_list_to_tbl(list_of_last_10_extracts)
#' }
#'
#' @export
extract_list_to_tbl <- function(extract_list) {

  extract_types <- unique(
    purrr::map_chr(
      extract_list,
      function(x) x$collection
    )
  )

  if(length(extract_types) != 1) {
    stop(
      "All extracts in `extract_list` must belong to same collection.",
      call. = FALSE
    )
  }

  purrr::map_dfr(extract_list, extract_to_tbl)

}




extract_to_tbl <- function(x) {
  UseMethod("extract_to_tbl")
}

#' @export
extract_to_tbl.usa_extract <- function(x) {

  if (is.character(x$samples)) x$samples <- list(x$samples)
  if (is.character(x$variables)) x$variables <- list(x$variables)
  x$download_links <- list(x$download_links)

  unclassed_extract <- unclass(x)

  do.call(tibble::tibble, unclassed_extract)

}

#' @export
extract_to_tbl.nhgis_extract <- function(x) {

  base_vars <- list(
    collection = x$collection,
    description = x$description %||% NA_character_,
    data_format = x$data_format %||% NA_character_,
    breakdown_and_data_type_layout = x$breakdown_and_data_type_layout %||%
      NA_character_,
    geographic_extents = list(x$geographic_extents) %||% list(NULL),
    submitted = x$submitted,
    download_links = list(x$download_links),
    number = x$number,
    status = x$status
  )

  ds <- c(
    list(
      name = unlist(x$datasets) %||% NA_character_,
      ds_tables = unname(x$ds_tables),
      ds_geog_levels = unname(x$ds_geog_levels),
      ds_years = if (is_empty(x$ds_years)) {
        list(NULL)
      } else {
        unname(purrr::map(x$ds_years, ~.x))
      },
      ds_breakdown_values = if (is_empty(x$ds_breakdown_values)) {
        list(NULL)
      } else {
        unname(purrr::map(x$ds_breakdown_values, ~.x))
      },
      tst_layout = NA_character_
    ),
    base_vars
  )

  ts <- c(
    list(
      name = unlist(x$time_series_tables) %||% NA_character_,
      tst_geog_levels = unname(x$tst_geog_levels),
      tst_layout = x$tst_layout,
      ds_tables = list(NULL),
      ds_years = list(NULL),
      ds_breakdown_values = list(NULL)
    ),
    base_vars
  )

  shp <- c(
    list(
      name = unlist(x$shapefiles) %||% NA_character_,
      ds_tables = list(NULL),
      ds_years = list(NULL),
      ds_breakdown_values = list(NULL),
      tst_geog_levels = list(NULL),
      ds_geog_levels = list(NULL),
      tst_layout = NA_character_
    ),
    base_vars
  )

  tbl1 <- do.call(tibble::tibble, ds)
  tbl1$data_type <- "datasets"

  tbl2 <- do.call(tibble::tibble, ts)
  tbl2$data_type <- "time_series_tables"

  tbl3 <- do.call(tibble::tibble, shp)
  tbl3$data_type <- "shapefiles"

  tbl <- dplyr::bind_rows(tbl1, tbl2, tbl3)
  tbl <- tbl[!is.na(tbl$name), ]

  var_order <- c("collection", "number", "description", "data_type",
                 "name", "ds_tables", "ds_geog_levels", "tst_geog_levels",
                 "ds_years", "ds_breakdown_values",  "geographic_extents",
                 "tst_layout", "breakdown_and_data_type_layout",
                 "data_format", "submitted", "download_links", "status")

  tbl[, var_order]

}


collapse_nhgis_extract_tbl <- function(extract_tbl) {

  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop(
      "Package \"tidyr\" must be installed to convert NHGIS extracts from tbl ",
      "to list format.",
      call. = FALSE
    )
  }

  stopifnot(unique(extract_tbl$collection) == "nhgis")

  # Convert pseudo-long extract_tbl format to extract-row format
  extract_tbl <- dplyr::group_by(extract_tbl, number, data_type)

  extract_tbl <- dplyr::mutate(
    extract_tbl,
    dplyr::across(
      c(ds_tables,
        ds_geog_levels,
        ds_years,
        ds_breakdown_values,
        tst_geog_levels),
      ~if (is.null(unlist(.x))) { .x } else { list(.x) }
    )
  )

  extract_tbl <- tidyr::pivot_wider(
    extract_tbl,
    names_from = data_type,
    values_from = name,
    values_fn = list
  )

  tbl_cols <- colnames(extract_tbl)

  extract_tbl <- dplyr::distinct(
    tidyr::fill(extract_tbl, dplyr::all_of(tbl_cols), .direction = "updown")
  )

  join_df <- tibble::tibble(
    shapefiles = list(NULL),
    time_series_tables = list(NULL),
    datasets = list(NULL)
  )

  join_cols <- intersect(tbl_cols, colnames(join_df))

  # Join to ensure all extract parameters are present
  extract_tbl <- dplyr::left_join(
    extract_tbl,
    join_df,
    by = join_cols
  )

  # For consistency of output after conversion to list
  # define_extract_nhgis() and tbl to list conversion should align on all fields
  extract_tbl <- dplyr::mutate(
    extract_tbl,
    dplyr::across(
      c(data_format, breakdown_and_data_type_layout, tst_layout),
      ~tidyr::replace_na(.x, list(NULL))
    )
  )

  # Reorder
  var_sort <- c("collection", "number", formalArgs(define_extract_nhgis),
                "submitted", "download_links", "status")
  extract_tbl <- extract_tbl[, var_sort]

  extract_tbl

}



get_extract_tbl_fields <- function(x) {
  UseMethod("get_extract_tbl_fields")
}

#' @export
get_extract_tbl_fields.nhgis_extract <- function(x) {
  c(
    formalArgs(define_extract_nhgis),
    "collection", "submitted", "download_links", "number", "status",
    "name", "data_type" # Used in long-format NHGIS tbl structure
  )
}

#' @export
get_extract_tbl_fields.usa_extract <- function(x) {
  c(
    formalArgs(define_extract_micro),
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





extract_list_from_json <- function(extract_json, ...) {
  UseMethod("extract_list_from_json")
}

#' @export
extract_list_from_json.nhgis_json <- function(extract_json, validate = FALSE) {

  list_of_extract_info <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {

      no_datasets <- is.null(x$datasets)
      no_tsts <- is.null(x$time_series_tables)

      out <- new_ipums_extract(
        collection = "nhgis",
        description = x$description,
        datasets = names(x$datasets),
        ds_tables = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$data_tables))
        },
        ds_geog_levels = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$geog_levels))
        },
        ds_years = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$years))
        },
        ds_breakdown_values = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$breakdown_values))
        },
        geographic_extents = unlist(x$geographic_extents),
        breakdown_and_data_type_layout = x$breakdown_and_data_type_layout,
        time_series_tables = names(x$time_series_tables),
        tst_geog_levels = if (no_tsts) {
          NULL
        } else {
          purrr::map(x$time_series_tables, ~unlist(.x$geog_levels))
        },
        tst_layout = x$time_series_table_layout,
        shapefiles = unlist(x$shapefiles),
        data_format = x$data_format,
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = x$download_links %||% EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = x$status %||% "unsubmitted"
      )

      if (validate) validate_ipums_extract(out)

      out
    }
  )
}

#' @export
extract_list_from_json.usa_json <- function(extract_json, validate = FALSE) {

  list_of_extract_info <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {
      out <- new_ipums_extract(
        collection = "usa",
        description = x$description,
        data_structure = names(x$data_structure),
        rectangular_on = ifelse(
          names(x$data_structure) == "rectangular",
          x$data_structure$rectangular$on,
          NA_character_
        ),
        data_format = x$data_format,
        samples = names(x$samples),
        variables = names(x$variables),
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = if ("download_links" %in% names(x)) {
          x$download_links
        } else EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = ifelse("status" %in% names(x), x$status, "unsubmitted")
      )
      if (validate) validate_ipums_extract(out)
      out
    }
  )
}

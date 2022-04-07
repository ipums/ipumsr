# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr


# Exported functions ------------------------------------------------------


# > Define extract ----

#' Define a microdata extract request
#'
#' Define an extract request object to be submitted via the IPUMS microdata
#' extract API. For an overview of ipumsr microdata API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param collection The IPUMS data collection for the extract.
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   \href{https://usa.ipums.org/usa-action/samples/sample_ids}{Sample ID values}.
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file (one of
#'   "fixed_width", "csv", "stata", "spss", or "sas9").
#' @param data_structure Currently, this must be "rectangular", which is also
#'   the default. In the future, the API will also support "hierarchical"
#'   extracts.
#' @param rectangular_on Currently, this must be "P", indicating that the
#'   extract will be rectangularized on person records. In the future, the API
#'   will also support household-only extracts (\code{rectangular_on = "H"}).
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#' @export
define_extract_micro <- function(collection,
                                 description,
                                 samples,
                                 variables,
                                 data_format = c("fixed_width", "csv", "stata",
                                                 "spss", "sas9"),
                                 data_structure = "rectangular",
                                 rectangular_on = "P") {

  data_format <- match.arg(data_format)
  if (data_structure != "rectangular") {
    stop(
      "Currently, the `data_structure` argument must be equal to ",
      "\"rectangular\"; in the future, the API will also support ",
      "\"hierarchical\" extracts.",
      call. = FALSE
    )
  }
  if (rectangular_on != "P") {
    stop(
      "Currently, the `rectangular_on` argument must be equal to \"P\"; in ",
      "the future, the API will also support `rectangular_on = \"H\".",
      call. = FALSE
    )
  }
  # For now this next block is irrelevant, but leave it for whenever we add
  # support for rectangular_on = "H"
  rectangular_on <- if(data_structure == "rectangular") {
    match.arg(rectangular_on)
  } else NA_character_

  stopifnot(is.character(collection), length(collection) == 1)
  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(data_structure), length(data_structure) == 1)
  stopifnot(is.character(rectangular_on), length(rectangular_on) == 1)
  stopifnot(is.character(data_format), length(data_format) == 1)
  stopifnot(is.character(samples))
  stopifnot(is.character(variables))

  extract <- new_ipums_extract(
    collection = collection,
    description = description,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format,
    samples = samples,
    variables = variables
  )

  extract <- validate_ipums_extract(extract)

  extract
}

#' Define an NHGIS extract request
#'
#' Define an extract request object to be submitted via the IPUMS NHGIS
#' extract API.
#'
#' @param description Description of the extract.
#' @param datasets Character name of datasets to include in the extract, if any.
#' @param data_tables Character vector of data tables associated with the
#'   specified datasets to include in the extract. Required if a dataset is
#'   specified.
#' @param ds_geog_levels Character vector of geographic levels associated with
#'   the specified dataset to include in the extract. Required if a dataset is
#'   specified.
#' @param years Character or integer vector of years associated with the
#'   specified dataset to include in the extract. Use "*" to select all
#'   available years for the specified dataset.
#' @param breakdown_values Character vector of selected breakdown values to
#'   associated with the specified dataset to include in the extract. If more
#'   than one breakdown value is requested, `breakdown_and_data_type_layout`
#'   must also be specified.
#' @param time_series_tables Character name of time series table to include in
#'   the extract, if any.
#' @param ts_geog_levels Character vector of geographic levels associated with
#'   the specified time series table to include in the extract. Required if a
#'   time series table is specified.
#' @param shapefiles Character vector of shapefiles to include in the extract,
#'   if any.
#' @param data_format The desired format of the extract data file (one of
#'   \code{"csv_no_header"}, \code{"csv_header"}, or \code{"fixed_width"}).
#'   \code{"csv_no_header"} provides a minimal header in the first row, while
#'   \code{"csv_header"} adds a second, more descriptive header row.
#'   Required when any dataset or time_series_table is selected.
#' @param breakdown_and_data_type_layout Character indicating the layout of the
#'   dataset when multiple data types or breakdown values are specified.
#'   \code{"separate_files"} splits up each data type or breakdown combo into
#'   its own file. \code{"single_file"} keeps all datatypes and breakdown combos
#'   in one file. Required when a dataset has multiple breakdowns or data types.
#' @param time_series_table_layout Character indicating the layout of the
#'   specified time series table. One of \code{"time_by_column_layout"},
#'   \code{"time_by_row_layout"}, or \code{"time_by_file_layout"}. Required when
#'   a time series table is specified.
#' @param geographic_extents A list of geographic instances to use as extents
#'   for the dataset specified in the request. To select all extents, use
#'   \code{"*"}. Required when a geographic level on a dataset is specified
#'   where \code{has_geog_extent_selection} is true.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition.
#'
#' @examples
#' my_extract <- define_extract_nhgis(
#'   description = "test nhgis extract 1",
#'   datasets = "1990_STF3",
#'   data_tables = "NP57",
#'   ds_geog_levels = "tract",
#'   data_format = "csv_no_header"
#' )
#'
#' @export
define_extract_nhgis <- function(description = "",
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
                                 geographic_extents = NULL) {

  n_datasets <- length(datasets)
  n_tsts <- length(time_series_tables)

  if (n_datasets > 0) {
    data_format <- data_format %||% "csv_header"
    breakdown_and_data_type_layout <- breakdown_and_data_type_layout %||%
      "separate_files"
  }

  if (n_tsts > 0) {
    data_format <- data_format %||% "csv_header"
    time_series_table_layout <- time_series_table_layout %||%
      "time_by_column_layout"
  }

  extract <- new_ipums_extract(
    collection = "nhgis",
    description = description,
    datasets = datasets,
    data_tables = data_tables,
    ds_geog_levels = ds_geog_levels,
    years = years,
    breakdown_values = breakdown_values,
    time_series_tables = time_series_tables,
    ts_geog_levels = ts_geog_levels,
    shapefiles = shapefiles,
    data_format = data_format,
    breakdown_and_data_type_layout = breakdown_and_data_type_layout,
    time_series_table_layout = time_series_table_layout,
    geographic_extents = geographic_extents
  )

  extract <- recycle_nhgis_extract_args(extract)
  extract <- validate_ipums_extract(extract)

  extract

}

recycle_nhgis_extract_args <- function(extract) {

  stopifnot(extract$collection == "nhgis")

  n_datasets <- length(extract$datasets)
  n_tsts <- length(extract$time_series_tables)

  ds_to_recycle <- c("data_tables", "ds_geog_levels",
                     "years", "breakdown_values")

  ts_to_recycle <- "ts_geog_levels"

  if (n_datasets > 0) {
    purrr::walk(
      ds_to_recycle,
      function(var)
        extract[[var]] <<- recycle_to_list(
          extract[[var]],
          n_datasets,
          labels = extract$datasets
        )
    )
  }

  if (n_tsts > 0) {
    purrr::walk(
      ts_to_recycle,
      function(var)
        extract[[var]] <<- recycle_to_list(
          extract[[var]],
          n_tsts,
          labels = extract$time_series_tables
        )
    )
  }

  extract

}

# > Define extract from json ----

#' Create an extract object from a JSON-formatted definition
#'
#' Create an object of class "ipums_extract" based on an extract definition
#' formatted as JSON. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_json A JSON string, or the path to file containing JSON.
#' @inheritParams define_extract_micro
#'
#' @family ipums_api
#' @return An object of class "ipums_extract".
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
define_extract_from_json <- function(extract_json, collection) {

  if (missing(collection)) {
    stop("`collection` is a required argument", call. = FALSE)
  }

  if (!collection %in% ipums_data_collections()$code_for_api) {
    stop(
      paste0(
        '"', collection, '"', " is not a valid code for an IPUMS data ",
        "collection. To see all valid collection codes, use ",
        "`ipums_data_collections()`"
      ),
      call. = FALSE
    )
  }

  extract_json <- new_ipums_json(extract_json, collection)

  tryCatch(
    list_of_extracts <- extract_list_from_json(
      extract_json,
      validate = TRUE
    ),
    error = function(cnd) {
      stop(
        conditionMessage(cnd),
        "\n\nDid you specify the correct collection for this extract?",
        call. = FALSE
      )
    }
  )

  if (length(list_of_extracts) != 1) {
    stop(
      paste0(
        "`extract_json` should contain the definition of one and only one ",
        "extract"
      ),
      call. = FALSE
    )
  }

  list_of_extracts[[1]]

}


# > Save extract as json ----

#' Save an extract definition to disk as JSON
#'
#' Save an extract definition to a JSON-formatted file. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams submit_extract
#' @param file File path at which to write the JSON-formatted extract
#'   definition.
#'
#' @details Note that this function only saves out the properties of an extract
#'   that are required to submit a new extract request, namely, the description,
#'   data structure, data format, samples, and variables.
#' @family ipums_api
#' @return The file path where the extract definition was written, invisibly.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
save_extract_as_json <- function(extract, file) {
  extract_as_json <- extract_to_request_json(extract)
  writeLines(jsonlite::prettify(extract_as_json), con = file)
  invisible(file)
}


# > Submit extract ----

#' Submit an extract request via the IPUMS API
#'
#' Given an extract definition object, submit an extract request via the IPUMS
#' API, and return a modified copy of the extract object with the newly-assigned
#' extract number. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An extract object created with \code{\link{define_extract_micro}} or
#'   returned from another ipumsr API function.
#' @param api_key API key associated with your user account. Defaults to the
#'   value of environment variable "IPUMS_API_KEY".
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition and newly-assigned extract number of the submitted extract.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' # `submit_extract()` returns an ipums_extract object updated to include the
#' # extract number, so it is often useful to name the return object:
#' submitted_extract <- submit_extract(my_extract)
#'
#' # If you didn't capture the return object of submit_extract for your most
#' # recent extract, you can recover that information with:
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
submit_extract <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- validate_ipums_extract(extract)

  response <- ipums_api_json_request(
    "POST",
    collection = extract$collection,
    path = NULL,
    body = extract_to_request_json(extract),
  )

  # extract_list_from_json() always returns a list of extracts, but in
  # this case there is always only one, so pluck it out
  extract <- extract_list_from_json(response)[[1]]

  # For NHGIS extracts, API does not currently return a status upon submission.
  # extract_list_from_json() will incorrectly populate this with "unsubmitted".
  if (extract$status == "unsubmitted") extract$status <- "submitted"

  message(
    sprintf(
      "Successfully submitted %s extract number %d",
      format_collection_for_printing(extract$collection),
      extract$number
    )
  )

  extract

}


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


# > Wait for extract ----

#' Wait for extract to finish
#'
#' Wait for an extract to finish by periodically checking its status via the
#' IPUMS API and returning when the extract is ready to download. For an
#' overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams define_extract_micro
#' @inheritParams download_extract
#' @inheritParams get_extract_info
#' @inheritParams submit_extract
#' @param initial_delay_seconds How many seconds to wait before first status
#'   check.
#' @param max_delay_seconds Maximum seconds to wait between status checks. The
#'   function doubles the wait time after each check, but will cap the wait
#'   wait time at this maximum value (300 seconds, or 5 minutes, by default).
#' @param timeout_seconds Maximum total number of seconds to continue waiting
#'   for the extract before throwing an error. Defaults to 10,800 seconds (three
#'   hours).
#' @param verbose If \code{TRUE}, the default, messages will be printed at the
#'   beginning of each wait interval with the current wait time, each time the
#'   status of the extract is checked, and when the extract is ready to
#'   download. Setting this argument to \code{FALSE} will silence these
#'   messages.
#'
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition and the URLs from which to download extract files.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Wait for extract by supplying extract object:
#' downloadable_extract <- wait_for_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' downloadable_extract <- wait_for_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' downloadable_extract <- wait_for_extract(c("usa", "1"))
#' }
#'
#' @export
wait_for_extract <- function(extract,
                             initial_delay_seconds = 0,
                             max_delay_seconds = 300,
                             timeout_seconds = 10800,
                             verbose = TRUE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {


  stopifnot(is.numeric(initial_delay_seconds))
  stopifnot(is.numeric(max_delay_seconds))
  stopifnot(is.null(timeout_seconds) || is.numeric(timeout_seconds))

  current_delay <- initial_delay_seconds
  is_timed_out <- FALSE
  is_finished <- FALSE
  is_error_state <- FALSE
  err_message <- "Unknown Error"

  wait_start <- Sys.time()

  while (!is_timed_out && !is_finished && !is_error_state) {
    if (current_delay > 0) {
      if (verbose) {
        message(paste("Waiting", current_delay, "seconds..."))
      }

      Sys.sleep(current_delay)
    }

    if (verbose) {
      message("Checking extract status...")
    }
    extract <- get_extract_info(extract, api_key)

    is_downloadable <- is_extract_ready(extract)

    is_failed <- !(is_downloadable ||
                     extract$status %in% c("queued", "started", "produced"))
    is_timed_out <- !is.null(timeout_seconds) &&
      as.numeric(Sys.time() - wait_start, units = "secs") > timeout_seconds


    if (is_downloadable) {
      if (verbose) {
        message("Extract ready to download")
      }
      is_finished <- TRUE
    } else if (is_failed) {
      err_message <- "Extract has finished, but is not in a downloadable state"
      is_error_state <- TRUE
    } else if (is_timed_out) {
      err_message <- "Max timeout elapsed"
    }

    if (current_delay == 0) {
      current_delay <- 10
    } else {
      current_delay <- min(c(current_delay * 2, max_delay_seconds))
    }
  }

  if (!is_finished) {
    stop(err_message)
  }

  extract
}


# > Check if downloadable ----

#' Is the extract ready to download?
#'
#' This function uses the IPUMS API to check whether the given extract is ready
#' to download, returning TRUE for extracts that are ready and FALSE for those
#' that are not. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @details
#' This function checks the "download_links" element of the supplied extract to
#' determine whether the extract files are available to download.
#' The "status" of a submitted extract is one of "queued", "started", "produced",
#' "canceled", "failed", or "completed". Only "completed" extracts can be ready
#' to download, but not all "completed" extracts are ready to download, because
#' extract files are subject to removal from the IPUMS servers 72 hours after
#' they first become available. Completed extracts older than 72 hours will
#' still have a "completed" status, but will return \code{FALSE} from
#' \code{is_extract_ready()}, because the extract files are no longer available.
#'
#' @inheritParams get_extract_info
#'
#' @family ipums_api
#' @return A logical vector of length one.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Check if extract is ready by supplying extract object:
#' is_extract_ready(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' is_extract_ready("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' is_extract_ready(c("usa", "1"))
#' }
#'
#' @export
is_extract_ready <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  if (is.na(extract$number)) {
    stop(
      "ipums_extract object has a missing value in the 'number' field. If ",
      "an extract object is supplied, it must be a submitted extract with a ",
      "non-missing extract number.", call. = FALSE
    )
  }

  # First check if extract object already contains download info...
  if (inherits(extract, "ipums_extract") &&
      extract_is_completed_and_has_links(extract)) {
    return(TRUE)
  }

  # ... if it doesn't contain download info, make sure we have the latest
  # status by fetching it via the API and checking again
  extract <- get_extract_info(extract, api_key)

  extract_is_completed_and_has_links(extract)
}


# > Download extract ----

#' Download an IPUMS data extract
#'
#' Download an IPUMS data extract via the IPUMS API. For an overview of ipumsr
#' API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams get_extract_info
#' @inheritParams define_extract_micro
#' @inheritParams submit_extract
#' @param download_dir In what folder should the downloaded files be saved?
#'   Defaults to current working directory.
#' @param overwrite Logical indicating whether to overwrite files that already
#'   exist. Defaults to \code{FALSE}.
#'
#' @family ipums_api
#' @return Invisibly, the path to the downloaded .xml DDI file.
#'
#' @examples
#' my_extract <- define_extract_micro("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Download extract by supplying extract object:
#' path_to_ddi_file <- download_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' path_to_ddi_file <- download_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' path_to_ddi_file <- download_extract(c("usa", "1"))
#' }
#'
#' @export
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  # Make sure we get latest extract status, but also make sure we don't check
  # the status twice
  is_ipums_extract_object <- inherits(extract, "ipums_extract")

  if (is_ipums_extract_object && extract_is_completed_and_has_links(extract)) {
    is_downloadable <- TRUE
  } else {
    extract <- get_extract_info(extract, api_key)
    is_downloadable <- extract_is_completed_and_has_links(extract)
  }

  if (!is_downloadable) {
    stop(
      paste0(
        format_collection_for_printing(extract$collection), " extract number ",
        extract$number, " is not ready to download"
      ),
      call. = FALSE
    )
  }

  download_dir <- normalizePath(download_dir, winslash = "/", mustWork = FALSE)
  download_dir_doesnt_exist <- !dir.exists(download_dir)

  if (download_dir_doesnt_exist) {
    stop("The directory ", download_dir, " does not exist.", call. = FALSE)
  }

  ipums_extract_specific_download(extract, download_dir, overwrite, api_key)

}

ipums_extract_specific_download <- function(extract,
                                            download_dir,
                                            overwrite,
                                            api_key) {
  UseMethod("ipums_extract_specific_download")
}

#' @export
ipums_extract_specific_download.usa_extract <- function(extract,
                                                        download_dir,
                                                        overwrite,
                                                        api_key) {

  ddi_url <- extract$download_links$ddi_codebook$url
  data_url <- extract$download_links$data$url

  ddi_file_path <- normalizePath(
    file.path(download_dir, basename(ddi_url)),
    winslash = "/",
    mustWork = FALSE
  )

  data_file_path <- normalizePath(
    file.path(download_dir, basename(data_url)),
    winslash = "/",
    mustWork = FALSE
  )

  ipums_api_download_request(ddi_url, ddi_file_path, overwrite, api_key)
  ipums_api_download_request(data_url, data_file_path, overwrite, api_key)

  message(
    paste0("DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
           "to ", data_file_path)
  )

  invisible(ddi_file_path)

}

#' @export
ipums_extract_specific_download.nhgis_extract <- function(extract,
                                                          download_dir,
                                                          overwrite,
                                                          api_key) {


  table_url <- extract$download_links$table_data
  gis_url <- extract$download_links$gis_data

  urls <- purrr::compact(
    list(
      table_url,
      gis_url
    )
  )

  file_paths <- purrr::map_chr(
    urls,
    ~normalizePath(
      file.path(download_dir, basename(.x)),
      winslash = "/",
      mustWork = FALSE
    )
  )

  file_names <- basename(file_paths)

  existing_files <- file_names[purrr::map_lgl(file_paths, file.exists)]

  # Currently, if any of the files to be downloaded (table or gis) exist,
  # no files are downloaded.
  if (length(existing_files) > 0 && !overwrite) {
    stop(
      "The following files already exist: ",
      paste0(existing_files, collapse = ", "),
      "\nIf you want to overwrite, set `overwrite` to TRUE.",
      call. = FALSE
    )
  }

  file_paths <- purrr::map2_chr(
    urls,
    file_paths,
    ~ipums_api_download_request(.x, .y, overwrite, api_key)
  )

  if (!is.null(table_url) && !is.null(gis_url)) {
    message(
      paste0(
        "Data file saved to ", file_paths[1],
        "\nGIS file saved to ", file_paths[2]
      )
    )
  } else if (!is.null(table_url)) {
    message(
      paste0("Data file saved to ", file_paths)
    )
  } else if (!is.null(gis_url)) {
    message(
      paste0("GIS file saved to ", file_paths)
    )
  }

  invisible(file_paths)

}


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

  # Remove when NHGIS API is updated
  # if (collection == "nhgis" && ipums_api_version("nhgis") == "v1" && validate) {
  #
  #   validate <- FALSE
  #
  #   warning(
  #     "The current version of the NHGIS API (v1) does not provide information ",
  #     "on `shapefiles`, `breakdown_and_data_type_layout`, ",
  #     "`time_series_table_layout` or `geographic_extents` for previously ",
  #     "submitted extracts. Extracts including these parameters may not be ",
  #     "reconstructed accurately and cannot be validated.\n\n",
  #     "Setting `validate = FALSE`",
  #     call. = FALSE
  #   )
  # }

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
      c(data_tables, ds_geog_levels, years, breakdown_values, ts_geog_levels),
      ~if (is.null(unlist(.x))) { .x } else { list(.x) }
    )
  )

  extract_tbl <- tidyr::pivot_wider(
    extract_tbl,
    names_from = data_type,
    values_from = nhgis_id,
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
      c(data_format, breakdown_and_data_type_layout, time_series_table_layout),
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
    "nhgis_id", "data_type" # Used in long-format NHGIS tbl structure
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

# > List data collections ----

#' List IPUMS data collections
#'
#' List IPUMS data collections with corresponding codes used by the IPUMS API.
#' Note that some data collections do not yet have API support. For an overview
#' of ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @family ipums_api
#' @return A \code{\link[tibble]{tbl_df}} with two columns containing the
#'   full collection name and the corresponding code used by the IPUMS API.
#'
#' @examples
#' # Print a tibble of all IPUMS data collections:
#' ipums_data_collections()
#'
#' @export
ipums_data_collections <- function() {
  tibble::tribble(
    ~collection_name, ~code_for_api,
    "IPUMS USA", "usa",
    "IPUMS CPS", "cps",
    "IPUMS International", "ipumsi",
    "IPUMS NHGIS", "nhgis",
    "IPUMS AHTUS", "ahtus",
    "IPUMS MTUS", "mtus",
    "IPUMS ATUS", "atus",
    "IPUMS DHS", "dhs",
    "IPUMS Higher Ed", "highered",
    "IPUMS MEPS", "meps",
    "IPUMS NHIS", "nhis",
    "IPUMS PMA", "pma"
  )
}


# Non-exported functions --------------------------------------------------

new_ipums_extract <- function(collection = NA_character_,
                              description = NA_character_,
                              submitted = FALSE,
                              download_links = EMPTY_NAMED_LIST,
                              number = NA_integer_,
                              status = "unsubmitted",
                              ...) {

  out <- list(
    collection = collection,
    description = description,
    ...,
    submitted = submitted,
    download_links = download_links,
    number = number,
    status = status
  )

  structure(
    out,
    class = c(paste0(collection, "_extract"), "ipums_extract", class(out))
  )

}

new_ipums_json <- function(json, collection) {

  structure(
    json,
    class = c(paste0(collection, "_json"), "ipums_json")
  )

}


standardize_extract_identifier <- function(extract) {
  if (inherits(extract, "ipums_extract")) return(extract)
  if (length(extract) == 1) {
    extract <- fostr_split(extract, ":")[[1]]
  }

  if (length(extract) != 2) {
    stop(
      paste0(
        "Expected extract to be either an `ipums_extract` object, a ",
        "length-two vector where the first element is an IPUMS collection ",
        "and the second is an extract number, or a single string with a ':' ",
        "separating the collection from the extract number."
      ),
      call. = FALSE
    )
  }
  collection <- extract[[1]]
  number <- suppressWarnings(as.numeric(extract[[2]]))
  if (is.na(number)) {
    stop("Expected extract number to be a number", call. = FALSE)
  }

  list(collection = collection, number = number)
}

validate_ipums_extract <- function(x) {
  UseMethod("validate_ipums_extract")
}

#' @export
validate_ipums_extract.nhgis_extract <- function(x) {

  types <- nhgis_extract_types(x)

  if(length(types) == 0) {
    stop(
      "At least one of `datasets`, `time_series_tables` or `shapefiles` ",
      "must be specified.",
      call. = FALSE
    )
  }

  is_missing <- purrr::map_lgl(
    c("collection", "description"),
    ~is.na(x[[.]]) || is_empty(x[[.]])
  )

  is_present <- purrr::map_lgl(
    names(x),
    ~!is.na(x[[.]]) && !is_empty(x[[.]])
  )

  vars_present <- names(x)[is_present]

  if (any(is_missing)) {
    stop(
      "The following elements of an nhgis_extract must not contain missing ",
      "values: ",
      paste0(c("collection", "description")[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  if("datasets" %in% types) {

    must_be_non_missing <- c("data_tables", "ds_geog_levels", "data_format")
    must_have_same_length <- c("data_tables", "ds_geog_levels",
                               "years", "breakdown_values")

    is_missing <- purrr::map_lgl(
      must_be_non_missing,
      function(y) any(purrr::map_lgl(x[[y]], ~is_empty(.x) || is.na(.x)))
    )

    if (any(is_missing)) {
      stop(
        "When a dataset is specified, the following must not contain ",
        "missing values: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`",
        call. = FALSE
      )
    }

    # Non-list elements will be recycled, so only need to worry about
    # var lengths if lists are provided in args.
    var_length <- purrr::map_dbl(must_have_same_length, ~length(x[[.]]))
    wrong_length <- purrr::map_lgl(
      must_have_same_length,
      ~is.list(x[[.]]) && length(x[[.]]) != length(x$datasets)
    )

    length_msg <- purrr::imap_chr(
      must_have_same_length,
      ~paste0("`", .x, "` (", var_length[.y], ")")
    )

    if (any(wrong_length)) {
      stop(
        "The number of selections provided in ",
        paste0(length_msg[!right_length], collapse = ", "),
        " does not match the number of datasets (", length(x$datasets),
        "). \nTo recycle selections across datasets, ensure values are stored ",
        "in a vector, not a list.",
        call. = FALSE
      )
    }

    if(!x$data_format %in% c("csv_no_header",
                             "csv_header",
                             "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_no_header`, `csv_header`, ",
        "or `fixed_width`",
        call. = FALSE
      )
    }

    # TODO: Add additional logic to catch other requirements here?
    #   1. years required when dataset has multiple years
    #   2. breakdown_and_data_type_layout required when dataset has multiple
    #      breakdowns or data types
    #
    # Would need to put together metadata functionality first.

  } else {

    ds_sub_vars <- c("data_tables", "ds_geog_levels",
                     "years", "breakdown_values")

    extra_vars <- ds_sub_vars[ds_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "No dataset provided for the following arguments: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if("time_series_tables" %in% types) {

    must_be_non_missing <- c("ts_geog_levels", "data_format",
                             "time_series_table_layout")

    is_missing <- purrr::map_lgl(
      must_be_non_missing,
      function(y) any(purrr::map_lgl(x[[y]], ~is.na(.x) || is_empty(.x)))
    )

    if (any(is_missing)) {
      stop(
        "When a time series table is specified, the following must not contain",
        " missing values: `",
        paste0(must_be_non_missing[is_missing], collapse = "`, `"), "`",
        call. = FALSE
      )
    }

    if (is.list(x$ts_geog_levels) &&
        length(x$ts_geog_levels) != length(x$time_series_tables)) {
      stop(
        "The number of selections provided in `ts_geog_levels` (",
        length(x$ts_geog_levels),
        ") does not match the number of time series tables (",
        length(x$time_series_tables),
        "). \nTo recycle selections across time series tables, ensure values ",
        "are stored in a vector, not a list.",
        call. = FALSE
      )
    }

    if(!x$data_format %in% c("csv_no_header",
                             "csv_header",
                             "fixed_width")) {
      stop(
        "`data_format` must be one of `csv_no_header`, `csv_header`, ",
        "or `fixed_width`",
        call. = FALSE
      )
    }

    if(!x$time_series_table_layout %in% c("time_by_column_layout",
                                          "time_by_row_layout",
                                          "time_by_file_layout")) {
      stop(
        "`time_series_table_layout` must be one of `time_by_column_layout`, ",
        "`time_by_row_layout`, or `time_by_file_layout`",
        call. = FALSE
      )
    }

  } else {

    ts_sub_vars <- c("ts_geog_levels", "time_series_table_layout")
    extra_vars <- ts_sub_vars[ts_sub_vars %in% vars_present]

    if (length(extra_vars) > 0) {
      warning(
        "No time series table provided for the following arguments: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }

  }

  if ("shapefiles" %in% types && length(types) == 1) {

    not_relevant <- c("data_format", "breakdown_and_data_type_layout",
                      "time_series_table_layout", "geographic_extents")
    extra_vars <- intersect(not_relevant, vars_present)

    if (length(extra_vars) > 0) {
      warning(
        "The following parameters are not relevant for extracts that consist ",
        "only of shapefiles: `",
        paste0(extra_vars, collapse = "`, `"), "`.",
        # "`. These parameters will be ignored.",
        call. = FALSE
      )
    }
  }

  x

}

#' @export
validate_ipums_extract.usa_extract <- function(x) {

  must_be_non_missing <- c("description", "data_structure",
                           "data_format", "samples", "variables")

  is_missing <- purrr::map_lgl(
    must_be_non_missing,
    ~any(is.null(x[[.]]) || is.na(x[[.]]) || is_empty(x[[.]]))
  )

  if (any(is_missing)) {
    stop(
      "The following elements of a usa_extract must not contain missing ",
      "values: ",
      paste0(must_be_non_missing[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  stopifnot(x$data_format %in% c("fixed_width", "csv","stata", "spss", "sas9"))
  stopifnot(x$data_structure == "rectangular")
  stopifnot(x$rectangular_on == "P")

  if (x$data_structure == "rectangular" & !x$rectangular_on %in% c("H", "P")) {
    stop("If `data_structure` is 'rectangular', `rectangular_on` must be one ",
         "of 'H' or 'P'")
  }

  if (x$data_structure == "hierarchical" & !is.na(x$rectangular_on)) {
    stop("If `data_structure` is 'hierarchical', `rectangular_on` must be ",
         "missing")
  }

  x

}

#' @export
validate_ipums_extract.ipums_extract <- function(x) {

  if(!x$collection %in% ipums_collection_versions()$collection) {
    stop(
      paste0(
        "No API version found for collection `", x$collection, "`\n",
        "IPUMS API is currently available for the following collections: ",
        paste0(ipums_collection_versions()$collection, collapse = ", ")
      ),
      call. = FALSE
    )
  } else {
    warning(
      paste0("Unable to validate extract for collection `", x$collection, "`.")
    )
  }

  x

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
      nhgis_id = unlist(x$datasets) %||% NA_character_,
      data_tables = unname(x$data_tables),
      ds_geog_levels = unname(x$ds_geog_levels),
      years = if (is_empty(x$years)) {
        list(NULL)
      } else {
        unname(purrr::map(x$years, ~.x))
      },
      breakdown_values = if (is_empty(x$breakdown_values)) {
        list(NULL)
      } else {
        unname(purrr::map(x$breakdown_values, ~.x))
      },
      time_series_table_layout = NA_character_
    ),
    base_vars
  )

  ts <- c(
    list(
      nhgis_id = unlist(x$time_series_tables) %||% NA_character_,
      ts_geog_levels = unname(x$ts_geog_levels),
      time_series_table_layout = x$time_series_table_layout,
      data_tables = list(NULL),
      years = list(NULL),
      breakdown_values = list(NULL)
    ),
    base_vars
  )

  shp <- c(
    list(
      nhgis_id = unlist(x$shapefiles) %||% NA_character_,
      data_tables = list(NULL),
      years = list(NULL),
      breakdown_values = list(NULL),
      ts_geog_levels = list(NULL),
      ds_geog_levels = list(NULL),
      time_series_table_layout = NA_character_
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
  tbl <- tbl[!is.na(tbl$nhgis_id), ]

  var_order <- c("collection", "number", "description", "data_type",
                 "nhgis_id", "data_tables", "ds_geog_levels", "ts_geog_levels",
                 "years",
                 "breakdown_values",  "geographic_extents",
                 "time_series_table_layout", "breakdown_and_data_type_layout",
                 "data_format", "submitted", "download_links", "status")

  tbl[, var_order]

}

#' Identify data types specified in an NHGIS extract
#'
#' An NHGIS extract will request at least one of dataset, time series tables or
#' shapefiles.
#'
#' @param extract An nhgis_extract object
#' @noRd
nhgis_extract_types <- function(extract) {

  stopifnot(extract$collection == "nhgis")

  possible_types <- c("datasets", "time_series_tables", "shapefiles")

  is_missing <- purrr::map_lgl(
    possible_types,
    ~any(
      is.null(extract[[.]]),
      is.na(extract[[.]]),
      is_list(extract[[.]]) && is_empty(extract[[.]]) && is_named(extract[[.]])
    )
  )

  types <- possible_types[!is_missing]

  if(length(types) == 0) {
    NULL
  } else {
    types
  }

}

#' @export
print.usa_extract <- function(x) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: ")
  )

  cat(to_cat)

  invisible(x)

}

#' @export
print.ipums_extract <- function(x) {

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE)
  )

  cat(to_cat)

  invisible(x)

}

#' @export
print.nhgis_extract <- function(x) {

  types <- nhgis_extract_types(x)

  if("datasets" %in% types) {

    n_datasets <- length(x$datasets)

    ds_to_cat <- purrr::map(
      1:n_datasets,
      ~format_dataset_for_printing(
        x$datasets[[.x]],
        x$data_tables[[.x]],
        x$ds_geog_levels[[.x]],
        x$years[[.x]],
        x$breakdown_values[[.x]]
      )
    )

    ds_to_cat <- purrr::reduce(ds_to_cat, paste0)

  } else {
    ds_to_cat <- NULL
  }

  if("time_series_tables" %in% types) {

    ts_to_cat <- purrr::map(
      1:length(x$time_series_tables),
      ~format_tst_for_printing(
        x$time_series_tables[[.x]],
        x$ts_geog_levels[[.x]]
      )
    )

    ts_to_cat <- purrr::reduce(ts_to_cat, paste0)

  } else {
    ts_to_cat <- NULL
  }

  if("shapefiles" %in% types) {

    shp_to_cat <- paste0(
      "\n",
      print_truncated_vector(x$shapefiles, "Shapefiles: ")
    )

  } else {
    shp_to_cat <- NULL
  }

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    ds_to_cat,
    ts_to_cat,
    shp_to_cat,
    "\n"
  )

  cat(to_cat)

  invisible(x)
}


format_collection_for_printing <- function(collection) {
  collection_info <- dplyr::filter(
    ipums_data_collections(),
    code_for_api == collection
  )

  if (nrow(collection_info) == 0) {
    return(UNKNOWN_DATA_COLLECTION_LABEL)
  }

  collection_info$collection_name
}

format_dataset_for_printing <- function(dataset,
                                        data_tables,
                                        geog_levels,
                                        years,
                                        breakdown_values) {
  paste0(
    "\n",
    print_truncated_vector(
      dataset,
      "Dataset: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      data_tables,
      "Tables: ",
      !is_empty(data_tables)
    ),
    "\n  ",
    print_truncated_vector(
      geog_levels,
      "Geog Levels: ",
      !is_empty(geog_levels)
    ),
    "\n  ",
    print_truncated_vector(
      years,
      "Years: ",
      !is_empty(years)
    ),
    "\n  ",
    print_truncated_vector(
      breakdown_values,
      "Breakdowns: ",
      !is_empty(breakdown_values)
    )
  )
}

format_tst_for_printing <- function(time_series_table,
                                    ts_geog_levels) {
  paste0(
    "\n",
    print_truncated_vector(
      time_series_table,
      "Time Series Table: ",
      FALSE
    ),
    "\n  ",
    print_truncated_vector(
      ts_geog_levels,
      "Geog Levels: ",
      !is_empty(ts_geog_levels)
    )
  )
}


UNKNOWN_DATA_COLLECTION_LABEL <- "Unknown data collection"


print_truncated_vector <- function(x, label = NULL, include_length = TRUE) {
  max_width <- min(getOption("width"), 80)
  max_width <- max(max_width, 20) # don't allow width less than 20
  full_list <- paste0(x, collapse = ", ")
  untruncated <- ifelse(
    include_length,
    paste0(label, "(", length(x), " total) ", full_list),
    paste0(label, full_list)
  )
  if (nchar(untruncated) > max_width) {
    return(paste0(substr(untruncated, 1, max_width - 3), "..."))
  }
  untruncated
  # will_be_truncated <- length(x) > truncate_at
  # x <- head(x, truncate_at)
  # out <- paste0(x, collapse = ", ")
  # if (will_be_truncated) {
  #   return(paste0(out, "..."))
  # }
  # out
}

extract_to_request_json <- function(extract) {
  UseMethod("extract_to_request_json")
}

#' @export
extract_to_request_json.nhgis_extract <- function(extract) {

  # if (is.null(extract$description) || is.na(extract$description)) {
  #   extract$description <- ""
  # }

  request_list <- list(
    datasets = format_nhgis_field_for_json(
      datasets = extract$datasets,
      data_tables = extract$data_tables,
      geog_levels = extract$ds_geog_levels,
      years = extract$years,
      breakdown_values = extract$breakdown_values
    ),
    time_series_tables = format_nhgis_field_for_json(
      time_series_tables = extract$time_series_tables,
      geog_levels = extract$ts_geog_levels
    ),
    shapefiles = extract$shapefiles,
    data_format = jsonlite::unbox(extract$data_format),
    description = jsonlite::unbox(extract$description),
    breakdown_and_data_type_layout = jsonlite::unbox(
      extract$breakdown_and_data_type_layout
    ),
    time_series_table_layout = jsonlite::unbox(
      extract$time_series_table_layout
    ),
    geographic_extents = extract$geographic_extents
  )

  request_list <- purrr::keep(
    request_list,
    ~!any(is.na(.x) || is_empty(.x))
  )

  jsonlite::toJSON(request_list)

}

#' @export
extract_to_request_json.usa_extract <- function(extract) {

  if (is.null(extract$description) || is.na(extract$description)) {
    extract$description <- ""
  }

  if (is.null(extract$data_format) || is.na(extract$data_format)) {
    extract$data_format <- ""
  }

  request_list <- list(
    description = extract$description,
    data_structure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    data_format = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}

#' @export
extract_to_request_json.ipums_extract <- function(extract) {

  if (is.na(extract$description)) {
    extract$description <- ""
  }

  request_list <- list(
    description = extract$description
  )

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}

format_nhgis_field_for_json <- function(...) {

  args <- rlang::list2(...)

  if (all(is.na(args[[1]])) || is_empty(args[[1]])) {
    return(NULL)
  }

  supfields <- purrr::map(purrr::compact(args[[1]]), c)
  n_supfields <- length(supfields)

  subfields <- args[2:length(args)]

  # if (recycle) {
  #   subfields <- purrr::map(
  #     subfields,
  #     function(x) purrr::map(
  #       recycle_to_list(
  #         x,
  #         n_supfields,
  #         labels = supfields
  #       ),
  #       function(y) if (!is.null(y)) as.character(y)
  #     )
  #   )
  # }

  subfields_grp <- purrr::map(
    1:n_supfields,
    ~purrr::map(subfields, .x)
  )

  subfields_formatted <- setNames(
    purrr::map(subfields_grp, purrr::compact),
    supfields
  )

  subfields_formatted

}

recycle_to_list <- function(x, n, labels = NULL) {

  if (n < 1) {
    return(x)
  }

  # EMPTY_NAMED_LIST is special case:
  if (is_list(x) && is_empty(x) && is_named(x)) {
    l <- rep(list(x), n)
  } else if (!is_list(x)) { # But otherwise we don't want to recycle lists
    l <- rep(list(x), n)
  } else {
    l <- x
  }

  # if (length(labels) == length(l)) {
  l <- setNames(l, labels)
  # }

  l

}

format_samples_for_json <- function(samples) {
  if (length(samples) == 1 && is.na(samples)) {
    return(EMPTY_NAMED_LIST)
  }
  sample_spec <- purrr::map(seq_along(samples), ~ EMPTY_NAMED_LIST)
  setNames(sample_spec, samples)
}


format_variables_for_json <- function(variables) {
  if (length(variables) == 1 && is.na(variables)) {
    return(EMPTY_NAMED_LIST)
  }
  var_spec <- purrr::map(seq_along(variables), ~ EMPTY_NAMED_LIST)
  var_spec <- setNames(var_spec, variables)
}


format_data_structure_for_json <- function(data_structure, rectangular_on) {
  if (is.null(data_structure) || is.na(data_structure)) {
    return(EMPTY_NAMED_LIST)
  } else if (data_structure == "rectangular") {
    return(list(rectangular = list(on = rectangular_on)))
  } else if (data_structure == "hierarchical") {
    return(list(hierarchical = EMPTY_NAMED_LIST))
  } else {
    return(EMPTY_NAMED_LIST)
  }
}


#' Writes the given url to file_path. Returns the file path of the
#' downloaded data. Raises an error if the request is not successful.
#' @noRd
ipums_api_download_request <- function(url,
                                       file_path,
                                       overwrite,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {


  file_already_exists <- file.exists(file_path)

  if (file.exists(file_path) && !overwrite) {
    stop("File ", file_path, " already exists. If you want to overwrite, set ",
         "`overwrite` to TRUE.", call. = FALSE)
  }

  response <- httr::GET(
    url,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    add_user_auth_header(api_key),
    httr::write_disk(file_path, overwrite = TRUE)
  )

  if (httr::http_status(response)$category != "Success") {
    stop(paste("Could not save", url, "to", file_path))
  }

  return(file_path)
}


#' Helper function to form, submit, and receive responses of requests expecting
#'   a JSON response.
#'
#' @param verb "GET" or "POST"
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Defaults to FALSE, which creates a body-less request.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to httr::modify_url.
#'
#' @return If the request returns a JSON response, this function returns a
#'   length-one character vector containing the response from the API
#'   formatted as JSON. Otherwise, the function throws an error.
#'
#' @noRd
ipums_api_json_request <- function(verb,
                                   collection,
                                   path,
                                   body = FALSE,
                                   queries = NULL,
                                   api_key = Sys.getenv("IPUMS_API_KEY")) {

  queries_is_null_or_named_list <- is.null(queries) ||
    is.list(queries) && !is.null(names(queries)) && !any(names(queries) == "")

  if (!queries_is_null_or_named_list) {
    stop("`queries` argument must be NULL or a named list")
  }

  api_url <- httr::modify_url(
    api_base_url(),
    path = path,
    query = c(
      list(collection = collection, version = ipums_api_version(collection)),
      queries
    )
  )

  res <- httr::VERB(
    verb = verb,
    url = api_url,
    body = body,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  if (httr::http_status(res)$category != "Success") {
    if (httr::status_code(res) == 400) {
      tryCatch(
        error_details <- parse_400_error(res),
        error = function(cond) {
          stop(
            "Received error from server (status code 400), but could not ",
            "parse response for more details."
          )
        }
      )
      stop(error_details, call. = FALSE)
    } else if (httr::status_code(res) == 404) {
      if (fostr_detect(path, "^extracts/\\d+$")) {
        extract_number <- as.numeric(fostr_split(path, "/")[[1]][[2]])
        most_recent_extract <- get_recent_extracts_info_list(
          collection,
          how_many = 1
        )
        most_recent_extract_number <- most_recent_extract[[1]]$number
        if (extract_number > most_recent_extract_number) {
          coll <- format_collection_for_printing(collection)
          stop(coll, " extract number ", extract_number, " does not exist; ",
               "most recent extract number is ", most_recent_extract_number,
               call. = FALSE)
        }
      }
      stop("URL not found", call. = FALSE)
    } else if (httr::status_code(res) %in% 500:599) {
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    } else { # other non-success codes, e.g. 300s
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    }
  }

  if (httr::http_type(res) != "application/json") {
    stop("Extract API did not return json", call. = FALSE)
  }

  new_ipums_json(
    httr::content(res, "text"),
    collection = collection
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
        data_tables = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$data_tables))
        },
        ds_geog_levels = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$geog_levels))
        },
        years = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$years))
        },
        breakdown_values = if (no_datasets) {
          NULL
        } else {
          purrr::map(x$datasets, ~unlist(.x$breakdown_values))
        },
        time_series_tables = names(x$time_series_tables),
        ts_geog_levels = if (no_tsts) {
          NULL
        } else {
          purrr::map(x$time_series_tables, ~unlist(.x$geog_levels))
        },
        shapefiles = unlist(x$shapefiles),
        data_format = x$data_format,
        breakdown_and_data_type_layout = x$breakdown_and_data_type_layout,
        time_series_table_layout = x$time_series_table_layout,
        geographic_extents = unlist(x$geographic_extents),
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



parse_400_error <- function(res) {
  response_content <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = FALSE
  )
  response_detail <- response_content$detail
  response_detail <- unlist(response_detail)
  error_message <- paste0(
    "Received status code 400, with the following details:\n\n",
    paste0(response_detail, collapse = "\n\n")
  )
  return(error_message)
}

# json_to_extract <- function(json, collection) {
#   json_as_data_frame <- jsonlite::fromJSON(json)
# }


extract_is_completed_and_has_links <- function(extract) {
  UseMethod("extract_is_completed_and_has_links")
}

#' @export
extract_is_completed_and_has_links.usa_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return (is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddi_codebook") &&
    has_url(download_links, "data")
}

#' @export
extract_is_completed_and_has_links.nhgis_extract <- function(extract) {

  status <- extract$status
  download_links <- extract$download_links

  # TODO: Check if there needs to be more logic here for NHGIS.
  status == "completed" && length(download_links) > 0

}


add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
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


api_base_url <- function() {
  api_url <- Sys.getenv("IPUMS_API_URL")
  if (api_url == "") return("https://api.ipums.org/extracts/")
  api_url
}


api_extracts_path <- function() {
  basename(api_base_url())
}


ipums_api_version <- function(collection) {

  versions <- ipums_collection_versions()

  if(!collection %in% versions$collection) {
    stop(
      paste0(
        "No API version found for collection `", collection, "`\n",
        "IPUMS API is currently available for the following collections: ",
        paste0(versions$collection, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  api_version <- Sys.getenv("IPUMS_API_VERSION")

  if (api_version == "") {
    versions[[which(versions$collection == collection), "version"]]
  } else {
    api_version
  }

}

ipums_collection_versions <- function() {
  tibble::tribble(
    ~collection, ~version,
    "usa", "beta",
    "nhgis", "v1"
  )
}

EMPTY_NAMED_LIST <- setNames(list(), character(0))

`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

#' Convert an absolute path to be relative to the working directory
#'
#' This is only used in unit tests at the moment
#' @noRd
convert_to_relative_path <- function(path) {
  path_components <- strsplit(path, "/|\\\\", perl = TRUE)[[1]]
  wd_components <- strsplit(getwd(), "/|\\\\", perl = TRUE)[[1]]
  if (identical(path_components, wd_components)) {
    stop(
      "Supplied path cannot be the path to the working directory",
      call. = FALSE
    )
  }
  path_length <- length(path_components)
  wd_length <- length(wd_components)
  min_length <- min(path_length, wd_length)
  path_components_min_length <- path_components[1:min_length]
  wd_components_min_length <- wd_components[1:min_length]
  components_match <- path_components_min_length == wd_components_min_length
  if (!any(components_match)) {
    stop("Supplied path must be an absolute path", call. = FALSE)
  }
  on_same_branch <- all(components_match)
  if (on_same_branch) {
    if (path_length > wd_length) {
      return(
        do.call(file.path, as.list(tail(path_components, -min_length)))
      )
    } else {
      return(
        do.call(file.path, as.list(rep("..", times = wd_length - path_length)))
      )
    }
  }
  split_point <- min(which(!components_match))
  path_relative_to_split <- path_components[split_point:length(path_components)]
  wd_relative_to_split <- wd_components[split_point:length(wd_components)]
  wd_levels_below_split <- length(wd_relative_to_split)
  do.call(
    file.path,
    c(
      as.list(rep("..", times = wd_levels_below_split)),
      as.list(path_relative_to_split)
    )
  )
}


#' Convert all download file paths in a cassette file to be relative to the
#' working directory
#'
#' @noRd
convert_paths_in_cassette_file_to_relative <- function(cassette_path) {
  lines <- readLines(cassette_path)
  file_path_lines <- which(fostr_detect(lines, "file: yes")) + 1
  orig_paths <- purrr::map_chr(
    lines[file_path_lines],
    ~strsplit(.x, "string: ")[[1]][[2]]
  )
  converted_paths <- purrr::map_chr(orig_paths, convert_to_relative_path)
  for (i in seq_along(orig_paths)) {
    lines <- gsub(orig_paths[[i]], converted_paths[[i]], lines, fixed = TRUE)
  }
  writeLines(lines, con = cassette_path)
}


skip_if_no_api_access <- function(have_api_access) {
  if (!have_api_access) {
    return(testthat::skip("no API access and no saved API responses"))
  }
}


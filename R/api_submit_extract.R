# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------

# > Submit extract ----

#' Submit an extract request via the IPUMS API
#'
#' Given an extract definition object, submit an extract request via the IPUMS
#' API, and return a modified copy of the extract object with the newly-assigned
#' extract number.
#'
#' @details
#' For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An \code{ipums_extract} for an IPUMS
#'   collection with API support. This can be a user-defined extract (see
#'   \code{\link{define_extract}}) or an extract object returned from another
#'   \code{ipumsr} API function.
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

# > Wait for extract ----

#' Wait for extract to finish
#'
#' Wait for an extract to finish by periodically checking its status via the
#' IPUMS API and returning when the extract is ready to download.
#'
#' @details
#' For an
#' overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
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
#' that are not.
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
#' For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
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


# Non-exported functions --------------------------------------------------

extract_to_request_json <- function(extract) {
  UseMethod("extract_to_request_json")
}

#' @export
extract_to_request_json.nhgis_extract <- function(extract) {

  # if (is.null(extract$description) || is.na(extract$description)) {
  #   extract$description <- ""
  # }

  extract$years <- purrr::map(
    extract$years,
    ~if(!is.null(.x)) as.character(.x)
  )

  if(!is.null(extract$geographic_extents)) {
    extract$geographic_extents <- as.character(extract$geographic_extents)
  }

  request_list <- list(
    datasets = format_nhgis_field_for_json(
      datasets = extract$datasets,
      data_tables = extract$ds_tables,
      geog_levels = extract$ds_geog_levels,
      years = extract$ds_years,
      breakdown_values = extract$ds_breakdown_values
    ),
    time_series_tables = format_nhgis_field_for_json(
      time_series_tables = extract$time_series_tables,
      geog_levels = extract$tst_geog_levels
    ),
    shapefiles = extract$shapefiles,
    data_format = jsonlite::unbox(extract$data_format),
    description = jsonlite::unbox(extract$description),
    breakdown_and_data_type_layout = jsonlite::unbox(
      extract$breakdown_and_data_type_layout
    ),
    time_series_table_layout = jsonlite::unbox(
      extract$tst_layout
    ),
    geographic_extents = extract$geographic_extents
  )

  request_list <- purrr::keep(
    request_list,
    ~!(any(is.na(.x)) || is_empty(.x))
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

  if (is_na(extract$description)) {
    extract$description <- ""
  }

  request_list <- list(
    description = extract$description
  )

  jsonlite::toJSON(request_list, auto_unbox = TRUE)

}

format_nhgis_field_for_json <- function(...) {

  dots <- rlang::list2(...)

  if (all(is.na(dots[[1]])) || is_empty(dots[[1]])) {
    return(NULL)
  }

  supfields <- purrr::map(purrr::compact(dots[[1]]), c)
  n_supfields <- length(supfields)

  subfields <- dots[2:length(dots)]

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

# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' Submit an extract request via the IPUMS API
#'
#' @description
#' Submit an extract request via the IPUMS API and return an [ipums_extract]
#' object containing the extract definition with a newly-assigned extract
#' request number.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param extract An [`ipums_extract`][ipums_extract-class] object.
#' @param api_key API key associated with your user account. Defaults to the
#'   value of the `IPUMS_API_KEY` environment variable. See
#'   [set_ipums_api_key()].
#'
#' @seealso
#' [download_extract()] to download an extract.
#'
#' [get_extract_info()] and [is_extract_ready()] to check the status of an
#'   extract request.
#'
#' @return An [`ipums_extract`][ipums_extract-class] object containing the
#'   extract definition and newly-assigned extract number of the submitted
#'   extract.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' \dontrun{
#' # Store your submitted extract request to obtain the extract number
#' submitted_extract <- submit_extract(my_extract)
#'
#' submitted_extract$number
#'
#' # This is useful for checking the extract request status
#' get_extract_info(submitted_extract)
#'
#' # You can always get the latest status, even if you forget to store the
#' # submitted extract request object
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # You can also check if submitted extract is ready
#' is_extract_ready(submitted_extract)
#'
#' # Or have R check periodically and download when ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
submit_extract <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (!inherits(extract, "ipums_extract")) {
    rlang::abort("Expected `extract` to be an `ipums_extract` object.")
  }

  extract <- validate_ipums_extract(extract)

  response <- ipums_api_json_request(
    "POST",
    collection = extract$collection,
    path = "extracts/",
    body = extract_to_request_json(extract),
    api_key = api_key
  )

  # extract_list_from_json() always returns a list of extracts, but in
  # this case there is always only one, so pluck it out
  extract <- extract_list_from_json(response, validate = TRUE)[[1]]

  message(
    sprintf(
      "Successfully submitted %s extract number %d",
      format_collection_for_printing(extract$collection),
      extract$number
    )
  )

  invisible(extract)
}

#' Wait for an extract request to finish processing
#'
#' @description
#' Wait for an extract request to finish by periodically checking its status
#' via the IPUMS API until it is complete.
#'
#' Completed extracts will have a value of `"completed"` in their `status`
#' field.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @inheritParams define_extract_usa
#' @inheritParams download_extract
#' @inheritParams get_extract_info
#' @inheritParams submit_extract
#' @param extract One of:
#'
#'  * An [`ipums_extract`][ipums_extract-class] object
#'   * The data collection and extract number formatted as a string of the
#'     form `"collection:number"` or as a vector of the form
#'     `c("collection", number)`
#'   * An extract number to be associated with your default IPUMS
#'     collection. See [set_ipums_default_collection()]
#'
#'   Extract numbers do not need to be zero-padded. That is, use `1`, not
#'   `"0001"`.
#'
#'   For a list of codes used to refer to each collection, see
#'   [ipums_data_collections()].
#' @param initial_delay_seconds Seconds to wait before first status check. The
#'   wait time will automatically increase by 10 seconds between each
#'   successive check.
#' @param max_delay_seconds Maximum interval to wait between status checks.
#'   When the wait interval reaches this value, checks will continue to
#'   occur at `max_delay_seconds` intervals until the extract is complete or
#'   `timeout_seconds` is reached. Defaults to 300 seconds (5 minutes).
#' @param timeout_seconds Maximum total number of seconds to continue waiting
#'   for the extract before throwing an error. Defaults to 10,800 seconds (3
#'   hours).
#' @param verbose If `TRUE`, print status updates to the R console at the
#'   beginning of each wait interval and upon extract completion.
#'   Defaults to `TRUE`.
#'
#' @return An [`ipums_extract`][ipums_extract-class] object containing the
#'   extract definition and the URLs from which to download extract files.
#'
#' @seealso
#' [download_extract()] to download an extract.
#'
#' [get_extract_info()] and [is_extract_ready()] to check the status of an
#'   extract request.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Wait for a particular extract request to complete by providing its
#' # associated `ipums_extract` object:
#' downloadable_extract <- wait_for_extract(submitted_extract)
#'
#' # Or by specifying the collection and number for the extract request:
#' downloadable_extract <- wait_for_extract("usa:1")
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' downloadable_extract <- wait_for_extract(1)
#'
#' # Use `download_extract()` to download the completed extract:
#' files <- download_extract(downloadable_extract)
#' }
wait_for_extract <- function(extract,
                             initial_delay_seconds = 0,
                             max_delay_seconds = 300,
                             timeout_seconds = 10800,
                             verbose = TRUE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {
  stopifnot(is.numeric(initial_delay_seconds) && initial_delay_seconds >= 0)
  stopifnot(is.numeric(max_delay_seconds) && max_delay_seconds > 0)
  stopifnot(is.null(timeout_seconds) || is.numeric(timeout_seconds) &&
              timeout_seconds > 0)

  extract <- standardize_extract_identifier(extract)

  is_extract <- inherits(extract, "ipums_extract")

  is_ready <- is_extract && extract_is_completed_and_has_links(extract)
  is_expired <- is_extract && !is_ready && extract$status == "completed"
  is_failed <- is_extract && extract$status %in% c("canceled", "failed")
  is_timed_out <- FALSE

  current_delay <- max(initial_delay_seconds, 0)

  wait_start <- Sys.time()

  while (!is_ready && !is_timed_out && !is_failed && !is_expired) {
    if (current_delay > 0) {
      if (verbose) {
        message(paste("Waiting", current_delay, "seconds..."))
      }
      Sys.sleep(current_delay)
    }

    if (verbose) {
      message("Checking extract status...")
    }

    extract <- get_extract_info(extract, api_key = api_key)

    is_ready <- extract_is_completed_and_has_links(extract)
    is_expired <- !is_ready && extract$status == "completed"
    is_failed <- extract$status %in% c("canceled", "failed")
    is_timed_out <- !is.null(timeout_seconds) &&
      as.numeric(Sys.time() - wait_start, units = "secs") > timeout_seconds

    current_delay <- min(c(current_delay + 10, max_delay_seconds))
  }

  if (is_expired) {
    rlang::abort(c(
      paste0(
        format_collection_for_printing(extract$collection), " extract ",
        extract$number, " has expired and its files have been deleted."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  } else if (is_failed) {
    rlang::abort(c(
      paste0(
        format_collection_for_printing(extract$collection), " extract ",
        extract$number, " has failed."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  } else if (is_timed_out) {
    rlang::abort(c(
      "Extract did not complete within the specified timeout limit.",
      "i" = "Use `is_extract_ready()` to check extract status manually."
    ))
  }

  if (!is_ready) {
    rlang::abort("Unknown error")
  } else if (verbose) {
    message(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " is ready to download."
      )
    )
  }

  invisible(extract)
}

#' Check if an extract is ready to download
#'
#' @description
#' Get the latest status of an in-progress extract request and determine if
#' it has finished processing and is ready for download.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' The "status" of a submitted extract is one of `"queued"`, `"started"`,
#' `"produced"`, `"canceled"`, `"failed"`, or `"completed"`.
#'
#' To be ready to download, an extract must have a `"completed"` status.
#' However, some requests that are `"completed"` may still be unavailable for
#' download, as extracts expire and are removed from IPUMS servers after a set
#' period of time (72 hours for microdata collections, 2 weeks for IPUMS NHGIS).
#'
#' Therefore, this function also checks the `"download_links"` field of the
#' extract request to determine if data are available for download. If an
#' extract has expired (that is, it has completed but its download links are
#' no longer available), this function will warn that the extract request
#' must be resubmitted.
#'
#' @inheritParams wait_for_extract
#'
#' @return A logical vector of length one.
#'
#' @seealso
#' [download_extract()] to download an extract.
#'
#' [get_extract_info()] to check the status of an extract request.
#'
#' [submit_extract()] to resubmit an expired extract request.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Check the extract request associated with a given `ipums_extract` object
#' is_extract_ready(submitted_extract)
#'
#' # Or by supplying the collection and extract number
#' is_extract_ready("usa:1")
#' is_extract_ready(c("usa", "1"))
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' is_extract_ready(1)
#' }
is_extract_ready <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)

  is_extract <- inherits(extract, "ipums_extract")

  is_ready <- is_extract && extract_is_completed_and_has_links(extract)
  is_expired <- is_extract && !is_ready && extract$status == "completed"
  is_failed <- is_extract && extract$status %in% c("failed", "canceled")

  # No need to get updated info if the extract is expired or already ready
  if (!is_expired && !is_failed && !is_ready) {
    extract <- get_extract_info(extract, api_key = api_key)

    is_ready <- extract_is_completed_and_has_links(extract)
    is_expired <- !is_ready && extract$status == "completed"
    is_failed <- extract$status %in% c("failed", "canceled")
  }

  if (is_expired) {
    rlang::warn(c(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " has expired ",
        "and its files have been deleted."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  } else if (is_failed) {
    rlang::warn(c(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " has failed."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  }

  is_ready
}

#' Download a completed IPUMS data extract
#'
#' @description
#' Download an IPUMS data extract via the IPUMS API and write to disk.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' For NHGIS extracts, data files and GIS files (shapefiles) will be saved in
#' separate .zip archives. `download_extract()` will return a character vector
#' including the file paths to all downloaded files.
#'
#' For microdata extracts, only the file path to the downloaded .xml DDI file
#' will be returned, as it is sufficient for reading the data provided in the
#' associated .gz data file.
#'
#' @inheritParams wait_for_extract
#' @inheritParams define_extract_usa
#' @inheritParams submit_extract
#' @param download_dir Path to the directory where the files should be written.
#'   Defaults to current working directory.
#' @param overwrite Logical value indicating whether to overwrite any files that
#'   already exist in `download_dir`. Defaults to `FALSE`.
#'
#' @return The path(s) to the files required to read the data
#'   requested in the extract, invisibly.
#'
#'   For NHGIS, paths will be named with either `"data"` (for tabular data
#'   files) or `"shape"` (for spatial data files) to
#'   indicate the type of data the file contains.
#'
#' @seealso
#' [read_ipums_micro()] or [read_nhgis()] to read tabular
#'   data from an IPUMS extract.
#'
#' [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#' [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @export
#'
#' @examples
#' usa_extract <- define_extract_usa(
#'   description = "2013-2014 ACS Data",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("SEX", "AGE", "YEAR")
#' )
#'
#' nhgis_extract <- define_extract_nhgis(
#'   description = "Example NHGIS extract",
#'   datasets = "1990_STF3",
#'   data_tables = "NP57",
#'   geog_levels = c("county", "tract")
#' )
#'
#' \dontrun{
#' submitted_extract <- submit_extract(usa_extract)
#'
#' # For microdata, the path to the DDI .xml codebook file is provided.
#' usa_xml_file <- download_extract(submitted_extract)
#'
#' # Load with a `read_ipums_micro_*()` function
#' usa_data <- read_ipums_micro(usa_xml_file)
#'
#' # You can also download previous extracts with their collection and number:
#' nhgis_files <- download_extract("nhgis:1")
#'
#' # NHGIS extracts return a path to both the tabular and spatial data files,
#' # as applicable.
#' #
#' # Load NHGIS tabular data
#' nhgis_data <- read_nhgis(data = nhgis_files["data"])
#'
#' # Load NHGIS spatial data
#' nhgis_geog <- read_ipums_sf(data = nhgis_files["shape"])
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("usa")
#' download_extract(1)
#' }
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)
  is_extract <- inherits(extract, "ipums_extract")

  if (is_extract) {
    extract <- validate_ipums_extract(extract)
    is_ready <- extract_is_completed_and_has_links(extract)

    # If not downloadable, check latest status, since we haven't done so yet.
    if (!is_ready) {
      extract <- get_extract_info(extract, api_key = api_key)
    }
  } else {
    extract <- get_extract_info(extract, api_key = api_key)
    is_ready <- extract_is_completed_and_has_links(extract)
  }

  is_expired <- !is_ready && extract$status == "completed"
  is_failed <- extract$status %in% c("canceled", "failed")

  if (is_expired) {
    rlang::abort(c(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " has expired ",
        "and its files have been deleted."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  } else if (is_failed) {
    rlang::abort(c(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " has failed."
      ),
      "i" = resubmission_hint(is_extract)
    ))
  } else if (!is_ready) {
    rlang::abort(c(
      paste0(
        format_collection_for_printing(extract$collection),
        " extract ", extract$number, " is not ready to download."
      ),
      "i" = paste0(
        "Use `wait_for_extract()` to wait for extract ",
        "completion, then reattempt download."
      )
    ))
  }

  download_dir <- normalizePath(download_dir, winslash = "/", mustWork = FALSE)

  if (!dir.exists(download_dir)) {
    rlang::abort(
      paste0("The directory `", download_dir, "` does not exist.")
    )
  }

  ipums_extract_specific_download(extract, download_dir, overwrite, api_key)
}

# Non-exported functions ---------------------------------------------------

#' Convert an `ipums_extract` object to a JSON string for API submission
#'
#' @param extract An `ipums_extract` object
#' @param include_endpoint_info Logical indicating whether to include
#'   collection and API version information in the JSON object
#'
#' @return A JSON string containing the formatted extract definition
#'
#' @noRd
extract_to_request_json <- function(extract, include_endpoint_info) {
  UseMethod("extract_to_request_json")
}

#' @export
extract_to_request_json.nhgis_extract <- function(
    extract,
    include_endpoint_info = FALSE) {
  extract$years <- purrr::map(
    extract$years,
    ~ if (!is.null(.x)) as.character(.x)
  )

  if (!is.null(extract$geographic_extents)) {
    extract$geographic_extents <- as.character(extract$geographic_extents)
  }

  request_list <- list(
    datasets = format_nhgis_field_for_json(
      datasets = extract$datasets,
      dataTables = extract$data_tables[extract$datasets],
      geogLevels = extract$geog_levels[extract$datasets],
      years = extract$years[extract$datasets],
      breakdownValues = extract$breakdown_values[extract$datasets]
    ),
    timeSeriesTables = format_nhgis_field_for_json(
      timeSeriesTables = extract$time_series_tables,
      geogLevels = extract$geog_levels[extract$time_series_tables]
    ),
    shapefiles = extract$shapefiles,
    dataFormat = jsonlite::unbox(extract$data_format),
    description = jsonlite::unbox(extract$description),
    breakdownAndDataTypeLayout = jsonlite::unbox(
      extract$breakdown_and_data_type_layout
    ),
    timeSeriesTableLayout = jsonlite::unbox(
      extract$tst_layout
    ),
    geographicExtents = geog_extent_lookup(
      extract$geographic_extents,
      state_geog_lookup$codes
    )
  )

  request_list <- purrr::keep(
    request_list,
    ~ !(any(is.na(.x)) || is_empty(.x))
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = jsonlite::unbox(extract$collection),
      version = jsonlite::unbox(ipums_api_version())
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list)
}

#' @export
extract_to_request_json.usa_extract <- function(
    extract,
    include_endpoint_info = FALSE) {
  if (is.null(extract$description) || is.na(extract$description)) {
    extract$description <- ""
  }

  if (is.null(extract$data_format) || is.na(extract$data_format)) {
    extract$data_format <- ""
  }

  request_list <- list(
    description = extract$description,
    dataStructure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    dataFormat = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = extract$collection,
      version = ipums_api_version()
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list, auto_unbox = TRUE)
}

#' @export
extract_to_request_json.cps_extract <- function(extract,
                                                include_endpoint_info = FALSE) {
  if (is.null(extract$description) || is.na(extract$description)) {
    extract$description <- ""
  }

  if (is.null(extract$data_format) || is.na(extract$data_format)) {
    extract$data_format <- ""
  }

  request_list <- list(
    description = extract$description,
    dataStructure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    dataFormat = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )

  if (include_endpoint_info) {
    endpoint_info <- list(
      collection = extract$collection,
      version = ipums_api_version()
    )
    request_list <- append(request_list, endpoint_info)
  }

  jsonlite::toJSON(request_list, auto_unbox = TRUE)
}

#' @export
extract_to_request_json.ipums_extract <- function(
    extract,
    include_endpoint_info = FALSE) {
  if (is_na(extract$description)) {
    extract$description <- ""
  }

  request_list <- list(
    description = extract$description
  )

  jsonlite::toJSON(request_list, auto_unbox = TRUE)
}


format_samples_for_json <- function(samples) {
  if (length(samples) == 1 && is.na(samples)) {
    return(EMPTY_NAMED_LIST)
  }
  sample_spec <- purrr::map(seq_along(samples), ~EMPTY_NAMED_LIST)
  purrr::set_names(sample_spec, samples)
}

format_variables_for_json <- function(variables) {
  if (length(variables) == 1 && is.na(variables)) {
    return(EMPTY_NAMED_LIST)
  }
  var_spec <- purrr::map(seq_along(variables), ~EMPTY_NAMED_LIST)
  purrr::set_names(var_spec, variables)
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

format_nhgis_field_for_json <- function(...) {
  dots <- rlang::list2(...)

  if (all(is.na(dots[[1]])) || is_empty(dots[[1]])) {
    return(NULL)
  }

  supfields <- purrr::map(purrr::compact(dots[[1]]), c)
  n_supfields <- length(supfields)

  subfields <- dots[seq(2, length(dots))]

  subfields_grp <- purrr::map(
    1:n_supfields,
    ~ purrr::map(subfields, .x)
  )

  subfields_formatted <- purrr::set_names(
    purrr::map(subfields_grp, purrr::compact),
    supfields
  )

  subfields_formatted
}

#' Writes the given url to file_path. Returns the file path of the
#' downloaded data. Raises an error if the request is not successful.
#'
#' @noRd
ipums_api_download_request <- function(url,
                                       file_path,
                                       overwrite,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (file.exists(file_path) && !overwrite) {
    rlang::abort(
      c(
        paste0("File `", file_path, "` already exists."),
        "To overwrite, set `overwrite = TRUE`"
      )
    )
  }

  response <- httr::GET(
    url,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(utils::packageVersion("ipumsr"))
      )
    ),
    add_user_auth_header(api_key),
    httr::write_disk(file_path, overwrite = TRUE),
    httr::progress()
  )

  if (httr::http_status(response)$category != "Success") {
    rlang::abort(
      c(
        "Failed to download extract files.",
        "i" = paste0(
          "The extract may have expired. ",
          "Check its latest status with `get_extract_info()`"
        )
      )
    )
  }

  return(file_path)
}

#' Collection-specific extract download
#'
#' S3 generic implementation to allow for collection-specific method dispatch
#' for extract downloading. This need arises because of differences
#' in the types of files and file formats provided upon download for different
#' collections.
#'
#' @noRd
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
  ddi_url <- extract$download_links$ddiCodebook$url
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
    paste0(
      "DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
      "to ", data_file_path
    )
  )

  invisible(ddi_file_path)
}

#' @export
ipums_extract_specific_download.cps_extract <- function(extract,
                                                        download_dir,
                                                        overwrite,
                                                        api_key) {
  ddi_url <- extract$download_links$ddiCodebook$url
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
    paste0(
      "DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
      "to ", data_file_path
    )
  )

  invisible(ddi_file_path)
}

#' @export
ipums_extract_specific_download.nhgis_extract <- function(extract,
                                                          download_dir,
                                                          overwrite,
                                                          api_key) {
  table_url <- extract$download_links$tableData$url
  gis_url <- extract$download_links$gisData$url

  urls <- purrr::compact(
    list(
      data = table_url,
      shape = gis_url
    )
  )

  file_paths <- purrr::map_chr(
    urls,
    ~ normalizePath(
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
    rlang::abort(
      c(
        paste0(
          "The following files already exist: ",
          paste0("\"", existing_files, "\"", collapse = ", ")
        ),
        "i" = "To overwrite, set `overwrite = TRUE`."
      )
    )
  }

  file_paths <- purrr::map2_chr(
    urls,
    file_paths,
    ~ ipums_api_download_request(.x, .y, overwrite, api_key)
  )

  if (!is.null(table_url) && !is.null(gis_url)) {
    message(
      paste0(
        "Data file saved to ", file_paths[1],
        "\nShapefile saved to ", file_paths[2]
      )
    )
  } else if (!is.null(table_url)) {
    message(
      paste0("Data file saved to ", file_paths)
    )
  } else if (!is.null(gis_url)) {
    message(
      paste0("Shapefile saved to ", file_paths)
    )
  }

  invisible(file_paths)
}

#' Helper function to form, submit, and receive responses of requests expecting
#'   a JSON response.
#'
#' @param verb `"GET"` or `"POST"`
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Defaults to `FALSE`, which creates a body-less request.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to [httr::modify_url].
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
    rlang::abort("`queries` argument must be `NULL` or a named list")
  }

  api_url <- httr::modify_url(
    api_base_url(),
    path = path,
    query = c(
      list(collection = collection, version = ipums_api_version()),
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
        as.character(utils::packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  if (httr::http_status(res)$category != "Success") {
    status <- httr::status_code(res)

    if (status == 400) {
      tryCatch(
        error_details <- parse_400_error(res),
        error = function(cond) {
          rlang::abort(
            paste0(
              "Received error from server (status code 400), but could not ",
              "parse response for more details."
            )
          )
        }
      )
      rlang::abort(error_details)
    } else if (status == 404) {
      if (fostr_detect(path, "^extracts/\\d+$")) {
        extract_number <- as.numeric(fostr_split(path, "/")[[1]][[2]])
        most_recent_extract_number <- get_last_extract_info(collection)$number

        if (extract_number > most_recent_extract_number) {
          coll <- format_collection_for_printing(collection)
          rlang::abort(
            c(
              paste0(
                coll, " extract number ",
                extract_number, " does not exist."
              ),
              paste0(
                "Most recent extract number: ",
                most_recent_extract_number
              )
            )
          )
        }
      }
      rlang::abort("URL not found")
    } else if (status %in% c(401, 403)) {
      rlang::abort(c(
        "The provided API Key is either missing or invalid.",
        "i" = paste0(
          "Please provide your API key to the `api_key` argument ",
          "or request a key at https://account.ipums.org/api_keys"
        ),
        "i" = "Use `set_ipums_api_key() to save your key for future use."
      ))
    } else { # other non-success codes, e.g. 300s + 500s
      rlang::abort(c(
        paste0(
          "Extract API request failed with status ",
          httr::status_code(res)
        ),
        paste0("URL: ", api_url),
        paste0("Content: ", httr::content(res, "text"))
      ))
    }
  }

  if (httr::http_type(res) != "application/json") {
    rlang::abort("Extract API did not return json")
  }

  new_ipums_json(
    httr::content(res, "text"),
    collection = collection
  )
}

#' Check if an extract is ready for download
#'
#' @param extract An `ipums_extract` object
#'
#' @return Logical indicating whether extract is ready for download
#'
#' @noRd
extract_is_completed_and_has_links <- function(extract) {
  UseMethod("extract_is_completed_and_has_links")
}

#' @export
extract_is_completed_and_has_links.usa_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return(is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddiCodebook") &&
    has_url(download_links, "data")
}

#' @export
extract_is_completed_and_has_links.cps_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return(is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddiCodebook") &&
    has_url(download_links, "data")
}

#' @export
extract_is_completed_and_has_links.nhgis_extract <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  status == "completed" && length(download_links) > 0
}

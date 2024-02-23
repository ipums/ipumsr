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
#' [wait_for_extract()] to wait for an extract to finish processing.
#'
#' [get_extract_info()] and [is_extract_ready()] to check the status of an
#'   extract request.
#'
#' [download_extract()] to download an extract's data files.
#'
#' @return An [`ipums_extract`][ipums_extract-class] object containing the
#'   extract definition and newly-assigned extract number of the submitted
#'   extract.
#'
#'   Note that some unspecified extract fields may be populated with default
#'   values and therefore change slightly upon submission.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_cps(
#'   description = "2018-2019 CPS Data",
#'   samples = c("cps2018_05s", "cps2019_05s"),
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
#' submitted_extract <- get_last_extract_info("cps")
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

  url <- api_request_url(
    collection = extract$collection,
    path = extract_request_path()
  )

  response <- ipums_api_extracts_request(
    "POST",
    collection = extract$collection,
    url = url,
    body = extract_to_request_json(extract),
    api_key = api_key
  )

  # extract_list_from_json() always returns a list of extracts, but in
  # this case there is always only one, so pluck it out
  extract <- extract_list_from_json(response)[[1]]

  message(
    paste0(
      "Successfully submitted ",
      format_collection_for_printing(extract$collection),
      " extract number ", extract$number
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
#' `is_extract_ready()` is a convenience function to check if an extract
#' is ready to download without committing your R session to waiting for
#' extract completion.
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @details
#' The `status` of a submitted extract will be one of `"queued"`, `"started"`,
#' `"produced"`, `"canceled"`, `"failed"`, or `"completed"`.
#'
#' To be ready to download, an extract must have a `"completed"` status.
#' However, some requests that are `"completed"` may still be unavailable for
#' download, as extracts expire and are removed from IPUMS servers after a set
#' period of time (72 hours for microdata collections, 2 weeks for IPUMS NHGIS).
#'
#' Therefore, these functions also check the `download_links` field of the
#' extract request to determine if data are available for download. If an
#' extract has expired (that is, it has completed but its download links are
#' no longer available), these functions will warn that the extract request
#' must be resubmitted.
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
#' @return For `wait_for_extract()`, an
#'   [`ipums_extract`][ipums_extract-class] object containing the extract
#'   definition and the URLs from which to download extract files.
#'
#'   For `is_extract_ready()`, a logical value indicating
#'   whether the extract is ready to download.
#'
#' @seealso
#' [download_extract()] to download an extract's data files.
#'
#' [get_extract_info()] to obtain the definition of a submitted extract request.
#'
#' @export
#'
#' @examples
#' my_extract <- define_extract_ipumsi(
#'   description = "Botswana data",
#'   samples = c("bw2001a", "bw2011a"),
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
#' downloadable_extract <- wait_for_extract("ipumsi:1")
#'
#' # If you have a default collection, you can use the extract number alone:
#' set_ipums_default_collection("ipumsi")
#'
#' downloadable_extract <- wait_for_extract(1)
#'
#' # Use `download_extract()` to download the completed extract:
#' files <- download_extract(downloadable_extract)
#'
#' # Use `is_extract_ready()` if you don't want to tie up your R session by
#' # waiting for completion
#' is_extract_ready("usa:1")
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

#' @rdname wait_for_extract
#' @export
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
#' @param overwrite If `TRUE`, overwrite any conflicting files that
#'   already exist in `download_dir`. Defaults to `FALSE`.
#' @param progress If `TRUE`, output progress bar showing the status of the
#'   download request. Defaults to `TRUE`.
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
#' \dontrun{
#' submitted_extract <- submit_extract(usa_extract)
#'
#' downloadable_extract <- wait_for_extract(submitted_extract)
#'
#' # For microdata, the path to the DDI .xml codebook file is provided.
#' usa_xml_file <- download_extract(downloadable_extract)
#'
#' # Load with a `read_ipums_micro_*()` function
#' usa_data <- read_ipums_micro(usa_xml_file)
#'
#' # You can also download previous extracts with their collection and number:
#' nhgis_files <- download_extract("nhgis:1")
#'
#' # NHGIS extracts return a path to both the tabular and spatial data files,
#' # as applicable.
#' nhgis_data <- read_nhgis(data = nhgis_files["data"])
#'
#' # Load NHGIS spatial data
#' nhgis_geog <- read_ipums_sf(data = nhgis_files["shape"])
#' }
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             progress = TRUE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)
  is_extract <- inherits(extract, "ipums_extract")

  if (is_extract) {
    is_ready <- extract_is_completed_and_has_links(extract)
  }

  # If not downloadable, check latest status, since we haven't done so yet.
  if (!is_extract || !is_ready) {
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

  ipums_extract_specific_download(
    extract,
    download_dir,
    overwrite = overwrite,
    progress = progress,
    api_key = api_key
  )
}

# Non-exported functions ---------------------------------------------------

#' Convert an `ipums_extract` object to a JSON string for API submission
#'
#' @param extract An `ipums_extract` object

#' @return A JSON string containing the formatted extract definition
#'
#' @noRd
extract_to_request_json <- function(extract) {
  UseMethod("extract_to_request_json")
}

#' @export
extract_to_request_json.nhgis_extract <- function(extract) {
  if (!is.null(extract$geographic_extents)) {
    extract$geographic_extents <- as.character(extract$geographic_extents)
  }

  request_list <- list(
    datasets = purrr::flatten(
      purrr::map(extract$datasets, format_for_json)
    ),
    timeSeriesTables = purrr::flatten(
      purrr::map(extract$time_series_tables, format_for_json)
    ),
    shapefiles = extract$shapefiles,
    dataFormat = jsonlite::unbox(extract$data_format),
    description = jsonlite::unbox(extract$description),
    breakdownAndDataTypeLayout = jsonlite::unbox(
      extract$breakdown_and_data_type_layout
    ),
    timeSeriesTableLayout = jsonlite::unbox(extract$tst_layout),
    geographicExtents = extract$geographic_extents,
    collection = jsonlite::unbox(extract$collection),
    version = jsonlite::unbox(ipums_api_version())
  )

  request_list <- purrr::keep(
    request_list,
    ~ !(any(is.na(.x)) || is_empty(.x))
  )

  jsonlite::toJSON(request_list)
}

#' @export
extract_to_request_json.micro_extract <- function(extract) {
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
    samples = purrr::flatten(purrr::map(extract$samples, format_for_json)),
    variables = purrr::flatten(purrr::map(extract$variables, format_for_json)),
    caseSelectWho = extract$case_select_who,
    dataQualityFlags = extract$data_quality_flags,
    collection = extract$collection,
    version = ipums_api_version()
  )

  request_list <- purrr::keep(
    request_list,
    ~ !(any(is.na(.x)) || is_empty(.x))
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

format_for_json <- function(x) {
  UseMethod("format_for_json")
}

#' @export
format_for_json.ipums_spec <- function(x) {
  # Name of field is in the `name` entry
  if ("name" %in% names(x)) {
    l <- purrr::compact(x[setdiff(names(x), "name")])
  } else {
    l <- EMPTY_NAMED_LIST
  }

  names(l) <- to_camel_case(names(l))
  purrr::set_names(list(l), x$name)
}

#' @export
format_for_json.var_spec <- function(x) {
  if (is_null(x$case_selections)) {
    case_selections <- NULL
  } else {
    case_selections <- purrr::set_names(
      list(as.list(x$case_selections)),
      x$case_selection_type
    )
  }

  l <- purrr::compact(
    list(
      dataQualityFlags = x$data_quality_flags,
      caseSelections = case_selections,
      attachedCharacteristics = as.list(x$attached_characteristics),
      preselected = x$preselected
    )
  )

  purrr::set_names(list(l), x$name)
}

#' @export
format_for_json.default <- function(x) {
  x
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
                                            progress,
                                            api_key) {
  UseMethod("ipums_extract_specific_download")
}

#' @export
ipums_extract_specific_download.micro_extract <- function(extract,
                                                          download_dir,
                                                          overwrite,
                                                          progress,
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

  ipums_api_download_request(
    ddi_url,
    ddi_file_path,
    overwrite = overwrite,
    progress = progress,
    api_key = api_key
  )

  ipums_api_download_request(
    data_url,
    data_file_path,
    overwrite = overwrite,
    progress = progress,
    api_key = api_key
  )

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
                                                          progress,
                                                          api_key) {
  table_url <- extract$download_links$table_data$url
  gis_url <- extract$download_links$gis_data$url

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
    ~ ipums_api_download_request(
      .x,
      .y,
      overwrite = overwrite,
      progress = progress,
      api_key = api_key
    )
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
extract_is_completed_and_has_links.micro_extract <- function(extract) {
  download_links <- extract$download_links
  is_complete <- extract$status == "completed"

  has_codebook <- has_url(download_links, "ddi_codebook")
  has_data <- has_url(download_links, "data")

  is_complete && has_codebook && has_data
}

#' @export
extract_is_completed_and_has_links.nhgis_extract <- function(extract) {
  download_links <- extract$download_links
  is_complete <- extract$status == "completed"

  has_table_data <- has_url(download_links, "table_data")
  has_gis_data <- has_url(download_links, "gis_data")

  is_complete && (has_table_data || has_gis_data)
}

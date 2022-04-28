# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions ------------------------------------------------------

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



# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr


#' List files available for analysis in an IPUMS extract
#'
#' Find which files can be loaded from an IPUMS extract. On Windows,
#' this is generally a zip file (which you can optionally unzip). On
#' macOS, they are generally unzipped for you, so there will be a
#' directory.
#'
#' @param file An IPUMS extract zip file or directory
#' @param types One or more of "data", "shape", or "raster" indicating
#'   what type of files to look for.
#' @param data_layer dplyr \code{\link[dplyr]{select}}-style notation for the data
#'   files to look for
#' @param shape_layer dplyr \code{\link[dplyr]{select}}-style notation for the
#'   shape files to look for
#' @param raster_layer dplyr \code{\link[dplyr]{select}}-style notation for the
#'   raster files to look for
#' @return A \code{tbl_df} data.frame containing the files available
#' @examples
#' nhgis_file <- ipums_example("nhgis0008_csv.zip")
#' ipums_list_files(nhgis_file) # Only one extract available
#'
#' @export
ipums_list_files <- function(file, types = NULL, data_layer = NULL,
                          shape_layer = NULL, raster_layer = NULL) {
  data_layer <- enquo(data_layer)
  shape_layer <- enquo(shape_layer)
  raster_layer <- enquo(raster_layer)

  if (is.null(types) | "data" %in% types) {
    data_files <- ipums_list_data(file, !!data_layer)
  }

  if (is.null(types) | "shape" %in% types) {
    shape_files <- ipums_list_shape(file, !!shape_layer)
  }

  if (is.null(types) | "raster" %in% types) {
    raster_files <- ipums_list_raster(file, !!raster_layer)
  }

  dplyr::bind_rows(data = data_files, shape = shape_files, raster = raster_files, .id = "type")
}

#' @rdname ipums_list_files
#' @export
ipums_list_data <- function(file, data_layer = NULL) {
  data_layer <- enquo(data_layer)
  tibble::tibble(
    file = find_files_in(file, "(dat|csv)(\\.gz)?", data_layer, multiple_ok = TRUE)
  )
}

#' @rdname ipums_list_files
#' @export
ipums_list_shape <- function(file, shape_layer = NULL) {
  shape_layer <- enquo(shape_layer)
  tibble::tibble(
    file = find_files_in(file, "(zip|shp)", shape_layer, multiple_ok = TRUE)
  )
}

#' @rdname ipums_list_files
#' @export
ipums_list_raster <- function(file, raster_layer = NULL) {
  raster_layer <- enquo(raster_layer)
  tibble::tibble(
    file = find_files_in(file, "tiff", raster_layer, multiple_ok = TRUE)
  )
}



# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' List files contained within a zipped IPUMS extract
#'
#' Identify the files that can be read from an IPUMS extract.
#'
#' @param file Path to a .zip archive containing the IPUMS extract to be
#'   examined.
#' @param file_select If the path in `file` contains multiple files, a
#'   [tidyselect selection][selection_language] identifying the files to be
#'   included in the output. Only files that match the provided expression
#'   will be included.
#'
#'   While less useful, this can also be provided as a string specifying an
#'   exact file name or an integer to match files by index position.
#' @param types One or more of `"data"` or `"shape"` indicating
#'   the type of files to include in the output. `"data"` refers to
#'   tabular data sources, while `"shape"` refers to spatial data sources.
#'
#'   The use of `"raster"` has been deprecated and will be removed in a
#'   future release.
#' @param data_layer,shape_layer,raster_layer `r lifecycle::badge("deprecated")`
#'   Please use `file_select` instead.
#'
#' @return A [`tibble`][tibble::tbl_df-class] containing the types and names of
#'   the available files.
#'
#' @export
#'
#' @seealso [read_ipums_micro()] or [read_nhgis()] to read tabular data
#'   from an IPUMS extract.
#'
#'   [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#' @examples
#' nhgis_file <- ipums_example("nhgis0712_csv.zip")
#'
#' # 2 available files in this extract
#' ipums_list_files(nhgis_file)
#'
#' # Look for files that match a particular pattern:
#' ipums_list_files(nhgis_file, file_select = matches("ds136"))
ipums_list_files <- function(file,
                             file_select = NULL,
                             types = NULL,
                             data_layer = deprecated(),
                             shape_layer = deprecated(),
                             raster_layer = deprecated()) {
  has_dl <- !missing(data_layer)
  has_sl <- !missing(shape_layer)
  has_rl <- !missing(raster_layer)

  if (file_is_dir(file)) {
    lifecycle::deprecate_warn(
      "0.6.3",
      I("Reading files through a directory "),
      details = "Use `dir()` to list files in a directory.",
      id = "dir_read"
    )
  }

  if (any(c(has_dl, has_sl, has_rl))) {
    lifecycle::deprecate_warn(
      "0.6.0",
      what = I(paste0(
        "Use of `data_layer`, `shape_layer`, and `raster_layer`",
        " in `ipums_list_files()`"
      )),
      with = "ipums_list_files(file_select = )"
    )
  }

  if ("raster" %in% types) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "ipums_list_files(types = 'must be one of \"data\" or \"shape\"')"
    )
  }

  if (has_dl) {
    data_layer <- enquo(data_layer)
  } else {
    data_layer <- enquo(file_select)
  }

  if (has_sl) {
    shape_layer <- enquo(shape_layer)
  } else {
    shape_layer <- enquo(file_select)
  }

  if (has_rl) {
    raster_layer <- enquo(raster_layer)
  } else {
    raster_layer <- enquo(file_select)
  }

  data_files <- NULL
  shape_files <- NULL
  raster_files <- NULL

  if (is.null(types) | "data" %in% types) {
    data_layer <- enquo(data_layer)

    data_files <- tibble::tibble(
      file = find_files_in(
        file,
        "(dat|csv)(\\.gz)?",
        data_layer,
        multiple_ok = TRUE
      )
    )
  }

  if (is.null(types) | "shape" %in% types) {
    shape_layer <- enquo(shape_layer)

    shape_files <- tibble::tibble(
      file = find_files_in(
        file,
        "(zip|shp)",
        shape_layer,
        multiple_ok = TRUE
      )
    )
  }

  if (is.null(types) | "raster" %in% types) {
    raster_layer <- enquo(raster_layer)

    raster_files <- tibble::tibble(
      file = find_files_in(
        file,
        "tiff",
        raster_layer,
        multiple_ok = TRUE
      )
    )
  }

  dplyr::bind_rows(
    data = data_files,
    shape = shape_files,
    raster = raster_files,
    .id = "type"
  )
}

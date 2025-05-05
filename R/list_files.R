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
#' @param types One or more of `"data"`, `"shape"`, or `"codebook"` indicating
#'   the type of files to include in the output. `"data"` refers to
#'   tabular data sources, `"shape"` refers to spatial data sources, and
#'   `"codebook"` refers to metadata text files that accompany data files.
#'
#' @return A [`tibble`][tibble::tbl_df-class] containing the types and names of
#'   the available files.
#'
#' @export
#'
#' @seealso [read_ipums_micro()] or [read_ipums_agg()] to read tabular data
#'   from an IPUMS extract.
#'
#'   [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#' @examples
#' nhgis_file <- ipums_example("nhgis0712_csv.zip")
#'
#' # 2 available data files in this extract (with codebooks)
#' ipums_list_files(nhgis_file)
#'
#' # Look for files that match a particular pattern:
#' ipums_list_files(nhgis_file, file_select = matches("ds136"))
ipums_list_files <- function(file, file_select = NULL, types = NULL) {
  if (file_is_dir(file)) {
    lifecycle::deprecate_warn(
      "0.6.3",
      I("Reading files through a directory "),
      details = "Use `dir()` to list files in a directory.",
      id = "dir_read"
    )
  }

  cb_files <- NULL
  data_files <- NULL
  shape_files <- NULL

  file_select <- enquo(file_select)

  if (is.null(types) || "codebook" %in% types) {
    cb_files <- tibble::tibble(
      file = find_files_in(
        file,
        "(xml|txt)$",
        file_select,
        multiple_ok = TRUE
      )
    )
  }

  if (is.null(types) || "data" %in% types) {
    data_files <- tibble::tibble(
      file = find_files_in(
        file,
        "(dat|csv)(\\.gz)?$",
        file_select,
        multiple_ok = TRUE
      )
    )
  }

  if (is.null(types) || "shape" %in% types) {
    shape_files <- tibble::tibble(
      file = find_files_in(
        file,
        "(zip|shp)$",
        file_select,
        multiple_ok = TRUE
      )
    )
  }

  dplyr::bind_rows(
    data = data_files,
    shape = shape_files,
    codebook = cb_files,
    .id = "type"
  )
}

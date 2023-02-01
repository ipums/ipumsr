# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Get path to IPUMS example datasets
#'
#' Construct file path to example extracts included with ipumsr. These data
#' are used in package examples and can be used to experiment with ipumsr
#' functionality.
#'
#' @param path Name of file. If `NULL`, all available example files will be
#'   listed.
#'
#' @return The path to a specific example file or a vector of all
#'   available files.
#'
#' @export
#'
#' @examples
#' # List all available example files
#' ipums_example()
#'
#' # Get path to a specific example file
#' file <- ipums_example("cps_00006.xml")
#'
#' read_ipums_ddi(file)
ipums_example <- function(path = NULL) {
  if (is.null(path)) {
    file <- dir(system.file("extdata", package = "ipumsr"))
  } else {
    file <- system.file("extdata", path, package = "ipumsr")
    if (!file.exists(file)) {
      all_files <- paste(dir(system.file("extdata", package = "ipumsr")), collapse = ", ")
      stop(paste(
        custom_format_text(
          "Could not find file '", path, "' in examples. Available files are:",
          indent = 2, exdent = 2
        ),
        custom_format_text(all_files, indent = 4, exdent = 4),
        sep = "\n"
      ))
    }
  }
  file
}

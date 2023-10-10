# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Launch a browser window to an IPUMS metadata page
#'
#' @description
#' Using an [ipums_ddi] object or IPUMS project
#' along with a variable name, guesses the most appropriate URL for
#' the variable's page on the IPUMS website.
#'
#' Currently, this is only available for Windows operating systems.
#'
#' @details
#' Note that some IPUMS projects (e.g. NHGIS) do not have variable-specific
#' pages.
#'
#' The generated URL may not work for variables that are constructed during the
#' extract creation process.
#'
#' @param x An [`ipums_ddi`] object. If left empty, `project` must be specified.
#' @param var Name of the variable to load
#' @param project Name of an IPUMS project. Must be one of:
#'   `"IPUMS-USA"`, `"IPUMS-CPS"`, `"IPUMS-International"`, `"IPUMS-DHS"`,
#'   `"ATUS-X"`, `"AHTUS-X"`, `"MTUS-X"`, `"NHIS"`, `"Higher Ed"`, `"NHGIS"`
#' @param launch If `TRUE`, launch the website.
#' @param verbose If `TRUE`, produces message if no variable-specific websites
#'   are found.
#' @param var_label Variable label for the provided `var`.
#'
#'   This may be useful if the URL produced by `var` alone is incorrect.
#'
#'   Only used if specifying `project`, not `x`.
#' @param homepage_if_missing If `TRUE`, return the project homepage if the
#'   project does not provide variable-specific web pages.
#'
#' @return The URL to the IPUMS webpage for the indicated project and variable
#'   (silently if launch is `TRUE`)
#'
#' @export
#'
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))
#'
#' \dontrun{
#' # Launch webpage for particular variable
#' ipums_website(ddi, "MONTH")
#' }
#'
#' # Can specify an IPUMS project instead of using an `ipums_ddi` object
#' ipums_website(var = "RECTYPE", project = "IPUMS-CPS", launch = FALSE)
ipums_website <- function(x,
                          var,
                          project = NULL,
                          launch = TRUE,
                          verbose = TRUE,
                          var_label = NULL,
                          homepage_if_missing = TRUE) {
  UseMethod("ipums_website")
}

#' @export
ipums_website.ipums_ddi <- function(x,
                                    var,
                                    project = NULL,
                                    launch = TRUE,
                                    verbose = TRUE,
                                    var_label = NULL,
                                    homepage_if_missing = TRUE) {
  if (is.null(project)) project <- x$ipums_project

  # Some convuluted code to check for "detailed variables", because their
  # urls aren't right
  var <- fix_for_detailed_var(x, var, var_label)

  url <- get_ipums_url(var, project, verbose, homepage_if_missing)

  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

#' @export
ipums_website.default <- function(x,
                                  var,
                                  project = NULL,
                                  launch = TRUE,
                                  verbose = TRUE,
                                  var_label = NULL,
                                  homepage_if_missing = TRUE) {
  if (is.null(project)) {
    project <- attributes(x)[["ipums_project"]]
  }

  if (missing(x)) {
    x <- NULL
  }

  # Some convuluted code to check for "detailed variables", because their
  # urls aren't right
  var <- fix_for_detailed_var(x, var, var_label)

  url <- get_ipums_url(var, project, verbose, homepage_if_missing)

  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

get_ipums_url <- function(var,
                          project,
                          verbose = TRUE,
                          homepage_if_missing = FALSE) {
  if (is.null(project)) {
    rlang::abort(c(
      paste0(
        "No project found. Use `project` to specify a project. ",
        "Available projects:"
      ),
      paste0("\"", all_proj_names(), "\"")
    ))
  }

  config <- get_proj_config(project)

  # TODO: This does not actually error on missing projects, because
  # get_proj_config() returns a default config for invalid projects.
  # Better check would just be that the project name is in all_proj_names()
  # Do when updating ipums_website()
  if (is.null(config)) {
    rlang::abort(c(
      "Unexpected project. Available projects:",
      paste0("\"", all_proj_names(), "\"")
    ))
  }

  if (verbose && !config$var_url) {
    message("Cannot give a variable-specific URL for this project.")
  }

  if (!homepage_if_missing && !config$var_url) {
    return(NULL)
  }

  config$url_function(var)
}


# Some convuluted code to check for "detailed variables", because their urls
# aren't right
fix_for_detailed_var <- function(object, var, var_label) {
  if (is.null(var_label) & !is.null(object)) {
    var_label <- ipums_var_label(object, one_of(var))
  }

  if (is.null(var_label)) {
    return(var)
  }

  is_det <- grepl("detailed version", tolower(var_label), fixed = TRUE)

  if (is_det && fostr_sub(var, -1) == "D") {
    var <- fostr_sub(var, 1, -2)
  }

  var
}

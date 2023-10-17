# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Launch a browser window to an IPUMS metadata page
#'
#' @description
#' Launch the documentation webpage for a given
#' IPUMS project and variable. The project can be provided in the form
#' of an [`ipums_ddi`] object or can be manually specified.
#'
#' This provides access to more extensive variable metadata than may be
#' contained within an `ipums_ddi` object itself.
#'
#' Note that some IPUMS projects (e.g. IPUMS NHGIS) do not have
#' variable-specific pages. In these cases, `ipums_website()` will launch the
#' project's main data selection page.
#'
#' @details
#' If `launch = TRUE`, you will need a valid registration for the specified
#' project to successfully launch the webpage.
#'
#' Not all IPUMS variables are found at webpages that exactly match the variable
#' names that are included in completed extract files (and `ipums_ddi` objects).
#' Therefore, there may be some projects and variables for which
#' `ipums_website()` will launch the page for a different variable or an
#' invalid page.
#'
#' @param x An [`ipums_ddi`] object or the name of an IPUMS project.
#'   See [`ipums_data_collections()`] for supported projects.
#' @param var Name of the variable to load. If `NULL`, provides the URL to the
#'   project's main data selection site.
#' @param launch If `TRUE`, launch a browser window to the metadata webpage.
#'   Otherwise, return the URL for the webpage.
#' @param verbose If `TRUE`, produce warnings when invalid URL specifications
#'   are detected.
#' @param homepage_if_missing If `TRUE`, return the IPUMS homepage if the
#'   IPUMS project in `x` is not recognized.
#' @param var_label `r lifecycle::badge("deprecated")` Variable label for the
#'   provided `var`. This is typically obtained from the input `ipums_ddi`
#'   object and is unlikely to be needed.
#' @param project `r lifecycle::badge("deprecated")` Please use `x` instead.
#'
#' @return The URL to the IPUMS webpage for the indicated project and variable
#'   (invisibly if `launch = TRUE`)
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
#' # Can also specify an IPUMS project instead of an `ipums_ddi` object
#' ipums_website("IPUMS CPS", var = "RECTYPE", launch = FALSE)
#'
#' # Shorthand project names from `ipums_data_collections()` are also accepted:
#' ipums_website("ipumsi", var = "YEAR", launch = FALSE)
ipums_website <- function(x,
                          var = NULL,
                          launch = TRUE,
                          verbose = TRUE,
                          homepage_if_missing = FALSE,
                          project = deprecated(),
                          var_label = deprecated()) {
  UseMethod("ipums_website")
}

#' @export
ipums_website.ipums_ddi <- function(x,
                                    var = NULL,
                                    launch = TRUE,
                                    verbose = TRUE,
                                    homepage_if_missing = FALSE,
                                    project = deprecated(),
                                    var_label = deprecated()) {
  if (!missing(project)) {
    lifecycle::deprecate_warn(
      "0.7.0",
      "ipums_website(project = )",
      "ipums_website(x = )"
    )
  }

  if (!missing(var_label)) {
    lifecycle::deprecate_warn(
      "0.7.0",
      "ipums_website(var_label = )"
    )
    var_label <- NULL
  }

  if (length(var) > 1) {
    var <- var[length(var)]

    if (verbose) {
      rlang::warn(
        paste0("Multiple variables specified. Using variable \"", var, "\"")
      )
    }
  }

  var <- fix_for_detailed_var(x, var = var, var_label = var_label)

  if (!rlang::is_null(var) && !var %in% x$var_info$var_name && verbose) {
    rlang::warn(
      paste0(
        "`var` \"", var, "\" was not found in the provided `ipums_ddi` object"
      )
    )
  }

  url <- get_ipums_url(
    x$ipums_project,
    var = var,
    verbose = verbose,
    homepage_if_missing = homepage_if_missing
  )

  if (launch) {
    utils::browseURL(url)
    invisible(url)
  } else {
    url
  }
}

#' @export
ipums_website.character <- function(x,
                                    var = NULL,
                                    launch = TRUE,
                                    verbose = TRUE,
                                    homepage_if_missing = FALSE,
                                    project = deprecated(),
                                    var_label = deprecated()) {
  # This is included only for consistency with previous behavior of
  # `ipums_website()`, where `project` was allowed instead of `x`.
  # Remove when removing `project` arg.
  if (missing(x)) {
    x <- project
  }

  if (!missing(project)) {
    lifecycle::deprecate_warn(
      "0.7.0",
      "ipums_website(project = )",
      "ipums_website(x = )"
    )
  }

  if (!missing(var_label)) {
    lifecycle::deprecate_warn(
      "0.7.0",
      "ipums_website(var_label = )"
    )
    var_label <- NULL
  }

  if (length(var) > 1) {
    var <- var[length(var)]

    if (verbose) {
      rlang::warn(
        paste0("Multiple variables specified. Using variable \"", var, "\"")
      )
    }
  }

  var <- fix_for_detailed_var(x, var = var, var_label = var_label)

  url <- get_ipums_url(
    x,
    var = var,
    verbose = verbose,
    homepage_if_missing = homepage_if_missing
  )

  if (launch) {
    utils::browseURL(url)
    invisible(url)
  } else {
    url
  }
}

get_ipums_url <- function(project,
                          var = NULL,
                          verbose = TRUE,
                          homepage_if_missing = FALSE) {
  config <- get_proj_config(
    project,
    default_if_missing = homepage_if_missing,
    verbose = verbose
  )

  if (verbose && !config$has_var_url && !rlang::is_null(var)) {
    rlang::warn(
      paste0("Cannot give a variable-specific URL for project \"", project, "\"")
    )
  }

  config$var_url(var)
}

# Detailed variables use the same URL as the non-detailed versions of those
# variables. We need to remove the ending "D" from these variable names.
# We identify detailed variables by checking for the text "detailed version"
# in their variable label.
fix_for_detailed_var <- function(object, var, var_label = NULL) {
  if (is.null(var_label) & !is.null(object)) {
    var_label <- ipums_var_label(object, any_of(var))
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

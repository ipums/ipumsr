# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Consolidate all project-specific code into one place to preserve sanity

# Project configs include:
# var_url:
# TRUE/FALSE to indicate whether the variables (in general) have a specific URL
# we can guess at.
# url_function:
# A function that returns a URL (can either be to the specific variable, or
# just to a general website depending on the value of var_url)

# Example URLS
# USA Ex: https://usa.ipums.org/usa-action/variables/ABSENT
# CPS Ex: https://cps.ipums.org/cps-action/variables/ABSENT
# IPUMSI Ex: https://international.ipums.org/international-action/variables/ABROADCHD
# DHS Ex: https://www.idhsdata.org/idhs-action/variables/ABDOMINYR
# ATUS Ex: https://www.atusdata.org/atus-action/variables/WT06 (Time use vars won't work...)
# AHTUS Ex: https://www.ahtusdata.org/ahtus-action/variables/EPNUM (Time use vars won't work...)
# MTUS Ex: https://www.mtusdata.org/mtus-action/variables/SAMPLE
# IHIS Ex: https://ihis.ipums.org/ihis-action/variables/ABGASTRUBYR
# Higher Ed Ex: https://highered.ipums.org/highered-action/variables/ACADV
# NHGIS Ex: https://data2.nhgis.org/main (can't get to specific variable...)

# Project specific configurations ------

#' List IPUMS projects with relevant metadata
#'
#' @description
#' Consolidate general information about all current IPUMS projects, including
#' project names, collection types, API codes, API support, and website URLs.
#'
#' If new IPUMS projects are added, supported by the API, or get updated
#' URLs, these paramteres can be adjusted here.
#'
#' For use in `ipums_data_collections()`, `ipums_website()`, and `ipums_view()`
#'
#' @noRd
proj_config <- function() {
  list(
    new_proj_config(
      "IPUMS USA",
      url_name = "usa",
      collection_type = "microdata",
      api_support = TRUE
    ),
    new_proj_config(
      "IPUMS CPS",
      url_name = "cps",
      collection_type = "microdata",
      api_support = TRUE
    ),
    new_proj_config(
      "IPUMS International",
      url_name = "international",
      collection_type = "microdata",
      code_for_api = "ipumsi",
      api_support = TRUE
    ),
    new_proj_config(
      "IPUMS NHGIS",
      url_name = "nhgis",
      collection_type = "aggregate data",
      api_support = TRUE,
      has_var_url = FALSE,
      home_url = "https://nhgis.org/",
      var_url = function(var = NULL) {
        "https://data2.nhgis.org/main/"
      }
    ),
    new_proj_config(
      "IPUMS IHGIS",
      url_name = "ihgis",
      collection_type = "aggregate data",
      has_var_url = FALSE,
      home_url = "https://ihgis.ipums.org/",
      var_url = function(var = NULL) {
        "https://data.ihgis.ipums.org/main"
      }
    ),
    new_proj_config(
      "IPUMS ATUS",
      url_name = "atus",
      collection_type = "microdata",
      api_support = TRUE,
      home_url = "https://www.atusdata.org/atus/",
      var_url = function(var = "group") {
        get_var_url("atusdata", "atus", var = var, ipums_domain = FALSE)
      }
    ),
    new_proj_config(
      "IPUMS AHTUS",
      url_name = "ahtus",
      collection_type = "microdata",
      api_support = TRUE,
      home_url = "https://www.ahtusdata.org/ahtus/",
      var_url = function(var = "group") {
        get_var_url("ahtusdata", "ahtus", var = var, ipums_domain = FALSE)
      }
    ),
    new_proj_config(
      "IPUMS MTUS",
      url_name = "mtus",
      collection_type = "microdata",
      api_support = TRUE,
      home_url = "https://www.mtusdata.org/mtus/",
      var_url = function(var = "group") {
        get_var_url("mtusdata", "mtus", var = var, ipums_domain = FALSE)
      }
    ),
    new_proj_config(
      "IPUMS DHS",
      url_name = "idhs",
      collection_type = "microdata",
      code_for_api = "dhs",
      home_url = "https://www.idhsdata.org/",
      var_url = function(var = "group") {
        get_var_url("idhsdata", "idhs", var = var, ipums_domain = FALSE)
      }
    ),
    new_proj_config(
      "IPUMS PMA",
      url_name = "pma",
      collection_type = "microdata"
    ),
    new_proj_config(
      "IPUMS MICS",
      url_name = "mics",
      collection_type = "microdata"
    ),
    new_proj_config(
      "IPUMS NHIS",
      url_name = "nhis",
      api_support = TRUE,
      collection_type = "microdata"
    ),
    new_proj_config(
      "IPUMS MEPS",
      url_name = "meps",
      api_support = TRUE,
      collection_type = "microdata"
    ),
    new_proj_config(
      "IPUMS Higher Ed",
      url_name = "highered",
      collection_type = "microdata"
    )
  )
}

default_config <- function() {
  new_proj_config(
    proj_name = "IPUMS",
    url_name = NULL,
    collection_type = NULL,
    code_for_api = NULL,
    api_support = NULL,
    has_var_url = FALSE,
    home_url = "https://www.ipums.org",
    var_url = function(var = NULL) {
      "https://www.ipums.org"
    }
  )
}

#' Specify the configuration for a new IPUMS project
#'
#' @param proj_name Name of the IPUMS project. Should generally be consistent
#'   with the names found in the DDI files for that project, if any.
#' @param url_name Name of the project as used in that project's website URL.
#'   For instance, IPUMS International uses `"international"` in its URLs.
#' @param collection_type Either `"microdata"` or `"aggregate data"` indicating
#'   the type of data this collection provides.
#' @param code_for_api The name of the project used when interacting with the
#'   IPUMS API (for collections that are supported by the API). For instance,
#'   `"ipumsi"` is used when submitting extract requests to the API for IPUMS
#'   International.
#'
#'   For collections that are not yet supported by the API, a similar placeholder
#'   value is used.
#' @param api_support Logical indicating whether the collection is supported by
#'   the IPUMS API.
#' @param has_var_url Logical indicating whether the collection has
#'   variable-specific URLs.
#' @param home_url URL for the project's homepage. If `NULL`, is generated
#'   from the provided `url_name`, with the form
#'   `"https://{url_name}.ipums.org"`
#' @param var_url Function of `var` that returns a variable-specific URL for
#'   the project. For projects that use standard IPUMS URL constructions, use
#'   `get_var_url()`. For projects that do not use standard
#'   URL constructions, you can write your own function that returns an
#'   appropriate URL.
#'
#' @noRd
new_proj_config <- function(proj_name,
                            url_name,
                            collection_type,
                            code_for_api = url_name,
                            api_support = FALSE,
                            has_var_url = TRUE,
                            home_url = NULL,
                            var_url = NULL) {
  list(
    proj_name = proj_name,
    url_name = url_name,
    collection_type = collection_type,
    code_for_api = code_for_api,
    api_support = api_support,
    has_var_url = has_var_url,
    home_url = home_url %||% paste0("https://", url_name, ".ipums.org/"),
    var_url = var_url %||% function(var = NULL) {
      get_var_url(url_name, var = var)
    }
  )
}

# Get the configuration for a specified IPUMS project
get_proj_config <- function(proj, default_if_missing = TRUE, verbose = TRUE) {
  proj <- get_proj_name(proj)

  config <- purrr::flatten(
    purrr::keep(
      proj_config(),
      ~ tolower(.x$proj_name) == tolower(proj)
    )
  )

  if (rlang::is_empty(config)) {
    if (!default_if_missing) {
      rlang::abort(c(
        "Project not found. Available projects:",
        paste0("\"", ipums_data_collections()$collection_name, "\"")
      ))
    } else {
      if (verbose) {
        rlang::warn("Project not found. Redirecting to IPUMS homepage.")
      }
      config <- default_config()
    }
  }

  config
}

#' Construct a variable-specific URL for an IPUMS project
#'
#' @param domain_proj Project name as used in the domain of the URL. For
#'   instance, IPUMS USA uses `"usa"` (for `"usa.ipums.org"`), while IPUMS ATUS
#'   uses `"atusdata"`.
#' @param path_proj Project name as used in the path of the URL. For instance,
#'   IPUMS USA uses `"usa"` (for `"/usa-action/variables"`). Defaults to
#'   `domain_proj`.
#' @param var Variable to include in the URL. If `NULL`, uses `"group"`, which
#'   navigates to the general variable-selection webpage.
#' @param ipums_domain Logical indicating whether to include `"ipums"` in the
#'   domain name of the URL. For instance, IPUMS USA uses `"usa.ipums.org"`,
#'   while IPUMS ATUS uses `"atusdata.org"`.
#'
#' @noRd
get_var_url <- function(domain_proj,
                        path_proj = domain_proj,
                        var = NULL,
                        ipums_domain = TRUE) {
  var <- var %||% "group"

  if (ipums_domain) {
    ipums_path <- ".ipums.org/"
  } else {
    ipums_path <- ".org/"
  }

  paste0(
    "https://", domain_proj, ipums_path,
    path_proj, "-action/variables/", var
  )
}

# Helper to ignore case in project names and allow use of API code names
# instead of full-length project names
get_proj_name <- function(proj) {
  collections <- ipums_data_collections()
  proj <- tolower(proj)

  if (proj %in% collections$code_for_api) {
    proj <- collections$collection_name[collections$code_for_api == proj]
  } else {
    # Included for compatibility with previous project names, which used
    # hyphens. Current naming conventions do not use hyphens, though
    # old DDI files (and current ones for IPUMS International) may still
    # include hyphens.
    proj <- fostr_replace_all(proj, "-", " ")
  }

  toupper(proj)
}

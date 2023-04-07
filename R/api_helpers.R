# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' List IPUMS data collections
#'
#' List IPUMS data collections with their corresponding codes used by the
#' IPUMS API. Note that some data collections do not yet have API support.
#'
#' Currently, ipumsr supports extract definitions for the following collections:
#'
#' * IPUMS USA
#' * IPUMS CPS
#' * IPUMS NHGIS
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @return A [`tibble`][tibble::tbl_df-class] with three columns containing the
#'   full collection name, the corresponding code used by the IPUMS API, and the
#'   status of API support for the collection.
#'
#' @export
ipums_data_collections <- function() {
  tibble::tribble(
    ~collection_name, ~code_for_api, ~api_support,
    "IPUMS USA", "usa", TRUE,
    "IPUMS CPS", "cps", TRUE,
    "IPUMS International", "ipumsi", FALSE,
    "IPUMS NHGIS", "nhgis", TRUE,
    "IPUMS AHTUS", "ahtus", FALSE,
    "IPUMS MTUS", "mtus", FALSE,
    "IPUMS ATUS", "atus", FALSE,
    "IPUMS DHS", "dhs", FALSE,
    "IPUMS Higher Ed", "highered", FALSE,
    "IPUMS MEPS", "meps", FALSE,
    "IPUMS NHIS", "nhis", FALSE,
    "IPUMS PMA", "pma", FALSE
  )
}

#' Set your IPUMS API key
#'
#' @description
#' Set your IPUMS API key as the value associated with the `IPUMS_API_KEY`
#' environment variable.
#'
#' The key can be stored for the duration of your session or for future
#' sessions. If saved for future sessions, it is added to the `.Renviron`
#' file in your home directory. If you choose to save your key to `.Renviron`,
#' this function will create a backup copy of the file before modifying.
#'
#' This function is modeled after the `census_api_key()` function
#' from [tidycensus](https://walker-data.com/tidycensus/).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param api_key API key associated with your user account.
#' @param save If `TRUE`, save the key for use in future
#'   sessions by adding it to the `.Renviron` file in your home directory.
#'   Defaults to `FALSE`, unless `overwrite = TRUE`.
#' @param overwrite If `TRUE`, overwrite any existing value of
#'   `IPUMS_API_KEY` in the `.Renviron` file with the provided `api_key`.
#'   Defaults to `FALSE`.
#' @param unset If `TRUE`, remove the existing value of `IPUMS_API_KEY`
#'   from the environment and the `.Renviron` file in your home directory.
#'
#' @return The value of `api_key`, invisibly.
#'
#' @seealso
#' [set_ipums_default_collection()] to set a default collection.
#'
#' @export
set_ipums_api_key <- function(api_key,
                              save = overwrite,
                              overwrite = FALSE,
                              unset = FALSE) {
  if (unset) {
    api_key <- unset_ipums_envvar("IPUMS_API_KEY")
  } else {
    api_key <- set_ipums_envvar(
      IPUMS_API_KEY = api_key,
      save = save,
      overwrite = overwrite
    )
  }

  invisible(api_key)
}

#' Set your default IPUMS collection
#'
#' @description
#' Set the default IPUMS collection as the value associated with the
#' `IPUMS_DEFAULT_COLLECTION` environment variable. If this environment variable
#' exists, IPUMS API functions that require a collection specification will use
#' the value of `IPUMS_DEFAULT_COLLECTION`, unless another collection is
#' indicated.
#'
#' The default collection can be stored for the duration of your session or
#' for future sessions. If saved for future sessions, it is added to the
#' `.Renviron` file in your home directory. If you choose to save your key
#' to `.Renviron`, this function will create a backup copy of the file before
#' modifying.
#'
#' This function is modeled after the `census_api_key()` function
#' from [tidycensus](https://walker-data.com/tidycensus/).
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @param collection Character string of the collection to set as your
#'   default collection. The collection must currently be supported
#'   by the IPUMS API. Use [ipums_data_collections()] to determine if a
#'   collection has API support.
#' @param save If `TRUE`, save the default collection for use in future
#'   sessions by adding it to the `.Renviron` file in your home directory.
#'   Defaults to `FALSE`, unless `overwrite = TRUE`.
#' @param overwrite If `TRUE`, overwrite any existing value of
#'   `IPUMS_DEFAULT_COLLECTION` in the `.Renviron` file with the provided
#'   `collection`. Defaults to `FALSE`.
#' @param unset if `TRUE`, remove the existing value of
#'  `IPUMS_DEFAULT_COLLECTION` from the environment and the `.Renviron` file in
#'  your home directory.
#'
#' @return The value of `collection`, invisibly.
#'
#' @seealso
#' [set_ipums_api_key()] to set an API key.
#'
#' @export
#'
#' @examples
#' set_ipums_default_collection("nhgis")
#'
#' \dontrun{
#' # Extract info will now be retrieved for the default collection:
#' get_last_extract_info()
#' get_recent_extracts_info()
#'
#' is_extract_ready(1)
#' get_extract_info(1)
#'
#' # Equivalent to:
#' get_extract_info("nhgis:1")
#' get_extract_info(c("nhgis", 1))
#'
#' # Other collections can be specified explicitly
#' # Doing so does not alter the default collection
#' is_extract_ready("usa:2")
#' }
#'
#' # Remove the variable from the environment and .Renviron, if saved
#' set_ipums_default_collection(unset = TRUE)
set_ipums_default_collection <- function(collection = NULL,
                                         save = overwrite,
                                         overwrite = FALSE,
                                         unset = FALSE) {
  if (unset) {
    collection <- unset_ipums_envvar("IPUMS_DEFAULT_COLLECTION")
  } else {
    collection <- tolower(collection)

    # Error if collection is not currently available for API
    check_api_support(collection)

    collection <- set_ipums_envvar(
      IPUMS_DEFAULT_COLLECTION = collection,
      save = save,
      overwrite = overwrite
    )
  }

  invisible(collection)
}

# Non-exported functions ---------------------------------------------------

#' Standardize accepted formats for identifying a particular extract request
#'
#' @description
#' Parses `"collection:number"` strings and `c("collection", number)` vectors
#' to identify a particular extract request. Where appropriate, determines
#' if an entire `"collection"` (with no extract number) is provided.
#'
#' @param extract One of:
#'   * An [`ipums_extract`][ipums_extract-class] object
#'   * An IPUMS collection with API support (omit to use your default
#'     collection)
#'   * The data collection and extract number formatted as a string of the
#'     form `"collection:number"` or as a vector of the form
#'     `c("collection", number)`
#'   * An extract number to be associated with your default IPUMS
#'     collection. See [set_ipums_default_collection()]
#' @param collection_ok Logical indicating whether `extract` can be an IPUMS
#'   collection (`TRUE`) or if it must be a specific extract (`FALSE`).
#'
#' @return An `ipums_extract` object or a list of length 2 indicating the
#'   collection and number of the provided `extract` identifier.
#'
#' @noRd
standardize_extract_identifier <- function(extract, collection_ok = FALSE) {
  if (inherits(extract, "ipums_extract")) {
    return(extract)
  }

  # If extract is length 1, must be a "collection:number" id
  # or a single number to be paired with the default collection
  if (length(extract) == 1) {
    if (fostr_detect(extract, ":")) {
      extract <- fostr_split(extract, ":")[[1]]
    } else {
      # Only use default collection if `extract` can be coerced to numeric.
      extract_as_num <- suppressWarnings(
        as.numeric(fostr_replace(extract, "L$", "")) # Handle int specification
      )
      extract_is_chr <- is.na(extract_as_num) || length(extract_as_num) == 0

      # If not coercible to numeric, we are dealing with a collection
      if (extract_is_chr) {
        if (!collection_ok) {
          rlang::abort(
            c(
              "Invalid `extract` argument. Expected `extract` to be one of:",
              "*" = "An `ipums_extract` object",
              "*" = "A string of the form \"collection:number\"",
              "*" = "A vector of the form `c(collection, number)`",
              "*" = paste0(
                "An integer indicating the extract number for the collection ",
                "specified by IPUMS_DEFAULT_COLLECTION. ",
                "See `?set_ipums_default_collection()`."
              )
            )
          )
        } else {
          check_api_support(extract)
          return(list(collection = extract, number = NA))
        }
      }

      extract <- c(get_default_collection(), extract)
    }
  }

  # At this point, should be in `c(collection, number)` format
  if (length(extract) > 1) {
    collection <- trimws(extract[[1]])
    number <- suppressWarnings(
      as.numeric(fostr_replace(extract[[2]], "L$", ""))
    )
  }

  if (length(extract) != 2 || is.na(number)) {
    rlang::abort(
      c(
        "Invalid `extract` argument. Expected `extract` to be one of:",
        "*" = "An `ipums_extract` object",
        "*" = "A string of the form \"collection:number\"",
        "*" = "A vector of the form `c(collection, number)`",
        "*" = paste0(
          "An integer indicating the extract number for the collection ",
          "specified by IPUMS_DEFAULT_COLLECTION. ",
          "See `?set_ipums_default_collection()`."
        )
      )
    )
  }

  check_api_support(collection)

  if (number != round(number)) {
    rlang::abort(
      paste0("Unable to interpret extract number ", number, " as integer.")
    )
  }

  list(collection = collection, number = number)
}

#' Get default IPUMS collection
#'
#' If a default collection is set, return it. Otherwise, throw error indicating
#' that no default is set.
#'
#' @noRd
get_default_collection <- function() {
  collection <- Sys.getenv("IPUMS_DEFAULT_COLLECTION")

  versions <- dplyr::filter(
    ipums_data_collections(),
    .data$api_support != "none"
  )

  if (!collection %in% versions$code_for_api) {
    if (collection == "") {
      rlang::abort(
        c(
          "No default collection set.",
          "i" = paste0(
            "Please specify a collection or use ",
            "`set_ipums_default_collection()` to add a default collection."
          )
        )
      )
    } else {
      rlang::abort(
        c(
          paste0(
            "The default collection is set to \"", collection,
            "\", which is not a supported IPUMS collection."
          ),
          "i" = paste0(
            "The IPUMS API currently supports the following collections: \"",
            paste0(versions$code_for_api, collapse = "\", \""), "\""
          ),
          "i" = paste0(
            "Please specify a collection or use ",
            "`set_ipums_default_collection()` to update your default ",
            "collection."
          )
        )
      )
    }
  }

  collection
}


#' Helper for setting IPUMS environmental variables
#'
#' @param ... Named vector of length 1 indicating the environment variable
#'   and value to set
#' @param save Logical indicating whether to write the variable to .Renviron
#' @param overwrite Logical indicating whether to overwrite an existing
#'   value of the variable in .Renviron
#'
#' @return The value of the new environmental variable, invisibly
#'
#' @noRd
set_ipums_envvar <- function(...,
                             save = overwrite,
                             overwrite = FALSE) {
  dots <- rlang::list2(...)

  stopifnot(length(dots) == 1 && is_named(dots))

  var_name <- names(dots)
  var_value <- dots[[var_name]]

  if (save) {
    home_dir <- Sys.getenv("HOME")
    renviron_file <- file.path(home_dir, ".Renviron")
    new_envvar <- paste0(var_name, "=\"", var_value, "\"")

    if (!file.exists(renviron_file)) {
      file.create(renviron_file)
      writeLines(new_envvar, con = renviron_file)
    } else {
      # TODO: Do we actually need to back this up?
      # If we're *sure* we're not touching any of the variables already in the
      # file, why worry so much about backing up?
      backup_file <- file.path(home_dir, ".Renviron_backup")

      backed_up_renviron <- file.copy(
        renviron_file,
        to = backup_file,
        overwrite = TRUE
      )

      if (!backed_up_renviron) {
        rlang::warn("Failed to back up .Renviron.")
      } else {
        message(
          "Existing .Renviron file copied to ", backup_file,
          " for backup purposes."
        )
      }

      renviron_lines <- readLines(renviron_file)
      var_match <- paste0("^(\\s?)+", var_name)

      has_existing_envvar <- any(fostr_detect(renviron_lines, var_match))

      if (isTRUE(overwrite) && has_existing_envvar) {
        renviron_lines[fostr_detect(renviron_lines, var_match)] <- new_envvar
        writeLines(renviron_lines, con = renviron_file)
      } else {
        if (has_existing_envvar) {
          rlang::abort(
            paste0(
              var_name,
              " already exists in .Renviron. To overwrite it, set ",
              "`overwrite = TRUE`."
            )
          )
        }

        writeLines(c(renviron_lines, new_envvar), con = renviron_file)
      }
    }

    Sys.setenv(...)

    message(
      "The environment variable ", var_name,
      " has been set and saved for future sessions."
    )
  } else {
    Sys.setenv(...)

    message(
      "The environment variable ", var_name,
      " has been set. To save it for future sessions, ",
      "set `save = TRUE`."
    )
  }

  invisible(var_value)
}

#' Helper for removing an IPUMS environmental variable
#'
#' @param var_name Name of the environmental variable to remove. Removes
#'   the variable from both the environment and .Renviron
#'
#' @return An empty character string (`""`)
#'
#' @noRd
unset_ipums_envvar <- function(var_name) {
  home_dir <- Sys.getenv("HOME")
  renviron_file <- file.path(home_dir, ".Renviron")

  msg <- NULL

  if (!file.exists(renviron_file)) {
    rlang::warn("No .Renviron file to update.")
  } else {
    renviron_lines <- readLines(renviron_file)
    var_match <- paste0("^(\\s?)+", var_name)

    line_is_envvar <- fostr_detect(renviron_lines, var_match)

    if (any(line_is_envvar)) {
      backup_file <- file.path(home_dir, ".Renviron_backup")

      backed_up_renviron <- file.copy(
        renviron_file,
        to = backup_file,
        overwrite = TRUE
      )

      if (!backed_up_renviron) {
        rlang::warn("Failed to back up .Renviron.")
      } else {
        message(
          "Existing .Renviron file copied to ", backup_file,
          " for backup purposes."
        )
      }

      msg <- " and removing from .Renviron"

      renviron_lines <- renviron_lines[!line_is_envvar]
      writeLines(renviron_lines, con = renviron_file)
    }
  }

  message(
    "Unsetting environment variable ", var_name, msg, "."
  )

  Sys.unsetenv(var_name)

  invisible("")
}

#' @export
print.ipums_extract <- function(x, ...) {
  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE)
  )

  cat(to_cat)

  invisible(x)
}

#' @export
print.usa_extract <- function(x, ...) {
  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: "),
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

#' @export
print.cps_extract <- function(x, ...) {
  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    "\n", print_truncated_vector(x$samples, "Samples: "),
    "\n", print_truncated_vector(x$variables, "Variables: "),
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

#' @export
print.nhgis_extract <- function(x, ...) {
  style_ds <- extract_field_styler(nhgis_print_color("dataset"), "bold")

  ds_to_cat <- purrr::map(
    seq_along(x$datasets),
    ~ format_field_for_printing(
      parent_field = list("Dataset: " = x$datasets[[.x]]),
      subfields = list(
        "Tables: " = x$data_tables[x$datasets][[.x]],
        "Geog Levels: " = x$geog_levels[x$datasets][[.x]],
        "Years: " = x$years[x$datasets][[.x]],
        "Breakdowns: " = x$breakdown_values[x$datasets][[.x]]
      ),
      parent_style = style_ds,
      subfield_style = extract_field_styler("bold")
    )
  )

  ds_to_cat <- c(
    ds_to_cat,
    format_field_for_printing(
      parent_field = list("Geographic extents: " = x$geographic_extents),
      parent_style = style_ds
    )
  )

  tst_to_cat <- purrr::map(
    seq_along(x$time_series_tables),
    ~ format_field_for_printing(
      parent_field = list("Time Series Table: " = x$time_series_tables[[.x]]),
      subfields = list(
        "Geog Levels: " = x$geog_levels[x$time_series_tables][[.x]]
      ),
      parent_style = extract_field_styler(
        nhgis_print_color("time_series_table"),
        "bold"
      ),
      subfield_style = extract_field_styler("bold")
    )
  )

  shp_to_cat <- format_field_for_printing(
    parent_field = list("Shapefiles: " = x$shapefiles),
    parent_style = extract_field_styler(
      nhgis_print_color("shapefile"),
      "bold"
    )
  )

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n", print_truncated_vector(x$description, "Description: ", FALSE),
    paste0(ds_to_cat, collapse = ""),
    paste0(tst_to_cat, collapse = ""),
    shp_to_cat,
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

format_collection_for_printing <- function(collection) {
  collection_info <- dplyr::filter(
    ipums_data_collections(),
    .data$code_for_api == collection
  )

  if (nrow(collection_info) == 0) {
    return(UNKNOWN_DATA_COLLECTION_LABEL)
  }

  collection_info$collection_name
}

#' Format an extract field for printing
#'
#' Format an extract field, along with associated subfields (if applicable),
#' which will be indented and displayed beneath the entry for the associated
#' parent field.
#'
#' @param parent_field Named list of length 1. Names serve as text that will
#'   be displayed before the list values
#' @param subfields Named list of arbitrary length. Names serve as text that
#'   will be displayed before the corresponding values in that list element.
#' @param parent_style Optional `crayon` style function used to style the name
#'   of the `parent_field`.
#' @param subfield_style Optional `crayon` style function used to style the
#'   name(s) of the `subfields`.
#' @param padding Number of line breaks to include between each parent field.
#'
#' @return Formatted text for printing
#'
#' @noRd
format_field_for_printing <- function(parent_field = NULL,
                                      subfields = NULL,
                                      parent_style = NULL,
                                      subfield_style = NULL,
                                      padding = 2) {
  stopifnot(length(parent_field) == 1)

  parent_val <- parent_field[[1]]
  parent_name <- names(parent_field)

  if (is.null(parent_val)) {
    return(NULL)
  }

  style_field <- parent_style %||% extract_field_styler("reset")
  style_subfield <- subfield_style %||% extract_field_styler("reset")

  output <- paste0(
    paste0(rep("\n", padding), collapse = ""),
    print_truncated_vector(
      parent_val,
      style_field(parent_name),
      FALSE
    )
  )

  if (!is.null(subfields)) {
    purrr::map(
      names(subfields),
      ~ if (!is.null(subfields[[.x]])) {
        output <<- paste0(
          output,
          "\n  ",
          print_truncated_vector(subfields[[.x]], style_subfield(.x), FALSE)
        )
      }
    )
  }

  output
}

#' Create a `crayon` style function if `crayon` is installed
#'
#' @param ... Values passed to [crayon::combine_styles()]
#'
#' @return A `crayon` style function
#'
#' @noRd
extract_field_styler <- function(...) {
  if (rlang::is_installed("crayon")) {
    style <- crayon::combine_styles(...)
  } else {
    style <- function(x) x
  }

  style
}

nhgis_print_color <- function(type) {
  type <- match.arg(type, c("dataset", "time_series_table", "shapefile"))

  switch(type,
    dataset = "blue",
    time_series_table = "green",
    shapefile = "yellow"
  )
}

UNKNOWN_DATA_COLLECTION_LABEL <- "Unknown data collection"

print_truncated_vector <- function(x, label = NULL, include_length = TRUE) {
  max_width <- min(getOption("width"), 80)
  max_width <- max(max_width, 20) # don't allow width less than 20

  full_list <- paste0(x, collapse = ", ")

  untruncated <- ifelse(
    include_length,
    paste0(label, "(", length(x), " total) ", full_list),
    paste0(label, full_list)
  )

  if (rlang::is_installed("crayon")) {
    count_chr <- function(x) crayon::col_nchar(x)
  } else {
    count_chr <- function(x) nchar(x)
  }

  if (count_chr(untruncated) > max_width) {
    return(paste0(substr(untruncated, 1, max_width - 3), "..."))
  }

  untruncated
}

parse_400_error <- function(res) {
  response_content <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = FALSE
  )
  response_detail <- response_content$detail
  response_detail <- unlist(response_detail)
  error_message <- c(
    paste0(
      "Received status code ",
      res$status_code,
      " with the following details:"
    ),
    purrr::set_names(response_detail, "x")
  )
  return(error_message)
}

add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
}

copy_ipums_extract <- function(extract) {
  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract
}

api_base_url <- function() {
  api_instance <- active_api_instance()

  if (api_instance == "") {
    url <- "https://api.ipums.org/"
  } else {
    url <- paste0("https://", api_instance, ".api.ipums.org/")
  }

  url
}

#' Get the active API instance to use for API requests
#'
#' @description
#' Edit the IPUMS_API_INSTANCE environment variable to test in-development
#' API functionality that has not yet been deployed to live. Development
#' features will typically be present on the `"demo"` instance.
#'
#' Note that non-live instances require their own unique API keys.
#'
#' @details
#' Set the IPUMS_API_INSTANCE envvar to `"demo"` to test in-development API
#' work.
#'
#' Note that `"internal"` is an accepted instance, but may have a different URL
#' construction and therefore may still produce invalid requests.
#'
#' Any invalid IPUMS_API_INSTANCE values are coerced to the standard live URL.
#'
#' Also note that ipumsr tests assume that the live API is active, not demo.
#'
#' @noRd
active_api_instance <- function() {
  api_instance <- Sys.getenv("IPUMS_API_INSTANCE")

  if (!api_instance %in% c("", "demo", "internal")) {
    api_instance <- ""
  }

  api_instance
}

#' Get the current API verison for a specified IPUMS collection
#'
#' Get current API version for a collection or throw an error if that collection
#' is not currently supported by API.
#'
#' @param collection IPUMS collection
#'
#' @noRd
ipums_api_version <- function() {
  api_version <- Sys.getenv("IPUMS_API_VERSION")

  if (api_version == "") {
    2
  } else {
    api_version
  }
}

check_api_support <- function(collection) {
  versions <- dplyr::filter(ipums_data_collections(), .data$api_support)

  if (!collection %in% versions$code_for_api) {
    rlang::abort(
      c(
        paste0("No API version found for collection \"", collection, "\""),
        "i" = paste0(
          "The IPUMS API currently supports the following collections: \"",
          paste0(versions$code_for_api, collapse = "\", \""), "\""
        )
      )
    )
  }

  invisible(collection)
}

#' Get API version from a JSON file
#'
#' @noRd
api_version_from_json <- function(extract_json) {
  extract <- jsonlite::fromJSON(
    extract_json,
    simplifyVector = FALSE
  )
  # Handles inconsistency between old API version JSON defs, which include
  # api_version, not version
  extract$version %||% extract$api_version
}

EMPTY_NAMED_LIST <- purrr::set_names(list(), character(0))

#' Convert geog extent values from names to codes and vice versa
#'
#' Allows users to specify geographic extents (currently states) by providing
#' full names, abbreviations, or NHGIS codes. These are converted to codes
#' before submitting to the API and converted to abbreviations for use in R.
#'
#' @param values Geog extent values to match to their associated abbreviations
#' @param lookup_key Named vector whose names match `values` and whose values
#'   represent the associated name to convert those values to
#'
#' @return A vector of length `values` with the recoded values
#'
#' @noRd
geog_extent_lookup <- function(values, lookup_key) {
  values_lower <- tolower(values)

  if (length(values_lower) == 0) {
    return(NULL)
  }

  recoded <- toupper(dplyr::recode(values_lower, !!!lookup_key))

  recoded
}

resubmission_hint <- function(is_extract) {
  if (!is_extract) {
    hint <- paste0(
      "Use `get_extract_info()` and `submit_extract()` to resubmit this ",
      "extract definition as a new extract request."
    )
  } else {
    hint <- paste0(
      "Use `submit_extract()` to resubmit this extract definition ",
      "as a new extract request."
    )
  }

  hint
}

# Helper to convert metadata camelCase names to snake_case
# for consistency with ipums_extract object naming.
to_snake_case <- function(x) {
  x <- tolower(gsub("([A-Z])","\\_\\1", x))
  x <- gsub("_{2,}", "_", x)
  gsub("^_", "", x)
}

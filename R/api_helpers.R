# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Exported functions -----------------------------------------------------------

#' List IPUMS data collections
#'
#' @description
#' List IPUMS data collections with their corresponding codes used by the
#' IPUMS API. Note that some data collections do not yet have API support.
#'
#' Currently, ipumsr supports extract definitions for the following collections:
#'
#' * IPUMS USA (`"usa"`)
#' * IPUMS CPS (`"cps"`)
#' * IPUMS International (`"ipumsi"`)
#' * IPUMS NHGIS (`"nhgis"`)
#'
#' Learn more about the IPUMS API in `vignette("ipums-api")`.
#'
#' @return A [`tibble`][tibble::tbl_df-class] with four columns containing the
#'   full collection name, the type of data the collection provides,
#'   the collection code used by the IPUMS API, and the
#'   status of API support for the collection.
#'
#' @export
#'
#' @examples
#' ipums_data_collections()
ipums_data_collections <- function() {
  purrr::map_dfr(
    proj_config(),
    ~ tibble::tibble(
      collection_name = .x$proj_name,
      collection_type = .x$collection_type,
      code_for_api = .x$code_for_api,
      api_support = .x$api_support
    )
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
#'   by the IPUMS API.
#'
#'   For a list of codes used to refer to each collection, see
#'   [ipums_data_collections()].
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
#' get_extract_history()
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

# Extract IDs ----------------

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
    .data$api_support
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

# Environment variables -----------------

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

# Print methods --------------

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
print.micro_extract <- function(x, ...) {
  styler <- extract_field_styler("bold")

  samps_to_cat <- purrr::compact(
    purrr::map(
      x$samples,
      function(x) {
        if (inherits(x, "samp_spec")) {
          x$name
        }
      }
    )
  )

  vars_to_cat <- purrr::compact(
    purrr::map(
      x$variables,
      function(x) {
        if (inherits(x, "var_spec")) {
          x$name
        }
      }
    )
  )

  to_cat <- paste0(
    ifelse(x$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(x$collection),
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""),
    "\n",
    print_truncated_vector(x$description, "Description: ", FALSE),
    "\n\n",
    print_truncated_vector(
      samps_to_cat,
      styler("Samples: ")
    ),
    "\n",
    print_truncated_vector(
      vars_to_cat,
      styler("Variables: ")
    ),
    "\n"
  )

  cat(to_cat)

  invisible(x)
}

#' @export
print.nhgis_extract <- function(x, ...) {
  style_ds <- extract_field_styler(nhgis_print_color("dataset"), "bold")
  ds_to_cat <- purrr::compact(purrr::map(
    x$datasets,
    function(d) {
      if (inherits(d, "ds_spec")) {
        format_field_for_printing(
          parent_field = list("Dataset: " = d$name),
          subfields = list(
            "Tables: " = d$data_tables,
            "Geog Levels: " = d$geog_levels,
            "Years: " = d$years,
            "Breakdowns: " = d$breakdown_values
          ),
          parent_style = style_ds,
          subfield_style = extract_field_styler("bold")
        )
      }
    }
  ))

  if (length(ds_to_cat) > 0) {
    ds_to_cat <- c(
      ds_to_cat,
      format_field_for_printing(
        parent_field = list("Geographic extents: " = x$geographic_extents),
        parent_style = style_ds
      )
    )
  }

  tst_to_cat <- purrr::compact(purrr::map(
    x$time_series_tables,
    function(t) {
      if (inherits(t, "tst_spec")) {
        format_field_for_printing(
          parent_field = list("Time Series Table: " = t$name),
          subfields = list(
            "Geog Levels: " = t$geog_levels,
            "Years: " = t$years
          ),
          parent_style = extract_field_styler(
            nhgis_print_color("time_series_table"),
            "bold"
          ),
          subfield_style = extract_field_styler("bold")
        )
      }
    }
  ))

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
    " extract ", ifelse(x$submitted, paste0("number ", x$number), ""), "\n",
    print_truncated_vector(x$description, "Description: ", FALSE),
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
                                      padding_top = 2) {
  stopifnot(length(parent_field) == 1)

  parent_val <- parent_field[[1]]
  parent_name <- names(parent_field)

  if (is.null(parent_val)) {
    return(NULL)
  }

  style_field <- parent_style %||% extract_field_styler("reset")
  style_subfield <- subfield_style %||% extract_field_styler("reset")

  output <- paste0(
    paste0(rep("\n", padding_top), collapse = ""),
    print_truncated_vector(parent_val, style_field(parent_name), FALSE)
  )

  if (!is.null(subfields)) {
    purrr::map(
      names(subfields),
      ~ if (!is.null(subfields[[.x]])) {
        output <<- paste0(
          output, "\n  ",
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

# Request handlers --------------------

# Helper to extract API-provided details for request errors.
# Will error when handling a response with empty body, so best to wrap
# in
parse_response_error <- function(res) {
  response_content <- jsonlite::fromJSON(
    # Avoid default encoding messages
    suppressMessages(httr::content(res, "text")),
    simplifyVector = FALSE
  )
  response_detail <- response_content$detail
  unlist(response_detail)
}

# Helper to handle errors and warnings for API requests.
# Called in ipums_api_request()
validate_api_request <- function(res, call = caller_env()) {
  is_downloads_request <- fostr_detect(res$url, "downloads")
  is_extract_request <- !is_downloads_request &&
    fostr_detect(res$url, "extracts/")

  status <- httr::status_code(res)

  if (httr::http_status(res)$category != "Success") {
    # Attempt to get details from response. NULL if response has empty body
    error_details <- tryCatch(
      purrr::set_names(parse_response_error(res), "x"),
      error = function(cnd) {
        NULL
      }
    )

    # Standard error message
    error_message <- paste0("API request failed with status ", status, ".")

    # Authorization errors could be related to invalid registration or key
    if (status %in% c(401, 403)) {
      # The API provides details for invalid registration cases, so check
      # if any error details exist.
      if (length(error_details) > 0) {
        rlang::abort(
          c("Invalid IPUMS registration.", error_details),
          call = call
        )
      }

      # Otherwise we should be dealing with a valid registration but invalid key
      rlang::abort(
        c(
          "The provided API key is either missing or invalid.",
          "i" = paste0(
            "Please provide your API key to the `api_key` argument ",
            "or request a key at https://account.ipums.org/api_keys"
          ),
          "i" = "Use `set_ipums_api_key()` to save your key for future use."
        ),
        call = call
      )
    }

    # If a downloads request, add hint to inform of possible issue
    if (is_downloads_request) {
      rlang::abort(
        c(
          error_message,
          "i" = paste0(
            "The extract may have expired. Check its status ",
            "with `get_extract_info()`"
          )
        ),
        call = call
      )
    }

    # 404 extract request with no details is an invalid extract number error
    if (status == 404 && is_extract_request && is_null(error_details)) {
      url_parts <- fostr_split(res$url, "/")[[1]]
      url_tail <- url_parts[length(url_parts)]
      number <- as.numeric(fostr_split(url_tail, "\\?")[[1]][[1]])

      rlang::abort(
        c(
          error_message,
          "x" = paste0(
            "Extract number ", number, " does not exist for this collection."
          )
        ),
        call = call
      )
    }

    if (status == 429) {
      rlang::abort(
        c(error_message, "x" = "Rate limit exceeded."),
        call = call
      )
    }

    # Other errors should get the general message
    rlang::abort(
      c(error_message, error_details),
      call = call
    )
  }

  # Download requests do not return JSON by design
  if (!is_downloads_request && httr::http_type(res) != "application/json") {
    rlang::abort("API request did not return JSON", call = call)
  }

  invisible(res)
}

api_extract_warnings <- function(extract_number, warnings) {
  warnings <- unlist(warnings)

  if (length(warnings) > 0) {
    rlang::warn(c(
      paste0(
        "Extract number ", extract_number, " contains unsupported features:"
      ),
      warnings
    ))
  }
}

add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
}

api_base_url <- function() {
  api_instance <- active_api_instance()

  if (api_instance == "" || api_instance == "internal") {
    url <- "https://api.ipums.org/"
  } else {
    url <- paste0("https://", api_instance, ".api.ipums.org/")
  }

  url
}

#' Generate an URL for IPUMS API requests
#'
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url. Helpers
#'   `extract_request_path()` and `metadata_request_path()` can be used
#'   to form URL paths for different endpoints.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to [httr::modify_url].
#'
#' @return A character containing the URL for a request to an IPUMS API extract
#'   endpoint.
#'
#' @noRd
api_request_url <- function(collection, path, queries = NULL) {
  check_api_support(collection)

  queries_is_null_or_named_list <- is.null(queries) ||
    is.list(queries) && is_named(queries)

  if (!queries_is_null_or_named_list) {
    rlang::abort("`queries` argument must be `NULL` or a named list")
  }

  api_url <- httr::modify_url(
    api_base_url(),
    path = path,
    query = c(
      list(collection = collection, version = ipums_api_version()),
      queries
    )
  )

  api_url
}

#' Helper to construct URL paths for API extract endpoints
#'
#' @param number Number of the extract to include in the path. Used for
#'   endpoints that request information for a single extract. If `NULL`, only
#'   the base extract path is returned.
#'
#' @return Path to include in the URL for an API extract request.
#'   This will be of the form `"extracts/{number}"` if `number` is provided.
#'   Otherwise, it will return `"extracts/"`.
#'
#' @noRd
extract_request_path <- function(number = NULL) {
  if (!rlang::is_null(number)) {
    number <- format(number, scientific = FALSE)
  }

  if (active_api_instance() == "internal") {
    path <- paste0("internal-extracts/", number)
  } else {
    path <- paste0("extracts/", number)
  }

  path
}

#' Helper to construct URL paths for API metadata endpoints
#'
#' @param collection Collection associated with the metadata endpoint
#' @param path List of elements to add to the URL path. If list is named,
#'   each element name is placed ahead of its associated value in the
#'   resulting URL. Elements with `NULL` values are fully removed, allowing
#'   for the specification of multiple possible path parameters that may
#'   or may not be present. This is useful as some metadata endpoints include
#'   multiple parameters (e.g. `data_tables` and `datasets` for NHGIS).
#'
#'   For instance, `list(datasets = "DS", data_tables = NULL)` would produce
#'   a path `"metadata/{collection}/datasets/DS/"`.
#'
#'   Unnamed elements are inserted in the order that they are provided.
#'
#' @return Path to include in the URL for an API metadata request.
#'
#' @noRd
metadata_request_path <- function(collection, ...) {
  path_args <- purrr::compact(rlang::list2(...))
  path_fields <- names(path_args)

  if (active_api_instance() == "internal") {
    metadata_path <- "internal-metadata"
  } else {
    metadata_path <- "metadata"
  }

  path_args <- c(
    metadata_path,
    collection,
    rbind(path_fields, unlist(path_args))
  )

  # Avoids extra `/` for unnamed args in `path`
  path_args <- path_args[which(path_args != "")]

  paste(path_args, collapse = "/")
}

#' Low-level function to make basic requests to the IPUMS API.
#'
#' A basic wrapper of `httr::VERB()` to add ipumsr user agent and API key
#' authorization and produce desired error messages on invalid responses.
#' This is wrapped in higher-level functions to attach endpoint-specific
#' behavior.
#'
#' @param verb `"GET"` or `"POST"`
#' @param url API url for the request.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Use `FALSE` for a body-less request.
#' @param api_key IPUMS API key
#' @param ... Additional parameters passed to `httr::VERB()`
#'
#' @return An `httr::response()` object
#'
#' @noRd
ipums_api_request <- function(verb,
                              url,
                              body = FALSE,
                              api_key = Sys.getenv("IPUMS_API_KEY"),
                              ...) {
  response <- httr::VERB(
    verb = verb,
    url = url,
    body = body,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(utils::packageVersion("ipumsr"))
      )
    ),
    add_user_auth_header(api_key),
    ...
  )

  validate_api_request(response)

  response
}

#' Make requests to paginated API endpoints
#'
#' For a starting URL, makes an API request and continue requesting additional
#' pages until the provided `max_pages` is reached. Use `max_pages = Inf` to
#' request all pages available. Pages are stored in the `links$nextPage` field
#' of the JSON response.
#'
#' @param url API url for the request
#' @param delay Number of seconds to delay between
#'   successive API requests. This is highly unlikely to be needed and is
#'   included only as a fallback for users to avoid hitting the rate limit.
#' @param api_key IPUMS API key
#' @param ... Additional arguments passed to `httr::VERB()`
#'
#' @return A list of `httr::response()` objects
#'
#' @noRd
ipums_api_paged_request <- function(url,
                                    max_pages = Inf,
                                    delay = 0,
                                    api_key = Sys.getenv("IPUMS_API_KEY"),
                                    ...) {
  response <- ipums_api_request(
    "GET",
    url = url,
    body = FALSE,
    api_key = api_key,
    ...
  )

  json_content <- jsonlite::fromJSON(
    httr::content(response, "text"),
    simplifyVector = FALSE
  )

  all_responses <- list(response)
  page_no <- 1

  while (page_no < max_pages && !is.null(json_content$links$nextPage)) {
    # Fallback in case someone hits the rate limit.
    Sys.sleep(delay)

    response <- ipums_api_request(
      "GET",
      url = json_content$links$nextPage,
      body = FALSE,
      api_key = api_key,
      ...
    )

    json_content <- jsonlite::fromJSON(
      httr::content(response, "text"),
      simplifyVector = FALSE
    )

    all_responses <- c(all_responses, list(response))
    page_no <- page_no + 1
  }

  all_responses
}

#' Convenience function to create an `ipums_json` from an API extract request.
#'
#' @noRd
ipums_api_extracts_request <- function(verb,
                                       collection,
                                       url,
                                       body = FALSE,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {
  response <- ipums_api_request(
    verb = verb,
    url = url,
    body = body,
    api_key = api_key,
    httr::content_type_json()
  )

  new_ipums_json(
    httr::content(response, "text"),
    collection = collection
  )
}

#' Writes the given url to file_path. Returns the file path of the
#' downloaded data.
#'
#' @noRd
ipums_api_download_request <- function(url,
                                       file_path,
                                       overwrite,
                                       progress = TRUE,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {
  if (file.exists(file_path) && !overwrite) {
    rlang::abort(
      c(
        paste0("File `", file_path, "` already exists."),
        "To overwrite, set `overwrite = TRUE`"
      )
    )
  }

  if (progress) {
    progress <- httr::progress()
  } else {
    progress <- NULL
  }

  ipums_api_request(
    "GET",
    url = url,
    body = FALSE,
    api_key = api_key,
    httr::write_disk(file_path, overwrite = TRUE),
    progress
  )

  file_path
}

# Helper to set the page size limit for each
# endpoint type. Currently, extract endpoints have a different
# limits as it is more expensive to provide large numbers of extract
# definitions than metadata records.
api_page_size_limit <- function(type) {
  if (type == "extracts") {
    limit <- 1500
  } else if (type == "metadata") {
    limit <- 2500
  } else {
    rlang::abort("Unrecognized endpoint type.")
  }

  limit
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
#' Get current API version for a collection.
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

# `spec` helpers -------------

#' Cast a vector or list to a list of `ipums_spec` objects
#'
#' This function is used to power the use of character strings as variable
#' names in extract definition functions. It is also used in extract revisions
#' to ensure a standardized input when interpreting revision arguments.
#'
#' @param x Vector or list of objects to cast to a list of `ipums_spec` objects.
#'   Vector elements are interpreted as names for the resulting `ipums_spec`
#'   objects. Objects in `x` that are already `ipums_spec` objects are
#'   unchanged.
#' @param class Character indicating the subclass (e.g. `"var_spec"`) of the
#'   output list elements. Elements that are not modified retain their original
#'   class.
#'
#' @return list of `ipums_spec` objects
#'
#' @noRd
spec_cast <- function(x, class) {
  if (inherits(x, "ipums_spec")) {
    x <- list(x)
  } else {
    # Named entries should be from combination of spec objects
    # with character strings in a vector, which is not allowed.
    to_cast <- purrr::map_lgl(x, ~ !inherits(.x, "ipums_spec"))

    if (any(rlang::have_name(to_cast) & to_cast)) {
      rlang::warn(c(
        paste0("Unexpected names in input when converting to `", class, "`"),
        paste0(
          "You may have combined `",
          class, "` objects with `c()` instead of `list()`"
        )
      ))
    }

    if (any(to_cast)) {
      x[to_cast] <- purrr::map(
        x[to_cast],
        ~ new_ipums_spec(.x, class = class)
      )
    }
  }

  x
}

#' Compare extract specifications and remove values
#'
#' This powers the logic in `remove_from_extract()` by comparing extract
#' definition specifications and specifications provided as arguments to
#' that function.
#'
#' The entries in `extract_spec` are matched to those in `spec` by name, and
#' values for detailed specifications are removed where they match.
#'
#' @param extract_spec List of `ipums_spec` objects containing the
#'   specifications contained in an extract definition.
#' @param spec List of `ipums_spec` objects containing the values to remove
#'   from those in `extract_spec`
#'
#' @return A list of `ipums_spec` objects
#'
#' @noRd
spec_remove <- function(extract_spec, spec) {
  spec_names <- purrr::map_chr(spec, ~ .x$name)

  extract_spec <- purrr::compact(
    purrr::map(
      extract_spec,
      function(x) {
        if (x$name %in% spec_names) {
          i <- which(spec_names == x$name)
          spec_setdiff(x, spec[[i]])
        } else {
          x
        }
      }
    )
  )

  if (length(extract_spec) == 0) {
    extract_spec <- NULL
  }

  extract_spec
}

#' Compare extract specifications and add values
#'
#' This powers the logic in `add_to_extract()` by comparing extract
#' definition specifications and specifications provided as arguments to
#' that function.
#'
#' The entries in `extract_spec` are matched to those in `spec` by name, and
#' values for detailed specifications are added where they do not match.
#'
#' @param extract_spec List of `ipums_spec` objects containing the
#'   specifications contained in an extract definition.
#' @param spec List of `ipums_spec` objects containing the values to add
#'   to those in `extract_spec`
#'
#' @return A list of `ipums_spec` objects
#'
#' @noRd
spec_add <- function(extract_spec, spec) {
  spec_names <- purrr::map_chr(spec, ~ .x$name)

  if (!rlang::is_null(extract_spec)) {
    extract_spec_names <- purrr::map_chr(extract_spec, ~ .x$name)
    new_spec <- spec[which(!spec_names %in% extract_spec_names)]

    extract_spec <- purrr::compact(
      purrr::map(
        extract_spec,
        function(x) {
          if (x$name %in% spec_names) {
            i <- which(spec_names == x$name)
            spec_union(x, spec[[i]])
          } else {
            x
          }
        }
      )
    )

    extract_spec <- c(extract_spec, new_spec)
  } else {
    extract_spec <- spec
  }

  extract_spec
}

# Helper to drive the logic comparing two spec objects and removing
# the detailed specification values present in both from the first spec object.
spec_setdiff <- function(spec, spec_mod, validate = FALSE) {
  UseMethod("spec_setdiff")
}

#' @export
spec_setdiff.ipums_spec <- function(spec, spec_mod, validate = FALSE) {
  if (spec$name != spec_mod$name) {
    return(spec)
  }

  args <- setdiff(names(spec_mod), "name")

  if (length(args) == 0) {
    return(NULL)
  }

  purrr::walk(
    args,
    function(x) {
      spec[[x]] <<- setdiff_null(spec[[x]], spec_mod[[x]])
    }
  )

  if (validate) {
    spec <- validate_ipums_extract(spec)
  }

  spec
}

#' @export
spec_setdiff.var_spec <- function(spec, spec_mod, validate = FALSE) {
  if (spec$name != spec_mod$name) {
    return(spec)
  }

  args <- setdiff(names(spec_mod), "name")

  cs_type <- spec$case_selection_type

  if (length(args) == 0) {
    return(NULL)
  }

  purrr::walk(
    args,
    function(x) {
      spec[[x]] <<- setdiff_null(spec[[x]], spec_mod[[x]])
    }
  )

  # `case_selections` may have been modified, but we want to ensure that
  # `case_selection_type` is retained if needed and removed if all case
  # selections are removed.
  if (!is_null(spec$case_selections)) {
    spec$case_selection_type <- cs_type
  } else {
    spec$case_selection_type <- NULL
  }

  if (validate) {
    spec <- validate_ipums_extract(spec)
  }

  spec
}

# Helper to drive the logic comparing two spec objects and unioning
# the detailed specification values present in the second with the first.
spec_union <- function(spec, spec_mod, validate = FALSE) {
  UseMethod("spec_union")
}

#' @export
spec_union.ipums_spec <- function(spec, spec_mod, validate = FALSE) {
  if (spec$name != spec_mod$name) {
    return(spec)
  }

  args <- setdiff(names(spec_mod), "name")

  purrr::walk(
    args,
    function(x) {
      spec[[x]] <<- union(spec[[x]], spec_mod[[x]])
    }
  )

  if (validate) {
    spec <- validate_ipums_extract(spec)
  }

  spec
}

#' @export
spec_union.var_spec <- function(spec, spec_mod, validate = FALSE) {
  if (spec$name != spec_mod$name) {
    return(spec)
  }

  args <- setdiff(names(spec_mod), "name")

  purrr::walk(
    args,
    function(x) {
      if (x == "case_selection_type") {
        spec[[x]] <<- spec_mod[[x]] %||% spec[[x]]
      } else {
        spec[[x]] <<- union(spec[[x]], spec_mod[[x]])
      }
    }
  )

  if (validate) {
    spec <- validate_ipums_extract(spec)
  }

  spec
}

# Misc ------------------

copy_ipums_extract <- function(extract) {
  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract
}

check_api_support <- function(collection) {
  versions <- dplyr::filter(ipums_data_collections(), .data$api_support)

  if (!collection %in% versions$code_for_api) {
    rlang::abort(
      c(
        paste0(
          "Unrecognized collection: \"", collection, "\""
        ),
        "i" = paste0(
          "The IPUMS API supports collections: \"",
          paste0(versions$code_for_api, collapse = "\", \""), "\""
        )
      )
    )
  }

  invisible(collection)
}

# Determine if a collection is considered microdata or aggregate data
collection_type <- function(collection) {
  if (is_empty(collection) || is_na(collection)) {
    return("ipums")
  }

  collections <- ipums_data_collections()
  type <- collections[collections$code_for_api == collection, ]$collection_type

  if (type == "microdata") {
    collection_type <- "micro"
  } else {
    collection_type <- "agg"
  }

  collection_type
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
  x <- tolower(gsub("([A-Z])", "\\_\\1", x))
  x <- gsub("_{2,}", "_", x)
  gsub("^_", "", x)
}

to_camel_case <- function(x) {
  gsub("\\_(\\w?)", "\\U\\1", x, perl = TRUE)
}

# Helper to check whether a given URL exists in an extract's download links
has_url <- function(links, name) {
  is.list(links[[name]]) && is.character(links[[name]][["url"]])
}

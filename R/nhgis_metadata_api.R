
# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# All metadata in one function
get_nhgis_metadata <- function(type = NULL,
                               dataset = NULL,
                               data_table = NULL,
                               time_series_table = NULL,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

  if (all(is.null(type),
          is.null(dataset),
          is.null(time_series_table))) {
    stop(
      "At least one of `type`, `dataset`, or `time_series_table`",
      " must be specified",
      call. = FALSE
    )
  }

  if (!is.null(type)) {
    metadata <- get_nhgis_metadata_summary(type, api_key)
  }

  if (!is.null(dataset) && is.null(data_table)) {
    metadata <- get_dataset_metadata(dataset, api_key)
  }

  if (!is.null(dataset) && !is.null(data_table)) {
    metadata <- get_table_metadata(dataset, data_table, api_key)
  }

  if (!is.null(time_series_table)) {
    metadata <- get_tst_metadata(time_series_table, api_key)
  }

  metadata

}

## Metadata for a single dataset
get_dataset_metadata <- function(dataset = NULL,
                                 api_key = Sys.getenv("IPUMS_API_KEY")) {

  stopifnot(length(dataset) <= 1)

  if (is.null(dataset)) {

    metadata <- get_nhgis_metadata_summary("datasets", api_key)

  } else {

    url <- httr::modify_url(
      url = paste(
        nhgis_api_metadata_url(),
        "datasets",
        dataset,
        sep = "/"
      ),
      query = paste0(
        "version=",
        ipums_api_version("nhgis")
      )
    )

    res <- httr::GET(
      url = url,
      httr::user_agent(
        paste0(
          "https://github.com/ipums/ipumsr ",
          as.character(packageVersion("ipumsr"))
        )
      ),
      httr::content_type_json(),
      add_user_auth_header(api_key)
    )

    metadata <- jsonlite::fromJSON(
      httr::content(res, "text"),
      simplifyVector = TRUE
    )

    if (httr::http_error(res)) {
      stop(
        "Extract submission failed for ", url, ".\n",
        "Status: ", metadata$status$code, "\n",
        "Details: ", metadata$detail,
        call. = FALSE
      )
    }

    metadata$data_tables <- tibble::as_tibble(metadata$data_tables)
    metadata$geog_levels <- tibble::as_tibble(metadata$geog_levels)
    metadata$geographic_instances <- tibble::as_tibble(metadata$geographic_instances)
    metadata$breakdowns <- tibble::as_tibble(metadata$breakdowns)

  }

  metadata

}

# Metadata for particular data_table
get_table_metadata <- function(dataset,
                               data_table = NULL,
                               api_key = Sys.getenv("IPUMS_API_KEY")) {

  stopifnot(length(dataset) <= 1 && length(data_table) <= 1)

  if (is.null(data_table)) {

    metadata <- get_dataset_metadata(dataset, api_key)$data_tables

  } else {

    url <- httr::modify_url(
      url = paste(
        nhgis_api_metadata_url(),
        "datasets",
        dataset,
        "data_tables",
        data_table,
        sep = "/"
      ),
      query = paste0(
        "version=",
        ipums_api_version("nhgis")
      )
    )

    res <- httr::GET(
      url = url,
      httr::user_agent(
        paste0(
          "https://github.com/ipums/ipumsr ",
          as.character(packageVersion("ipumsr"))
        )
      ),
      httr::content_type_json(),
      add_user_auth_header(api_key)
    )

    metadata <- jsonlite::fromJSON(
      httr::content(res, "text"),
      simplifyVector = TRUE
    )

    if (httr::http_error(res)) {
      stop(
        "Extract submission failed for ", url, ".\n",
        "Status: ", metadata$status$code, "\n",
        "Details: ", metadata$detail,
        call. = FALSE
      )
    }

    metadata$variables <- tibble::as_tibble(metadata$variables)

  }

  metadata

}


## Metadata for a single time series table
## More natural to make this give you all TSTs if time_series_table is NULL?
get_tst_metadata <- function(time_series_table = NULL,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {


  stopifnot(length(time_series_table) <= 1)

  if (is.null(time_series_table)) {

    metadata <- get_nhgis_metadata_summary("time_series_tables", api_key)

  } else {

    url <- httr::modify_url(
      url = paste(
        nhgis_api_metadata_url(),
        "time_series_tables",
        time_series_table,
        sep = "/"
      ),
      query = paste0(
        "version=",
        ipums_api_version("nhgis")
      )
    )

    res <- httr::GET(
      url = url,
      httr::user_agent(
        paste0(
          "https://github.com/ipums/ipumsr ",
          as.character(packageVersion("ipumsr"))
        )
      ),
      httr::content_type_json(),
      add_user_auth_header(api_key)
    )

    metadata <- jsonlite::fromJSON(
      httr::content(res, "text"),
      simplifyVector = TRUE
    )

    if (httr::http_error(res)) {
      stop(
        "Extract submission failed for ", url, ".\n",
        "Status: ", metadata$status$code, "\n",
        "Details: ", metadata$detail,
        call. = FALSE
      )
    }

    metadata$time_series <- tibble::as_tibble(metadata$time_series)
    metadata$years <- tibble::as_tibble(metadata$years)
    metadata$geog_levels <- tibble::as_tibble(metadata$geog_levels)

  }

  metadata

}

## Metadata for shapefiles
# Currently allow for shapefile argument for format consistency with other
# functions, even though it just does a simple filter...
get_shp_metadata <- function(shapefile = NULL,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  metadata <- get_nhgis_metadata_summary("shapefiles", api_key)

  if (!is.null(shapefile)) {
    metadata <- metadata[which(metadata$name == shapefile),]
  }

  tibble::as_tibble(metadata)

}

## Metadata for all datasets, time series tables, or shapefiles
get_nhgis_metadata_summary <- function(type,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {

  if (!type %in% c("datasets", "time_series_tables", "shapefiles")) {
    stop(
      "`type` must be one of `datasets`, `time_series_tables`, or ",
      "`shapefiles`.",
      call. = FALSE
    )
  }

  url <- httr::modify_url(
    url = paste(
      nhgis_api_metadata_url(),
      type,
      sep = "/"
    ),
    query = paste0(
      "version=",
      ipums_api_version("nhgis")
    )
  )

  res <- httr::GET(
    url = url,
    httr::user_agent(
      paste0(
        "https://github.com/ipums/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  # Needs improved error handling. Not sure which errors metadata API throws.
  if(httr::http_error(res)) {
    stop(
      "Extract submission failed for ", url, ".\n",
      "Status: ", httr::status_code(res), "\n",
      call. = FALSE
    )
  }

  metadata <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = TRUE
  )

  tibble::tibble(metadata)

}

nhgis_api_metadata_url <- function() {
  "https://api.ipums.org/metadata/nhgis"
}

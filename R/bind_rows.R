# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Bind multiple data frames by row, preserving labelled attributes
#'
#' Analogous to [`dplyr::bind_rows()`][dplyr::bind_rows], but preserves the
#' labelled attributes provided with IPUMS data.
#'
#' @inheritParams dplyr::bind_rows
#' @param ... Data frames or [`tibbles`][tibble::tbl_df-class] to combine.
#'   Each argument can be a data frame or a
#'   list of data frames. When binding, columns are matched by name. Missing
#'   columns will be filled with `NA`.
#'
#' @return Returns the same type as the first input. Either a `data.frame`,
#'   `tbl_df`, or `grouped_df`
#'
#' @export
#'
#' @examples
#' file <- ipums_example("nhgis0712_csv.zip")
#'
#' d1 <- read_nhgis(
#'   file,
#'   file_select = 1,
#'   verbose = FALSE
#' )
#'
#' d2 <- read_nhgis(
#'   file,
#'   file_select = 2,
#'   verbose = FALSE
#' )
#'
#' # Variables have associated label attributes:
#' ipums_var_label(d1$PMSAA)
#'
#' # Preserve labels when binding data sources:
#' d <- ipums_bind_rows(d1, d2)
#' ipums_var_label(d$PMSAA)
#'
#' # dplyr `bind_rows()` drops labels:
#' d <- dplyr::bind_rows(d1, d2)
#' ipums_var_label(d$PMSAA)
ipums_bind_rows <- function(..., .id = NULL) {
  # TODO: Rewrite in C++?
  # Definitely not exactly the same logic as dplyr, but should cover most cases
  d_list <- purrr::list_flatten(list(...))

  if (!all(purrr::map_lgl(d_list, is.data.frame))) {
    rlang::abort(
      paste0(
        "Each argument to `ipums_bind_rows()` must be a data.frame or a ",
        "list of data.frames."
      )
    )
  }

  unique_var_names <- unique(purrr::flatten_chr(purrr::map(d_list, names)))

  attrs_by_var <- purrr::map(
    unique_var_names,
    function(vvv) {
      all_attrs <- purrr::map(
        purrr::keep(d_list, ~ vvv %in% names(.)),
        ~ attributes(.[[vvv]])
      )

      if (length(all_attrs) == 1) {
        return(all_attrs[[1]])
      }

      first_attrs <- all_attrs[[1]]
      all_equal <- purrr::map_lgl(all_attrs[-1], ~ identical(., first_attrs))

      if (all(all_equal)) {
        return(first_attrs)
      } else {
        return(FALSE)
      }
    }
  )

  names(attrs_by_var) <- unique_var_names

  # Remove all IPUMS attributes (we will put back the matching ones after
  # row-binding)
  d_list <- purrr::map(d_list, zap_ipums_attributes)

  vars_w_incompat_attrs <- unique_var_names[
    purrr::map_lgl(attrs_by_var, ~ is_false(.))
  ]

  if (length(vars_w_incompat_attrs) > 0) {
    rlang::warn(c(
      paste0(
        "IPUMS attributes have been removed from the following columns, which ",
        "had incompatible attributes across the data frames to be combined: "
      ),
      paste0(paste0("`", vars_w_incompat_attrs, "`"), collapse = ", ")
    ))
  }

  out <- dplyr::bind_rows(d_list, .id = .id)

  # Reassign attributes for columns where all attributes matched
  purrr::iwalk(
    purrr::keep(attrs_by_var, ~ !is_false(.)),
    function(attr, vname) {
      attributes(out[[vname]]) <<- attr
    }
  )

  out
}

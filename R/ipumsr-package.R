# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

# Imports ------------------

#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom utils packageVersion
#' @importFrom utils tail
#' @importFrom dplyr %>%
#' @import rlang
"_PACKAGE"

# Re-exports ---------------

# --- Haven ----

# Import and reexport helpful label functions from haven
#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @importFrom haven zap_labels
#' @export
haven::zap_labels

#' @importFrom haven is.labelled
#' @export
haven::is.labelled

#' @importFrom zeallot %<-%
#' @export
zeallot::`%<-%`

# --- readr import diagnostics ---

#' @importFrom readr problems
#' @export
readr::problems

#' @importFrom readr spec
#' @export
readr::spec

# ---- tidyselect select helpers ----

#' tidyselect selection language in ipumsr
#'
#' @description
#' Slightly modified implementation of tidyselect
#' [selection language][tidyselect::language] in ipumsr.
#'
#' ## Syntax
#' In general, the selection language in ipumsr operates the same as in
#' tidyselect.
#'
#' Where applicable, variables can be selected with:
#'
#' - A character vector of variable names (`c("var1", "var2")`)
#' - A bare vector of variable names (`c(var1, var2)`)
#' - A selection helper from tidyselect (`starts_with("var")`). See below for
#'   a list of helpers.
#'
#' ## Primary differences
#' - tidyselect selection is generally intended for use with column variables
#'   in data.frame-like objects. In contrast, ipumsr allows selection language
#'   syntax in other cases as well (for instance, when selecting files
#'   from within a .zip archive). ipumsr functions will indicate whether they
#'   support the selection language.
#' - Selection with [`where()`][tidyselect::where] is not consistently
#'   supported.
#'
#' ## Selection helpers (from tidyselect)
#'
#' - `var1`:`var10`: variables lying between `var1` on the left and `var10`
#'   on the right.
#' - `starts_with("a")`: names that start with `"a"`
#' - `ends_with("z")`: names that end with `"z"`
#' - `contains("b")`: names that contain `"b"`
#' - `matches("x.y")`: names that match regular expression `x.y`
#' - `num_range(x, 1:4)`: names following the pattern `x1, x2, ..., x4`
#' - `all_of(vars)`/`any_of(vars)`: matches names stored in the character vector
#'   `vars`. `all_of(vars)` will error if the variables aren't present;
#'   `any_of(vars)` will match just the variables that exist.
#' - `everything()`: all variables
#' - `last_col()`: furthest column to the right
#'
#' Operators for combining those selections:
#'
#' - `!selection`: only variables that don't match `selection`
#' - `selection1 & selection2`: only variables included in both `selection1`
#'   and `selection2`
#' - `selection1 | selection2`: all variables that match either `selection1` or
#'   `selection2`
#'
#' @name selection_language
#'
#' @keywords internal
#'
#' @examples
#' cps_file <- ipums_example("cps_00157.xml")
#'
#' # Load 3 variables by name
#' read_ipums_micro(
#'   cps_file,
#'   vars = c("YEAR", "MONTH", "PERNUM"),
#'   verbose = FALSE
#' )
#'
#' # "Bare" variables are supported
#' read_ipums_micro(
#'   cps_file,
#'   vars = c(YEAR, MONTH, PERNUM),
#'   verbose = FALSE
#' )
#'
#' # Standard tidyselect selectors are also supported
#' read_ipums_micro(cps_file, vars = starts_with("ASEC"), verbose = FALSE)
#'
#' # Selection methods can be combined
#' read_ipums_micro(
#'   cps_file,
#'   vars = c(YEAR, MONTH, contains("INC")),
#'   verbose = FALSE
#' )
#'
#' read_ipums_micro(
#'   cps_file,
#'   vars = starts_with("S") & ends_with("P"),
#'   verbose = FALSE
#' )
#'
#' # Other selection arguments also support this syntax.
#' # For instance, load a particular file based on a tidyselect match:
#' read_nhgis(
#'   ipums_example("nhgis0731_csv.zip"),
#'   file_select = contains("nominal_state"),
#'   verbose = FALSE
#' )
NULL

#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with

#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with

#' @importFrom tidyselect contains
#' @export
tidyselect::contains

#' @importFrom tidyselect matches
#' @export
tidyselect::matches

#' @importFrom tidyselect num_range
#' @export
tidyselect::num_range

#' @importFrom tidyselect one_of
#' @export
tidyselect::one_of

#' @importFrom tidyselect all_of
#' @export
tidyselect::all_of

#' @importFrom tidyselect any_of
#' @export
tidyselect::any_of

#' @importFrom tidyselect everything
#' @export
tidyselect::everything

#' @importFrom tidyselect last_col
#' @export
tidyselect::last_col

# --- Lifecycle ---

#' @importFrom lifecycle deprecated
lifecycle::deprecated

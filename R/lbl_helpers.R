# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Convert labelled data values to NA
#'
#' Convert data values in a [`labelled`][haven::labelled()] vector
#' to `NA` based on the value labels associated with that vector. Ignores
#' values that do not have a label.
#'
#' @details
#' Several `lbl_*()` functions include arguments that can be passed a function
#' of `.val` and/or `.lbl`. These refer to the existing values and
#' labels in the input vector, respectively.
#'
#' Use `.val` to refer to the *values* in the vector's value labels.
#' Use `.lbl` to refer to the *label names* in the vector's value labels.
#'
#' Note that not all `lbl_*()` functions support both of these arguments.
#'
#' @param x A [`labelled`][haven::labelled()] vector
#' @param .predicate A function taking `.val` and `.lbl` arguments that
#'   returns `TRUE` for all values that should be converted to `NA`.
#'
#'   Can be provided as an anonymous function or formula. See Details section.
#'
#' @return A [`labelled`][haven::labelled()] vector
#'
#' @export
#'
#' @family lbl_helpers
#'
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 30, 99, 30, 10),
#'   c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
#' )
#'
#' # Convert labelled values greater than 90 to `NA`
#' lbl_na_if(x, function(.val, .lbl) .val >= 90)
#'
#' # Can use purrr-style notation
#' lbl_na_if(x, ~ .lbl %in% c("Maybe"))
#'
#' # Or refer to named function
#' na_function <- function(.val, .lbl) .val >= 90
#' lbl_na_if(x, na_function)
lbl_na_if <- function(x, .predicate) {
  pred_f <- as_lbl_function(.predicate, caller_env())

  labels <- attr(x, "labels")
  to_zap <- pred_f(.val = unname(labels), .lbl = names(labels))

  if (any(is.na(to_zap))) {
    rlang::abort(paste0(
      "Predicate function cannot return missing values in ",
      "`lbl_na_if()`."
    ))
  }

  vals_to_zap <- unname(labels[to_zap])
  new_labels <- labels[!to_zap]

  out <- x
  out[out %in% vals_to_zap] <- NA
  attr(out, "labels") <- new_labels

  out
}

#' Modify value labels for a labelled vector
#'
#' @description
#' Update the mapping between values and labels in a
#' [`labelled`][haven::labelled()] vector. These functions allow you to
#' simultaneously update data values and the existing value labels.
#' Modifying data values directly does not result in updated value labels.
#'
#' Use `lbl_relabel()` to manually specify new value/label mappings. This
#' allows for the addition of new labels.
#'
#' Use `lbl_collapse()` to collapse detailed labels into more general
#' categories. Values can be grouped together and associated with individual
#' labels that already exist in the `labelled` vector.
#'
#' Unlabelled values will be converted to `NA`.
#'
#' @inherit lbl_na_if details
#'
#' @param x A [`labelled`][haven::labelled()] vector
#' @param ... Arbitrary number of two-sided formulas.
#'
#'   The left hand side should be a label placeholder created with [lbl()] or a
#'   value that already exists in the data.
#'
#'   The right hand side should be a function taking `.val` and `.lbl`
#'   arguments that evaluates to `TRUE` for all
#'   cases that should receive the label specified on the left hand side.
#'
#'   Can be provided as an anonymous function or formula. See Details section.
#' @param .fun A function taking `.val` and `.lbl` arguments that returns
#'   the value associated with an existing label in the vector. Input values to
#'   this function will be relabeled with the label of the function's output
#'   value.
#'
#'   Can be provided as an anonymous function or formula. See Details section.
#'
#' @return A [`labelled`][haven::labelled()] vector
#'
#' @export
#'
#' @family lbl_helpers
#'
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 21, 30, 99, 30, 10),
#'   c(
#'     Yes = 10, `Yes - Logically Assigned` = 11,
#'     No = 20, Unlikely = 21, Maybe = 30, NIU = 99
#'   )
#' )
#'
#' # Convert cases with value 11 to value 10 and associate with 10's label
#' lbl_relabel(x, 10 ~ .val == 11)
#' lbl_relabel(x, lbl("Yes") ~ .val == 11)
#'
#' # To relabel using new value/label pairs, use `lbl()` to define a new pair
#' lbl_relabel(
#'   x,
#'   lbl(10, "Yes/Yes-ish") ~ .val %in% c(10, 11),
#'   lbl(90, "???") ~ .val == 99 | .lbl == "Maybe"
#' )
#'
#' # Collapse labels to create new label groups
#' lbl_collapse(x, ~ (.val %/% 10) * 10)
#'
#' # These are equivalent
#' lbl_collapse(x, ~ ifelse(.val == 10, 11, .val))
#' lbl_relabel(x, 11 ~ .val == 10)
lbl_relabel <- function(x, ...) {
  if (is.null(attr(x, "labels", exact = TRUE))) {
    rlang::abort(c(
      "`x` must be labelled.",
      "i" = "To add labels to an unlabelled vector, use `lbl_define()`."
    ))
  }
  dots <- list(...)

  transformation <- ipums_val_labels(x)
  transformation$new_val <- transformation$val
  transformation$new_lbl <- transformation$lbl
  old_labels <- attr(x, "labels")

  for (ddd in dots) {
    # Figure out which values we're changing from rhs
    ddd_rhs <- ddd
    rlang::f_lhs(ddd_rhs) <- NULL
    to_change <- as_lbl_function(ddd_rhs)(
      .val = transformation$val,
      .lbl = transformation$lbl
    )

    # Figure out which label we're changing to from lhs
    ddd_lhs <- ddd
    rlang::f_rhs(ddd_lhs) <- NULL
    lblval <- rlang::eval_tidy(rlang::as_quosure(ddd_lhs))
    lblval <- fill_in_lbl(lblval, old_labels)

    transformation$new_val[to_change] <- lblval$.val
    transformation$new_lbl[to_change] <- lblval$.lbl
  }

  new_lbls <- dplyr::distinct(
    transformation,
    val = .data$new_val,
    lbl = .data$new_lbl
  )

  lbl_count <- table(new_lbls$val)

  if (any(lbl_count > 1)) {
    dup_lbls <- new_lbls[new_lbls$val %in% names(lbl_count)[lbl_count > 1], ]
    dup_lbls <- dplyr::group_by(dup_lbls, .data$val)
    dup_lbls <- dplyr::summarize(
      dup_lbls,
      all_lbls = paste0("\"", lbl, "\"", collapse = ", ")
    )

    rlang::abort(c(
      "Values cannot have more than 1 label.",
      paste("Value", dup_lbls$val, "maps to:", dup_lbls$all_lbls)
    ))
  }

  new_lbls <- dplyr::arrange(new_lbls, .data$val)
  new_lbls <- purrr::set_names(new_lbls$val, new_lbls$lbl)

  out <- transformation$new_val[match(x, transformation$val)]
  attributes(out) <- attributes(x)
  attr(out, "labels") <- new_lbls

  out
}

#' @rdname lbl_relabel
#' @export
lbl_collapse <- function(x, .fun) {
  pred_f <- as_lbl_function(.fun, caller_env())

  old_attributes <- attributes(x)

  label_info <- tibble::tibble(
    old_val = unname(old_attributes$labels),
    old_label = names(old_attributes$labels),
    new_val = pred_f(.val = .data$old_val, .lbl = .data$old_label),
    vals_equal = .data$old_val == .data$new_val
  )

  # Arrange so that if value existed in old values it is first, otherwise
  # first old value
  label_info <- dplyr::group_by(label_info, .data$new_val)
  label_info <- dplyr::arrange(
    label_info,
    .data$new_val,
    dplyr::desc(.data$vals_equal),
    .data$old_val
  )
  label_info <- dplyr::mutate(label_info, new_label = .data$old_label[1])
  label_info <- dplyr::ungroup(label_info)

  new_labels <- dplyr::select(
    label_info,
    dplyr::one_of(c("new_label", "new_val"))
  )
  new_labels <- dplyr::distinct(new_labels)
  new_labels <- tibble::deframe(new_labels)

  new_attributes <- old_attributes
  new_attributes$labels <- new_labels

  out <- label_info$new_val[match(x, label_info$old_val)]

  attributes(out) <- new_attributes

  out
}

#' Define labels for an unlabelled vector
#'
#' Create a [`labelled`][haven::labelled] vector from an unlabelled
#' vector using [lbl_relabel()] syntax, allowing for the grouping of multiple
#' values into a single label. Values not assigned a label remain unlabelled.
#'
#' @inherit lbl_na_if details
#'
#' @param x An unlabelled vector
#' @param ... Arbitrary number of two-sided formulas.
#'
#'   The left hand side should be a label placeholder created with [lbl()].
#'
#'   The right hand side should be a function taking `.val` that
#'   evaluates to `TRUE` for all cases that should receive the label specified
#'   on the left hand side.
#'
#'   Can be provided as an anonymous function or formula. See Details section.
#'
#' @return A [`labelled`][haven::labelled()] vector
#'
#' @family lbl_helpers
#'
#' @export
#'
#' @examples
#' age <- c(10, 12, 16, 18, 20, 22, 25, 27)
#'
#' # Group age values into two label groups.
#' # Values not captured by the right hand side functions remain unlabelled
#' lbl_define(
#'   age,
#'   lbl(1, "Pre-college age") ~ .val < 18,
#'   lbl(2, "College age") ~ .val >= 18 & .val <= 22
#' )
lbl_define <- function(x, ...) {
  if (!is.null(attr(x, "labels", exact = TRUE))) {
    rlang::abort(c(
      "`x` should be unlabelled.",
      "To relabel a labelled vector, use `lbl_relabel()`."
    ))
  }

  unique_x <- sort(unique(x))
  tmp_lbls <- rep(NA_character_, length(unique_x))
  attr(x, "labels") <- purrr::set_names(unique_x, tmp_lbls)
  x <- lbl_relabel(x, ...)
  label_is_na <- is.na(names(attr(x, "labels")))

  haven::labelled(x, labels = attr(x, "labels")[!label_is_na])
}

#' Add labels for unlabelled values
#'
#' Add labels for values that don't already have them in a
#' [`labelled`][haven::labelled()] vector.
#'
#' @param x A [`labelled`][haven::labelled()] vector
#' @param ... Arbitrary number of label placeholders created with [lbl()]
#'   indicating the value/label pairs to add.
#' @param vals Vector of values to be labelled. If `NULL`, labels all unlabelled
#'   values that exist in the data.
#' @param labeller A function that takes values being added as an argument and
#'   returns the labels to associate with those values. By default, uses the
#'   values themselves after converting to character.
#'
#' @return A [`labelled`][haven::labelled()] vector
#'
#' @family lbl_helpers
#'
#' @export
#'
#' @examples
#' x <- haven::labelled(
#'   c(100, 200, 105, 990, 999, 230),
#'   c(`Unknown` = 990, NIU = 999)
#' )
#'
#' # Add new labels manually
#' lbl_add(
#'   x,
#'   lbl(100, "$100"),
#'   lbl(105, "$105"),
#'   lbl(200, "$200"),
#'   lbl(230, "$230")
#' )
#'
#' # Add labels for all unlabelled values
#' lbl_add_vals(x)
#'
#' # Update label names while adding
#' lbl_add_vals(x, labeller = ~ paste0("$", .))
#'
#' # Add labels for select values
#' lbl_add_vals(x, vals = c(100, 200))
lbl_add <- function(x, ...) {
  dots <- list(...)

  new_vals <- purrr::map_dbl(dots, ~ .$.val)
  new_vals <- new_vals[new_vals %in% attr(x, "labels")]

  if (length(new_vals) > 0) {
    rlang::abort(paste0(
      "Some values have more than 1 label: ",
      paste0(new_vals, collapse = ", ")
    ))
  }

  purrr::reduce(
    dots,
    .init = x,
    function(.x, .y) {
      old_labels <- attr(.x, "labels")

      # Figure out which label we're changing to
      lblval <- fill_in_lbl(.y, old_labels)

      # Make changes to vector
      out <- .x

      new_labels <- tibble::tibble(
        label <- c(names(old_labels), lblval$.lbl),
        value = c(unname(old_labels), lblval$.val)
      )
      new_labels <- dplyr::distinct(new_labels)
      new_labels <- dplyr::arrange(new_labels, .data$value)
      new_labels <- tibble::deframe(new_labels)

      # TODO: if unlabelled vector is passed, this does not convert to labelled
      # class. Instead try: haven::labelled(out, labels = new_labels)
      # Also consider adding label arg to pass to haven::labelled()
      attr(out, "labels") <- new_labels
      out
    }
  )
}

#' @export
#' @rdname lbl_add
lbl_add_vals <- function(x, labeller = as.character, vals = NULL) {
  old_labels <- attr(x, "labels")
  old_labels <- tibble::tibble(
    val = unname(old_labels),
    lbl = names(old_labels)
  )

  if (is.null(vals)) {
    vals <- dplyr::setdiff(unique(x), old_labels$val)
  } else {
    if (any(vals %in% unname(old_labels))) {
      rlang::abort("Some values have more than 1 label.")
    }
  }

  new_labels <- tibble::tibble(
    val = vals,
    lbl = purrr::map_chr(vals, rlang::as_function(labeller))
  )

  new_labels <- dplyr::bind_rows(old_labels, new_labels)
  new_labels <- dplyr::arrange(new_labels, .data$val)
  new_labels <- purrr::set_names(new_labels$val, new_labels$lbl)

  out <- x
  # TODO: if unlabelled vector is passed, this does not convert to labelled
  # class. See above.
  attr(out, "labels") <- new_labels
  out
}

#' Clean unused labels
#'
#' Remove labels that do not appear in the data. When converting labelled
#' values to a factor, this avoids the creation of additional factor levels.
#'
#' @param x A [`labelled`][haven::labelled()] vector
#'
#' @return A [`labelled`][haven::labelled()] vector
#'
#' @family lbl_helpers
#'
#' @export
#'
#' @examples
#' x <- haven::labelled(
#'   c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   c(Q1 = 1, Q2 = 2, Q3 = 3, Q4 = 4)
#' )
#'
#' lbl_clean(x)
#'
#' # Compare the factor levels of the normal and cleaned labels after coercion
#' as_factor(lbl_clean(x))
#'
#' as_factor(x)
lbl_clean <- function(x) {
  old_labels <- attr(x, "labels")

  unused_labels <- unname(old_labels) %in% dplyr::setdiff(
    unname(old_labels),
    unique(unname(x))
  )

  out <- x
  attr(out, "labels") <- old_labels[!unused_labels]
  out
}

# Based on rlang::as_function
# Changed so that instead of function having args .x & .y, it has
# .val and .lbl
as_lbl_function <- function(x, env = caller_env()) {
  if (rlang::is_function(x)) {
    return(x)
  }

  if (rlang::is_quosure(x)) {
    return(eval(rlang::expr(function(...) rlang::eval_tidy(!!x))))
  }

  if (rlang::is_formula(x)) {
    if (length(x) > 2) {
      rlang::abort("Can't convert a two-sided formula to a function")
    }

    args <- list(
      ... = rlang::missing_arg(),
      .val = quote(..1),
      .lbl = quote(..2)
    )

    fn <- new_function(args, rlang::f_rhs(x), rlang::f_env(x))

    return(fn)
  }

  if (rlang::is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }

  abort_coercion_function(x)
}

# Adapted from rlang:::abort_coercion
abort_coercion_function <- function(x) {
  x_type <- friendly_type_of(x)
  abort(paste0("Can't convert ", x_type, " to function"))
}

#' Make a label placeholder object
#'
#' Define a new label/value pair. For use in functions like [lbl_relabel()]
#' and [lbl_add()].
#'
#' @inherit lbl_na_if details
#'
#' @param ... Either one or two arguments specifying the label (`.lbl`) and
#'   value (`.val`) to use in the new label pair.
#'
#'   If arguments are named, they must be named `.val` and/or `.lbl`.
#'
#'   If a single unnamed value is passed, it is used as the `.lbl` for the new
#'   label. If two unnamed values are passed, they are used as the `.val` and
#'   `.lbl`, respectively.
#'
#' @return A `label_placeholder` object
#'
#' @family lbl_helpers
#'
#' @export
#'
#' @examples
#' # Label placeholder with no associated value
#' lbl("New label")
#'
#' # Label placeholder with a value/label pair
#' lbl(10, "New label")
#'
#' # Use placeholders as inputs to other label handlers
#' x <- haven::labelled(
#'   c(100, 200, 105, 990, 999, 230),
#'   c(`Unknown` = 990, NIU = 999)
#' )
#'
#' x <- lbl_add(
#'   x,
#'   lbl(100, "$100"),
#'   lbl(105, "$105"),
#'   lbl(200, "$200"),
#'   lbl(230, "$230")
#' )
#'
#' lbl_relabel(x, lbl(9999, "Missing") ~ .val > 900)
lbl <- function(...) {
  dots <- list(...)

  if (!is.null(names(dots)) && any(!names(dots) %in% c(".val", ".lbl", ""))) {
    rlang::abort("Expected only arguments named `.lbl` and `.val`")
  }

  if (length(dots) == 1) {
    if (!is.null(names(dots)) && names(dots) == ".val") {
      out <- list(.val = dots[[1]], .lbl = NULL)
    } else {
      out <- list(.val = NULL, .lbl = dots[[1]])
    }
  } else if (length(dots) == 2) {
    if (is.null(names(dots))) {
      names(dots) <- c(".val", ".lbl")
    } else {
      named_val <- names(dots) == ".val"
      named_lbl <- names(dots) == ".lbl"

      if (any(named_val)) {
        names(dots)[!named_val] <- ".lbl"
      }

      if (any(named_lbl)) {
        names(dots)[!named_lbl] <- ".val"
      }
    }

    out <- list(.val = dots[[".val"]], .lbl = dots[[".lbl"]])
  } else {
    rlang::abort("Expected either 1 or 2 arguments.")
  }

  class(out) <- "lbl_placeholder"
  out
}

fill_in_lbl <- function(lblval, orig_labels) {
  if (!inherits(lblval, "lbl_placeholder")) {
    lblval <- lbl(.val = lblval)
  }

  if (is.null(lblval$.lbl) & is.null(lblval$.val)) {
    rlang::abort(
      "Could not fill in label because neither label nor value is specified"
    )
  }
  if (is.null(lblval$.lbl)) {
    found_val <- unname(orig_labels) == lblval$.val

    if (!any(found_val)) {
      rlang::abort(
        paste0("Could not find value ", lblval$.val, " in existing labels.")
      )
    }

    lblval$.lbl <- names(orig_labels)[found_val]
  }
  if (is.null(lblval$.val)) {
    found_lbl <- names(orig_labels) == lblval$.lbl

    if (!any(found_lbl)) {
      rlang::abort(
        paste0("Could not find label \"", lblval$.lbl, "\" in existing labels.")
      )
    }

    lblval$.val <- unname(orig_labels)[found_lbl]
  }

  lblval
}

#' Remove label attributes from a data frame or labelled vector
#'
#' Remove all label attributes (value labels, variable labels, and variable
#' descriptions) from a data frame or vector.
#'
#' @param x A data frame or [labelled][haven::labelled()] vector
#'   (for instance, from a data frame column)
#'
#' @return An object of the same type as `x` without `"val_labels"`,
#' `"var_label`", and `"var_desc"` attributes.
#'
#' @family lbl_helpers
#'
#' @export
#'
#' @examples
#' cps <- read_ipums_micro(ipums_example("cps_00157.xml"))
#'
#' attributes(cps$YEAR)
#' attributes(zap_ipums_attributes(cps$YEAR))
#'
#' cps <- zap_ipums_attributes(cps)
#' attributes(cps$YEAR)
#' attributes(cps$INCTOT)
zap_ipums_attributes <- function(x) {
  UseMethod("zap_ipums_attributes")
}

#' @export
zap_ipums_attributes.default <- function(x) {
  x <- zap_labels(x)
  attr(x, "label") <- NULL
  attr(x, "var_desc") <- NULL
  x
}

#' @export
zap_ipums_attributes.data.frame <- function(x) {
  for (iii in seq_len(ncol(x))) {
    x[[iii]] <- zap_ipums_attributes(x[[iii]])
  }
  x
}

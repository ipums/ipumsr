# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' View a static webpage with variable metadata from an IPUMS extract
#'
#' @description
#' For a given [`ipums_ddi`] object or data frame, display metadata about
#' its contents in the RStudio viewer pane. This includes extract-level
#' information as well as metadata for the variables included in the
#' input object.
#'
#' It is also possible to save the output to an external HTML file without
#' launching the RStudio viewer.
#'
#' @details
#' `ipums_view()` requires that the htmltools, shiny, and DT packages are
#' installed. If `launch = TRUE`, RStudio and the rstudioapi package must
#' also be available.
#'
#' Note that if `launch = FALSE` and `out_file` is unspecified, the output
#' file will be written to a temporary directory. Some operating systems
#' may be unable to open the HTML file from the temporary directory; we
#' suggest that you manually specify the `out_file` location in this case.
#'
#' @param x An `ipums_ddi` object or a data frame with IPUMS attributes
#'   attached.
#'
#'   Note that file-level information (e.g. extract notes) is only
#'   available when `x` is an `ipums_ddi` object.
#' @param out_file Optional location to save the output HTML file. If `NULL`,
#'   makes a temporary file.
#' @param launch Logical indicating whether to launch the HTML file in the
#'   RStudio viewer pane. If `TRUE`, RStudio and rstudioapi must be available.
#'
#' @return The file path to the output HTML file (invisibly, if `launch = TRUE`)
#'
#' @export
#'
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))
#'
#' \dontrun{
#' ipums_view(ddi)
#' ipums_view(ddi, "codebook.html", launch = FALSE)
#' }
ipums_view <- function(x, out_file = NULL, launch = TRUE) {
  if (!requireNamespace("htmltools", quietly = TRUE) ||
    !requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE)) {
    rlang::abort(c(
      "Packages htmltools, shiny, and DT are required to run `ipums_view()`.",
      "i" = paste0(
        "Install them with `install.packages(c(\"htmltools\", ",
        "\"shiny\", \"DT\"))`"
      )
    ))
  }

  if (is.null(out_file)) {
    if (!launch) {
      rlang::warn(c(
        paste0("Some operating systems may have trouble opening an HTML ",
               "file from a temporary directory."),
        "i" = "Use `out_file` to specify an alternate output location."
      ))
    }

    out_file <- paste0(tempfile(), ".html")
  }

  file_info <- ipums_file_info(x)
  var_info <- ipums_var_info(x)

  # Make sure var_info has all required columns
  var_info <- dplyr::bind_rows(var_info, empty_var_info_df())

  html_page <- shiny::basicPage(
    htmltools::tags$h1("IPUMS Data Dictionary Viewer"),
    file_info_html(file_info),
    purrr::pmap(
      var_info,
      display_ipums_var_row,
      project = file_info$ipums_project
    )
  )

  html_page <- add_jquery_dependency(html_page)

  htmltools::save_html(html_page, out_file)

  if (launch) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(out_file)
    } else {
      rlang::abort(c(
        "RStudio and the rstudioapi package are required when `launch = TRUE`",
        "i" = "Install rstudioapi with `install.packages(\"rstudioapi\")`"
      ))
    }
    invisible(out_file)
  } else {
    out_file
  }
}

file_info_html <- function(file_info) {
  if (is.null(file_info)) {
    htmltools::tags$div(
      htmltools::tags$h2("Extract Information"),
      htmltools::tags$p("Not available")
    )
  } else {
    htmltools::tags$div(
      htmltools::tags$h2("Extract Information"),
      htmltools::tags$p(
        htmltools::tags$b("Project: "),
        file_info$ipums_project
      ),
      htmltools::tags$p(
        htmltools::tags$b("Date Created: "),
        file_info$extract_date
      ),
      htmltools::tags$p(
        htmltools::tags$b("Extract Notes: "),
        convert_single_linebreak_to_brtags(file_info$extract_notes)
      ),
      htmltools::tags$p(
        htmltools::tags$b("Conditions / Citation"),
        htmltools::tags$a(
          "(Click to expand)",
          `data-toggle` = "collapse",
          `href` = "#collapseConditions",
          `aria-expanded` = "false"
        ),
        htmltools::tags$div(
          split_double_linebreaks_to_ptags(file_info$conditions),
          split_double_linebreaks_to_ptags(file_info$citation),
          id = "collapseConditions",
          class = "collapse"
        )
      ),
      htmltools::tags$h2("Variable Information"),
      htmltools::tags$p("Click variable name for more details")
    )
  }
}

display_ipums_var_row <- function(var_name,
                                  var_label,
                                  var_desc,
                                  val_labels,
                                  code_instr,
                                  project,
                                  ...) {
  if (is.na(var_label)) {
    var_label <- "-"
  }

  if (is.na(var_desc)) {
    var_desc <- "No variable description available..."
  }

  vd_html <- split_double_linebreaks_to_ptags(var_desc)

  if (is.na(code_instr)) code_instr <- "N/A"
  code_instr <- split_double_linebreaks_to_ptags(code_instr)

  if (nrow(val_labels) > 0) {
    # The line below is a hacky solution to a problem that collapses the
    # height of the value labels table such that no rows are visible when viewed
    # in the RStudio Viewer pane
    val_lbls_height_in_pixels <- paste0(
      200 + min(nrow(val_labels), 10) * 30,
      "px"
    )
    value_labels <- DT::datatable(
      val_labels,
      style = "bootstrap",
      rownames = FALSE,
      width = "100%",
      height = val_lbls_height_in_pixels
    )
  } else {
    value_labels <- htmltools::tags$p("N/A")
  }

  url <- try(
    ipums_website(
      x = project,
      var = var_name,
      launch = FALSE,
      verbose = FALSE
    ),
    silent = TRUE
  )

  if (inherits(url, "try-error")) {
    link <- NULL
  } else {
    link <- htmltools::a(href = url, "More details")
  }

  expandable_div(
    var_name,
    var_label,
    shiny::fluidRow(
      shiny::column(
        6,
        htmltools::tags$h3("Variable Description"),
        vd_html,
        link
      ),
      shiny::column(
        6,
        htmltools::tags$h3("Value Labels"),
        htmltools::tags$h4("Coding Instructions"),
        code_instr,
        htmltools::tags$h4("Labelled Values"),
        value_labels
      )
    )
  )
}


expandable_div <- function(title, subtitle, content) {
  htmltools::tags$div(
    class = "panel panel-default",
    htmltools::tags$div(
      class = "panel-heading",
      htmltools::tags$div(
        class = "panel-title",
        htmltools::tags$a(
          class = "accordion-toggle",
          `data-toggle` = "collapse",
          `data-parent` = "#accordion",
          `aria-expanded` = "false",
          href = paste0("#", title),
          htmltools::tags$div(
            htmltools::tags$i(class = "more-less glyphicon glyphicon-plus"),
            htmltools::tags$h2(
              title,
              style = "display:inline-block; padding-right:0.5em"
            ),
            htmltools::tags$h5(subtitle)
          )
        )
      )
    ),
    htmltools::tags$div(
      id = title,
      class = "panel-colapse collapse",
      htmltools::tags$div(
        class = "panel-body",
        content
      )
    )
  )
}

split_double_linebreaks_to_ptags <- function(x) {
  if (is.null(x) || x == "") {
    return("")
  }
  out <- fostr_split(x, "\n\n")[[1]]
  purrr::map(out, htmltools::tags$p)
}

convert_single_linebreak_to_brtags <- function(x) {
  if (is.null(x) || x == "") {
    return(NULL)
  }

  split <- fostr_split(x, "\n")[[1]]
  if (length(split) == 1) {
    return(x)
  }

  out <- vector(mode = "list", length = (length(split) - 1) * 2 - 1)

  for (iii in seq_along(split)) {
    out[[(iii - 1) * 2 + 1]] <- split[iii]
    if (iii != length(split)) {
      out[[(iii - 1) * 2 + 2]] <- htmltools::tags$br()
    }
  }

  out
}

add_jquery_dependency <- function(page) {
  # Get jquery file from DT package's installed files (in case
  # no DT's are included in output)
  jquery_dir <- c(
    href = "shared/jquery",
    file = system.file("htmlwidgets/lib/jquery/", package = "DT")
  )

  page <- htmltools::attachDependencies(
    page,
    htmltools::htmlDependency(
      "jquery",
      "1.12.4",
      jquery_dir,
      script = "jquery.min.js"
    ),
    append = TRUE
  )

  htmltools::htmlDependencies(page) <- rev(htmltools::htmlDependencies(page))

  page
}

empty_var_info_df <- function() {
  tibble::tibble(
    var_name = character(0),
    var_label = character(0),
    var_desc = character(0),
    val_labels = list(),
    code_instr = character(0)
  )
}

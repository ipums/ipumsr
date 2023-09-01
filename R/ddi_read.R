# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' `ipums_ddi` class
#'
#' @description
#' The `ipums_ddi` class provides a data structure for storing the metadata
#' information contained in IPUMS codebook files. These objects are primarily
#' used when loading IPUMS data, but can also be
#' used to explore metadata for an IPUMS extract.
#'
#' - For microdata projects, this information is provided in
#' [DDI codebook](https://ddialliance.org/learn/what-is-ddi)
#' (.xml) files.
#' - For NHGIS, this information is provided in .txt codebook files.
#'
#' The codebook file contains metadata about the extract files themselves,
#' including file name, file path, and extract date as well as information about
#' variables present in the data, including variable names, descriptions, data
#' types, implied decimals, and positions in the fixed-width files.
#'
#' This information is used to correctly parse IPUMS
#' fixed-width files and attach additional variable metadata to data upon load.
#'
#' Note that codebook metadata for NHGIS extracts can also be stored in
#' an `ipums_ddi` object, even though these codebooks are distributed as .txt
#' files, not .xml files. These files do not adhere to the same standards as
#' the DDI codebook files, so some `ipums_ddi` fields will be left blank when
#' reading NHGIS codebooks.
#'
#' ## Creating an `ipums_ddi` object
#'
#' - To create an `ipums_ddi` object from an IPUMS microdata extract, use
#' [read_ipums_ddi()].
#' - To create an `ipums_ddi` object from an IPUMS NHGIS extract, use
#' [read_nhgis_codebook()]
#'
#' ## Loading data
#'
#' - To load the data associated with an `ipums_ddi` object, use
#' [read_ipums_micro()], [read_ipums_micro_chunked()], or
#' [read_ipums_micro_yield()]
#'
#' ## View metadata
#'
#' - Use [ipums_var_info()] to explore variable-level metadata for the variables
#' included in a dataset.
#' - Use [ipums_file_info()] to explore file-level metadata for an extract.
#'
#' @name ipums_ddi-class
#'
#' @keywords internal
#'
#' @aliases ipums_ddi
NULL

#' Read metadata about an IPUMS microdata extract from a DDI codebook (.xml)
#' file
#'
#' @description
#' Reads the metadata about an IPUMS extract from a
#' [DDI codebook](https://ddialliance.org/learn/what-is-ddi) into an
#' [ipums_ddi] object.
#'
#' These metadata contains parsing instructions for the associated fixed-width
#' data file, contextual labels for variables and values in the data, and
#' general extract information.
#'
#' See *Downloading IPUMS files* below for information about downloading
#' IPUMS DDI codebook files.
#'
#' # Downloading IPUMS files
#' The DDI codebook (.xml) file provided with IPUMS microdata extracts can be
#' downloaded through the IPUMS extract interface or (for some collections)
#' within R using the IPUMS API.
#'
#' If using the IPUMS extract interface:
#' - Download the DDI codebook by right clicking on the **DDI** link in the
#'   **Codebook** column of the extract interface and selecting **Save as...**
#'   (on Safari, you may have to select **Download Linked File As...**).
#'   Be sure that the codebook is downloaded in .xml format.
#'
#' If using the IPUMS API:
#' - For supported collections, use [download_extract()] to download a completed
#'   extract via the IPUMS API. This automatically downloads both the DDI
#'   codebook and the data file from the extract and
#'   returns the path to the codebook file.
#'
#' @param ddi_file Path to a DDI .xml file downloaded from
#'   [IPUMS](https://www.ipums.org/). See *Downloading IPUMS files* below.
#' @param lower_vars Logical indicating whether to convert variable names to
#'   lowercase. Defaults to `FALSE` for consistency with IPUMS conventions.
#' @param data_layer,file_select `r lifecycle::badge("deprecated")` Reading
#'   DDI files contained in a .zip archive has been deprecated. Please provide
#'   the full path to the .xml file to be loaded in `ddi_file`.
#'
#' @return An [ipums_ddi] object with metadata information.
#'
#' @seealso [read_ipums_micro()], [read_ipums_micro_chunked()] and
#'   [read_ipums_micro_yield()] to read data from IPUMS microdata extracts.
#'
#'   [ipums_var_info()] and [ipums_file_info()] to view metadata about an
#'   [ipums_ddi] object.
#'
#'   [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @export
#'
#' @examples
#' # Example codebook file
#' ddi_file <- ipums_example("cps_00157.xml")
#'
#' # Load data into an `ipums_ddi` obj
#' ddi <- read_ipums_ddi(ddi_file)
#'
#' # Use the object to load its associated data
#' cps <- read_ipums_micro(ddi)
#'
#' head(cps)
#'
#' # Or get metadata information directly
#' ipums_var_info(ddi)
#'
#' ipums_file_info(ddi)[1:2]
#'
#' # If variable metadata have been lost from a data source, reattach from
#' # its corresponding `ipums_ddi` object:
#' cps <- zap_ipums_attributes(cps)
#'
#' ipums_var_label(cps$STATEFIP)
#'
#' cps <- set_ipums_var_attributes(cps, ddi$var_info)
#'
#' ipums_var_label(cps$STATEFIP)
read_ipums_ddi <- function(ddi_file,
                           lower_vars = FALSE,
                           file_select = deprecated(),
                           data_layer = deprecated()) {
  if (!missing(data_layer)) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "read_ipums_ddi(data_layer = )"
    )
    file_select <- enquo(data_layer)
  } else if (!missing(file_select)) {
    lifecycle::deprecate_warn(
      "0.6.3",
      "read_ipums_ddi(file_select = )"
    )
    file_select <- enquo(file_select)
  } else {
    file_select <- NULL
    file_select <- enquo(file_select)
  }

  if (file_is_zip(ddi_file)) {
    lifecycle::deprecate_warn(
      "0.6.3",
      I("Reading DDI files through a zip archive "),
      details = "Please provide the full path to the DDI file to be loaded."
    )
  } else {
    dir_read_deprecated(ddi_file)
  }

  custom_check_file_exists(ddi_file)

  ddi_file_load <- find_files_in(
    ddi_file,
    "xml",
    file_select,
    none_ok = FALSE,
    multiple_ok = FALSE
  )

  if (file_is_zip(ddi_file)) {
    ddi_file_load <- unz(ddi_file, ddi_file_load)
  } else if (file_is_dir(ddi_file)) {
    ddi_file_load <- file.path(ddi_file, ddi_file_load)
  }

  ddi_xml <- xml2::read_xml(ddi_file_load, file_select = NULL)

  # Basic information
  conditions <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:dataAccs/d1:useStmt/d1:conditions"
  )

  citation <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:dataAccs/d1:useStmt/d1:citReq"
  )

  ipums_project <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:citation/d1:serStmt/d1:serName"
  )

  extract_notes <- xml_text_from_path_collapsed(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:notes"
  )

  extract_date <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:citation/d1:prodStmt/d1:prodDate/@date"
  )
  extract_date <- as.Date(extract_date)

  # File information
  files <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:fileDscr")

  if (length(files) > 1) {
    rlang::warn("Extracts with multiple files not supported, using first file.")
  }

  file_name <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileName"
  )

  file_type <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/@type"
  )

  file_encoding <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileType/@charset"
  )

  # Get rectype info if hierarchical
  if (file_type == "hierarchical") {
    rectypes <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/@rectype"
    )

    rectype_idvar <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/@recidvar"
    )
    rectype_idvar <- rectype_idvar[length(rectype_idvar)]

    rectypes_keyvars <- xml2::xml_text(
      xml2::xml_find_first(
        xml2::xml_find_all(
          ddi_xml,
          "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp"
        ),
        "@keyvar"
      )
    )
    rectypes_keyvars <- fostr_split(rectypes_keyvars, "[[:blank:]]+")
    rectypes_keyvars <- purrr::map(rectypes_keyvars, ~ .[!is.na(.)])
    rectypes_keyvars <- tibble::tibble(
      rectype = rectypes,
      keyvars = rectypes_keyvars
    )

    # For some reason our extract engine can't provide value labels for rectypes
    # So get it from file structure area
    rt_lbls <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/d1:labl"
    )
    rectype_labels <- tibble::tibble(
      val = rectypes,
      lbl = rt_lbls
    )
  } else {
    rectypes <- NULL
    rectype_idvar <- NULL
    rectype_labels <- NULL
    rectypes_keyvars <- NULL
  }

  # Get variable specific information
  var_info <- get_var_info_from_ddi(
    ddi_xml,
    file_type,
    rectype_idvar,
    rectype_labels
  )

  if (lower_vars) {
    var_info$var_name <- tolower(var_info$var_name)

    if (!is.null(rectype_idvar)) {
      rectype_idvar <- tolower(rectype_idvar)
    }

    if (!is.null(rectypes_keyvars)) {
      rectypes_keyvars$keyvars <- purrr::map(rectypes_keyvars$keyvars, tolower)
    }
  }

  if (file_is_dir(ddi_file)) {
    file_path <- ddi_file
  } else {
    file_path <- dirname(ddi_file)
  }

  new_ipums_ddi(
    file_name = file_name,
    file_path = file_path,
    file_type = file_type,
    ipums_project = ipums_project,
    extract_date = extract_date,
    extract_notes = extract_notes,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    rectypes_keyvars = rectypes_keyvars,
    var_info = var_info,
    conditions = conditions,
    citation = citation,
    file_encoding = file_encoding
  )
}

xml_text_from_path_first <- function(xml, path) {
  xml2::xml_text(xml2::xml_find_first(xml, path))
}

xml_text_from_path_collapsed <- function(xml, path, collapse = "\n\n") {
  out <- xml2::xml_text(xml2::xml_find_all(xml, path))
  paste(out, collapse = collapse)
}

xml_text_from_path_all <- function(xml, path) {
  xml2::xml_text(xml2::xml_find_all(xml, path))
}

get_var_info_from_ddi <- function(ddi_xml,
                                  file_type,
                                  rt_idvar,
                                  rectype_labels) {
  var_info_xml <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:dataDscr/d1:var")

  if (length(var_info_xml) == 0) {
    return(NULL)
  }

  var_name <- xml2::xml_attr(var_info_xml, "name")
  start <- as.numeric(
    xml_text_from_path_first(var_info_xml, "d1:location/@StartPos")
  )
  end <- as.numeric(
    xml_text_from_path_first(var_info_xml, "d1:location/@EndPos")
  )
  width <- as.numeric(
    xml_text_from_path_first(var_info_xml, "d1:location/@width")
  )
  var_label <- xml_text_from_path_first(var_info_xml, "d1:labl")
  var_desc <- xml_text_from_path_first(var_info_xml, "d1:txt")
  imp_decim <- as.numeric(
    xml2::xml_attr(var_info_xml, "dcml")
  )

  var_type <- xml_text_from_path_first(var_info_xml, "d1:varFormat/@type")
  var_intrvl <- xml2::xml_attr(var_info_xml, "intrvl")
  var_type <- dplyr::case_when(
    var_type == "numeric" & var_intrvl == "discrete" & (width < 10) ~ "integer",
    var_type == "numeric" ~ "numeric",
    var_type == "character" ~ "character",
    TRUE ~ "character" # Default to character if it's unexpected
  )

  code_instr <- fostr_replace(
    xml_text_from_path_first(var_info_xml, "d1:codInstr"),
    "^Codes",
    ""
  )

  if (file_type == "hierarchical") {
    rectype_by_var <- fostr_split(xml2::xml_attr(var_info_xml, "rectype"), " ")
  } else {
    rectype_by_var <- NA
  }

  # Value labels
  # Some come from parsed code sections
  lbls_from_code_instr <- parse_labels_from_code_instr(code_instr, var_type)

  # For hierarchical, RECTYPE comes from elsewhere in the DDI
  if (file_type == "hierarchical") {
    # If var is numeric, need to convert
    rt_type <- var_type[var_name == rt_idvar]

    if (length(rt_type) == 1 && rt_type %in% c("numeric", "integer")) {
      rectype_labels$val <- suppressWarnings(as.numeric(rectype_labels$val))
    }

    rectype_labels <- dplyr::filter(rectype_labels, !is.na(.data$val))
    rectype_labels <- dplyr::arrange(rectype_labels, .data$val)

    # Replace in the code_instructions
    if (nrow(rectype_labels) > 0) {
      lbls_from_code_instr[[which(var_name == rt_idvar)]] <- rectype_labels
    }
  }

  val_labels <- purrr::pmap(
    list(var_info_xml, var_type, lbls_from_code_instr),
    function(vvv, vtype, extra_labels) {
      lbls <- xml2::xml_find_all(vvv, "d1:catgry")

      if (length(lbls) == 0) {
        return(extra_labels)
      }

      lbls <- tibble::tibble(
        val = xml_text_from_path_all(lbls, "d1:catValu"),
        lbl = xml_text_from_path_all(lbls, "d1:labl")
      )

      if (vtype %in% c("numeric", "integer")) lbls$val <- as.numeric(lbls$val)

      # Drop labels that are the same as the value
      # But leading 0's can be ignored if numeric
      if (vtype %in% c("numeric", "integer")) {
        lnum <- suppressWarnings(as.numeric(lbls$lbl))
        lbls <- dplyr::filter(lbls, (is.na(lnum) | .data$val != lnum))
      } else {
        lbls <- dplyr::filter(lbls, .data$val != .data$lbl)
      }

      out <- dplyr::bind_rows(lbls, extra_labels)
      dplyr::arrange(out, .data$val)
    }
  )

  make_var_info_from_scratch(
    var_name = var_name,
    var_label = var_label,
    var_desc = var_desc,
    val_labels = val_labels,
    code_instr = code_instr,
    start = start,
    end = end,
    imp_decim = imp_decim,
    var_type = var_type,
    rectypes = rectype_by_var
  )
}

#' Read metadata from an NHGIS codebook (.txt) file
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Read the variable metadata contained in the .txt codebook file included with
#' NHGIS extracts into an [ipums_ddi] object.
#'
#' Because NHGIS variable metadata do not
#' adhere to all the standards of microdata DDI files, some of the `ipums_ddi`
#' fields will not be populated.
#'
#' This function is marked as experimental while we determine whether
#' there may be a more robust way to standardize codebook and DDI reading across
#' IPUMS collections.
#'
#' @param cb_file Path to a .zip archive containing an NHGIS extract or to an
#'   NHGIS codebook (.txt) file.
#' @param file_select If `cb_file` is a .zip archive or directory that contains
#'   multiple codebook files, an expression identifying the file to read.
#'   Accepts a character string specifying the file name, a
#'   [tidyselect selection][selection_language], or an index position of the
#'   file. Ignored if `cb_file` is the path to a single codebook file.
#' @param raw If `TRUE`, return a character vector containing the lines
#'   of `cb_file` rather than an `ipums_ddi` object. Defaults to
#'   `FALSE`.
#'
#' @return If `raw = FALSE`, an `ipums_ddi` object with information on the
#'   variables contained in the data for the extract associated with the given
#'   `cb_file`.
#'
#'   If `raw = TRUE`, a character vector with one element for each
#'   line of the given `cb_file`.
#'
#' @export
#'
#' @seealso [read_nhgis()] to read tabular data from an IPUMS NHGIS extract.
#'
#'   [read_ipums_sf()] to read spatial data from an IPUMS extract.
#'
#'   [ipums_list_files()] to list files in an IPUMS extract.
#'
#' @examples
#' # Example file
#' nhgis_file <- ipums_example("nhgis0972_csv.zip")
#'
#' # Read codebook as an `ipums_ddi` object:
#' codebook <- read_nhgis_codebook(nhgis_file)
#'
#' # Variable-level metadata about the contents of the data file:
#' ipums_var_info(codebook)
#'
#' ipums_var_label(codebook, "PMSA")
#'
#' # If variable metadata have been lost from a data source, reattach from
#' # the corresponding `ipums_ddi` object:
#' nhgis_data <- read_nhgis(nhgis_file, verbose = FALSE)
#'
#' nhgis_data <- zap_ipums_attributes(nhgis_data)
#' ipums_var_label(nhgis_data$PMSA)
#'
#' nhgis_data <- set_ipums_var_attributes(nhgis_data, codebook$var_info)
#' ipums_var_label(nhgis_data$PMSA)
#'
#' # You can also load the codebook in raw format to display in the console
#' codebook_raw <- read_nhgis_codebook(nhgis_file, raw = TRUE)
#'
#' # Use `cat` for human-readable output
#' cat(codebook_raw[1:20], sep = "\n")
read_nhgis_codebook <- function(cb_file,
                                file_select = NULL,
                                raw = FALSE) {
  dir_read_deprecated(cb_file)

  file_select <- enquo(file_select)

  custom_check_file_exists(cb_file)

  cb_name <- find_files_in(
    cb_file,
    "txt",
    file_select = file_select,
    multiple_ok = FALSE,
    none_ok = FALSE
  )

  if (file_is_zip(cb_file)) {
    cb <- readr::read_lines(unz(cb_file, cb_name), progress = FALSE)
  } else if (file_is_dir(cb_file)) {
    cb <- readr::read_lines(file.path(cb_file, cb_name), progress = FALSE)
  } else {
    cb <- readr::read_lines(cb_file, progress = FALSE)
  }

  if (raw) {
    return(cb)
  }

  # Section markers are a line full of dashes
  # (setting to 5+ to eliminate false positives)
  section_markers <- which(fostr_detect(cb, "^[-]{5,}$"))

  dd <- find_cb_section(cb, "^Data Dictionary$", section_markers)

  context_start <- which(dd == "Context Fields ") + 1
  context_end <- which(fostr_detect(dd, "^[[:blank:]]$")) - 1
  context_end <- min(context_end[context_end > context_start])
  context_rows <- seq(context_start, context_end)

  context_vars <- fostr_named_capture(
    dd[context_rows],
    "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$"
  )
  context_vars$var_desc <- ""
  context_vars <- context_vars[!is.na(context_vars$var_name), ]

  data_type_rows <- which(fostr_detect(dd, "(Data Type|Breakdown)"))
  blank_rows <- which(fostr_detect(dd, "^[[:blank:]]+$"))

  data_types <- dd[data_type_rows]

  # If multiple data types, process variable info for each data
  # type separately.
  if (length(data_type_rows) > 0) {
    data_type_sections <- purrr::map(
      data_type_rows,
      ~ seq(.x, blank_rows[which(.x <= blank_rows)[1]] - 1)
    )

    # Combine multiple lines of data type/breakdown value info into single
    # string to attach to var info
    data_types <- purrr::map(
      data_type_sections,
      ~ parse_breakdown(
        dd[.x][length(.x):2] # Go in reverse so data types come before brkdowns
      )
    )

    data_type_rows <- purrr::map2(
      data_type_rows,
      c(data_type_rows[-1], length(dd) + 1),
      ~ seq(.x, .y - 1)
    )

    table_name_rows <- purrr::map(
      data_type_rows,
      ~ .x[fostr_detect(dd[.x], "^[[:blank:]]*(Table)|(Data Type)")]
    )

    table_sections <- purrr::map2(
      data_type_rows,
      table_name_rows,
      function(dt, tn) {
        purrr::map2(
          tn,
          c(tn[-1], max(dt) + 1),
          ~ seq(.x, .y - 1)
        )
      }
    )

    table_sections <- purrr::flatten(
      purrr::map2(table_sections, data_types, ~ set_names(.x, .y))
    )

    if (any(fostr_detect(cb, "^Time series layout:"))) {
      table_vars <- purrr::map_dfr(
        table_sections,
        ~ read_nhgis_tst_tables(dd, .x)
      )
    } else {
      table_vars <- purrr::map2_dfr(
        table_sections,
        names(table_sections),
        ~ read_nhgis_ds_tables(dd, .x, data_type = .y)
      )
    }
  } else {
    table_name_rows <- which(
      fostr_detect(dd, "^[[:blank:]]*(Table)|(Data Type)")
    )

    table_sections <- purrr::map2(
      table_name_rows,
      c(table_name_rows[-1], length(dd)),
      ~ seq(.x, .y - 1)
    )

    if (any(fostr_detect(cb, "^Time series layout:"))) {
      table_vars <- purrr::map_dfr(
        table_sections,
        ~ read_nhgis_tst_tables(dd, .x)
      )
    } else {
      table_vars <- purrr::map_dfr(
        table_sections,
        ~ read_nhgis_ds_tables(dd, .x)
      )
    }
  }

  var_info <- make_var_info_from_scratch(
    var_name = c(context_vars$var_name, table_vars$var_name),
    var_label = c(context_vars$var_label, table_vars$var_label),
    var_desc = c(context_vars$var_desc, table_vars$var_desc)
  )

  # Get License and Condition section
  conditions_text <- find_cb_section(
    cb,
    "^Citation and Use of .+ Data",
    section_markers
  )

  conditions_text <- paste(conditions_text, collapse = "\n")

  new_ipums_ddi(
    file_name = cb_name,
    file_type = "rectangular",
    ipums_project = "NHGIS",
    var_info = var_info,
    conditions = conditions_text
  )
}

#' Parse NHGIS codebook lines with data type or breakdown info
#'
#' @description
#' Extracts the apporpriate name for data types and breakdown values from an
#' NHGIS codebook for maximal similarity to the content of NHGIS enhanced
#' header rows.
#'
#' Lines with a double colon ("::") have titles following the double colon.
#' Lines with single colons have titles following the first single colon.
#' Lines with no colons are typically data types and should have the entire
#' line extracted.
#'
#' @param bkdown_lines Lines corresponding to a single data type or breakdown
#'   section of the codebook. Typically start with "Breakdown" or "Data Type"
#'
#' @return Character vector of length `bkdown_lines` with the extracted
#'   data type or breakdown titles
#'
#' @noRd
parse_breakdown <- function(bkdown_lines) {
  dt_bkdwn <- purrr::map(
    bkdown_lines,
    ~ if (fostr_detect(.x, "::")) {
      fostr_named_capture(.x, "::(?<x>[^\\(]+)")$x
    } else if (fostr_detect(.x, ":")) {
      fostr_named_capture(.x, ":(?<x>[^\\(]+)")$x
    } else {
      .x
    }
  )

  paste0(trimws(dt_bkdwn), collapse = ": ")
}

#' Helper function to read codebook information for an NHGIS
#' extract that contains time series tables.
#'
#' @param dd Character vector of lines contained in the codebook's
#'   "Data Dictionary" section.
#' @param table_rows Indices of rows that include table variable information
#'   within the provided `dd`.
#'
#' @return tibble of variable information
#'
#' @noRd
read_nhgis_tst_tables <- function(dd, table_rows) {
  table_name_and_code <- fostr_named_capture(
    dd[table_rows[1]],
    paste0(
      "^[[:blank:]]*Table .+?:[[:blank:]]+\\((?<table_code>.+?)\\)",
      "[[:blank:]]+(?<table_name>.+)$"
    )
  )

  nhgis_table_code <- table_name_and_code$table_code
  table_name <- table_name_and_code$table_name

  time_series_headers <- fostr_detect(
    dd[table_rows],
    "^[[:blank:]]+Time series"
  )

  vars <- dd[table_rows][!time_series_headers]
  vars <- vars[-1] # First row was table name/code
  vars <- fostr_named_capture(
    vars,
    "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
    only_matches = TRUE
  )

  vars$var_desc <- paste0("Table ", nhgis_table_code, ": ", table_name)

  vars
}

#' Helper function to read codebook information for an NHGIS
#' extract that contains datasets.
#'
#' @param dd Character vector of lines contained in the codebook's
#'   "Data Dictionary" section.
#' @param table_rows Indices of rows that include table variable information
#'   within the provided `dd`.
#'
#' @return tibble of variable information
#'
#' @noRd
read_nhgis_ds_tables <- function(dd, table_rows, data_type = NULL) {
  if (fostr_detect(dd[table_rows[1]], "Data Type")) {
    rows <- dd[table_rows]

    # Start at first blank row. Rows before this are part of the
    # Data type or breakdown value, not variables.
    rows <- rows[seq(
      min(which(fostr_detect(rows, "^[[:blank:]]+$"))),
      length(rows)
    )]

    vars <- fostr_named_capture(
      rows,
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
      only_matches = TRUE
    )

    vars$var_desc <- ""
  } else {
    table_name <- fostr_named_capture_single(
      dd[table_rows[1]],
      "^[[:blank:]]*Table .+?:[[:blank:]]+(?<table_name>.+)$"
    )

    universe <- fostr_named_capture_single(
      dd[table_rows[2]],
      "^[[:blank:]]*Universe:[[:blank:]]+(?<universe>.+)$"
    )

    nhgis_table_code <- fostr_named_capture_single(
      dd[table_rows[4]],
      "^[[:blank:]]*NHGIS code:[[:blank:]]+(?<table_code>.+)$"
    )

    vars <- fostr_named_capture(
      dd[table_rows[-1:-4]],
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
      only_matches = TRUE
    )

    if (!is_null(data_type)) {
      vars$var_label <- paste0(data_type, ": ", vars$var_label)
    }

    vars$var_desc <- paste0(
      "Table ", nhgis_table_code, ": ", table_name,
      " (Universe: ", universe, ")"
    )
  }

  vars
}

find_cb_section <- function(cb_text, section, section_markers) {
  start <- which(fostr_detect(cb_text, section))
  start <- 2 + start[
    start - 1 %in% section_markers & start + 1 %in% section_markers
  ]

  end <- min(c(length(cb_text), section_markers[section_markers > start])) - 1
  cb_text[seq(start, end)]
}

new_ipums_ddi <- function(file_name = NULL,
                          file_path = NULL,
                          file_type = NULL,
                          ipums_project = NULL,
                          extract_date = NULL,
                          extract_notes = NULL,
                          rectypes = NULL,
                          rectype_idvar = NULL,
                          rectypes_keyvars = NULL,
                          var_info = NULL,
                          conditions = NULL,
                          citation = NULL,
                          file_encoding = NULL) {
  ddi <- list(
    file_name = file_name,
    file_path = file_path,
    file_type = file_type,
    ipums_project = ipums_project,
    extract_date = extract_date,
    extract_notes = extract_notes,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    rectypes_keyvars = rectypes_keyvars,
    var_info = var_info,
    conditions = conditions,
    citation = citation,
    file_encoding = file_encoding
  )

  structure(ddi, class = "ipums_ddi")
}

make_var_info_from_scratch <- function(var_name = "",
                                       var_label = "",
                                       var_desc = "",
                                       val_labels = NULL,
                                       code_instr = "",
                                       start = NA,
                                       end = NA,
                                       imp_decim = 0,
                                       var_type = "",
                                       rectypes = NA) {
  val_labels <- val_labels %||% list(
    tibble::tibble(val = numeric(0), lbl = character(0))
  )

  tibble::tibble(
    var_name = var_name,
    var_label = var_label,
    var_desc = var_desc,
    val_labels = val_labels,
    code_instr = code_instr,
    start = start,
    end = end,
    imp_decim = imp_decim,
    var_type = var_type,
    rectypes = rectypes
  )
}

make_empty_labels <- function(vt) {
  tibble::tibble(
    val = if (vt == "character") character(0) else numeric(0),
    lbl = character(0)
  )
}

# Helper to get labels out of free text from codInstr in xml
parse_labels_from_code_instr <- function(code, var_type) {
  purrr::map2(
    code,
    var_type,
    function(x, vt) {
      if (is.na(x)) {
        return(make_empty_labels(vt))
      }
      lines <- fostr_split(x, "\n")[[1]]
      labels <- parse_code_regex(lines, vt)
      dplyr::arrange(labels, .data$val)
    }
  )
}

parse_code_regex <- function(x, vtype) {
  if (vtype %in% c("numeric", "integer")) {
    labels <- fostr_named_capture(
      x,
      "^(?<val>-?[0-9.,]+)(([[:blank:]][[:punct:]]|[[:punct:]][[:blank:]]|[[:blank:]]|=)+)(?<lbl>.+?)$",
      only_matches = TRUE
    )

    labels$val <- as.numeric(fostr_replace_all(labels$val, ",", ""))
  } else {
    labels <- fostr_named_capture(
      x,
      "^(?<val>[[:graph:]]+)(([[:blank:]]+[[:punct:]|=]+[[:blank:]])+)(?<lbl>.+)$",
      only_matches = TRUE
    )
  }

  labels
}

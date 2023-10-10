# Make down-sampled version of a CPS extract for use in the big data vignette
#
# We needed to create a smaller version of the CPS
# extract used as example data in the big data vignette.
#
# This script produces:
#   * inst/extdata/cps_00097.xml
#   * inst/extdata/cps_00097.dat.gz
#
# Used in big data vignette
devtools::load_all()

set.seed(2023 - 3 - 20)

ddi_path <- define_extract_cps(
  description = "Reproducing cps00011 example data",
  samples = "cps2011_03s",
  variables = c(
    "YEAR", "SERIAL", "ASECWTH", "CPSID", "ASECFLAG", "FOODSTMP",
    "MONTH", "PERNUM", "CPSIDP", "ASECWT", "AGE", "EMPSTAT", "AHRSWORKT",
    "HEALTH"
  )
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract("inst/extdata")


# `sample_one_in = 10` means "sample one in 10 households"
downsample_data_file <- function(ddi_path, sample_one_in = 10) {
  data_path <- fostr_replace(ddi_path, "\\.xml", ".dat.gz")
  data_lines <- readr::read_lines(data_path)

  ddi <- read_ipums_ddi(ddi_path)

  serial_start <- ddi$var_info$start[ddi$var_info$var_name == "SERIAL"]
  serial_end <- ddi$var_info$end[ddi$var_info$var_name == "SERIAL"]

  serial <- fostr_sub(data_lines, serial_start, serial_end)
  unique_serial <- unique(serial)

  sample_size <- round(length(unique_serial) / sample_one_in)
  sampled_serial <- sample(unique_serial, sample_size)

  sampled_data_lines <- data_lines[serial %in% sampled_serial]

  uncompressed_data_path <- fostr_replace(data_path, "\\.gz$", "")
  readr::write_lines(sampled_data_lines, uncompressed_data_path)

  R.utils::gzip(
    filename = uncompressed_data_path,
    destname = data_path,
    overwrite = TRUE
  )

  invisible(data_path)
}

downsample_data_file(ddi_path)

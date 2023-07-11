test_that("normal ddi doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  ddi <- read_ipums_ddi(ipums_example("cps_00157.xml"))
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("empty ddi doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  ddi <- new_ipums_ddi()
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("normal microdata doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  data <- read_ipums_micro(ipums_example("cps_00157.xml"), verbose = FALSE)
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("attribute-less microdata doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  data <- read_ipums_micro(
    ipums_example("cps_00157.xml"),
    verbose = FALSE, var_attrs = NULL
  )
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("nhgis codebook doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  cb <- read_nhgis_codebook(ipums_example("nhgis0972_csv.zip"))
  ipums_view(cb, launch = FALSE)
  expect_true(TRUE)
})

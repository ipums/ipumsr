test_that("Can get website URL for `ipums_ddi` object", {
  ddi <- read_ipums_ddi(ipums_example("cps_00097.xml"))

  expect_equal(
    ipums_website(ddi, launch = FALSE),
    "https://cps.ipums.org/cps-action/variables/group"
  )
  expect_equal(
    ipums_website(ddi, var = "AGE", launch = FALSE),
    "https://cps.ipums.org/cps-action/variables/AGE"
  )
})

test_that("Can get URL for projects that do not have variables", {
  ddi <- read_nhgis_codebook(
    ipums_example("nhgis0712_csv.zip"),
    file_select = 1
  )

  expect_equal(
    ipums_website(ddi, launch = FALSE),
    "https://data2.nhgis.org/main/"
  )
  expect_warning(
    url <- ipums_website(ddi, var = "GISJOIN", launch = FALSE),
    "Cannot give a variable-specific URL for project \"NHGIS\""
  )
  expect_equal(url, "https://data2.nhgis.org/main/")
})

test_that("Can get website URL for manually specified project and variable", {
  expect_equal(
    ipums_website("IPUMS USA", launch = FALSE),
    "https://usa.ipums.org/usa-action/variables/group"
  )
  expect_equal(
    ipums_website("IPUMS USA", var = "RACE", launch = FALSE),
    "https://usa.ipums.org/usa-action/variables/RACE"
  )
  expect_equal(
    ipums_website("IPUMS-USA", var = "RACE", launch = FALSE),
    "https://usa.ipums.org/usa-action/variables/RACE"
  )
  expect_equal(
    ipums_website("ipUms UsA", var = "RACE", launch = FALSE),
    "https://usa.ipums.org/usa-action/variables/RACE"
  )
})

test_that("Can get website URL when specifying project using API code", {
  expect_equal(
    ipums_website("dhs", launch = FALSE),
    "https://idhsdata.org/idhs-action/variables/group"
  )
  expect_equal(
    ipums_website("dhs", var = "SAMPLE", launch = FALSE),
    "https://idhsdata.org/idhs-action/variables/SAMPLE"
  )
  expect_equal(
    ipums_website("dhs", var = "SAMPLE", launch = FALSE),
    ipums_website("IPUMS DHS", var = "SAMPLE", launch = FALSE)
  )
})

test_that("Can get URL when multiple vars specified", {
  expect_warning(
    url <- ipums_website(
      "IPUMS International",
      var = c("RELATE", "AGE"),
      launch = FALSE
    ),
    "Multiple variables specified.+AGE"
  )
  expect_equal(
    url,
    "https://international.ipums.org/international-action/variables/AGE"
  )
})

test_that("We use IPUMS homepage if invalid project", {
  expect_warning(
    expect_warning(
      url <- ipums_website(
        "foobar",
        var = "foobar",
        launch = FALSE,
        homepage_if_missing = TRUE
      ),
      "Redirecting to IPUMS homepage"
    ),
    "Cannot give a variable-specific URL"
  )
  expect_equal(url, "https://www.ipums.org")
  expect_error(
    ipums_website(
      "foobar",
      launch = FALSE,
      homepage_if_missing = FALSE,
      verbose = FALSE
    ),
    "Project not found"
  )
})

test_that("Can get URL for website even if variable is not in DDI", {
  ddi <- read_ipums_ddi(ipums_example("cps_00097.xml"))

  expect_equal(
    ipums_website(ddi, "HEALTH", launch = FALSE),
    "https://cps.ipums.org/cps-action/variables/HEALTH"
  )
  expect_warning(
    url <- ipums_website(ddi, "RELATE", launch = FALSE),
    "`var` \"RELATE\" was not found in the provided `ipums_ddi`"
  )
  expect_equal(url, "https://cps.ipums.org/cps-action/variables/RELATE")
})

test_that("Can still use deprecated `project` arg (until it is removed)", {
  lifecycle::expect_deprecated(
    x <- ipums_website(project = "atus", var = "AGE", launch = FALSE)
  )

  expect_equal(x, "https://atusdata.org/atus-action/variables/AGE")
  expect_equal(x, ipums_website(x = "atus", var = "AGE", launch = FALSE))
})

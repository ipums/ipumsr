test_that("simple bind rows example", {
  test1 <- tibble::tibble(
    x = haven::labelled(c(1, 2, 3), c("xyz" = 1)),
    y = 2:4
  )
  attr(test1$x, "label") <- "A var label"

  test2 <- tibble::tibble(
    x = haven::labelled(c(1), c("xyz" = 1)),
    z = "a"
  )
  attr(test2$x, "label") <- "A var label"

  bound <- ipums_bind_rows(test1, test2)

  expect_equal(names(bound), c("x", "y", "z"))
  expect_equal(as.vector(bound$x), c(1, 2, 3, 1))
  expect_equal(attributes(bound$x), attributes(test1$x))
})


test_that("mismatched attributes in bind rows", {
  test1 <- tibble::tibble(
    x = haven::labelled(c(1, 2, 3), c("xyz" = 1)),
    y = 2:4
  )
  attr(test1$x, "label") <- "A var label"

  test2 <- tibble::tibble(
    x = haven::labelled(c(1), c("xyz" = 1)),
    z = "a"
  )
  attr(test2$x, "label") <- "A different var label"

  expect_warning(bound <- ipums_bind_rows(test1, test2))

  expect_equal(names(bound), c("x", "y", "z"))
  expect_equal(as.vector(bound$x), c(1, 2, 3, 1))
})


test_that("ipums_bind_rows can handle list of data.frames", {
  test1 <- tibble::tibble(
    x = haven::labelled(c(1, 2, 3), c("xyz" = 1)),
    y = 2:4
  )
  attr(test1$x, "label") <- "A var label"

  test2 <- tibble::tibble(
    x = haven::labelled(c(1), c("xyz" = 1)),
    z = "a"
  )
  attr(test2$x, "label") <- "A var label"

  bound <- ipums_bind_rows(test1, list(test1, test2))

  expect_equal(names(bound), c("x", "y", "z"))
  expect_equal(as.vector(bound$x), c(1, 2, 3, 1, 2, 3, 1))
})


test_that("ipums_bind_rows throws error for nested list of data.frames", {
  test1 <- tibble::tibble(
    x = haven::labelled(c(1, 2, 3), c("xyz" = 1)),
    y = 2:4
  )
  attr(test1$x, "label") <- "A var label"

  test2 <- tibble::tibble(
    x = haven::labelled(c(1), c("xyz" = 1)),
    z = "a"
  )
  attr(test2$x, "label") <- "A var label"

  expect_error(
    ipums_bind_rows(test1, list(list(test1, test2))),
    regexp = "must be a data.frame or a list of data.frames"
  )
})

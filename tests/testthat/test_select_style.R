test_that("select_var_rows respects tidyselect syntax", {
  # Note that select_var_rows takes the quosure as an argument
  # Seems okay because this is an internal function, but possibly
  # is a little weird.

  vars <- c("RECTYPE", "SEX", "REPWT1", "REPWT2")
  test_df <- tibble::tibble(
    var_name = vars,
    x = seq_along(vars)
  )

  # Bare column names
  expect_equal(
    ipumsr:::select_var_rows(test_df, rlang::quo(c(RECTYPE, SEX))),
    dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
  )

  # String
  expect_equal(
    ipumsr:::select_var_rows(test_df, rlang::quo(c("RECTYPE", "SEX"))),
    dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
  )

  # variable from environment
  my_vars <- c("RECTYPE", "SEX")
  expect_equal(
    ipumsr:::select_var_rows(
      test_df,
      rlang::quo(tidyselect::all_of(my_vars))
    ),
    dplyr::filter(test_df, var_name %in% c("RECTYPE", "SEX"))
  )

  # dplyr::select helpers
  expect_equal(
    ipumsr:::select_var_rows(test_df, rlang::quo(starts_with("REP"))),
    dplyr::filter(test_df, var_name %in% c("REPWT1", "REPWT2"))
  )

  # NULL returns full dataframe
  expect_equal(
    ipumsr:::select_var_rows(test_df, rlang::quo(NULL)),
    test_df
  )
})

test_that("find_files_in respects tidyselect syntax", {
  file_names <- c("abc.txt", "test.csv", "test1.txt", "test2.txt")
  zipped_misc <- file.path(vcr::vcr_test_path("fixtures"), "zipped_misc.zip")

  # Gets all files
  expect_equal(
    find_files_in(zipped_misc, multiple_ok = TRUE),
    file_names
  )

  # Errors if multiple is not okay
  expect_error(
    ipumsr:::find_files_in(zipped_misc, multiple_ok = FALSE)
  )

  # Can filter on extension
  expect_equal(
    ipumsr:::find_files_in(zipped_misc, name_ext = "csv", multiple_ok = TRUE),
    "test.csv"
  )

  # Bare file names
  expect_equal(
    ipumsr:::find_files_in(
      zipped_misc,
      file_select = rlang::quo(c(test1.txt, test2.txt)),
      multiple_ok = TRUE
    ),
    c("test1.txt", "test2.txt")
  )

  # String
  expect_equal(
    ipumsr:::find_files_in(
      zipped_misc,
      file_select = rlang::quo(c("test1.txt", "test2.txt")),
      multiple_ok = TRUE
    ),
    c("test1.txt", "test2.txt")
  )

  # variable from environment
  my_vars <- c("test1.txt", "test2.txt")
  expect_equal(
    ipumsr:::find_files_in(
      zipped_misc,
      file_select = rlang::quo(tidyselect::all_of(my_vars)),
      multiple_ok = TRUE
    ),
    c("test1.txt", "test2.txt")
  )


  # dplyr::select helpers
  expect_equal(
    ipumsr:::find_files_in(
      zipped_misc,
      file_select = rlang::quo(starts_with("test")),
      multiple_ok = TRUE
    ),
    c("test.csv", "test1.txt", "test2.txt")
  )
})

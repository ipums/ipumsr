modify_ready_extract_cassette_file <- function(cassette_file_name,
                                               n_requests = 1) {
  ready_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), cassette_file_name
  )

  ready_lines <- readLines(ready_extract_cassette_file)
  request_lines <- which(grepl("^- request:", ready_lines))

  start_line <- request_lines[length(request_lines) - n_requests + 1]

  writeLines(
    c(
      ready_lines[[1]],
      ready_lines[start_line:length(ready_lines)]
    ),
    con = ready_extract_cassette_file
  )
}

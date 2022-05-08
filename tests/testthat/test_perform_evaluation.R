test_that("we can run in .debug with a SQLite database", {
  library(inspectEHR)

  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  
  events <- .events %>%
    dplyr::mutate(
      datetime = strftime(datetime),
      date = strftime(date, format = "%Y-%m-%d"),
      time = strftime(time, format = "%H:%M:%S")) %>%
    DBI::dbWriteTable(connection, "events", .)
  
  episodes <- .episodes %>%
    dplyr::mutate(
      start_date = strftime(start_date)
      ) %>%
    DBI::dbWriteTable(connection, "episodes", .)
  
  provenance <- .provenance %>%
    dplyr::mutate(
      date_created = strftime(date_created),
      date_parsed = strftime(date_parsed),
    ) %>%
    DBI::dbWriteTable(connection, "provenance", .)
  
  variables <- DBI::dbWriteTable(connection, "variables", .variables)
  
  my_result <- inspectEHR::perform_evaluation(
    connection = connection,
    output_folder = "./output/",  # note trailing slash
    verbose = TRUE
  )

  expect_true(my_result)
  
  DBI::dbDisconnect(connection)

})

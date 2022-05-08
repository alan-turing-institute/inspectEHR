# Set up our test database
setup_connection <- function() {
  connection <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  events <- inspectEHR:::.events %>%
    dplyr::mutate(
      datetime = strftime(datetime),
      date = strftime(date, format = "%Y-%m-%d"),
      time = strftime(time, format = "%H:%M:%S")) %>%
    DBI::dbWriteTable(connection, "events", .)
  
  episodes <- inspectEHR:::.episodes %>%
    dplyr::mutate(
      start_date = strftime(start_date)
    ) %>%
    DBI::dbWriteTable(connection, "episodes", .)
  
  provenance <- inspectEHR:::.provenance %>%
    dplyr::mutate(
      date_created = strftime(date_created),
      date_parsed = strftime(date_parsed),
    ) %>%
    DBI::dbWriteTable(connection, "provenance", .)
  
  variables <- DBI::dbWriteTable(connection, "variables", inspectEHR:::.variables)

  return(connection)
}

# Disconnect from our test database
cleanup_connection <- function(connection) {
  DBI::dbDisconnect(connection)
}
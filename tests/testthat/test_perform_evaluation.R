test_that("we can run in .debug with a SQLite database", {

  ctn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  expect_true(
    perform_evaluation(
      connection = ctn,
      output_folder = "./output/",  # note trailing slash
      verbose = TRUE,
      .debug = TRUE
    )
  )

  DBI::dbDisconnect(ctn)
})

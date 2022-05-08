test_that("We can run with a SQLite database", {

  # Get a connection with demo data
  connection <- setup_connection()
  
  my_result <- inspectEHR::perform_evaluation(
    connection = connection,
    output_folder = "./output/",  # note trailing slash
    verbose = TRUE
  )

  cleanup_connection(connection)
  
  expect_true(my_result)

})

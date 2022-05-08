packageVersion("inspectEHR")
results = testthat::test_file("tests/testthat/test_perform_evaluation.R")

# https://stackoverflow.com/a/61013209/3324095
failed_tests <- sapply(results, function(r) {
  !is(r$result[[1]], "expectation_success")
})

if (any(failed_tests)) {
    quit(status=1)
}

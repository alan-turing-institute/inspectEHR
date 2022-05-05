library(devtools)
load_all()

testthat::test_file("./tests/testthat/test_perform_evaluation.R")
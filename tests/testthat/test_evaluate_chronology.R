`%>%` <- magrittr::`%>%`

df <- evaluate_chronology(.debug = TRUE)

test_that("chronology always return the correct object", {
  expect_true(is_event_evaluation(df))
})

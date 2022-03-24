`%>%` <- magrittr::`%>%`
core <- make_core(.debug = TRUE)

sys_bp <- extract(core, code_name = "NIHR_HIC_ICU_0112")
dia_bp <- extract(core, code_name = "NIHR_HIC_ICU_0114")

bp_compare <- evaluate_comparison(
  event_1 = sys_bp, event_2 = dia_bp,
  relationship = ">")

test_that("compare gt works", {
  expect_true(is_event_evaluation(bp_compare))
  expect_equal(nrow(bp_compare), 0)
})

bp_compare <- evaluate_comparison(
  event_1 = sys_bp, event_2 = dia_bp,
  relationship = "<")

test_that("compare lt works", {
  expect_true(is_event_evaluation(bp_compare))
  expect_gt(nrow(bp_compare), 0)
})

death_date <- extract(core, code_name = "NIHR_HIC_ICU_0042")
death_time <- extract(core, code_name = "NIHR_HIC_ICU_0043")

death_compare <- evaluate_comparison(
  event_1 = death_date,
  event_2 = death_time,
  relationship = "exists")

test_that("compare exists works", {
  expect_true(is_event_evaluation(death_compare))
  expect_equal(nrow(death_compare), 0)
})

death_compare <- evaluate_comparison(
  event_1 = death_date,
  event_2 = death_time,
  relationship = "not_exists")

test_that("compare exists works", {
  expect_true(is_event_evaluation(death_compare))
  expect_gt(nrow(death_compare), 0)
})

comparisons_lookup <- tibble::tribble(
  ~event_a, ~event_b, ~operation,
  # Death date and time (should both exist together)
  "NIHR_HIC_ICU_0042", "NIHR_HIC_ICU_0043", "exist",
  # Body removal date and time (should both exist together)
  "NIHR_HIC_ICU_0038", "NIHR_HIC_ICU_0039", "exist",
  # BSD removal date and time (should both exist together)
  "NIHR_HIC_ICU_0044", "NIHR_HIC_ICU_0045", "exist",
  # Systolic BP and Diastolic BP (Sys > Dia)
  "NIHR_HIC_ICU_0112", "NIHR_HIC_ICU_0114", ">"
)

`%>%` <- magrittr::`%>%`
core <- make_core(.debug = TRUE)

two_d_items <- c("NIHR_HIC_ICU_0108",
                 "NIHR_HIC_ICU_0116",
                 "NIHR_HIC_ICU_0126",
                 "NIHR_HIC_ICU_0129")

extract_all <- two_d_items %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- two_d_items

periodicity_all <- extract_all %>%
  purrr::map(~ evaluate_periodicity(.x))

test_that("periodicity always return the correct object", {
  all_evals <- periodicity_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

## I have ensured that the periodicity is too low for codes 3 and 4,
## and correct (+1, see below) for codes 1 and 2

test_that("periodicities have been captured correctly", {
  all_evals <- periodicity_all %>%
    purrr::map_int(~ nrow(.x))
  expect_equivalent(all_evals[1], 1)
  # I introduced a duplicate event to test `evaluate_duplicate` which is
  # also picked up here as a single row that exceeds the periodicity.
  # which is fun.
  expect_equivalent(all_evals[2], 1)
  expect_gt(all_evals[3], 0)
  expect_gt(all_evals[4], 0)
})

`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Coverage ====

cov_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("coverage", avail_methods))
    }) %>%
  purrr::map(
    .f = ~ evaluate_coverage(x = .x, reference_tbl = ref)
  )

test_that("coverage always return the correct object", {
  all_evals <- cov_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

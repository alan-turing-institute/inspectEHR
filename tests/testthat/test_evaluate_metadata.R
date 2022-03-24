`%>%` <- magrittr::`%>%`
core <- make_core(.debug = TRUE)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Meta_data ====

meta_all <- extract_all %>%
  purrr::map(
    .f = ~ evaluate_metadata(.x)
  )

test_that("metadata always return the correct object", {
  all_evals <- meta_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

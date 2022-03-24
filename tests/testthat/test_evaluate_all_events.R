`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)
ref <- make_reference(.debug = TRUE)
epi_length <- characterise_episodes(.debug = TRUE)
epi_length <- evaluate_episodes(epi_length)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Do it ====

eval_all <- extract_all %>%
  purrr::map(
    .f = ~ evaluate_events(.x, los_table = epi_length, reference_tbl = ref)
  )

test_that("Evaluations always return the correct object", {
  all_evals <- eval_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

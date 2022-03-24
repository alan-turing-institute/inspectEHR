`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)
epi_length <- characterise_episodes(.debug = TRUE)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Boundaries ====

bound_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("bounds", avail_methods))
    }) %>%
  purrr::map(
    .f = ~ evaluate_bounds(.x, epi_length)
  )

test_that("bounds always return the correct object", {
  all_evals <- bound_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

`%>%` <- magrittr::`%>%`
core <- make_core(.debug = TRUE)

## Ranges and Sets ====

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

range_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("range", avail_methods))
    }) %>%
  purrr::map(~ evaluate_range(.x))

test_that("ranges always return the correct object", {
  all_evals <- range_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

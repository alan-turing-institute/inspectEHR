`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Distribution ====

dist_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("evaluate_distribution", avail_methods))
    }) %>%
  purrr::map(
    .f = ~ evaluate_distribution(.x)
  )

test_that("distribution always return the correct object", {
  all_evals <- dist_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

## Time Distribution ====

dist_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("evaluate_time_distribution", avail_methods))
    }) %>%
  purrr::map(
    .f = ~ evaluate_time_distribution(.x)
  )

test_that("time distribution always return the correct object", {
  all_evals <- dist_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

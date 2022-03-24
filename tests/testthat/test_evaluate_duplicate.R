`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

## Duplication ====

dup_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("duplicate", avail_methods))
    }) %>%
  purrr::map(
    .f = ~ evaluate_duplicate(.x)
  )

test_that("duplicate always return the correct object", {
  all_evals <- dup_all %>%
    purrr::map_lgl(~ is_event_evaluation(.x))
  expect_true(all(all_evals))
})

## Look for the 9 events that have been intentionally places as duplicates

test_that("duplicate events have been found", {
  expect_equal(
      dup_all %>%
      purrr::discard(~ nrow(.x) == 0) %>%
      length(),
      9L)
})


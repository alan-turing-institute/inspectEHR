`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)
ref <- make_reference(.debug = TRUE)

ms <- evaluate_global_missingness(core_tbl = core, reference_tbl = ref)

test_that("missingness always return the correct object", {
  expect_true(is_event_missingness(ms))
})

not_in_test_data <- sort(setdiff(.variables$code_name, unique(.events$code_name)))

test_that("global missingness identified correctly", {
  expect_identical(sort(unique(ms$code_name)), not_in_test_data)
})

extract_all <- unique(.events$code_name) %>%
  purrr::map(~ extract(core_table = core, code_name = .x))

names(extract_all) <- unique(.events$code_name)

ms_all <- extract_all %>%
  purrr::discard(
    .p = function(x) {
      avail_methods <- methods(class = class(x)[1])
      !any(grepl("local_missingness", avail_methods))
    }) %>%
  purrr::map(~ evaluate_local_missingness(.x, ref))

test_that("local missingness returns correct object", {
  all_evals <- ms_all %>%
    purrr::map_lgl(~ is_event_missingness(.x))
  expect_true(all(all_evals))
})

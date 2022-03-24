core <- make_core(.debug = TRUE)
int_2d <- extract(core, code_name = "NIHR_HIC_ICU_0108") # integer-2d

test_that("Basic KS Test works", {
  df <- ks_test(int_2d)
  expect_true(tibble::is_tibble(df))
  expect_identical(names(df), c("site_a", "site_b", "statistic"))
})

test_that("KS Test works on time of contribution", {
  df <- ks_test(int_2d, col_name = "datetime")
  expect_true(tibble::is_tibble(df))
  expect_identical(names(df), c("site_a", "site_b", "statistic"))
})

test_that("ks always return the correct object", {
  extract_all <- unique(.events$code_name) %>%
    purrr::map(~ extract(core_table = core, code_name = .x))

  names(extract_all) <- unique(.events$code_name)

  ks_all <- extract_all %>%
    purrr::discard(~ !is.numeric(.x$value)) %>%
    purrr::map(~ ks_test(.x))

  all_evals <- ks_all %>%
    purrr::map_lgl(~ identical(names(.x), c("site_a", "site_b", "statistic")))
  expect_true(all(all_evals))
})

int_2d <- int_2d %>%
  filter(site == "A")

test_that("ks fails safely if only 1 comparitor group", {
  expect_warning(ks_solo <- ks_test(int_2d))
  expect_false(is.null(attr(ks_solo, "code_name")))
  expect_true(tibble::is_tibble(ks_solo))
  expect_identical(names(ks_solo), c("site_a", "site_b", "statistic"))
})

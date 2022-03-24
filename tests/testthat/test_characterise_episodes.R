episodes <- characterise_episodes(.debug = TRUE)
ve_episodes <- evaluate_episodes(episodes)
spells <- characterise_spells(ve_episodes)

test_that("Table properties are correct", {
  expect_true(tibble::is_tibble(episodes))
  expect_identical(names(episodes),
               c("episode_id", "nhs_number", "epi_start_dttm",
                 "epi_end_dttm", "outcome", "los_days", "site"))
  expect_true(tibble::is_tibble(attr(episodes, "invalid_records")))
  expect_identical(na.omit(episodes), episodes)

  expect_true(tibble::is_tibble(ve_episodes))
  expect_identical(names(ve_episodes),
                   c("episode_id", "nhs_number", "epi_start_dttm",
                     "epi_end_dttm", "outcome", "los_days", "site"))
  expect_true(tibble::is_tibble(attr(ve_episodes, "invalid_records")))
  expect_identical(na.omit(ve_episodes), ve_episodes)

  expect_true(tibble::is_tibble(spells))
  expect_identical(names(spells),
                   c("spell_id", "episode_id", "nhs_number", "site",
                     "epi_start_dttm", "epi_end_dttm", "los_days"))
  expect_identical(na.omit(spells), spells)
})



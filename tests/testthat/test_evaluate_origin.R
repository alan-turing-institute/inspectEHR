`%>%` <- magrittr::`%>%`

core <- make_core(.debug = TRUE)
epi <- characterise_episodes(.debug = TRUE)
df <- extract(core, code_name = "NIHR_HIC_ICU_0108")

ori <- evaluate_origin(df, epi)

test_that("origins always return the correct object", {
  expect_true(is_event_evaluation(ori))
})

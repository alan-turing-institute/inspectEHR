test_that("merge_datetime", {

  df1 <- tibble::tibble(
    this_date = rep("2020-10-30", 5),
    this_time = rep("12:00:00", 5)
  ) %>%
    mutate(dttm = merge_datetime(this_date, this_time))

  expect_true(tibble::is_tibble(df1))
  expect_identical(names(df1), c("this_date", "this_time", "dttm"))
  expect_identical(class(df1$dttm), c("POSIXct", "POSIXt"))

  df2 <- tibble::tibble(
    this_date = c("2020-10-30", NA),
    this_time = c(NA, "12:00:00")
  ) %>%
    mutate(dttm = merge_datetime(this_date, this_time))

  expect_true(tibble::is_tibble(df2))
  expect_identical(names(df2), c("this_date", "this_time", "dttm"))
  expect_identical(class(df2$dttm), c("POSIXct", "POSIXt"))
  expect_true(all(is.na(df2$dttm)))

  df3 <- tibble::tibble(
    this_date = rep(as.Date("2020-10-30"), 5),
    this_time = hms::as_hms(rep("12:00:00", 5))
  ) %>%
    mutate(dttm = merge_datetime(this_date, this_time))

  expect_true(tibble::is_tibble(df3))
  expect_identical(names(df3), c("this_date", "this_time", "dttm"))
  expect_identical(class(df3$dttm), c("POSIXct", "POSIXt"))

  df4 <- tibble::tibble(
    this_date = c(as.Date("2020-10-30"), as.Date(NA)),
    this_time = c(hms::as_hms(NA), hms::as_hms("12:00:00"))
  ) %>%
    mutate(dttm = merge_datetime(this_date, this_time))

  expect_true(tibble::is_tibble(df4))
  expect_identical(names(df4), c("this_date", "this_time", "dttm"))
  expect_identical(class(df4$dttm), c("POSIXct", "POSIXt"))
  expect_true(all(is.na(df4$dttm)))

})


test_that("parse_range works", {

  x <- purrr::map(
    c("[0, 5]", "(0, 5]", "[0, 5)", "(0, 5)"),
    ~ parse_range(.))

  expect_identical(x[[1]]$lims, c(0, 5))
  expect_identical(x[[2]]$lims, c(0, 5))
  expect_identical(x[[3]]$lims, c(0, 5))
  expect_identical(x[[4]]$lims, c(0, 5))

  expect_identical(x[[1]]$lower_lim, `>=`)
  expect_identical(x[[2]]$lower_lim, `>`)
  expect_identical(x[[3]]$lower_lim, `>=`)
  expect_identical(x[[4]]$lower_lim, `>`)

  expect_identical(x[[1]]$upper_lim, `<=`)
  expect_identical(x[[2]]$upper_lim, `<=`)
  expect_identical(x[[3]]$upper_lim, `<`)
  expect_identical(x[[4]]$upper_lim, `<`)

  x <- purrr::map(
    c("[Inf, Inf]", "(-inf, inf]", "[0, Inf)", "(Inf, 5)"),
    ~ parse_range(.))

  expect_identical(x[[1]]$lims, c(-Inf, Inf))
  expect_identical(x[[2]]$lims, c(-Inf, Inf))
  expect_identical(x[[3]]$lims, c(0, Inf))
  expect_identical(x[[4]]$lims, c(-Inf, 5))

  expect_identical(x[[1]]$lower_lim, `>=`)
  expect_identical(x[[2]]$lower_lim, `>`)
  expect_identical(x[[3]]$lower_lim, `>=`)
  expect_identical(x[[4]]$lower_lim, `>`)

  expect_identical(x[[1]]$upper_lim, `<=`)
  expect_identical(x[[2]]$upper_lim, `<=`)
  expect_identical(x[[3]]$upper_lim, `<`)
  expect_identical(x[[4]]$upper_lim, `<`)

  expect_error(parse_range(c(3, 5)))
  expect_error(parse_range("{3, 5}"))

})


test_that("Base calendar works", {
  ref <- make_reference(.debug = TRUE)
  base_cal <- make_base_calendar(ref, "day")

  expect_true(tibble::is_tibble(base_cal))
  expect_identical(names(base_cal), c("site", "date"))
  expect_identical(class(base_cal$site), "character")
  expect_identical(class(base_cal$date), "Date")

  base_cal <- make_base_calendar(ref, "month")

  expect_true(tibble::is_tibble(base_cal))
  expect_identical(names(base_cal), c("site", "year", "month"))
  expect_identical(class(base_cal$site), "character")
  expect_identical(class(base_cal$year), "integer")
  expect_identical(class(base_cal$month), "integer")
})

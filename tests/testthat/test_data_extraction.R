core <- make_core(.debug = TRUE)

str_1d <- extract(core_table = core, code_name = "NIHR_HIC_ICU_0073") # string-1d
int_1d <- extract(core, code_name = "NIHR_HIC_ICU_0060") # integer-1d
dbl_1d <- extract(core, code_name = "NIHR_HIC_ICU_0017") # Height - real-1d
dt_1d <- extract(core, code_name = "NIHR_HIC_ICU_0033") # DoB - date-1d
tm_1d <- extract(core, code_name = "NIHR_HIC_ICU_0043") # Deathtime - time-1d
dttm_1d <- extract(core, code_name = "NIHR_HIC_ICU_0411") # Admission - datetime-1d
int_2d <- extract(core, code_name = "NIHR_HIC_ICU_0108") # integer-2d
dbl_2d <- extract(core, code_name = "NIHR_HIC_ICU_0116") # cvp real-2d
str_2d <- extract(core, code_name = "NIHR_HIC_ICU_0126") # airway string-2d

test_that("core table forms correctly", {
  expect_identical(names(core), c("episode_id", "nhs_number", "start_date",
                           "provenance", "filename", "date_created", "version",
                           "date_parsed", "site", "theme", "notes", "code_name",
                           "string", "string2", "string3", "datetime", "date",
                           "time", "real", "integer", "integer2", "event_id"))
})

test_that("events are extracted to the correct data type", {
  expect_identical(class(str_1d$value), "character")
  expect_identical(class(int_1d$value), "integer")
  expect_identical(class(dbl_1d$value), "numeric")
  expect_identical(class(dt_1d$value), "Date")
  expect_identical(class(tm_1d$value), c("hms", "difftime"))
  expect_identical(class(dttm_1d$value), c("POSIXct", "POSIXt"))
  expect_identical(class(str_2d$value), "character")
  expect_identical(class(int_2d$value), "integer")
  expect_identical(class(dbl_2d$value), "numeric")
})

test_that("events carry the correct class", {
  expect_true(tibble::is_tibble(str_1d))
  expect_true(tibble::is_tibble(int_1d))
  expect_true(tibble::is_tibble(dbl_1d))
  expect_true(tibble::is_tibble(dt_1d))
  expect_true(tibble::is_tibble(tm_1d))
  expect_true(tibble::is_tibble(dttm_1d))
  expect_true(tibble::is_tibble(str_2d))
  expect_true(tibble::is_tibble(int_2d))
  expect_true(tibble::is_tibble(dbl_2d))
  expect_true(class(str_1d)[1] == "string_1d")
  expect_true(class(int_1d)[1] == "integer_1d")
  expect_true(class(dbl_1d)[1] == "real_1d")
  expect_true(class(dt_1d)[1] == "date_1d")
  expect_true(class(tm_1d)[1] == "time_1d")
  expect_true(class(dttm_1d)[1] == "datetime_1d")
  expect_true(class(str_2d)[1] == "string_2d")
  expect_true(class(int_2d)[1] == "integer_2d")
  expect_true(class(dbl_2d)[1] == "real_2d")
})

test_that("metadata is extracted correctly", {
  expect_identical(
    names(dbl_2d),
    c("episode_id", "event_id", "site", "code_name", "datetime",
      "value", "meta_1"))
})

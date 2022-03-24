#' @title Evaluate (global) event missingness
#'
#' @param core_tbl a core table returned from [make_core()]
#' @param reference_tbl a reference table returned from [make_reference()]
#'
#' @importFrom dplyr left_join full_join filter select distinct collect mutate
#'   pull if_else
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble add_column
#'
#' @return a tibble with (potential) error codes ready for import into the
#'   `events_missing` table.
#' @export
#' @md
evaluate_global_missingness <- function(core_tbl = NULL, reference_tbl = NULL) {
  # Capture all events, and ascertain where they came from
  unique_events <- core_tbl %>%
    distinct(.data$site, .data$code_name) %>%
    collect() %>%
    add_column(contributed = TRUE, .after = TRUE)

  all_sites <- unique(unique_events$site)

  # Make a new container with with codes replicated for each site
  all_events <- expand_grid(
    site = all_sites,
    code_name = qref$code_name)

  failures <- full_join(x = all_events, y = unique_events,
    by = c(
      "site" = "site",
      "code_name" = "code_name")) %>%
  mutate(
    across(
      .data$contributed,
      ~ if_else(is.na(.x), FALSE, .x))) %>%
  filter(!contributed)

  base_cal <- make_base_calendar(reference_tbl, resolution = "month")

  failures <- full_join(
    failures, base_cal, by = "site"
  ) %>%
    select(
      .data$site,
      .data$code_name,
      .data$year,
      .data$month
    )

  create_missing_log(
    failures,
    eval_code = "VE_CP_02",
    description = "Non temporal missing data pattern in data item"
  )

}


#' @title Evaluate (local) missingness
#'
#' @details Checks to ensure that long term data item contribution is
#'   consistent. This evaluation is performed because sometimes data pipelines
#'   in contributing hospitals change resulting in a silently disruption to data
#'   contribution. As such, some events that were previously contributed
#'   disappear without warning.
#'
#' @template param-x
#' @param reference_tbl reference table returned from [make_reference()]
#'
#' @return a tibble with (potential) error codes ready for import into the
#'   `events_missing` table.
#' @export
#' @md
evaluate_local_missingness <- function(x = NULL, reference_tbl = NULL) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_local_missingness", x)
}

#' @importFrom rlang warn
#' @importFrom glue glue
#' @export
evaluate_local_missingness.default <- function(x = NULL, reference_tbl = NULL, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_local_missingness methods applicable for {code_name} ({this_class})"))
  return(start_missing())
}

#' @export
evaluate_local_missingness.integer_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.string_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.real_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.date_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.time_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.datetime_1d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_1d(x = x, reference_tbl = reference_tbl)
}


#' @importFrom dplyr left_join select mutate group_by summarise n_distinct
#' @importFrom lubridate as_date floor_date ceiling_date year month
#' @importFrom tidyr nest unnest replace_na
#' @importFrom purrr map
#' @export
evaluate_local_missingness_1d <- function(x, reference_tbl) {

  # Check number of events per site per day
  site_monthly_events <- x %>%
    left_join(
      reference_tbl %>% select(-.data$site),
      by = "episode_id") %>%
    group_by(
      .data$site,
      month = as.integer(lubridate::month(.data$start_date)),
      year = as.integer(lubridate::year(.data$start_date))) %>%
    tally(name = "event_count") %>%
    ungroup()

  # Create a running calendar that starts with the first episode
  # and ends with with last episode
  base_cal <- make_base_calendar(reference_tbl, resolution = "month")

  failures <- full_join(
    base_cal, site_monthly_events,
    by = c("site", "year", "month")) %>%
    filter(is.na(.data$event_count)) %>%
    select(
      .data$site,
      .data$year,
      .data$month) %>%
    add_column(code_name = attr(x, "code_name"), .after = TRUE)

  create_missing_log(
    failures = failures,
    eval_code = "VE_CP_04",
    description = "Temporal missing data pattern in data item")
}

#' @export
evaluate_local_missingness.integer_2d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_2d(x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.string_2d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_2d(x, reference_tbl = reference_tbl)
}

#' @export
evaluate_local_missingness.real_2d <- function(x, reference_tbl) {
  x <- evaluate_local_missingness_2d(x, reference_tbl = reference_tbl)
}


#' @importFrom lubridate as_date floor_date ceiling_date
#' @importFrom dplyr mutate group_by summarise n_distinct left_join filter tally arrange
#' @importFrom tidyr nest unnest replace_na
#' @importFrom purrr map
#' @export
evaluate_local_missingness_2d <- function(x, reference_tbl) {

  # Daily event count by site
  site_monthly_events <- x %>%
    mutate(date = as_date(datetime)) %>%
    group_by(
      site,
      year = as.integer(lubridate::year(datetime)),
      month = as.integer(lubridate::month(datetime))) %>%
    tally(name = "event_count") %>%
    ungroup()

  # Create a running calendar that starts with the first episode
  # and ends with with last episode
  base_cal <- make_base_calendar(reference_tbl, resolution = "month")

  failures <- full_join(
    base_cal, site_monthly_events,
    by = c("site", "year", "month")) %>%
    filter(is.na(.data$event_count)) %>%
    select(
      .data$site,
      .data$year,
      .data$month) %>%
    add_column(code_name = attr(x, "code_name"), .after = TRUE)

  create_missing_log(
    failures = failures,
    eval_code = "VE_CP_04",
    description = "Temporal missing data pattern in data item")

}

#' @title Evaluate a comparison between events
#'
#' @details Checks the relationship between two events of interest. This takes
#' two forms depending upon whether or not the data items are of type 1d or 2d.
#' In all cases, the function evaluates the functional relationship of `event_1`
#' `test_relationship` `event_2`, where `test_relationship` acts as an operator
#' between the two.
#'
#' ## 1-dimensional events (time invariant)
#' These items are checked without reference to any timepoint.
#'
#' ## 2-dimensional events (time series)
#' These items are checked with respect to their timestamp, which is used to
#' identify co-incident events that are suitable for comparison.
#'
#' ### Examples of comparisons
#' * systolic blood pressure is always above diastolic
#' * mean blood pressure is always below systolic
#' * all date time pairings exist for date and time of:
#'     - death
#'     - withdrawal
#'     - ready for discharge
#'     - brain stem death
#'
#' @param event_1 an extracted event returned by [extract()]. This event will
#'   be placed on the *left* hand side of the operator defined by the
#'   `relationship` argument.
#' @param event_2 an extracted event returned by [extract()]. This event will
#'   be placed on the *right* hand side of the operator defined by the
#'   `relationship` argument.
#' @param relationship character vector length 1 describing the type of
#'   relationship to assert:
#'   * exists: both events exist at the same time (2d) or for the same
#'     episode(1d)
#'   * not_exists: both events do not exist at the same time (2d) or for the
#'     same episode(1d)
#'   * "==": event_1 == event_2
#'   * "!=": event_1 != event_2
#'   * ">": event_1 > event_2
#'   * ">=": event_1 >= event_2
#'   * "<": event_1 < event_2
#'   * "<=": event_1 <= event_2
#'
#' @return
#' @export
#' @md
#'
#' @seealso [extract()] [perform_evaluation()]
evaluate_comparison <- function(event_1 = NULL,
                                event_2 = NULL,
                                relationship = c("exists",
                                                 "not_exists",
                                                 "==",
                                                 "!=",
                                                 ">",
                                                 ">=",
                                                 "<",
                                                 "<=")) {

  if (is.null(event_1) | is.null(event_2)) {
    warn("You must supply 2 extracted events returned from `inspectEHR::extract()`")
    return(start_evaluation())
  }

  code_1 <- attr(event_1, "code_name")
  code_2 <- attr(event_2, "code_name")

  data_type_1 <- qref$type[qref$code_name == code_1]
  data_type_2 <- qref$type[qref$code_name == code_2]

  if (data_type_1 != data_type_2) {
    abort("These data items are not of the same type (1d/2d)")
  }

  if (data_type_1 == 1) {
    prep_data <- full_join(
      event_1[,c("episode_id", "event_id", "value")],
      event_2[,c("episode_id", "event_id", "value")],
      by = "episode_id")
  } else {
    prep_data <- full_join(
      event_1[,c("episode_id", "event_id", "datetime", "value")],
      event_2[,c("episode_id", "event_id", "datetime", "value")],
      by = c("episode_id", "datetime"))
  }

  operator <- match.arg(relationship)

  response <- operator %>%
    base::switch(
      "exists" = compare_exists(x = prep_data),
      "not_exists" = compare_not_exists(x = prep_data),
      "==" = compare_operator(x = prep_data, operator = `==`),
      "!=" = compare_operator(x = prep_data, operator = `!=`),
      ">=" = compare_operator(x = prep_data, operator = `>=`),
      ">" = compare_operator(x = prep_data, operator = `>`),
      "<=" = compare_operator(x = prep_data, operator = `<=`),
      "<" = compare_operator(x = prep_data, operator = `<`)
    )

  df1 <- event_1 %>%
      filter(.data$event_id %in% response$event_id)

  if (nrow(df1) > 0) {
    df1 <- df1 %>%
      select(
        .data$site,
        .data$episode_id,
        .data$event_id,
        .data$code_name,
        .data$value) %>%
      mutate(value = as.character(.data$value))
  } else {
    df1 <- tibble(
      site = as.character(NULL),
      episode_id = as.integer(NULL),
      event_id = as.integer(NULL),
      code_name = as.character(NULL),
      value = as.character(NULL)
    )
  }

  df2 <- event_2 %>%
    filter(.data$event_id %in% response$event_id)

  if (nrow(df2) > 0) {
    df2 <- df2 %>%
      select(
        .data$site,
        .data$episode_id,
        .data$event_id,
        .data$code_name,
        .data$value) %>%
      mutate(value = as.character(.data$value))
  } else {
    df2 <- tibble(
      site = as.character(NULL),
      episode_id = as.integer(NULL),
      event_id = as.integer(NULL),
      code_name = as.character(NULL),
      value = as.character(NULL)
    )
  }

  failures <- bind_rows(df1, df2)

  if (nrow(failures) > 0) {
    failures <- failures %>%
      mutate(
        eval_code = "VE_AP_01",
        description = "Two or more events not not obey a logical constraint"
      )
    return(failures)
  } else {
    return(start_evaluation())
  }

}

compare_not_exists <- function(x) {

  x %>%
    filter(!is.na(.data$value.x) & !is.na(.data$value.y)) %>%
    select(contains("event_id")) %>%
    pivot_longer(everything()) %>%
    select(event_id = .data$value)

}


compare_exists <- function(x) {

  x %>%
    filter(
      (is.na(.data$value.x) & !is.na(.data$value.y)) |
      (!is.na(.data$value.x) & is.na(.data$value.y))
    ) %>%
    select(contains("event_id")) %>%
    pivot_longer(everything()) %>%
    select(event_id = .data$value)

}

compare_operator <- function(x, operator) {

  x %>%
    filter(!operator(.data$value.x, .data$value.y)) %>%
    select(contains("event_id")) %>%
    pivot_longer(cols = contains("event_id")) %>%
    select(event_id = .data$value)

}


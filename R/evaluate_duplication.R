#' @title Evaluate event duplication
#'
#' @details Evaluates events as being suspicious for being duplicated or not.
#' This is trained on the datetime of an object (if 2d), and looks for two
#' events that are perfectly co-incident. 1d events are checked for raw
#' duplication, since they should only occur once per episode anyway. Note, when
#' 2 values are deemed to be duplicates, the one that first appears in the
#' database is cleared as verified, while the second (or more) is/are not.
#'
#' @template param-x
#' @template return-event-quality
#'
#' @export
evaluate_duplicate <- function(x = NULL) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_duplicate", x)
}

#' @importFrom rlang warn
#' @export
evaluate_duplicate.default <- function(x) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_duplicate methods applicable for:{code_name}, class{this_class}"))
  return(start_evaluation())
}


#' @importFrom dplyr ungroup distinct mutate select right_join mutate_at if_else
#' @importFrom rlang .data
#' @importFrom lubridate round_date
#' @export
evaluate_duplicate_2d <- function(x) {

  failures <- x %>%
    ungroup() %>%
    distinct(
      .data$episode_id,
      .data$datetime,
      .data$value,
      .keep_all = TRUE
    ) %>%
    mutate(duplicate = 0L) %>%
    select(.data$event_id, .data$duplicate) %>%
    right_join(x, by = "event_id") %>%
    mutate(duplicate = if_else(
      is.na(.data$duplicate), TRUE, FALSE)
    ) %>%
    filter(.data$duplicate) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_UP_02",
    "Event is likely a duplicate"
  )
}

#' @export
evaluate_duplicate.real_2d <- function(x) {
  evaluate_duplicate_2d(x)
}

#' @export
evaluate_duplicate.integer_2d <- function(x) {
  evaluate_duplicate_2d(x)
}

#' @export
evaluate_duplicate.string_2d <- function(x) {
  evaluate_duplicate_2d(x)
}

#' @export
evaluate_duplicate_1d <- function(x) {

  failures <- x %>%
    ungroup() %>%
    distinct(
      .data$episode_id,
      .data$value,
      .keep_all = TRUE
    ) %>%
    mutate(duplicate = 0L) %>%
    select(.data$event_id, .data$duplicate) %>%
    right_join(x, by = "event_id") %>%
    mutate(duplicate = if_else(is.na(.data$duplicate), TRUE, FALSE)) %>%
    filter(.data$duplicate) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_UP_02",
    "Event is likely a duplicate"
  )
}

#' @export
evaluate_duplicate.real_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

#' @export
evaluate_duplicate.integer_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

#' @export
evaluate_duplicate.string_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

#' @export
evaluate_duplicate.datetime_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

#' @export
evaluate_duplicate.date_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

#' @export
evaluate_duplicate.time_1d <- function(x) {
  evaluate_duplicate_1d(x)
}

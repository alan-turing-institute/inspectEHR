#' @title Evaluate event periodicity
#'
#' @details Evaluates the periodicity of a data item. This is the frequency of
#' events submitted. Only defined for 2d data.
#'
#' A potential flaw of the current approach is that patients who undergo large
#' breaks from treatments during a prolonged ICU stay may erroneously flag via
#' this method (e.g. propofol). As such, quite conservative limits are typically
#' applied so as to reduce false positive findings. This flag should be taken as
#' a light warning.
#'
#' @seealso [evaluate_coverage()], [parse_periodicity()]
#' @family evaluations
#'
#' @template param-x
#' @param periodicity the limits of anticipated periodicity given as a character
#'   vector of length 1 in the form "(1, 6)" where the numbers are comma
#'   separated and represent the lower and upper bounds respectively. The
#'   numbers reflect the limits of the anticipated periodicity in the number of
#'   events expected in an average 24 hour period on ICU. Round and square
#'   brackets are used for non-inclusive and inclusive limits respectively.
#'
#' @template return-event-quality
#' @md
#' @export
evaluate_periodicity <- function(x = NULL, periodicity = NULL) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_periodicity", x)
}


#' @importFrom glue glue
#' @importFrom rlang warn
#' @export
evaluate_periodicity.default <- function(x, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_periodicity methods applicable for:{code_name}, class{this_class}"))
  return(start_evaluation())
}


#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr left_join select filter mutate if_else
evaluate_periodicity_2d <- function(x = NULL, periodicity = NULL) {

  code_name <- attr(x, "code_name")

  if (is.null(periodicity)) {
    lims <- parse_periodicity(qref$periodicity[qref$code_name == code_name])
  } else {
    lims <- parse_periodicity(periodicity)
  }

  failures <- x %>%
    group_by(.data$episode_id) %>%
    mutate(count = n()) %>%
    filter(.data$count < 2) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  failures <- x %>%
    group_by(.data$episode_id) %>%
    mutate(count = n()) %>%
    filter(.data$count >= 2) %>%
    mutate(next_time = c(.data$datetime[2:n()], as.POSIXct(NA))) %>%
    mutate(gap = as.numeric(difftime(next_time, datetime, units = "hours"))) %>%
    mutate(periodicity = 24/gap) %>%
    ungroup() %>%
    filter(!is.na(.data$periodicity)) %>%
    filter(
      !lims$lower_lim(.data$periodicity, lims$lims[1]) |
      !lims$upper_lim(.data$periodicity, lims$lims[2])) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value) %>%
    bind_rows(failures)

  create_failure_log(
    failures,
    "VE_TP_05",
    "Events occur outside anticipated patient level periodicity"
  )
}

#' @export
evaluate_periodicity.real_2d <- function(x = NULL) {
  evaluate_periodicity_2d(x = x)
}

#' @export
evaluate_periodicity.integer_2d <- function(x = NULL) {
  evaluate_periodicity_2d(x = x)
}

#' @export
evaluate_periodicity.string_2d <- function(x = NULL) {
  evaluate_periodicity_2d(x = x)
}

#' @title Evaluate event temporal boundaries
#'
#' @details Evaluates events for being outside the time boundaries of an
#' associated episode. This is not necessarily a problem, for example,
#' microbiology data could be returned after the patient has died or left the
#' ICU. However, it is often demonstrative of a bigger problem in the way in
#' which data has been exported from the source hospital. For example, data is
#' sometimes contributed from more than one episode, and duplicated across the
#' episodes.
#'
#' @template param-x
#' @template param-los-table
#'
#' @template return-event-quality
#'
#' @export
#' @importFrom rlang abort
evaluate_bounds <- function(x = NULL, los_table = NULL) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_bounds", x)
}

#' @importFrom rlang warn
#' @export
evaluate_bounds.default <- function(x = NULL, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_bounds methods applicable for {code_name} ({this_class})"))
  return(start_evaluation())
}

#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr left_join select filter mutate if_else
evaluate_bounds_2d <- function(x = NULL, los_table = NULL) {
  if (is.null(los_table)) abort("You must supply an episode length table.")

  failures <- x %>%
    left_join(
      los_table[,c("episode_id", "epi_start_dttm", "epi_end_dttm")],
      by = "episode_id") %>%
    mutate(out_of_bounds = if_else(
      .data$datetime < .data$epi_start_dttm | .data$datetime > .data$epi_end_dttm, TRUE, FALSE)) %>%
    filter(.data$out_of_bounds) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_TP_03",
    "Events occur outside the timespan of an episode"
  )

}

#' @export
evaluate_bounds.real_2d <- function(x = NULL, los_table = NULL) {
  evaluate_bounds_2d(x = x, los_table = los_table)
}

#' @export
evaluate_bounds.integer_2d <- function(x = NULL, los_table = NULL) {
  evaluate_bounds_2d(x = x, los_table = los_table)
}

#' @export
evaluate_bounds.string_2d <- function(x = NULL, los_table = NULL) {
  evaluate_bounds_2d(x = x, los_table = los_table)
}

#' @title Evaluate event origins
#'
#' @details evaluates that an event originates in a valid episode
#'
#' @template param-x
#' @template param-los-table
#'
#' @template return-event-quality
#' @export
#' @md
evaluate_origin <- function(x = NULL, los_table = NULL) {
  failures <- attr(los_table, "invalid_records") %>%
    distinct(.data$episode_id) %>%
    inner_join(x, by = "episode_id") %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_RC_04",
    "Event originates in episode failing quality evaluation"
  )
}

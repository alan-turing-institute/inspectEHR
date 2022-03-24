#' @title Evaluate metadata
#'
#' @details Checks to see if metadata is present. This does not (as yet) check
#' that the metadata is correct, only that it exsists.
#'
#' @importFrom dplyr filter_at any_vars select vars
#' @importFrom tidyselect contains
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @template param-x
#' @template return-event-quality
#' @export
evaluate_metadata <- function(x) {

  if (any(grepl(pattern = "meta", names(x)))) {
    failures <- x %>%
      filter_at(vars(contains("meta")), any_vars(is.na(.))) %>%
      select(
        .data$site,
        .data$episode_id,
        .data$event_id,
        .data$code_name,
        .data$value)

    create_failure_log(
      failures,
      "VE_CP_05",
      "Metadata is absent"
    )
  } else {
    start_evaluation()
  }

}

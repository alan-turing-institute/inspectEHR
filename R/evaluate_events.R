#' @title Apply all quality evaluations to CC-HIC events
#'
#' @details Applies appropriate data quality evaluation flags to an extracted
#'   dataitem. The evaluation functions that operate at the event level are
#'   designed to perform a single simple check on a standardised object (the
#'   table returned by [extract()]) and return a standardised table
#'   detailing the results of that check. Each check can be run independently if
#'   so desired.
#'
#' @template param-x
#' @template param-los-table
#' @param range character string denoting the reference range to be used in
#'   overriding the default reference range. A string is used as the form "\[0,
#'   5\]" and "(0, 5)" are used to denote the type of limits to apply.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! abort
#' @importFrom tibble as_tibble
#'
#' @template return-event-quality
#' @export
#' @md
evaluate_events <- function(x = NULL,
                            los_table = NULL,
                            reference_tbl = NULL,
                            range = NULL) {

  if (is.null(los_table)) abort("You must supply an episode table")
  # Aborts function if the class is not recognised
  if (!is_extract(x)) abort("this function is not defined for this class")

  if (nrow(x) == 0) {

    # If the table is empty, we can save time and return an empty evaluation
    return(start_evaluation())

  } else {

    # Check available methods for this class
    avail_methods <- methods(class = class(x)[1])
    event_class <- class(x)[1]

    failing <- start_evaluation()

    ## Check event originates in an episode that has passed quality evaluation
    failing <- evaluate_origin(x, los_table = los_table) %>%
      bind_rows(failing)

    # Ranges
    if (any(grepl("evaluate_range", avail_methods))) {
      failing <- evaluate_range(x, range = range) %>%
        bind_rows(failing)
    }

    # Boundaries
    if (any(grepl("evaluate_bounds", avail_methods))) {
      failing <- evaluate_bounds(x, los_table = los_table) %>%
        bind_rows(failing)
    }

    # Duplicates
    if (any(grepl("evaluate_duplicate", avail_methods))) {
      failing <- evaluate_duplicate(x) %>%
        bind_rows(failing)
    }

    # Periodicity
    if (any(grepl("evaluate_periodicity", avail_methods))) {
      failing <- evaluate_periodicity(x) %>%
        bind_rows(failing)
    }

    # # Metadata
    failing <- evaluate_metadata(x) %>%
      bind_rows(failing)

    # Distributions
    if (any(grepl("evaluate_distribution", avail_methods))) {
      failing <- evaluate_distribution(x) %>%
        bind_rows(failing)
    }

    # Time Distributions
    if (any(grepl("evaluate_time_distribution", avail_methods))) {
      failing <- evaluate_time_distribution(x) %>%
        bind_rows(failing)
    }

    return(as_tibble(failing))
  }

}


start_evaluation <- function(x) {
  tibble(
    site = as.character(NULL),
    episode_id = as.integer(NULL),
    event_id = as.integer(NULL),
    code_name = as.character(NULL),
    value = as.character(NULL),
    eval_code = as.character(NULL),
    description = as.character(NULL)
  )
}


start_missing <- function(x) {
  tibble(
    site = as.character(NULL),
    code_name = as.character(NULL),
    year = as.integer(NULL),
    month = as.integer(NULL),
    eval_code = as.character(NULL),
    description = as.character(NULL)
  )
}

#' @title Create a failure log
#'
#' @details Creates a failure log ready to be imported into the `events_quality`
#'   table. This ensures there is a standardised format in use and returns an
#'   empty table should there not be any failures.
#'
#' @param failures a table (possible with nrow == 0 if there are no errors) with
#'   the following columns: `site`, `episode_id`, `event_id`, `code_name`,
#'   `value`. If not already done so, the `value` column will be converted to
#'   a string representation.
#' @param eval_code character vector length 1 with the data quality evaluation
#'   code
#' @param description character vector length 1 with a standardised text
#'   description of the code in use
#'
#' @return
#' @export
#' @md
create_failure_log <- function(failures, eval_code, description) {
  if (nrow(failures) > 0) {
    failures %>%
      select(
        .data$site,
        .data$episode_id,
        .data$event_id,
        .data$code_name,
        .data$value) %>%
      mutate(across(.data$value, ~ as.character(.x))) %>%
      mutate(
        eval_code = eval_code,
        description = description
      )
  } else {
    start_evaluation()
  }
}


#' @title Create a missing event log
#'
#' @details Creates a missing event log ready to be imported into the
#'   `events_missing` table. This ensures there is a standardised format in use
#'   and returns an empty table should there not be any failures.
#'
#' @param failures a table (possible with nrow == 0 if there are no errors) with
#'   the following columns: `site` and `code_name`. Each corresponding to a site
#'   that does not contribute any data for the specified code.
#' @param eval_code character vector length 1 with the data quality evaluation
#'   code
#' @param description character vector length 1 with a standardised text
#'   description of the code in use
#'
#' @return
#' @export
create_missing_log <- function(failures, eval_code, description) {
  if (nrow(failures) > 0) {
    failures %>%
      select(
        .data$site,
        .data$code_name,
        .data$year,
        .data$month
      ) %>%
      mutate(
        eval_code = eval_code,
        description = description
      )
  } else {
    start_missing()
  }
}

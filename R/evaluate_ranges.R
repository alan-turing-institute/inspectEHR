#' @title Evaluate event ranges
#'
#' @details Evaluates events as being out of range in the broadest possible
#'   sense. This includes: being numerically out of range, categorical data not
#'   in a predefined set, and codes not conforming to the appropriate standard
#'   (post codes, ICNARC etc). These ranges have been decided based upon ICNARC
#'   reference ranges, prior evidence (usually case report for exceptional
#'   values) or expert opinion. Reference ranges are all found in in the [qref]
#'   and can be overridden by supplying a range to the `range` argument.
#'
#' @template param-x
#' @param range character vector length 1 denoting the reference range to be
#'   used in overriding the default reference range. A string is used as the
#'   form "\[0, 5\]" and "(0, 5)" are used to denote inclusive and exclusive
#'   limits respectively.
#'
#' @return a tibble with (potential) error codes ready for import into the
#'   `events_quality` table.
#' @export
#'
#' @importFrom rlang abort
evaluate_range <- function(x = NULL, range = NULL) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_range", x)
}


#' @importFrom rlang warn
#' @export
evaluate_range.default <- function(x, range = NULL, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_range methods applicable for {code_name} ({this_class})"))
  return(start_evaluation())
}


#' @title Evaluate numeric event ranges
#'
#' @inheritParams evaluate_range
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate case_when select
#' @importFrom rlang .data
evaluate_range_numeric <- function(x = NULL, range = NULL) {

  code_name <- attr(x, "code_name")

  if (is.null(range)) {
    lims <- parse_range(qref$ranges[qref$code_name == code_name])
  } else {
    lims <- parse_range(range)
  }

  failures <- x %>%
    filter(
      !lims$lower_lim(.data$value, lims$lims[1]) |
      !lims$upper_lim(.data$value, lims$lims[2])) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_03",
    "Numeric data falls outside range of possibility"
  )
}

#' @export
evaluate_range.real_2d <- function(x = NULL, range = NULL) {
  evaluate_range_numeric(x = x, range = range)
}

#' @export
evaluate_range.real_1d <- function(x = NULL, range = NULL) {
  evaluate_range_numeric(x = x, range = range)
}

#' @export
evaluate_range.integer_2d <- function(x = NULL, range = NULL) {
  evaluate_range_numeric(x = x, range = range)
}

#' @export
evaluate_range.integer_1d <- function(x = NULL, range = NULL) {
  evaluate_range_numeric(x = x, range = range)
}

#' @title Evaluate string event ranges (set membership)
#'
#' @template param-x
#' @param range character vector containing the valid and complete set from
#'   which the data item values can take. If `NULL` (the default) the values
#'   from `qref$possible_values` are taken.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data !! enquo
#' @importFrom tidyr unnest
evaluate_range_string <- function(x = NULL, range = NULL) {

  .code_name <- attr(x, "code_name")

  # Check to see if we have a solution in the dq_ref
  if (is.null(range)) {
    solutions <- qref %>%
      filter(
        .data$code_name == {{ .code_name }},
        !is.null(.data$possible_values)
      ) %>%
      select(.data$possible_values) %>%
      pull() %>%
      unlist()
  } else {
    solutions <- range
  }

  if (!is.null(solutions) || !all(is.na(solutions))) {
    # This handles the majority of string enumerated cases
    possible_values <- qref %>%
      filter(.data$code_name == {{ .code_name }}) %>%
      select(.data$possible_values) %>%
      unnest(cols = c(.data$possible_values)) %>%
      select(.data$possible_values) %>%
      pull()

    failures <- x %>%
      mutate(
        range_error = case_when(
          is.na(.data$value) ~ as.logical(NA),
          .data$value %in% possible_values ~ FALSE,
          TRUE ~ TRUE
        )
      ) %>%
      filter(.data$range_error) %>%
      select(
        .data$site,
        .data$episode_id,
        .data$event_id,
        .data$code_name,
        .data$value)

    failures <- create_failure_log(
      failures,
      "VE_VC_04",
      "Categorical data not in set"
    )
  } else {

    # NIHR_HIC_ICU_0076 - Post code
    # This evaluates for full post code only
    if (.code_name == "NIHR_HIC_ICU_0076") {
      failures <- evaluate_post_code(x)
    }

    # NIHR_HIC_ICU_0073 - NHS Number
    if (.code_name == "NIHR_HIC_ICU_0073") {
      failures <- evaluate_nhs_number(x)
    }

    # NIHR_HIC_ICU_0399 - Primary Admission Reason
    # NIHR_HIC_ICU_0088 - Secondary Admission Reason
    # NIHR_HIC_ICU_0912 - Ultimate Primary
    # NIHR_HIC_ICU_0074 - Other Conditions in PMHx
    if (.code_name %in% c("NIHR_HIC_ICU_0399",
                         "NIHR_HIC_ICU_0088",
                         "NIHR_HIC_ICU_0912",
                         "NIHR_HIC_ICU_0074")) {
      failures <- evaluate_icnarc(x)
    }

  }

  if (exists("failures")) {
    return(failures)
  } else {
    this_class <- class(x)[1]
    warn(glue("No evaluate_range methods applicable for {.code_name} ({this_class})"))
    start_evaluation()
  }

}

#' @export
evaluate_range.string_2d <- function(x = NULL, range = NULL) {
  evaluate_range_string(x, range = range)
}

#' @export
evaluate_range.string_1d <- function(x = NULL, range = NULL) {
  evaluate_range_string(x, range = range)
}


evaluate_post_code <- function(x) {
  failures <- x %>%
    mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.logical(NA),
        verify_post_code(.data$value) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_01",
    "Value does not conform to external standard"
  )
}

evaluate_nhs_number <- function(x) {
  failures <- x %>%
    dplyr::mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.logical(NA),
        validate_nhs(.data$value) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_01",
    "Value does not conform to external standard"
  )
}

evaluate_icnarc <- function(x) {
  failures <- x %>%
    mutate(
      range_error = case_when(
        is.na(.data$value) ~ as.logical(NA),
        verify_icnarc(.data$value) ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_01",
    "Value does not conform to external standard"
  )
}

#' @title Evaluate date event ranges
#'
#' @details Verifies that all dates are before now (i.e. not in the future) and not
#' before 1900-01-01, which seems reasonable.
#'
#' @template param-x
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
evaluate_range.date_1d <- function(x = NULL, ...) {

  failures <- x %>%
    mutate(
      range_error = if_else(
        .data$value > Sys.Date() | .data$value < as.Date("1900-01-01"), TRUE, FALSE
      )
    ) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_05",
    "Date type data falls outside possible range"
  )
}

#' @title Range Checks - Times
#'
#' @details Verifies that all times are between 00:00:00 and 23:59:59
#'
#' @template param-x
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
#' @importFrom hms as_hms
evaluate_range.time_1d <- function(x = NULL, ...) {

  failures <- x %>%
    mutate(
      range_error = if_else(
        .data$value > as_hms("23:59:59") | .data$value < as_hms("00:00:00"), TRUE, FALSE)) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_05",
    "Date type data falls outside possible range"
  )
}


#' @title Range Checks - Datetimes
#'
#' @details Verifies that all dates are before now (i.e. not in the future) and not
#' before 1900-01-01 00:00:00, which seems reasonable.
#'
#' @template param-x
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr case_when mutate select
evaluate_range.datetime_1d <- function(x = NULL, ...) {

  failures <- x %>%
    mutate(
      range_error = if_else(
        .data$value > Sys.time() | .data$value < as.POSIXct("1900-01-01 00:00:00"), TRUE, FALSE)
    ) %>%
    filter(.data$range_error) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VE_VC_05",
    "Date type data falls outside possible range"
  )
}

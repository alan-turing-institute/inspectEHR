#' @title Pairwise Kolmogorov-Smirnov Distance
#'
#' @details Performs a pairwise comparison of any 2 continuous distributions and
#' returns their KS distance. If the chosen column is of class `POSIXct` then
#' the comparison occurs over the time component only, since the date component
#' is unlikely to yield a useful comparison.
#'
#' @template param-x
#' @param col_name column name for the column of interest (defaults to `value`)
#'
#' @family site level evaluation
#'
#' @export
#' @md
#'
#' @importFrom rlang .data inform
#' @importFrom utils combn
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @importFrom hms as_hms
#' @importFrom tibble tibble
#' @importFrom purrr map2_dbl
ks_test <- function(x, col_name = "value") {

  sites <- unique(x$site)
  site_count <- length(sites)

  if (site_count < 2) {
    warn("Comparison must have 2 or more sites")
    out <- tibble(
      site_a = as.character(NULL),
      site_b = as.character(NULL),
      statistic = as.numeric(NULL))
    attr(out, "code_name") <- attr(x, "code_name")
    return(out)
  }

  site_pairs <- as.data.frame(t(combn(sites, 2)), stringsAsFactors = FALSE) %>%
    as_tibble()
  names(site_pairs) <- c("site_a", "site_b")

  if (lubridate::is.POSIXct(x[[col_name]])) {
    x <- x %>%
      mutate(working_col = as.numeric(as_hms(.data[[col_name]])))
  } else {
    x <- x %>%
      mutate(working_col = as.numeric(.data[[col_name]]))
  }

  out <- site_pairs %>%
    mutate(
      statistic = map2_dbl(
        .x = .data$site_a,
        .y = .data$site_b,
        .f =  function(a, b) {

          suppressWarnings(
            ks.test(
              x$working_col[x$site == a],
              x$working_col[x$site == b]
            )$statistic %>%
            as.numeric()
          )
        }
      )
    )

  attr(out, "code_name") <- attr(x, "code_name")
  return(out)
}

#' @title Evaluate distribution
#'
#' @details Evaluate the distribution between numeric data using the KS distance
#'
#' @template param-x
#' @param col_name column name for the column of interest (defaults to `value`)
#' @param threshold the limit value of the KS distance to consider a "different"
#'   distribution. Default = 0.5
#'
#' @seealso [ks_test()]
#'
#' @template return-event-quality
#' @export
#' @md
evaluate_distribution <- function(x = NULL, col_name = "value", threshold = 0.5) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_distribution", x)
}

#' @importFrom rlang warn
#' @export
evaluate_distribution.default <- function(x = NULL, col_name = "value", threshold = 0.5, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_distribution methods applicable for {code_name} ({this_class})"))
  return(start_evaluation())
}

evaluate_distribution_generic <- function(x = NULL,
                                          col_name = "value",
                                          threshold = 0.5) {

  failures <- ks_test(x = x, col_name = col_name) %>%
    group_by(.data$site_a) %>%
    summarise(
      outlier = all(.data$statistic > threshold)
    ) %>%
    filter(.data$outlier) %>%
    select(site = .data$site_a) %>%
    left_join(x, by = "site") %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VA_AP_01",
    "Values do not share a common distributions across sites"
  )

}

#' @export
evaluate_distribution.real_1d <- function(x = NULL, col_name = "value", threshold = 0.5) {
  evaluate_distribution_generic(x = x, col_name = col_name)
}

#' @export
evaluate_distribution.integer_1d <- function(x = NULL, col_name = "value", threshold = 0.5) {
  evaluate_distribution_generic(x = x, col_name = col_name)
}

#' @export
evaluate_distribution.real_2d <- function(x = NULL, col_name = "value", threshold = 0.5) {
  evaluate_distribution_generic(x = x, col_name = col_name)
}

#' @export
evaluate_distribution.integer_2d <- function(x = NULL, col_name = "value", threshold = 0.5) {
  evaluate_distribution_generic(x = x, col_name = col_name)
}

#' @title Evaluate time distribution
#'
#' @details Evaluate the distribution between time data using the KS distance
#'
#' @template param-x
#' @param col_name column name for the column of interest (defaults to `datetime`)
#' @param threshold the limit value of the KS distance to consider a "different"
#'   distribution. Default = 0.5
#'
#' @seealso [ks_test()]
#'
#' @template return-event-quality
#' @export
#' @md
evaluate_time_distribution <- function(x = NULL, col_name = "datetime", threshold = 0.5) {
  if (is.null(x)) {
    abort("You must supply an extracted data item returned from `inspectEHR::extract()`")
  }
  UseMethod("evaluate_time_distribution", x)
}

#' @importFrom rlang warn
#' @export
evaluate_time_distribution.default <- function(x = NULL, col_name = "datetime", threshold = 0.5, ...) {
  code_name <- attr(x, "code_name")
  this_class <- class(x)[1]
  warn(glue("No evaluate_time_distribution methods applicable for {code_name} ({this_class})"))
  return(start_evaluation())
}

evaluate_time_distribution_generic <- function(x = NULL,
                                          col_name = "value",
                                          threshold = 0.5) {

  failures <- ks_test(x = x, col_name = col_name) %>%
    group_by(.data$site_a) %>%
    summarise(
      outlier = all(.data$statistic > threshold)
    ) %>%
    filter(.data$outlier) %>%
    select(site = .data$site_a) %>%
    left_join(x, by = "site") %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name,
      .data$value)

  create_failure_log(
    failures,
    "VA_TP_01",
    "Values do not share a common temporal distribution across sites"
  )

}


#' @export
evaluate_time_distribution.real_2d <- function(x = NULL, col_name = "datetime", threshold = 0.5) {
  evaluate_time_distribution_generic(x = x, col_name = col_name)
}

#' @export
evaluate_time_distribution.integer_2d <- function(x = NULL, col_name = "datetime", threshold = 0.5) {
  evaluate_time_distribution_generic(x = x, col_name = col_name)
}

#' @export
evaluate_time_distribution.string_2d <- function(x = NULL, col_name = "datetime", threshold = 0.5) {
  evaluate_time_distribution_generic(x = x, col_name = col_name)
}

#' @title Study safely
#'
#' @details Interrogates the CC-HIC database and provides a vector of episode
#'   ids that can "safely" be used for your study based on the data and date
#'   ranges you require. Think of this as an implementation of a venn diagram
#'   for data quality This means that these episodes meet the following
#'   criteria:
#'   * Episodes themselves are valid
#'   * Episodes come from sites that contribute data for the chosen bellweather
#'   fields specified in `code_names`.
#'   * Episodes come from time period where we know data is being contributed.
#'   * Events are of good quality during the time period examined
#'
#' ## Choosing Bellweather Variables
#'
#' It is better to select a small number of important variables to `code_names`.
#' The more variables that are supplied, the lower the likelihood that any
#' "safe" data will be found. The best, and most pragmatic, returns from this
#' function are therefore ones that select a small number (around 10) critical
#' fields that the study would most certainly not be possible without.
#'
#' @importFrom tidyr expand_grid
#' @importFrom dplyr tbl collect filter summarise bind_rows select group_by
#'   mutate distinct anti_join
#' @importFrom magrittr `%>%`
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#'
#' @ctn connection to a study database
#' @param code_names string vector of code_names to be extracted for the study.
#'   These are referred to as "bellweather" variables for the study. It is
#'   likely that if *all* variables for a study are used here, that no safe
#'   areas will be found. It is better to specify a small number of important
#'   variables, that you believe are indicative of overall quality.
#' @param from study starting date
#' @param to study ending date
#'
#' @return
#' @export
#' @md
study_safely <- function(ctn, code_names, from, to) {

  code_names <- unique(code_names)
  outside_code <- all(code_names %in% qref$code_name)

  stopifnot(outside_code)

  stopifnot(
    all(!is.null(code_names), !is.null(from), !is.null(to), !is.null(ctn))
  )

  ps <- tbl(ctn, "dq_score") %>% collect()

  site_valid <- ps %>%
    split(.$site) %>%
    map(~ .x %>%
          filter(contributed == "Yes") %>%
          summarise(
            site = unique(site),
            site_valid = sum(code_name %in% code_names) == length(code_names))) %>%
    bind_rows()

  episodes <- tbl(ctn, "episodes") %>%
    left_join(tbl(ctn, "provenance"), by = c("provenance" = "file_id")) %>%
    select(site, start_date) %>%
    collect() %>%
    group_by(site) %>%
    summarise(
      first = min(as.Date(start_date)),
      last = max(as.Date(start_date))
    ) %>%
    mutate(
      from = lubridate::ymd(from),
      to = lubridate::ymd(to)) %>%
    mutate(first = pmax(first, from),
           last = pmin(last, to)) %>%
    select(site, first, last)

  base_calendar <- episodes %>%
    filter(site %in% site_valid$site[site_valid$site_valid]) %>%
    group_by(site) %>%
    nest(date = c(first, last)) %>%
    mutate(date = map(date, ~ seq.Date(.x$first, .x$last, by = "day"))) %>%
    unnest(date) %>%
    mutate(
      year = year(date),
      month = month(date)) %>%
    ungroup() %>%
    distinct(site, year, month)

  etc <- tbl(ctn, "event_temporal_completeness") %>%
    filter(code_name %in% code_names) %>%
    collect()

  anti_join(base_calendar, etc, by = c("site", "year", "month"))
}

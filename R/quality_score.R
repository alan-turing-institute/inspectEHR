#' @title Data Quality Score for CC-HIC Episodes
#'
#' Creates a simple score based on the number of submitted and number
#' of validated episodes
#'
#' @param connection a database connection to the CC-HIC database returned by
#'   [DBI::dbConnect()]
#'
#' @return a tabulation of episode scores by site
#' @export
#' @md
score_episodes <- function(connection) {

  episodes <- tbl(connection, "episodes")
  episodes_quality <- tbl(connection, "episodes_quality")
  provenance <- tbl(connection, "provenance")

  ref <- left_join(
    episodes, provenance,
    by = c("provenance" = "file_id")) %>%
    select(.data$episode_id,
           .data$nhs_number,
           .data$start_date,
           .data$site)

  inner_join(
    ref %>%
      group_by(site) %>%
      tally(name = "submitted"),
  anti_join(ref, episodes_quality,
            by = "episode_id") %>%
      group_by(site) %>%
  tally(name = "passed_evaluation"),
    by = "site") %>%
    collect() %>%
  mutate(score = .data$passed_evaluation/.data$submitted)

}

#' @title Data Quality Score for CC-HIC Events
#'
#' @param connection a database connection to the CC-HIC database returned by
#'   [DBI::dbConnect()]
#'
#' @return a tabulation of event scores by site
#' @export
score_events <- function(connection) {

  # Create a reference table
  ref <- make_reference(connection = connection)

  # Starting score is zero if data item is not contributed.
  etc <- tbl(connection, "events_missing") %>%
    collect() %>%
    mutate(score = 0)

  base_calendar <- make_base_calendar(ref, resolution = "month") %>%
    nest(date_sites = c(.data$site, .data$year, .data$month)) %>%
    add_column(link = 1L)

  # Now every code is represented for every site and every month
  expanded_calendar <- tibble(
    code_name = .variables$code_name,
    link = 1L) %>%
    left_join(base_calendar, by = "link") %>%
    unnest(.data$date_sites) %>%
    select(-.data$link)

  # Scores of zero now added to expanded calender
  base_score <- full_join(expanded_calendar, etc, by = c("site", "year", "month", "code_name")) %>%
    select(-eval_code, -description)

  slim_core <- tbl(connection, "events") %>%
    select(.data$episode_id,
           .data$event_id,
           .data$code_name,
           .data$datetime) %>%
    left_join(
      tbl(ctn, "episodes") %>%
        select(
          .data$episode_id,
          .data$start_date,
          .data$provenance),
      by = "episode_id") %>%
    left_join(
      tbl(ctn, "provenance") %>%
         select(
           .data$file_id,
           .data$site),
      by = c("provenance" = "file_id"))

  # To account for 1d vs 2d events
  n_submitted_events <- slim_core %>%
    mutate(
      datetime = if_else(is.na(.data$datetime), .data$start_date, .data$datetime),
      year = date_part("year", .data$datetime),
      month = date_part("month", .data$datetime)) %>%
    group_by(.data$site, .data$code_name, .data$year, .data$month) %>%
    tally(name = "submitted") %>%
    collect() %>%
    mutate_at(vars(.data$submitted), ~ as.numeric(.))

  n_passed_events <- slim_core %>%
    anti_join(tbl(ctn, "events_quality") %>%
                select(.data$event_id), by = "event_id") %>%
    mutate(
      datetime = if_else(
        is.na(.data$datetime), .data$start_date, .data$datetime),
      year = date_part("year", .data$datetime),
      month = date_part("month", .data$datetime)) %>%
    group_by(.data$site, .data$code_name, .data$year, .data$month) %>%
    tally(name = "passed_validation") %>%
    collect() %>%
    mutate_at(vars(passed_validation), ~ as.numeric(.))

  cont_score <- full_join(
    x = n_passed_events,
    y = n_submitted_events,
    by = c("site", "code_name", "year", "month")) %>%
    mutate_at(vars(passed_validation, submitted), ~ if_else(is.na(.), 0, .)) %>%
    mutate(score = passed_validation/submitted) %>%
    select(-passed_validation, -submitted)

  base_score %>%
    filter(score == 0) %>%
    bind_rows(cont_score)

}


summarise_event_score <- function(x, level = c("site", "concept")) {

  level <- match.arg(level)
  if (level == "site") {
    x %>%
      group_by(.data$site) %>%
      summarise(
        score = mean(.data$score)
      )
  } else {
    x %>%
      group_by(.data$site, .data$code_name) %>%
      summarise(
        score = mean(.data$score)
      )
  }


}

#' @title Evaluate the chronology of key events
#'
#' @details Evaluates the temporal sequence of key events with respect to the
#'   ICNARC temporal schema. This function behaves differently to the other
#'   `evaluate_` functions in that is accepts a database connection (not an
#'   extracted dataitem). This is because the temporal schema is based upon many
#'   events and so it was more convenient to extract them simultaneously with
#'   [wranglEHR::extract_demographics()].
#'
#' @param connection a connection to the CC-HIC database
#' @param decompose logical flag to output an object (if `TRUE`) ready to import
#'   into the `events_quality` database table.
#' @param .debug logical flag to run in debug mode.
#'
#' @importFrom wranglEHR extract_demographics
#' @importFrom tibble tribble add_column
#' @importFrom rlang abort .data
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter group_by arrange left_join mutate n
#'
#' @seealso [decompose_chronology()]
#'
#' @return if `decompose = FALSE` then the violations themselves are returned
#'   for further processing. If `decompose = TRUE` (the default) then a tibble
#'   with (potential) error codes ready for import into the `events_quality`
#'   table.
#'
#' @export
#' @md
evaluate_chronology <- function(connection = NULL,
                                decompose = TRUE,
                                .debug = FALSE){

  if (is.null(connection) && !.debug) {
    abort("You must supply a database connection")
  }

  ## Each date and time code has been given a letter assignment for ease of
  ## ordering. Letters with a base R conflict are omitted.

  chrono_codes <- tribble(
    ~code_name, ~order, ~short_name,
    "NIHR_HIC_ICU_0033", "a", "dob",
    "NIHR_HIC_ICU_0032", "b", "admission_hosp",
    "NIHR_HIC_ICU_0411", "d", "admission_icu",
    "NIHR_HIC_ICU_0050", "e", "ready_discharge_dt",
    "NIHR_HIC_ICU_0051", "f", "ready_discharge_tm",
    NA_character_, "ef", "ready_discharge_dttm",
    "NIHR_HIC_ICU_0048", "g", "withdraw_dt",
    "NIHR_HIC_ICU_0049", "h", "withdraw_tm",
    NA_character_, "gh", "withdraw_dttm",
    "NIHR_HIC_ICU_0042", "j", "death_dt",
    "NIHR_HIC_ICU_0043", "k", "death_tm",
    NA_character_, "jk", "death_dttm",
    "NIHR_HIC_ICU_0044", "l", "bsd_dt",
    "NIHR_HIC_ICU_0045", "m", "bsd_tm",
    NA_character_, "lm", "bsd_dttm",
    "NIHR_HIC_ICU_0038", "o", "body_removed_dt",
    "NIHR_HIC_ICU_0039", "p", "body_removed_tm",
    NA_character_, "op", "body_removed_dttm",
    "NIHR_HIC_ICU_0412", "r", "discharge_icu",
    "NIHR_HIC_ICU_0406", "s", "discharge_hosp",
    NA_character_, "z", "today"
  )

  dtb <- extract_demographics(
    connection = connection,
    code_names = chrono_codes$code_name[!is.na(chrono_codes$code_name)],
    rename = chrono_codes$order[!is.na(chrono_codes$code_name)]
    #,.debug = .debug
    ) %>%
    add_column(z = Sys.time())

  failures <- dtb %>%
    mutate(
      ef = if(all(c("e", "f") %in% names(.))) merge_datetime(.data$e, .data$f) else as.POSIXct(NA),
      gh = if(all(c("g", "h") %in% names(.))) merge_datetime(.data$g, .data$h) else as.POSIXct(NA),
      jk = if(all(c("j", "k") %in% names(.))) merge_datetime(.data$j, .data$k) else as.POSIXct(NA),
      lm = if(all(c("l", "m") %in% names(.))) merge_datetime(.data$l, .data$m) else as.POSIXct(NA),
      op = if(all(c("o", "p") %in% names(.))) merge_datetime(.data$o, .data$p) else as.POSIXct(NA)) %>%
    select(-any_of(c("e", "f", "g", "h", "j", "k", "l", "m", "o", "p"))) %>%
    pivot_longer(cols = -.data$episode_id) %>%
    arrange(.data$episode_id, .data$name) %>%
    filter(!is.na(.data$value)) %>%
    left_join(chrono_codes, by = c("name" = "order")) %>%
    select(-.data$name) %>%
    group_by(.data$episode_id) %>%
    mutate(
      name_next = c(.data$short_name[2:n()], NA),
      value_next = c(.data$value[2:n()], NA)) %>%
    mutate(eva = .data$value <= .data$value_next) %>%
    filter(!.data$eva) %>%
    select(-.data$eva)

  if (decompose) {
    decompose_chronology(connection = connection, x = failures, .debug = .debug)
  } else {
    return(failures)
  }

}


#' @title Decompose chronology
#'
#' @details
#' Takes the failure logs from [evaluate_chronology()] and decomposes it into
#' an error log ready for importing into the `events_quality` database table.
#' This is done because it was convenient to have an intermediate stage of this
#' process for plotting purposes.
#'
#' @param connection a connection to the CC-HIC database.
#' @param x the output returned from the [evaluate_chronology()] function.
#' @param .debug logical flag to run in debug mode.
#'
#' @seealso [evaluate_chronology()]
#'
#' @template return-event-quality
#' @export
#' @md
decompose_chronology <- function(connection, x, .debug = FALSE) {

  if (is.null(connection) && !.debug) {
    abort("You must supply a database connection")
  }

  core <- make_core(connection = connection, .debug = .debug)

  pull_codes <- unique(x$code_name)
  pull_epi <- unique(x$episode_id)

  failures <- core %>%
    filter(
      .data$episode_id %in% pull_epi,
      .data$code_name %in% pull_codes
    ) %>%
    collect() %>%
    inner_join(
      x %>%
        select(
          .data$episode_id,
          .data$code_name),
      by = c("episode_id", "code_name")) %>%
    select(
      .data$site,
      .data$episode_id,
      .data$event_id,
      .data$code_name) %>%
    tibble::add_column(value = NA_character_)

  warn(
    message =
"the value column has not been implemented yet in `decompose_chronology()`
therefore this column currently defaults to `NA`",
    frequency = "regularly")

  create_failure_log(
    failures,
    "VE_TP_02",
    "Chronology of key events is correct"
  )
}

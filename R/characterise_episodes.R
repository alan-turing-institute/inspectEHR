#' @title Characterise episodes
#'
#' @details There are several ways to potentially define an episode.
#'   Fundamentally, a start and an end datetime are required for an unambiguous
#'   episode. The start datetime is already required to import episodes into the
#'   CC-HIC database (it is a primary key) and so is ensured to be 100% complete
#'   if the schema definition has been applied. This function therefore serves
#'   to identify the end of an episode, which is often much less clearly
#'   defined. Potential sources of information, in order of precedence include:
#'   * datetime of unit departure: `0412`
#'   * date and time of in-unit death: `0042` and `0043`
#'   * date and time of brainstem death: `0044` and `0045`
#'   * date and time of body removed: `0038` and `0039`
#'
#'   Further, these end times must correlate with an outcome status:
#'   * vital status at discharge from ICU: `0097`
#'   * declaration of brainstem death: `0400`
#'
#' @param connection a connection to the CC-HIC database
#' @param .debug logical flag. If \code{TRUE} the function will extract from
#'  internal package test data
#'
#' @return a tibble that characterises each episode. The attribute
#'   `invalid_records` contains information related to invalid records and the
#'   reason for invalidation
#' @export
#'
#' @importFrom rlang abort
#' @importFrom dplyr setdiff tbl
#' @importFrom tidyselect ends_with
#' @md
characterise_episodes <- function(connection = NULL, .debug = FALSE) {

  if (is.null(connection) && !.debug) {
    abort("You must supply a database connection")
  }

  # Extract Data
  df <- prep_characterise_episodes(connection, .debug = .debug)

  # Reconcile dates and times in datetimes and remove the old columns
  df <- df %>%
    mutate(
      death_dttm = merge_datetime(.data$death_date, .data$death_time),
      bsd_dttm = merge_datetime(.data$bsd_date, .data$bsd_time),
      body_dttm = merge_datetime(.data$body_date, .data$body_time)) %>%
    select(-ends_with("date"), -ends_with("time"))

  # Validate NHS Number
  df <- df %>%
    mutate(nhs_validation = if_else(
      validate_nhs(.data$nhs), 1L, 0L
    ))

  # Collect invalid NHS Numbers
  invalid_records <- df %>%
    filter(.data$nhs_validation == 0) %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VA_VC_01",
      reason = "invalid nhs number")

  # Collect episodes without an outcome
  invalid_records <- df %>%
    filter(.data$outcome == "E" | is.na(.data$outcome)) %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VA_CP_01",
      reason = "no ICU outcome status") %>%
    bind_rows(invalid_records)

  # Look for death data that is in conflict
  invalid_records <- df %>%
    filter(!is.na(.data$death_dttm)) %>%
    group_by(.data$nhs) %>%
    distinct(.data$death_dttm) %>%
    tally() %>%
    filter(.data$n > 1) %>%
    left_join(df, by = "nhs") %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VE_UP_01",
      reason = "duplicate and conflicting death times") %>%
    bind_rows(invalid_records)

  # Create a new end time aligned on our preferences
  df <- df %>%
    mutate(
      epi_end_dttm = case_when(
        .data$outcome == "A" & is.na(.data$src_end_dttm) ~ as.POSIXct(NA),
        .data$outcome == "A" & !is.na(.data$src_end_dttm)
        ~ .data$src_end_dttm,
        .data$outcome == "D" & !is.na(.data$death_dttm) & (.data$bsd == 0 | is.na(.data$bsd))
        ~ .data$death_dttm,
        .data$outcome == "D" & bsd == 1 & !is.na(.data$bsd_dttm)
        ~ .data$bsd_dttm,
        TRUE ~ as.POSIXct(NA)
      )
    )

  # Check for episodes that have missing end
  invalid_records <- df %>%
    filter(is.na(.data$epi_end_dttm)) %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VE_CP_01",
      reason = "episode end cannot be reconciled") %>%
    bind_rows(invalid_records)

  # Check for episodes that have an invalid LOS
  invalid_records <- df %>%
    filter(.data$epi_end_dttm <= .data$epi_start_dttm) %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VE_TP_01",
      reason = "episode length <= 0") %>%
    bind_rows(invalid_records)

  # Check for episodes that have duplicate start
  invalid_records <- df %>%
    ungroup() %>%
    distinct(.data$nhs, .data$epi_start_dttm, .keep_all = TRUE) %>%
    select(.data$episode_id) %>%
    anti_join(df, by = "episode_id") %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VE_UP_01",
      reason = "duplicate start time of episode") %>%
    bind_rows(invalid_records)

  # Check for episodes that have duplicate end
  invalid_records <- df %>%
    ungroup() %>%
    distinct(.data$nhs, .data$epi_end_dttm, .keep_all = TRUE) %>%
    select(.data$episode_id) %>%
    anti_join(df, by = "episode_id") %>%
    select(.data$episode_id) %>%
    mutate(
      code = "VE_UP_01",
      reason = "duplicate end time of episode") %>%
    bind_rows(invalid_records)

  # Check for episodes that are overlapping
  repeat_admissions <- df %>%
    group_by(.data$nhs) %>%
    filter(n() >= 2)

  if (nrow(repeat_admissions) > 0) {
    invalid_records <- df %>%
      group_by(.data$nhs) %>%
      filter(n() >= 2) %>%
      arrange(.data$nhs, .data$epi_start_dttm) %>%
      mutate(next_time = c(.data$epi_start_dttm[2:n()], as.POSIXct(NA))) %>%
      mutate(time_out = difftime(.data$next_time, .data$epi_end_dttm)) %>%
      ungroup() %>%
      filter(.data$time_out < 0) %>%
      select(.data$episode_id) %>%
      mutate(
        code = "VE_VC_04",
        reason = "overlapping episodes"
      ) %>%
      bind_rows(invalid_records)
  }

  # Return formatted table.
  df <- df %>%
    select(
      .data$episode_id, .data$nhs, .data$epi_start_dttm,
      .data$epi_end_dttm, .data$outcome) %>%
    rename(nhs_number = .data$nhs) %>%
    anti_join(invalid_records, by = "episode_id") %>%
    arrange(.data$nhs_number, .data$epi_start_dttm) %>%
    mutate(los_days = as.numeric(
      difftime(
        .data$epi_end_dttm, .data$epi_start_dttm, units = "hours"))/24)

  if (.debug) {
    episodes <- .episodes
    provenance <- .provenance
  } else {
    episodes <- tbl(connection, "episodes")
    provenance <- tbl(connection, "provenance")
  }

  df <- left_join(episodes, provenance,
                  by = c("provenance" = "file_id")
  ) %>%
    select(.data$episode_id, .data$site) %>%
    collect() %>%
    left_join(df, ., by = "episode_id")

  attr(df, "invalid_records") <- invalid_records

  return(df)
}


#' @title Prepare data for characterise episodes
#'
#' @details Extracts data necessary for episode characterisation
#'
#' @importFrom wranglEHR extract_demographics
#' @importFrom tibble tribble
#' @importFrom rlang abort
#' @importFrom dplyr mutate across
#' @importFrom tidyselect ends_with
#' @importFrom hms as_hms
#' @importFrom lubridate ymd ymd_hms
#' @importFrom magrittr %>%
#'
#' @param connection a connection to the CC-HIC database
prep_characterise_episodes <- function(connection = NULL, .debug = FALSE) {

  if (is.null(connection) && !.debug) {
    abort("You must supply a database connection")
  }

  df_extract <- tribble(
    ~codes, ~names,
    "NIHR_HIC_ICU_0411", "epi_start_dttm",
    "NIHR_HIC_ICU_0412", "src_end_dttm",
    "NIHR_HIC_ICU_0042", "death_date",
    "NIHR_HIC_ICU_0043", "death_time",
    "NIHR_HIC_ICU_0044", "bsd_date",
    "NIHR_HIC_ICU_0045", "bsd_time",
    "NIHR_HIC_ICU_0038", "body_date",
    "NIHR_HIC_ICU_0039", "body_time",
    "NIHR_HIC_ICU_0073", "nhs",
    "NIHR_HIC_ICU_0097", "outcome",
    "NIHR_HIC_ICU_0400", "bsd"
  )

  df <- extract_demographics(
    connection = connection,
    code_names = df_extract$codes,
    rename = df_extract$names,
    .debug = .debug)

  ## For this particular purpose, we need to add in columns that might
  ## be all NA, since extract_demographics does not return fields that do
  ## not exist.

  missing_names <- setdiff(df_extract$names, names(df))

  for (i in seq_along(missing_names)) {
    df[[missing_names[i]]] <- NA_character_
  }

  df %>%
    mutate(across(ends_with("time"), ~ as_hms(.))) %>%
    mutate(across(ends_with("date"), ~ ymd(.))) %>%
    mutate(across(ends_with("dttm"), ~ ymd_hms(.)))
}


#' Characterise Spells
#'
#' Some sites have patients check out of one ICU and into another (for example
#' ICU stepdown to HDU). This checks to see if patients are discharged from one
#' unit and admitted to another wihtin a pre-defined time period, specified in
#' the minutes argument.
#'
#' This only evaluates episodes that have already been flagged as valid by the
#' \code{\link{characterise_episodes}} function.
#'
#' @param df episode length table
#' @param minutes numeric scalar to define transition period
#' @export
#'
#' @return a table with episodes reconciled as spells
characterise_spells <- function(df = NULL, minutes = 30) {
  df %>%
    arrange(.data$nhs_number, .data$epi_start_dttm) %>%
    group_by(.data$nhs_number) %>%
    mutate(time_out = .data$epi_start_dttm[-1] %>%
             difftime(
               .data$epi_end_dttm[-length(.data$epi_end_dttm)], units = "mins") %>%
             as.integer() %>%
             c(NA)) %>%
    mutate(new_spell = if_else(
      lag(.data$time_out) > minutes | is.na(lag(.data$time_out)),
      TRUE, FALSE)) %>%
    ungroup() %>%
    mutate(spell_id = cumsum(.data$new_spell)) %>%
    select(.data$spell_id, .data$episode_id, .data$nhs_number, .data$site,
           .data$epi_start_dttm, .data$epi_end_dttm, .data$los_days)
}

#' Report Weekly Admissions
#'
#' Shows the number of patients and unique admissions to each site by week.
#'
#' @param reference_table returned from \code{make_reference}
#' @return a summary containing patient and episode tallies
#'
#' @importFrom dplyr group_by ungroup summarise n_distinct mutate
#' @importFrom magrittr %>%
#' @importFrom lubridate day month year
#' @importFrom rlang .data
weekly_admissions <- function(reference_table = NULL) {
  if (is.null(reference_table)) {
    abort("You must supply a reference table")
  }

  reference_table %>%
    mutate(
      year = year(.data$start_date),
      month = month(.data$start_date, label = TRUE),
      week_of_month = as.integer(ceiling(day(.data$start_date) / 7))
    ) %>%
    group_by(.data$site, .data$year, .data$month, .data$week_of_month) %>%
    summarise(
      patients = n_distinct(.data$nhs_number),
      episodes = n_distinct(.data$episode_id)
    ) %>%
    ungroup()
}


#' Reports Case Numbers according to day
#'
#' @param unique_cases_tbl from \code{pull_cases_all}
#'
#' @return breakdown of unique daily cases
#'
#' @importFrom dplyr group_by summarise n_distinct
#' @importFrom magrittr %>%
#' @importFrom lubridate day month year wday
report_cases_daily <- function(unique_cases_tbl = NULL) {
  cases <- unique_cases_tbl %>%
    mutate(
      year = lubridate::year(start_date),
      month = lubridate::month(start_date, label = TRUE),
      week_of_month = as.integer(ceiling(lubridate::day(start_date) / 7)),
      wday = lubridate::wday(start_date, label = TRUE)
    ) %>%
    dplyr::group_by(site, year, month, week_of_month, wday) %>%
    dplyr::summarise(
      patients = dplyr::n_distinct(nhs_number),
      episodes = dplyr::n_distinct(episode_id)
    )
}


#' Daily Admissions for each Site
#'
#' Calculates the number of admissions for each calendar day, stratified
#' by site. This is a complete table, i.e. days with 0 admissions are not
#' listed
#'
#' @param reference the reference table generated by \code{\link{make_reference}}
#' @param by_site the named site of interest as a character
#'
#' @return a tibble with the number of unique episodes admitted for a given day
#'
#' @importFrom dplyr filter mutate group_by summarise n_distinct
#' @importFrom lubridate date
daily_admissions <- function(reference = NULL, by_site = NULL) {

  admissions <- reference %>%
    filter(site == by_site) %>%
    mutate(date = lubridate::date(start_date)) %>%
    group_by(date) %>%
    summarise(episodes = n_distinct(episode_id)) %>%
    filter(episodes > 0)
}


#' Event Occurrances for each Site
#'
#' Calculates the number of event occurances for each calendar day, stratified
#' by site. Days with 0 event submissions are not listed
#'
#' @param extracted_event extracted HIC event
#' @param by_site a site code as a character vector
#'
#' @return a tibble with the number of unique episodes admitted for a given day
event_occurrances <- function(extracted_event = NULL, by_site = "UCL") {
  occurances <- extracted_event %>%
    filter(site == by_site) %>%
    mutate(date = lubridate::date(datetime)) %>%
    group_by(date) %>%
    summarise(events = n_distinct(internal_id)) %>%
    filter(events > 0)

  return(occurances)
}


#' Report total admission numbers for each ICU
#'
#' Reports on ICNARC CMP unit codes to describe the numbers of reported cases by
#' site. This is dependent on accurate parsing of the xml schema for hic code
#' 0002 (INCARC CMP unit code). And as such, may dramatically under-report. To
#' see a comprehensive list of actual cases that are not dependent upon this,
#' see \code{\link{weekly_admissions}}.
#'
#' @param events_table the CC-HIC events database table
#' @param reference_table the reference table returned from
#'   \code{\link{make_reference}}
#' @export
#'
#' @importFrom dplyr filter select collect right_join group_by summarise
#'   n_distinct rename
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @return a tibble with unique episodes and patients reported by ICU
total_unit_admissions <- function(events_table = NULL, reference_table = NULL) {
  events_table %>%
    filter(.data$code_name == "NIHR_HIC_ICU_0002") %>%
    select(.data$episode_id, .data$string) %>%
    collect() %>%
    right_join(reference_table, by = "episode_id") %>%
    select(.data$episode_id, .data$nhs_number, .data$start_date,
                  .data$string, .data$site) %>%
    group_by(.data$site, .data$string) %>%
    summarise(
      earliest = min(.data$start_date),
      latest = max(.data$start_date),
      patients = n_distinct(.data$nhs_number),
      episodes = n_distinct(.data$episode_id)
    ) %>%
    mutate(
      span_months = as.integer(
        difftime(.data$latest, .data$earliest, units = "weeks")/4)) %>%
    rename(unit = .data$string)
}








#' Resolve DateTime
#'
#' Many events in CC-HIC are stored in separate date and time columns/objects.
#' This function attempts to reconcile and combine these times when possible. Of
#' note, date and time information is not always stored with consistent rules.
#' For example, death date and time, are often stored for every patient in every
#' episode, even though the patient can only die once. The following are some
#' date and time pairings that denote a singular event:
#' \itemize{
#'   \item "NIHR_HIC_ICU_0042", "NIHR_HIC_ICU_0043" - Unit Death
#'   \item "NIHR_HIC_ICU_0038", "NIHR_HIC_ICU_0039" - Body Removal
#'   \item "NIHR_HIC_ICU_0044", "NIHR_HIC_ICU_0045" - Brain stem death
#'   \item "NIHR_HIC_ICU_0048", "NIHR_HIC_ICU_0049" - Treatment Withdrawal
#'   \item "NIHR_HIC_ICU_0050", "NIHR_HIC_ICU_0051" - Discharge ready
#' }
#' If a date or time component is missing, nothing is returned as the datetime
#' cannot be accurately formed.
#'
#' @param df a table that contains columns for the date and time of interest
#' @param date_code the column name for the date of interest
#' @param time_code the column name for the time of interest
#'
#' @return a table with the correct datetime pairing for the codes given
#'
#' @importFrom rlang .data sym
resolve_date_time <- function(df = NULL,
                              date_code = NULL,
                              time_code = NULL) {

  if (any(is.null(c(df, date_code, time_code)))) {
    rlang::abort("you must supply a dataframe and two column names")
  }

  dc <- rlang::enquo(date_code)
  tc <- rlang::enquo(time_code)

  df <- df %>%
    mutate(
      dttm = if_else(
        !is.na(!!dc) & !is.na(!!tc),
      paste0(format(!!dc), " ", format(!!tc)), as.character(NA)
      )
    ) %>%
    mutate(
      dttm = if_else(!is.na(dttm), lubridate::ymd_hms(dttm), as.POSIXct(NA)))

  return(df)
}

#' Summarise Non-Verifiable Episodes
#'
#' Provides an overview of the reasons for episode invalidation
#'
#' @param df the episode table returned from \code{\link{characterise_episodes}}
#' @export
#'
#' @return a tibble containing summary information for validation at episode
#'   level
episode_varacity <- function(df) {

  attr(df, "invalid_records") %>%
    group_by(reason) %>%
    tally()

}

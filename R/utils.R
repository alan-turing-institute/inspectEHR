## Connection ====

#' @title Retrieve database tables
#'
#' @details Places all tables from the database connection into a local list.
#'   This makes accessing the tables straightforward.
#'
#' @param connection a database connection object returned from
#'   [DBI::dbConnect()]
#' @param schema character string length 1 detailing the name of the target
#'   schema
#'
#' @importFrom DBI dbListTables
#' @importFrom dplyr tbl
#' @importFrom dbplyr in_schema
#' @importFrom rlang abort
#' @importFrom purrr map
#'
#' @return a list containing pointers to tables within the sql connection.
#' @export
#' @md
#' @family connection
retrieve_tables <- function(connection = NULL, schema = "public") {
  if (is.null(connection)) {
    abort("a connection object must be provided")
  }

  all_tables <- dbListTables(connection)

  tbl_list <- all_tables %>%
    map(~ tbl(connection, in_schema(schema = schema, table = .x)))

  names(tbl_list) <- all_tables

  return(tbl_list)
}

#' @title Write Notify
#'
#' @details Writes out local tables into a database, and let's you know when
#'   it's done. Pass via ... options like: overwrite, append, copy
#'
#' @param connection a database connection returned by [DBI::dbConnect()]
#' @param target_name character vector length 1 detailing the database table
#'   to write to
#' @param local_table the local table object to write out
#' @param verbose logical flag to print results to the console
#' @param ... optional additional arguments to pass to [DBI::dbWriteTable()]
#'
#' @importFrom DBI dbWriteTable
#' @importFrom cli cli_alert_success cli_alert_danger
#' @family connection
write_notify <- function(connection,
                         target_name,
                         local_table,
                         verbose = TRUE,
                         ...) {
  success <- dbWriteTable(
    conn = connection,
    name = target_name,
    value = local_table, ...)
  if (success) {
    if (verbose) cli_alert_success("Table: {target_name} successfully written")
  } else {
    if (verbose) cli_alert_danger("An unknown error occurred")
  }
}


#' @importFrom DBI dbSendStatement dbHasCompleted dbClearResult
#' @family connection
transact_sql <- function(ctn, query_str) {
  st <- dbSendStatement(ctn, query_str)
  check <- dbHasCompleted(st)
  dbClearResult(st)
  return(check)
}


#' @title Make reference table
#'
#' @details Prepares a small local table with the following fields:
#'   * episode_id: primary key for the `episodes` table
#'   * nhs_number: core patient identifier
#'   * start_date: the ICU episode start date
#'   * site: the hospital site of origin
#'
#' @importFrom dplyr tbl left_join select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @param connection a database connection returned by [DBI::dbConnect()]
#' @param translate_site a lookup table to translate site names on export in a
#'   consistent way. The table requires two columns: `site` and `translation`.
#'   The `site` column should be a character vector with names of the existing
#'   sites as they are stored in the database (e.g. `c("GSTT", "RGT")` etc).
#'   The `translation` column should be a character vector with new names to
#'   obfuscate the real names (e.g. `c("Site A", "Site B")` etc). This is a 1:1
#'   mapping so doesn't confer any real privacy advantage, but is useful as a
#'   means to casually obscure data origins.
#'
#' @return a tibble with episode level data with site
#' @export
#' @examples
#' \dontrun{
#' # Make a database connection object using DBI::dbConnect()
#' # ctn <- DBI::dbConnect()
#' ref <- make_reference(ctn)
#' head(ref)
#' DBI::dbDisconnect(ctn)
#' }
make_reference <- function(connection = NULL, translate_site = NULL) {
  if (is.null(connection)) {
    abort("You must supply a database connection")
  }

  episodes <- tbl(connection, "episodes")
  provenance <- tbl(connection, "provenance")

  out <- left_join(
    episodes, provenance, by = c("provenance" = "file_id")) %>%
    select(.data$episode_id, .data$nhs_number, .data$start_date, .data$site) %>%
    collect()

  if (!is.null(translate_site)) {

    check_is_tibble <- tibble::is_tibble(translate_site)
    check_names <- identical(names(translate_site), c("site", "translation"))
    check_all_sites <-
      all(translate_site$site %in% unique(out$site))

    safe_proceed <- all(check_is_tibble, check_names, check_all_sites)

    if (safe_proceed) {

      out <- out %>%
        left_join(translate_site, by = "site") %>%
        select(-.data$site) %>%
        rename(site = .data$translation)

    } else {

      abort("The translation table provoided did not meet specifications.")

    }

  }
  
  if (class(out$start_date) == "character") {
    out$start_date <- as.POSIXct(out$start_date)
  }

  return(out)
}


#' @title Make core table
#'
#' @details Prepares the appropriate table joins in the CC-HIC database to ready
#'   for querying
#'
#' @param connection a database connection returned by [DBI::dbConnect()]
#'
#' @importFrom rlang abort
#' @importFrom dplyr tbl left_join inner_join
#' @importFrom magrittr %>%
#'
#' @return a remote database table
#' @export
#'
#' @examples
#' \dontrun{
#' # Make a database connection object using DBI::dbConnect()
#' # ctn <- DBI::dbConnect()
#' core <- make_core(ctn)
#' head(core)
#' DBI::dbDisconnect(ctn)
#' }
make_core <- function(connection = NULL) {

  if (is.null(connection)) {
    abort("You must supply a database connection")
  }

  events <- tbl(connection, "events")
  episodes <- tbl(connection, "episodes")
  provenance <- tbl(connection, "provenance")
  
  episodes %>%
    left_join(provenance, by = c("provenance" = "file_id")) %>%
    inner_join(events, by = "episode_id")

}


find_max_time <- function(events, time_col) {
  quo_timecol <- enquo(time_col)

  max_time <- events %>%
    group_by(episode_id) %>%
    summarise(maxtime = max(!!quo_timecol, na.rm = TRUE)) %>%
    collect() %>%
    mutate(maxtime = as.POSIXct(maxtime, origin = "1970-01-01 00:00:00"))

  return(max_time)
}


#' @title lookup CC-HIC codes
#'
#' @param search_term code name for a CC-HIC data item, either as a full string
#'   (e.g. "NIHR_HIC_ICU_0108") or as a number (e.g. 108), or the short_name
#'   of the concept.
#' @param "code" or "name" depending if you want to search by hic_code or
#'   short_name
#'
#' @return a lookup table
#' @export
#'
#' @importFrom stringr str_pad
#' @importFrom dplyr filter select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' lookup_hic(108)
#' lookup_hic("NIHR_HIC_ICU_0108")
lookup_hic <- function(search_term = "NIHR_HIC_ICU_0108",
                       type = c("code", "name")) {

  type <- match.arg(type)

  if (type == "code") {
    if (is.numeric(search_term)) {
      .code_name <- str_pad(search_term, width = 4, side = "left", pad = "0")
      .code_name <- paste0("NIHR_HIC_ICU_", .code_name)
    }

    qref %>%
      filter(.data$code_name == .code_name) %>%
      select(.data$code_name, .data$short_name)
  } else {
    qref %>%
      filter(grepl(search_term, .data$code_name, ignore.case = TRUE)) %>%
      select(.data$code_name, .data$short_name)
  }



}


is.error <- function(x) {
  inherits(x, "try-error")
}


#' Round any
round_any <- function(x, accuracy = 1) {
  round(x / accuracy) * accuracy
}


#' Inverse Logistic Function
inv_logit <- function(x) {
  p <- 1 / (1 + exp(-x))
  ifelse(x == Inf, 1, p)
}


#' Daily Events for HIC Data item by Site
#'
#' Calculates the number of events contributed for each calendar day, stratified
#' by site. This is a complete table, i.e. days with 0 admissions are not
#' listed
#'
#' @param df extracted data item
#' @param reference the reference table generated by [make_reference()]
#' @param by_site the named site of interest as a character
#'
#' @return a tibble with the number of unique episodes admitted for a given day
#'
#' @importFrom dplyr filter mutate group_by summarise n_distinct
#' @importFrom lubridate date
#' @md
daily_events <- function(df = NULL, reference = NULL, by_site = NULL) {

  admissions <- df %>%
    filter(.data$site == by_site) %>%
    left_join(reference %>%
                select(episode_id, start_date), by = "episode_id") %>%
    mutate(date = lubridate::as_date(start_date)) %>%
    group_by(date) %>%
    summarise(events = n_distinct(event_id)) %>%
    filter(events > 0)
}


#' @title Make base calendar
#'
#' Creates a base cadence calendar for all contributing sites so that each
#' year and month of submission is enumerated.
#'
#' @param reference_tbl a reference table returned from [make_reference()]
#' @param resolution character vector length 1 with the desired base resolution
#'   of the table
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom purrr map
#' @importFrom lubridate as_date floor_date ceiling_date
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr nest unnest
#'
#' @return
#' @export
#' @md
make_base_calendar <- function(reference_tbl, resolution = c("day", "month")) {
  res <- match.arg(resolution)

  base_cal <- reference_tbl %>%
    group_by(.data$site) %>%
    summarise(
      start = as_date(
        floor_date(
          min(.data$start_date), unit = "month")),
      end = lubridate::as_date(
        ceiling_date(
          max(.data$start_date), unit = "month")-1)) %>%
    nest(date = c(.data$start, .data$end)) %>%
    mutate(date = map(date, ~ seq.Date(.x$start, .x$end, by = res))) %>%
    unnest(.data$date)

  if (res == "month") {
    base_cal <- base_cal %>%
      mutate(
        year = as.integer(lubridate::year(.data$date)),
        month = as.integer(lubridate::month(.data$date))) %>%
      select(-.data$date)
  }
  return(base_cal)
}


#' @importFrom dplyr filter select pull select_if
#' @importFrom rlang !!
#' @importFrom magrittr %>%
find_2d_meta <- function(metadata, c_name) {
  select_row <- filter(metadata, code_name == c_name)

  prim_col <- select_row %>%
    select(primary_column) %>%
    pull()

  select_row %>%
    select(-c(code_name, long_name, primary_column, datetime, !!prim_col)) %>%
    select_if(~ any(!is.na(.x))) %>%
    names()
}



#' @title Parse range limits
#'
#' @param ranges a character vector of length 1.
#'
#' @return
#' @export
#'
#' @examples
#' # Inclusive interval
#' parse_range("[0, 100]")
#' # Non-inclusive interval
#' parse_range("(0, 100)")
#' # Open positive interval
#' parse_range("[0, Inf)")
parse_range <- function(ranges = NULL) {

  if (class(ranges) != "character") abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")

  if (is.na(ranges)) {
    return(
      list(
        lims = c(-Inf, Inf),
        lower_lim = `>=`,
        upper_lim = `<=`)
    )
  }

  raw_lims <- stringr::str_split(ranges, ",", simplify = TRUE)

  if (length(raw_lims) != 2) {
    abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")
  }

  check_infinites <- grepl("inf", raw_lims, ignore.case = TRUE)
  lims <- vector("numeric", 2)

  if (check_infinites[1]) {
    lims[1] <- -Inf
  } else {
    lims[1] <- readr::parse_number(raw_lims[1])
  }

  if (check_infinites[2]) {
    lims[2] <- Inf
  } else {
    lims[2] <- readr::parse_number(raw_lims[2])
  }

  lower_boundaries <- stringr::str_sub(ranges, 1, 1)

  lower_lim <- case_when(
    lower_boundaries == "[" ~ c(`>=`),
    lower_boundaries == "(" ~ c(`>`),
    TRUE ~ list(NA_character_)
  )[[1]]

  upper_boundaries <- stringr::str_sub(ranges, nchar(ranges), nchar(ranges))

  upper_lim <- case_when(
    upper_boundaries == "]" ~ c(`<=`),
    upper_boundaries == ")" ~ c(`<`),
    TRUE ~ list(NA_character_)
  )[[1]]

  if (!is.function(lower_lim) | !is.function(upper_lim)) {
    abort("You must supply a range in the format `(##, ##)`/`[##, ##]` or similar")
  }

  return(list(lims = lims, lower_lim = lower_lim, upper_lim = upper_lim))

}


parse_periodicity <- function(periodicity = NULL) {
  parse_range(ranges = periodicity)
}

jitter_dates <- function(x, sd = 5) {

  x + lubridate::days(as.integer(rnorm(length(x), 0, sd)))

}


#' @title Merge datetime columns
#'
#' @details Dates and times are often split across concepts in CC-HIC. This function
#' takes a date and time column vectors and returns the combined asset as a
#' datetime class.
#'
#' @param date a vector of class date, or string of default format `%Y-%m-%d`
#' @param time a vector of class time, or string of default format `%H:%M:%S`
#' @param format a string vector of length 1 with the format to parse the
#'   combined date and time information. See [base::strptime()].
#'
#' @seealso [base::strptime()]
#' @md
merge_datetime <- function(date, time, format = "%Y-%m-%d %H:%M:%S") {
  as.POSIXct(paste(date, time), format = format)
}

# Class Checking ====

is_extract <- function(x) {
  check_class <- class(x)[1] %in% .preserved_classes
  check_form <- tibble::is_tibble(x)
  check_code_name <- !is.null(attr(x, "code_name"))
  all(check_class, check_form, check_code_name)
}

is_string_2d <- function(x) inherits(x, "string_2d")
is_string_1d <- function(x) inherits(x, "string_1d")
is_integer_2d <- function(x) inherits(x, "integer_2d")
is_integer_1d <- function(x) inherits(x, "integer_1d")
is_real_2d <- function(x) inherits(x, "real_2d")
is_real_1d <- function(x) inherits(x, "real_1d")
is_datetime_1d <- function(x) inherits(x, "datetime_1d")
is_date_1d <- function(x) inherits(x, "date_1d")
is_time_1d <- function(x) inherits(x, "time_1d")

# Checking quality objects =====

is_event_evaluation <- function(x) {
  if(!tibble::is_tibble(x)) {
    warn("is not a tibble")
    return(FALSE)
  }

  if(dplyr::is_grouped_df(x)) {
    warn("is a grouped tibble")
    return(FALSE)
  }

  .cols <- purrr::map_chr(x, ~ class(.)[1])

  if (length(.cols) != 7) {
    warn("does not have the right number of columns")
    return(FALSE)
  } else {
    all(
      purrr::map_chr(x, ~ class(.)[1]) ==
        c(
          "site" = "character",
          "episode_id" = "integer",
          "event_id" = "integer",
          "code_name" = "character",
          "value" = "character",
          "eval_code" = "character",
          "description" = "character")
    )
  }
}


is_event_missingness <- function(x) {
  if(!tibble::is_tibble(x)) {
    warn("is not a tibble")
    return(FALSE)
  }

  if(dplyr::is_grouped_df(x)) {
    warn("is a grouped tibble")
    return(FALSE)
  }

  .cols <- purrr::map_chr(x, ~ class(.)[1])

  if (length(.cols) != 6) {
    warn("does not have the right number of columns")
    return(FALSE)
  } else {
    all(
      purrr::map_chr(x, ~ class(.)[1]) ==
        c(
          "site" = "character",
          "code_name" = "character",
          "year" = "integer",
          "month" = "integer",
          "eval_code" = "character",
          "description" = "character")
    )
  }
}


.utter_nonsense <- function(x) {

  .verb <- c("reorganising", "sorting", "cateloguing", "categorising", "ordering",
    "resolving", "grading")
  .noun <- c("chickpeas", "ocelots", "memory", "salad bowl")

  return(paste(sample(.verb, 1), sample(.noun, 1)))

}



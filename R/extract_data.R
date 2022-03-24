#' Extract CC-HIC Data
#'
#' @details
#' Extracts CC-HIC data from the database events tables and appends them with
#' the correct class for further processing.
#'
#' @param core_table core table returned from [make_core()]
#' @param code_name a string vector of length 1 with the CC-HIC data item to be
#'   extracted.
#' @param translate_site a lookup table to translate site names on export in a
#'   consistent way. The table requires two columns: `site` and `translation`.
#'   The `site` column should be a character vector with names of the existing
#'   sites as they are stored in the database (e.g. `c("GSTT", "RGT")` etc).
#'   The `translation` column should be a character vector with new names to
#'   obfuscate the real names (e.g. `c("Site A", "Site B")` etc). This is a 1:1
#'   mapping so doesn't confer any real privacy advantage, but is useful as a
#'   means to casually obscure data origins.
#'
#' @importFrom rlang .data !! abort
#' @importFrom magrittr %>%
#'
#' @return A tibble with 1 row per event
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#' # ctn <- DBI::dbConnect() create database connection
#' core <- make_core(ctn)
#' df <- extract(core, "NIHR_HIC_ICU_0108")
#' }
extract <- function(core_table = NULL, code_name = NULL, translate_site = NULL) {

  if (is.null(core_table)) abort("You must include the core table")
  if (!(code_name %in% qref$code_name)) abort("This is not a valid code")

  # Identify the correct column type to pull out
  q_type <- paste0("x", qref[qref$code_name == code_name, "type", drop = TRUE])
  q_class <- type <- qref[qref$code_name == code_name, "class", drop = TRUE]
  q_col <- qref[qref$code_name == code_name, "primary_column", drop = TRUE]

  # extract chosen input variable from the core table
  extracted_table <- q_type %>%
    base::switch(
      x1 = extract_1d(core_table, input = code_name, data_location = q_col),
      x2 = extract_2d(core_table, input = code_name, data_location = q_col)
    )

  if (!is.null(translate_site)) {

    check_is_tibble <- tibble::is_tibble(translate_site)
    check_names <- identical(names(translate_site), c("site", "translation"))
    check_all_sites <-
      all(translate_site$site %in% unique(extracted_table$site))

    safe_proceed <- all(check_is_tibble, check_names, check_all_sites)

    if (safe_proceed) {

      extracted_table <- extracted_table %>%
        left_join(translate_site, by = "site") %>%
        select(-.data$site) %>%
        rename(site = .data$translation)

    } else {

      abort("The translation table provoided did not meet specifications.")

    }

  }

  class(extracted_table) <- append(class(extracted_table), q_class, after = 0)
  attr(extracted_table, "code_name") <- code_name

  return(extracted_table)
}


#' Extract 1d Values
#'
#' This function extracts the correct column from the CC-HIC database
#' depending upon what type of data is called for
#'
#' @param core_table the core table from make_core
#' @param input the HIC code for the variable of interest
#' @param data_location the column name that stores the primary data for this
#' variable
#'
#' @return a tibble with HIC data for a specified variable
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect select rename arrange
extract_1d <- function(core_table = NULL, input = NULL, data_location = NULL) {

  core_table %>%
    filter(.data$code_name == input) %>%
    collect() %>%
    select(
      .data$episode_id,
      .data$event_id,
      .data$site,
      .data$code_name,
      .data[[data_location]]
    ) %>%
    rename(value = .data[[data_location]]) %>%
    arrange(.data$episode_id)

}


#' Extract 2d Values
#'
#' This function extracts the correct column from the CC-HIC database
#' depending upon what type of data is called for. It additionally pulls
#' out the datetime column, which accompanies any data for this class
#'
#' @param core_table a core table
#' @param input the input variable of choice
#' @param data_location the column name that stores the primary data for this
#' variable
#'
#' @return a long table with 1 row per event from the CC-HIC database
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom dplyr filter collect select rename arrange
extract_2d <- function(core_table = NULL, input = NULL, data_location = NULL) {

  meta_names <- find_2d_meta(.variables, input)

  df <- core_table %>%
    filter(.data$code_name == input) %>%
    collect() %>%
    select(
      .data$episode_id,
      .data$event_id,
      .data$site,
      .data$code_name,
      .data$datetime,
      .data[[data_location]],
      !!!meta_names
    ) %>%
    rename(value = .data[[data_location]]) %>%
    arrange(.data$episode_id, .data$datetime)

  if (length(meta_names) > 0) {
    replace_meta <- paste0("meta_", seq_along(meta_names))
    names(meta_names) <- replace_meta

    df <- df %>%
      rename(!!!meta_names)
  }

  return(df)

}


#' Combine extracted dataitems
#'
#' @details
#' Safely combines extracted dataitems in preparation for using
#' [evaluate_comparison()]. If the data classes of the primary value differ
#' between the tables, then their missingness patterns are returned instead.
#'
#' @param a an extracted data item returned by [extract()]
#' @param b an extracted data item returned by [extract()]
#'
#' @return a tibble with combined data
#' @export
#' @md
#'
#' @examples
combine <- function(a, b) {
  if (identical(class(a$value), class(b$value))) {
    bind_rows(a, b)
  } else {
    a$value <- !is.na(a$value)
    b$value <- !is.na(b$value)
    bind_rows(a, b)
  }
}

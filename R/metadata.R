#' Produce CC-HIC data dictionary
#'
#' Uses the variable table of the CC-HIC database to produce a lightweight data
#' dictionary
#'
#' @param metadata CC-HIC variables table
#'
#' @importFrom dplyr select mutate filter
#' @importFrom rlang .data
#'
#' @return a data dictionary
#' @export
make_dict <- function(metadata = .variables) {
  if (is.null(metadata)) {
    stop("You need to provide a metadata (variables) table")
  }

  data_columns <- metadata %>%
    select(-c(.data$code_name, .data$long_name, .data$primary_column)) %>%
    colnames()

  statics <- metadata %>%
    mutate(nas = metadata %>%
      select(data_columns) %>%
      apply(1, function(x) sum(!is.na(x)))) %>%
    filter(nas == 1) %>%
    select(c(.data$code_name, .data$long_name, .data$primary_column)) %>%
    mutate(type = "1d")

  dynamics <- metadata %>%
    filter(!(.data$code_name %in% statics$code_name)) %>%
    select(c(.data$code_name, .data$long_name, .data$primary_column)) %>%
    mutate(type = "2d")

  rbind(statics, dynamics)
}

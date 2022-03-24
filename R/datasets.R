#' Data quality reference table for CC-HIC.
#'
#' Version 1. A dataset containing useful metadata of each dataitem in CC-HIC
#'
#' @format A \code{tibble} with 255 rows and 13 variables:
#' \describe{
#'   \item{code_name}{the NIHR_HIC_ICU number}
#'   \item{short_name}{a short name}
#'   \item{long_name}{a longer name}
#'   \item{primary_column}{the primary column used for data storage in the
#'     events table of the cc-hic database}
#'   \item{type}{1d/2d}
#'   \item{class}{hic data class}
#'   \item{ranges}{the lower and upper bounds of a possible value}
#'   \item{possible_values}{a list column with possible values the dataitem can take}
#'   \item{periodicity}{the lower and upper bounds of anticipated periodicity}
#'   \item{prop_missing}{the proportion of episodes that are expected to be
#'     missing the data item}
#'   \item{assumed_units}{the assumed units}
#'   \item{dist_compare}{indicator to which distributional check to be
#'     performed}
#'   \item{notes}{any other notes}
#'
#' }
"qref"

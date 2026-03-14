#' Keep only rows for Brazilian states
#'
#' Filters `x` to rows whose `state` value is one of the abbreviations stored in
#' the internal `brazilian_states` object.
#'
#' @param x A data frame containing a `state` column.
#'
#' @return A filtered data frame containing only rows with valid Brazilian state
#'   abbreviations in `state`.
#'
#' @details
#' The function stops if `x` does not contain a `state` column.
#'
#' @examples
#' df <- data.frame(state = c("SP", "RJ", "ND"), value = 1:3)
#' filter_states(df)
#'
#' @export
filter_states <- function(x) {
  if (!"state" %in% names(x)) {
    stop("The 'state' column does not exist.")
  }
  dplyr::filter(x, state %in% brazilian_states)
}

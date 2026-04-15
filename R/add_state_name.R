#' Add state names from Comex Stat state abbreviations
#'
#' Joins the package state reference table to `x` using the `state` column and
#' appends `state_name`.
#'
#' @param x A data frame containing a `state` column with Brazilian state
#'   abbreviations.
#' @param drop_code Logical. If `TRUE` (default), removes `state` from the
#'   result after the join.
#'
#' @return A data frame with the same rows as `x`, plus `state_name`.
#'
#' @examples
#' df <- data.frame(state = c("SP", "RJ"))
#' add_state_name(df)
#'
#' @export
add_state_name <- function(
    x,
    drop_code = TRUE
) {
  state_table <- get_state_table(verbose = FALSE)

  temp <- dplyr::left_join(
    x,
    dplyr::select(state_table, state, state_name),
    by = "state"
  )

  temp <- dplyr::relocate(
    temp,
    state_name,
    .after = state
  )

  if (drop_code) {
    temp <- dplyr::select(temp, -state)
  }

  return(temp)

}

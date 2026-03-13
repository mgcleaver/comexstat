#' Add state name based on Comex Stat's state code
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

  if (drop_code) {
    temp <- dplyr::select(temp, -state)
  }

  return(temp)

}
